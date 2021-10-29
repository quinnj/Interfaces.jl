module Interfaces

export isinterfacetype, @interface, @implements, InterfaceImplementationError

struct InterfaceImplementationError <: Exception
    msg::String
end

isinterfacetype(::Type{T}) where {T} = false

function interface end

struct Interface
    name::Symbol
    exprs::Vector{Any}
end

# TODO: custom show for Interface

function implements end

function implemented(f, args, mods)
    impls = Base.methods(f, args, mods)
    return length(impls) > 0
end

function recursiveswapT!(T::Symbol, expr)
    for i = 1:length(expr.args)
        arg = expr.args[i]
        if arg === T
            expr.args[i] = :T
        elseif arg isa Expr
            recursiveswapT!(T, arg)
        end
    end
    return
end

function convertargs(T, nm, args)
    isempty(args) && throw(ArgumentError("invalid `$T` interface method with zero arguments: `$nm()`"))
    for i = 1:length(args)
        arg = args[i]
        if arg isa Symbol
            args[i] = :Any
        elseif arg.head === :(::) && length(arg.args) == 1
            recursiveswapT!(T, arg)
            args[i] = arg.args[1]
        elseif arg.head === :(::) && length(arg.args) == 2
            recursiveswapT!(T, arg)
            args[i] = arg.args[2]
        else
            throw(ArgumentError("invalid `$T` interface method argument for method `$nm`: `$arg`"))
        end
    end
    return Expr(:curly, :Tuple, args...)
end

unconvertargs(::Type{T}) where {T <: Tuple} = Any[Expr(:(::), fieldtype(T, i)) for i = 1:fieldcount(T)]

function methodparts(T, x::Expr)
    @assert x.head === :call
    methodname = x.args[1]
    args = convertargs(T, methodname, x.args[2:end])
    return methodname, args
end

requiredmethod(T, nm, args, shouldthrow) = :(Interfaces.implemented($nm, $args, mods) || ($shouldthrow && Interfaces.missingmethod($T, $nm, $args, mods)))

@noinline missingmethod(T, f, args, mods) = throw(InterfaceImplementationError("missing `$T` interface method definition: `$(Expr(:call, f, unconvertargs(args)...))`, in module(s): `$mods`"))
@noinline invalidreturntype(T, f, args, RT1, RT2) = throw(InterfaceImplementationError("invalid return type for `$T` interface method definition: `$(Expr(:call, f, unconvertargs(args)...))`; inferred $RT1, required $RT2"))
@noinline atleastonerequired(T, expr) = throw(InterfaceImplementationError("for `$T` interface, one of the following method definitions is required: `$expr`"))

function toimplements!(T::Symbol, arg::Expr, shouldthrow::Bool=true)
    if arg.head == :call
        nm, args = methodparts(T, arg)
        return requiredmethod(T, nm, args, shouldthrow)
    elseif arg.head == :(::)
        nm, args = methodparts(T, arg.args[1])
        RT = arg.args[2]
        return quote
            check = $(requiredmethod(T, nm, args, shouldthrow))
            RT = Base._return_type($nm, $args)
            check |= Interfaces.isinterfacetype($RT) ?
                Interfaces.implements(RT, $RT) : RT <: $RT
            check || ($shouldthrow && Interfaces.invalidreturntype($nm, $args, RT, $RT))
        end
    elseif arg.head == :if
        origarg = arg
        recursiveswapT!(T, arg.args[1])
        arg.args[2] = toimplements!(T, arg.args[2])
        while length(arg.args) > 2
            if arg.args[3].head == :elseif
                arg = arg.args[3]
                recursiveswapT!(T, arg.args[1])
                arg.args[2] = toimplements!(T, arg.args[2])
            else
                # else block
                arg.args[3] = toimplements!(T, arg.args[3])
                break
            end
        end
        return origarg
    elseif arg.head == :||
        argcopy = copy(arg)
        origarg = arg
        while arg.head == :||
            arg.args[1] = toimplements!(T, arg.args[1], false)
            arg = arg.args[2]
        end
        arg.args[2] = toimplements!(T, arg.args[2], false)
        return :(($origarg) || Interfaces.atleastonerequired($T, $argcopy))
    elseif arg.head == :block
        map!(x -> toimplements!(T, x, shouldthrow), arg.args, arg.args)
        return arg
    else
        throw(ArgumentError("unsupported expression in @interface block for `$T`: `$arg`"))
    end
end

macro interface(T, block)
    @assert T isa Symbol
    @assert block isa Expr && block.head == :block
    Base.remove_linenums!(block)
    iface = Interface(T, deepcopy(block.args))
    filter!(x -> !(x isa String), block.args)
    toimplements!(T, block)
    return esc(quote
        Interfaces.isinterfacetype(::Type{$T}) = true
        Interfaces.interface(::Type{$T}) = $iface
        function Interfaces.implements(::Type{T}, ::Type{$T}; mods::Vector{Module}=[parentmodule(T)]) where {T}
            $block
        end
    end)
end

macro implements(T, IT)
    return esc(quote
        @assert Interfaces.implements($T, $IT)
        Interfaces.implements(::Type{$T}, ::Type{$IT}) = true
    end)
end

end # module

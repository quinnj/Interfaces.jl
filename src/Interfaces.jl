module Interfaces

export isinterfacetype, @interface, @implements, InterfaceImplementationError

struct InterfaceImplementationError <: Exception
    msg::String
end

isinterfacetype(::Type{T}) where {T} = false
isinterfacetype(::Type{Type{T}}) where {T} = isinterfacetype(T)

function interface end

struct Interface
    name
    exprs::Vector{Any}
end

function Base.show(io::IO, x::Interface)
    println(io, "Interface: $(x.name)")
    foreach(ex -> showexpr(io, ex), x.exprs)
    return
end

unwrapblock(ex::Expr) = ex.head == :block ? ex.args[1] : ex

function showexpr(io::IO, arg, indent=1)
    if arg.head == :call
        println(io, "  "^indent * "* method definition: `$arg`")
    elseif arg.head == :(::)
        println(io, "  "^indent * "* method definition with required return type: `$arg`")
    elseif arg.head == :<:
        println(io, "  "^indent * "* subtyping: `T <: $(arg.args[2])`")
    elseif arg.head == :if
        println(io, "  "^indent * "* conditional requirements:")
        println(io, "  "^(indent + 1) * "* if $(unwrapblock(arg.args[1]))")
        showexpr(io, arg.args[2], indent + 2)
        while length(arg.args) > 2
            if arg.args[3].head == :elseif
                arg = arg.args[3]
                println(io, "  "^(indent + 1) * "* elseif $(unwrapblock(arg.args[1]))")
                showexpr(io, arg.args[2], indent + 2)
            else
                # else block
                println(io, "  "^(indent + 1) * "* else")
                showexpr(io, arg.args[3], indent + 2)
                break
            end
        end
    elseif arg.head == :||
        println(io, "  "^indent * "* one of the following required:")
        while true
            showexpr(io, arg.args[1], indent + 1)
            arg.args[2].head == :|| || break
            arg = arg.args[2]
        end
        showexpr(io, arg.args[2], indent + 1)
    elseif arg.head == :block
        for x in arg.args
            showexpr(io, x, indent)
        end
    elseif arg.head == :macrocall && arg.args[1] == Symbol("@optional")
        println(io, "  "^indent * "* optional interface requirement:")
        showexpr(io, arg.args[3], indent + 1)
    end
    return
end

function implements end

implements(::Type{Type{T}}, ::Type{Type{IT}}, mods::Vector{Module}=[parentmodule(T)]) where {T, IT} =
    implements(T, IT, mods)

function implemented(f, args, mods)
    impls = Base.methods(f, args, mods)
    return length(impls) > 0
end

"""
Replace all arguments `T` with `sym` (default `:T`) in the given `expr::Expr`.

Used to swap `::SomeInterface` -> `::T`, because `@interface` uses syntax like:

    @interface SomeInterface begin
        foo(::SomeInterface)
    end

to express "`T` implements the interface `SomeInterface` if there is a method `foo(::T)`",
i.e. we need there to be a `foo` method that accepts the type `T` not the type `SomeInterface`.
"""
function recursiveswapsymbols!(IT, expr, sym=:T)
    for i = 1:length(expr.args)
        arg = expr.args[i]
        if arg == IT
            expr.args[i] = sym
        elseif arg isa Expr
            recursiveswapsymbols!(IT, arg, sym)
        end
    end
    return
end

"""
Convert arguments to just their type constraints.
Used as part of converting method signature syntax to a something we can check for with
`methods(f, args)`.

    # Given `foo(x::Int, ::Float64, z)`
    args = [:(1::Int), :(::Float64), :z]
    convertargs(:SomeInterface, :foo, args) == :(Tuple{Int, Float64, Any})
"""
function convertargs(IT, nm, args)
    isempty(args) && throw(ArgumentError("invalid `$IT` interface method with zero arguments: `$nm()`"))
    for i = 1:length(args)
        arg = args[i]
        if arg isa Symbol
            args[i] = :Any
        elseif arg.head === :(::) && length(arg.args) == 1
            recursiveswapsymbols!(IT, arg)
            args[i] = arg.args[1]
        elseif arg.head === :(::) && length(arg.args) == 2
            recursiveswapsymbols!(IT, arg)
            args[i] = arg.args[2]
        else
            throw(ArgumentError("invalid `$IT` interface method argument for method `$nm`: `$arg`"))
        end
    end
    return Expr(:curly, :Tuple, args...)
end

unconvertargs(::Type{T}) where {T <: Tuple} = Any[Expr(:(::), fieldtype(T, i)) for i = 1:fieldcount(T)]

"""
Extract the (quoted) function name and argument types from a method signature.

    methodparts(:SomeInterface, :(foo(x::Int, ::Float64, z))) == (:foo, :(Tuple{Int, Float64, Any}))
"""
function methodparts(IT, x::Expr)
    @assert x.head === :call
    methodname = x.args[1]
    args = convertargs(IT, methodname, x.args[2:end])
    return methodname, args
end

requiredmethod(IT, nm, args, shouldthrow) = :(Interfaces.implemented($nm, $args, mods) || ($shouldthrow && Interfaces.missingmethod($IT, $nm, $args, mods)))

function requiredreturn(T, nm, args, shouldthrow, RT_sym, __RT__)
    return quote
        check = $(requiredmethod(T, nm, args, shouldthrow))
        $RT_sym = Interfaces.returntype($nm, $args)
        # @show $RT_sym, $nm, $args, Interfaces.isinterfacetype($__RT__)
        check |= Interfaces.isinterfacetype($__RT__) ?  Interfaces.implements($RT_sym, $__RT__) : $RT_sym <: $__RT__
        check || ($shouldthrow && Interfaces.invalidreturntype($nm, $args, $RT_sym, $__RT__))
    end
end

@noinline missingmethod(IT, f, args, mods) = throw(InterfaceImplementationError("missing `$IT` interface method definition: `$(Expr(:call, f, unconvertargs(args)...))`, in module(s): `$mods`"))
@noinline invalidreturntype(IT, f, args, RT1, RT2) = throw(InterfaceImplementationError("invalid return type for `$IT` interface method definition: `$(Expr(:call, f, unconvertargs(args)...))`; inferred $RT1, required $RT2"))
@noinline subtypingrequired(IT, T) = throw(InterfaceImplementationError("interface `$IT` requires implementing types to subtype, like: `struct $T <: $IT`"))
@noinline atleastonerequired(IT, expr) = throw(InterfaceImplementationError("for `$IT` interface, one of the following method definitions is required: `$expr`"))

function toimplements!(IT, arg::Expr, shouldthrow::Bool=true)
    if arg.head == :call
        # required method definition
        nm, args = methodparts(IT, arg)
        return requiredmethod(IT, nm, args, shouldthrow)
    elseif arg.head == :(::)
        # required method definition and required return type
        nm, args = methodparts(IT, arg.args[1])
        annotation = arg.args[2]
        if !isa(annotation, Symbol) && annotation.head == :where
            # `::(T where T)` or `::(T where T<:Foo)`
            sym = annotation.args[1]
            __RT__ = annotation.args[2] isa Symbol ? Any : annotation.args[2].args[2]
        else # `::Foo` or `::Union{Foo,Bar}` or `::Type{Foo}`
            sym = gensym()
            __RT__ = annotation
        end
        return requiredreturn(IT, nm, args, shouldthrow, sym, __RT__)
    elseif arg.head == :<:
        return :((T <: $IT) || Interfaces.subtypingrequired($IT, T))
    elseif arg.head == :if
        # conditional requirement
        origarg = arg
        recursiveswapsymbols!(IT, arg.args[1])
        arg.args[2] = toimplements!(IT, arg.args[2])
        while length(arg.args) > 2
            if arg.args[3].head == :elseif
                arg = arg.args[3]
                recursiveswapsymbols!(IT, arg.args[1])
                arg.args[2] = toimplements!(IT, arg.args[2])
            else
                # else block
                arg.args[3] = toimplements!(IT, arg.args[3])
                break
            end
        end
        return origarg
    elseif arg.head == :||
        # one of many required
        argcopy = copy(arg)
        origarg = arg
        while true
            arg.args[1] = toimplements!(IT, arg.args[1], false)
            arg.args[2].head == :|| || break
            arg = arg.args[2]
        end
        arg.args[2] = toimplements!(IT, arg.args[2], false)
        return :(($origarg) || Interfaces.atleastonerequired($IT, $(Meta.quot(argcopy))))
    elseif arg.head == :block
        # not expected at top-level of @interface block
        # but can be block of if-else or || expressions
        map!(x -> toimplements!(IT, x, shouldthrow), arg.args, arg.args)
        return arg
    elseif arg.head == :macrocall && arg.args[1] == Symbol("@optional")
        return quote
            if @isdefined(debug) && debug
                $(toimplements!(IT, arg.args[3]))
            else
                true
            end
        end
    else
        io = IOBuffer()
        dump(io, arg)
        ex = String(take!(io))
        throw(ArgumentError("unsupported expression in @interface block for `$IT`: `$ex`"))
    end
end

macro interface(IT, alias_or_block, maybe_block=nothing)
    @assert IT isa Symbol || IT.head == :.
    if alias_or_block isa Symbol
        alias = alias_or_block
        block = maybe_block
    else
        alias = IT
        block = alias_or_block
    end
    @assert block isa Expr && block.head == :block
    Base.remove_linenums!(block)
    if IT !== alias
        # Swap the alias for the InterfaceType itself, so later we can generically swap the
        # InterfaceType for an arbitrary symbol e.g. :T, regardless of if an alias was used.
        recursiveswapsymbols!(alias, block, IT)
    end
    iface = Interface(IT, deepcopy(block.args))
    filter!(x -> !(x isa String), block.args)
    toimplements!(IT, block)
    return esc(quote
        Interfaces.isinterfacetype(::Type{$IT}) = true
        Interfaces.interface(::Type{$IT}) = $iface
        function Interfaces.implements(::Type{T}, ::Type{$IT}, mods::Vector{Module}=[parentmodule(T)]) where {T}
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

@noinline returntype(@nospecialize(f), @nospecialize(args)) = Base.return_types(f, args)[1]

end # module

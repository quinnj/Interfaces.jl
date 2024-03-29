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

function Base.show(io::IO, ::MIME"text/plain", x::Interface)
    print(io, "Interface: $(x.name)")
    foreach(ex -> showexpr(io, ex), x.exprs)
    return
end

unwrapblock(ex::Expr) = ex.head == :block ? ex.args[1] : ex

function showexpr(io::IO, arg, indent=1)
    if arg.head == :call
        print(io, "\n" * "  "^indent * "* method definition: `$arg`")
    elseif arg.head == :(::)
        print(io, "\n" * "  "^indent * "* method definition with required return type: `$arg`")
    elseif arg.head == :<:
        print(io, "\n" * "  "^indent * "* subtyping: `T <: $(arg.args[2])`")
    elseif arg.head == :if
        print(io, "\n" * "  "^indent * "* conditional requirements:")
        print(io, "\n" * "  "^(indent + 1) * "* if $(unwrapblock(arg.args[1]))")
        showexpr(io, arg.args[2], indent + 2)
        while length(arg.args) > 2
            if arg.args[3].head == :elseif
                arg = arg.args[3]
                print(io, "\n" * "  "^(indent + 1) * "* elseif $(unwrapblock(arg.args[1]))")
                showexpr(io, arg.args[2], indent + 2)
            else
                # else block
                print(io, "\n" * "  "^(indent + 1) * "* else")
                showexpr(io, arg.args[3], indent + 2)
                break
            end
        end
    elseif arg.head == :||
        print(io, "\n" * "  "^indent * "* one of the following required:")
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
        print(io, "\n" * "  "^indent * "* optional interface requirement:")
        showexpr(io, arg.args[3], indent + 1)
    end
    return
end

function implements end

implements(::Type{Type{T}}, ::Type{Type{IT}}, mods::Vector{Module}=[parentmodule(T)]; debug::Bool=false) where {T, IT} =
    implements(T, IT, mods; debug=debug)

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

"""
Construct the method signature from a function name and argument types.

    methodsig(:foo, Tuple{Int64, Float64, Any}) == :(foo(::Int64, ::Float64, ::Any))
"""
methodsig(f, args) = Expr(:call, f, unconvertargs(args)...)

function requiredmethod(IT, nm, args, shouldthrow)
    return quote
        check = Interfaces.implemented($nm, $args, mods)
        check || ($shouldthrow && Interfaces.missingmethod(loglevel, $IT, $nm, $args, mods))
        check
    end
end

function requiredreturn(IT, nm, args, shouldthrow, RT_sym, __RT__)
    return quote
        check = $(requiredmethod(IT, nm, args, shouldthrow))
        if check
            $RT_sym = Interfaces.returntype($nm, $args)
            # @show $RT_sym, $nm, $args, Interfaces.isinterfacetype($__RT__)
            check &= Interfaces.isinterfacetype($__RT__) ?  Interfaces.implements($RT_sym, $__RT__) : $RT_sym <: $__RT__
            check || ($shouldthrow && Interfaces.invalidreturntype(loglevel, $IT, $nm, $args, $RT_sym, $__RT__))
        end
        check
    end
end

@noinline function missingmethod(loglevel, IT, f, args, mods)
    log_or_throw(loglevel, "missing `$IT` interface method definition: `$(Interfaces.methodsig(f, args))`, in module(s): `$mods`")
end

@noinline function invalidreturntype(loglevel, IT, f, args, RT1, RT2)
    log_or_throw(loglevel, "invalid return type for `$IT` interface method definition: `$(methodsig(f, args))`; inferred $RT1, required $RT2")
end

@noinline function subtypingrequired(loglevel, IT, T)
    log_or_throw(loglevel, "interface `$IT` requires implementing types to subtype, like: `struct $T <: $IT`")
end

@noinline function atleastonerequired(loglevel, IT, expr)
    log_or_throw(loglevel, "for `$IT` interface, one of the following method definitions is required: `$expr`")
end

function log_or_throw(loglevel, msg)
    if loglevel === :required
        log_required(msg)
    elseif loglevel === :optional
        log_optional(msg)
    else
        throw(InterfaceImplementationError(msg))
    end
end

function log_required(msg)
    printstyled("[ InterfaceImplementationError: "; color=Base.error_color())
    println(msg)
end

function log_optional(msg)
    printstyled("[ @optional definition missing: "; color=Base.warn_color())
    println(msg)
end

# `shouldthrow` indicates whether or not it is an error for a particular expression not
# to be satisfied. We set this to false for the individual expressions in `ex1 || ex2`.
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
        RT = arg.args[2]
        if RT == IT
            return quote
                check = T <: $RT
                check || Interfaces.subtypingrequired(loglevel, $RT, T)
                check
            end
        else
            return quote
                check = Interfaces.isinterfacetype($RT) ? Interfaces.implements(T, $RT, mods) : T <: $RT
                check || Interfaces.subtypingrequired(loglevel, $RT, T)
                check
            end
        end
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
        return quote
            check = $origarg
            check || Interfaces.atleastonerequired(loglevel, $IT, $(Meta.quot(argcopy)))
            check
        end
    elseif arg.head == :block
        # not expected at top-level of @interface block
        # but can be block of if-else or || expressions
        map!(x -> toimplements!(IT, x, shouldthrow), arg.args, arg.args)
        return arg
    elseif arg.head == :macrocall && arg.args[1] == Symbol("@optional")
        return quote
            if debug
                oldlvl = loglevel
                loglevel = :optional
                check = $(toimplements!(IT, arg.args[3], shouldthrow))
                loglevel = oldlvl
                check
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
        function Interfaces.implements(::Type{T}, ::Type{$IT}, mods::Vector{Module}=[parentmodule(T)]; debug::Bool=false) where {T}
            loglevel = debug ? :required : :none
            $block
        end
    end)
end

macro implements(T, IT, debug=false)
    x = debug isa Bool ? debug :
        (debug isa Expr && debug.head == :(=)) ? debug.args[2] :
        throw(ArgumentError("unsupported argument in @implements: `$debug`"))
    return esc(quote
        @assert Interfaces.implements($T, $IT; debug=$x)
        Interfaces.implements(::Type{$T}, ::Type{$IT}) = true
    end)
end

@noinline returntype(@nospecialize(f), @nospecialize(args)) = Base.return_types(f, args)[1]

end # module

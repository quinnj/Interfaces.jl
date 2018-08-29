module Interfaces

"""
```julia
@interface Indexable begin
    getindex(::Indexable, ::Int)
    setindex!(::Indexable, _, ::Int)
    firstindex(::Indexable)
    lastindex(::Indexable)
end
```

expands to

```julia
struct Indexable <: Interface end

Interfaces.interface(::Type{Indexable}) =
    And([
        HasMethod(getindex, T->Tuple{T, Int}),
        HasMethod(setindex!, T->Tuple{T, Union{}, Int}),
        HasMethod(firstindex, T->Tuple{T}),
        HasMethod(lastindex, T->Tuple{T})
    ])

struct Table <: Interface end

Interfaces.interface(::Type{Table}) =
    And([
        HasMethod(Tables.schema, x->Tuple{x}),
        HasMethod(Tables.AccessStyle, x->Tuple{x}),
        If([Condition(x->Tables.AccessStyle(x)===Tables.RowAccess())=>HasMethod(Tables.rows, x->Tuple{x}),
            Condition(x->Tables.AccessStyle(x)===Tables.ColumnAccess())=>HasMethod(Tables.columns, x->Tuple{x}),
           ], False())
    ])
```
"""
macro interface(T, block)

end

function interface end
interface(::Type{T}) where {T} = True()

function isa end

isa(x::X, ::Type{I}) where {X, I} = isa(X, I)
isa(::Type{T}, ::Type{S}) where {T, S} = satisfies(T, interface(S))

abstract type Interface end

abstract type InterfaceNode end

struct True <: InterfaceNode end
satisfies(::Type{T}, ::True) where {T} = true

struct False <: InterfaceNode end
satisfies(::Type{T}, ::False) where {T} = false

struct And{T} <: InterfaceNode
    nodes::T
end
And(x...) = And(x)

satisfies(::Type{T}, n::And) where {T} = _satisfies(T, n, n.nodes...)
_satisfies(::Type{T}, n::And, i...) where {T} = satisfies(T, i[1]) && _satisfies(T, n, Base.tail(i)...)
_satisfies(::Type{T}, n::And) where {T} = true

struct HasMethod{F} <: InterfaceNode
    f::F
    argtypes # T->Tuple{T, ...}
end

satisfies(::Type{T}, h::HasMethod) where {T} = hasmethod(h.f, h.argtypes(T))

struct IsaInterface{I} <: InterfaceNode end
satisfies(::Type{T}, i::IsaInterface{I}) where {T, I} = isa(T, I)

struct If{T, D} <: InterfaceNode
    conditions::T
    default::D
end

satisfies(::Type{T}, i::If) where {T} = _satisfies(T, i, i.conditions)
_satisfies(::Type{T}, i::If, x::Tuple) where {T} = satisfies(T, x[1].first) ? satisfies(T, x[1].second) : _satisfies(T, i, Base.tail(x))
_satisfies(::Type{T}, i::If, x::Pair) where {T} =  satisfies(T, x.first) ? satisfies(T, x.second) : satisfies(T, i.default)

struct Condition{F} <: InterfaceNode
    condition::F
end

satisfies(::Type{T}, c::Condition) where {T} = c.condition(T)

"""
```julia
@implements Array Indexable
```

expands to

```julia
Interfaces.isa(Array, Indexable) || throw(InterfaceError(Array, Indexable))
Interfaces.isa(::Type{<:Array}, ::Type{Indexable}) = true
```
"""
macro implements(T, I)

end

struct InterfaceError <: Exception
    T
    I
end

function Base.showerror(io::IO, e::InterfaceError)
    println(io, "$(e.T) doesn't satisfy the $(e.I) interface")
end

struct Iterable <: Interface end

Interfaces.interface(::Type{Iterable}) =
    And(
        HasMethod(Base.iterate, T->Tuple{T}),
        HasMethod(Base.iterate, T->Tuple{T, Union{}}),
        If((Condition(T->Base.isa(Base.IteratorSize(T), Base.HasShape))=>HasMethod(size, T->Tuple{T}),
            Condition(T->Base.IteratorSize(T) === Base.HasLength())=>HasMethod(length, T->Tuple{T})
        ), True()),
        If((Condition(T->Base.IteratorEltype(T) === Base.HasEltype())=>HasMethod(eltype, T->Tuple{T})
        ), True()),
    )

end # module

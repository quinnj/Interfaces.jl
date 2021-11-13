using Test, Interfaces, Tables

abstract type Iterable end

@interface Iterable begin

    iterate(::Iterable)
    iterate(::Iterable, st)

    Base.IteratorSize(::Type{Iterable})::Union{Base.HasLength, Base.HasShape, Base.IsInfinite, Base.SizeUnknown}

    if Base.IteratorSize(Iterable) == Base.HasLength()
        length(::Iterable)
    elseif Base.IteratorSize(Iterable) isa Base.HasShape
        length(::Iterable)
        size(::Iterable)
    end

    Base.IteratorEltype(::Type{Iterable})::Union{Base.HasEltype, Base.EltypeUnknown}

    if Base.IteratorEltype(Iterable) == Base.HasEltype()
        eltype(::Iterable) || eltype(::Type{Iterable})
    end

end

@test Interfaces.implements(Int, Iterable, [Base])

abstract type Indexable end

@interface Indexable begin
    getindex(::Indexable, i)
    setindex!(::Indexable, v, i)
    firstindex(::Indexable)
    lastindex(::Indexable)
end

@test Interfaces.implements(Array, Indexable, [Base])

@interface AbstractArray T begin

    T <: AbstractArray
    size(::T)
    Base.IndexStyle(::Type{T})::Union{Base.IndexLinear, Base.IndexCartesian}

    if Base.IndexStyle(T) == Base.IndexLinear()
        getindex(::T, i::Int)
        setindex!(::T, v, i::Int)
    elseif Base.IndexStyle(T) == Base.IndexCartesian()
        getindex(::T, I::Vararg{Int})
        setindex!(::T, v, I::Vararg{Int})
    end

end

@test Interfaces.implements(Array, AbstractArray, [Base])

struct Foo end

@test_throws InterfaceImplementationError Interfaces.implements(Foo, AbstractArray)

@test_throws InterfaceImplementationError Interfaces.implements(Foo, Iterable)

@interface Tables.AbstractColumns T begin
    Tables.getcolumn(::T, ::Int)
    Tables.getcolumn(::T, ::Symbol)
    Tables.columnnames(::T)
end

@interface Tables.AbstractRow T begin
    Tables.getcolumn(::T, ::Int)
    Tables.getcolumn(::T, ::Symbol)
    Tables.columnnames(::T)
end

abstract type AbstractTable end

@interface AbstractTable begin
    Tables.isrowtable(::Type{AbstractTable}) ||
        Tables.rowaccess(::Type{AbstractTable}) ||
            Tables.columnaccess(::Type{AbstractTable})
    if Tables.isrowtable(AbstractTable) || Tables.rowaccess(AbstractTable)
        Tables.rows(::AbstractTable)::RT where {RT <: Iterable}
        if Base.IteratorEltype(RT) == Base.HasEltype()
            eltype(::RT)::Type{Tables.AbstractRow} ||
                eltype(::Type{RT})::Type{Tables.AbstractRow}
        end
    elseif Tables.columnaccess(AbstractTable)
        Tables.istable(::Type{AbstractTable})
        Tables.columns(::AbstractTable)::Tables.AbstractColumns
    end

    @optional Tables.schema(::AbstractTable)::Union{Nothing, Tables.Schema}
end

@test Interfaces.implements(Tables.DictRowTable, AbstractTable)
@test Interfaces.implements(Tables.DictColumnTable, AbstractTable)

abstract type Example end

@interface Example T begin

    # required subtyping
    T <: Example

    # required method definition
    foo1(arg::Example, arg2::Int)

    # method def w/ return type
    foo2(arg::Example)::Float64

    # method def w/ interface return type
    foo3(arg::Example)::Iterable

    # one of many required
    foo4(arg::Example) || foo5(arg::Example) || foo6(arg::Example)

    # conditional
    if foo7(T)
        foo8(arg::Example)
        foo9(arg::Example)::RT where {RT <: Iterable}
        foo10(::RT)::Int
    elseif foo11(T)
        foo12(arg::Example)
    else
        foo13(arg::Example) || foo14(arg::Example)
    end

    @optional foo15(arg::Example)

end

@test repr("text/plain", Interfaces.interface(Example)) == """
Interface: Example
  * subtyping: `T <: Example`
  * method definition: `foo1(arg::Example, arg2::Int)`
  * method definition with required return type: `foo2(arg::Example)::Float64`
  * method definition with required return type: `foo3(arg::Example)::Iterable`
  * one of the following required:
    * method definition: `foo4(arg::Example)`
    * method definition: `foo5(arg::Example)`
    * method definition: `foo6(arg::Example)`
  * conditional requirements:
    * if foo7(Example)
      * method definition: `foo8(arg::Example)`
      * method definition with required return type: `foo9(arg::Example)::(RT where RT <: Iterable)`
      * method definition with required return type: `foo10(::RT)::Int`
    * elseif foo11(Example)
      * method definition: `foo12(arg::Example)`
    * else
      * one of the following required:
        * method definition: `foo13(arg::Example)`
        * method definition: `foo14(arg::Example)`
  * optional interface requirement:
    * method definition: `foo15(arg::Example)`"""

abstract type AbstractStridedArray end

@interface AbstractStridedArray A begin
    A <: AbstractArray
    strides(::A)
    Base.unsafe_convert(::Type{<:Ptr}, ::A)
    Base.elsize(::Type{<:A})
    @optional stride(::A, i::Int)
end

@test Interfaces.implements(Vector{Int}, AbstractStridedArray, [Base])

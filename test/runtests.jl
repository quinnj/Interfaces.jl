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

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

@interface AbstractArray begin

    <: AbstractArray
    size(::AbstractArray)
    Base.IndexStyle(::Type{AbstractArray})::Union{Base.IndexLinear, Base.IndexCartesian}

    if Base.IndexStyle(AbstractArray) == Base.IndexLinear()
        getindex(::AbstractArray, i::Int)
        setindex!(::AbstractArray, v, i::Int)
    elseif Base.IndexStyle(AbstractArray) == Base.IndexCartesian()
        getindex(::AbstractArray, I::Vararg{Int})
        setindex!(::AbstractArray, v, I::Vararg{Int})
    end

end

@test Interfaces.implements(Array, AbstractArray, [Base])

struct Foo end

@test_throws InterfaceImplementationError Interfaces.implements(Foo, AbstractArray)

@test_throws InterfaceImplementationError Interfaces.implements(Foo, Iterable)

@interface Tables.AbstractColumns begin
    Tables.getcolumn(::Tables.AbstractColumns, ::Int)
    Tables.getcolumn(::Tables.AbstractColumns, ::Symbol)
    Tables.columnnames(::Tables.AbstractColumns)
end

@interface Tables.AbstractRow begin
    Tables.getcolumn(::Tables.AbstractRow, ::Int)
    Tables.getcolumn(::Tables.AbstractRow, ::Symbol)
    Tables.columnnames(::Tables.AbstractRow)
end

abstract type AbstractTable end

@interface AbstractTable begin
    Tables.isrowtable(::Type{AbstractTable}) ||
        Tables.rowaccess(::Type{AbstractTable}) ||
            Tables.columnaccess(::Type{AbstractTable})
    if Tables.isrowtable(AbstractTable) || Tables.rowaccess(AbstractTable)
        Tables.rows(::AbstractTable) -> RT::Iterable
        if Base.IteratorEltype(RT) == Base.HasEltype()
            eltype(::RT)::Type{Tables.AbstractRow} ||
                eltype(::Type{RT})::Type{Tables.AbstractRow}
                
        end
    elseif Tables.columnaccess(AbstractTable)
        Tables.istable(::Type{AbstractTable})
        Tables.columns(::AbstractTable)::AbstractColumns
    end

    # @optional schema(::AbstractTable)::Union{Nothing, Tables.Schema}
end

@test Interfaces.implements(Tables.DictRowTable, AbstractTable)
using Test, Interfaces

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
        eltype(::Iterable)
    end

end

@test Interfaces.implements(Int, Iterable; mods=[Base])

struct Foo end

@test_throws InterfaceImplementationError Interfaces.implements(Foo, Iterable)

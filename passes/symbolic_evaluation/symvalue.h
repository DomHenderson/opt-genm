#pragma once

#include <cstdint>
#include <exception>
#include <string_view>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/StringRef.h>

#include "core/type.h"

class SymValue {
public:
    SymValue(Type type): type(type) {}

    enum class Kind {
        BOOL,
        BLOCKREF,
        EXTERN,
        FLOAT,
        FUNCREF,
        HEAPPTR,
        INT,
        STATICPTR,
        UNKNOWN
    };

    virtual Kind get_kind() const = 0;
    virtual Type get_type() const { return type; }
    virtual SymValue *copy_cast(Type type) const = 0;
private:
    Type type;
};

class OffsetOutOfBoundsException: public std::exception {};

class StaticPtrSymValue: public SymValue {
public:
    StaticPtrSymValue(std::string_view atomName, unsigned offset, unsigned max, Type type);
    StaticPtrSymValue(std::string_view atomName, llvm::APInt offset, unsigned max, Type type);
    Kind get_kind() const override { return Kind::STATICPTR; }
    std::string_view get_name() { return name; }
    llvm::APInt get_offset() { return offset; }
    unsigned get_max() { return max; }
    virtual StaticPtrSymValue *copy_cast(Type type) const override { return new StaticPtrSymValue(name, offset, max, type); }
    std::string toString() const;
private:
    std::string_view name;
    llvm::APInt offset;
    unsigned max;
};

class HeapPtrSymValue: public SymValue {
public:
    HeapPtrSymValue(std::string_view atomName, int offset, Type type);
    HeapPtrSymValue(std::string_view atomName, llvm::APInt offset, Type type);
    Kind get_kind() const override { return Kind::HEAPPTR; }
    std::string_view get_name() { return name; }
    llvm::APInt get_offset() { return offset; }
    virtual HeapPtrSymValue *copy_cast(Type type) const override { return new HeapPtrSymValue(name, offset, type); }
private:
    std::string_view name;
    llvm::APInt offset;
};

class BlockRefSymValue: public SymValue {
public:
    BlockRefSymValue(std::string_view name, Type type): SymValue(type), name(name) {}
    Kind get_kind() const override { return Kind::BLOCKREF; }
    std::string_view get_name() const { return name; }
    virtual BlockRefSymValue *copy_cast(Type type) const override { return new BlockRefSymValue(name, type); }
private:
    std::string_view name;
};

class BoolSymValue: public SymValue {
public:
    BoolSymValue(bool b, Type type): SymValue (type), b(b) {}
    Kind get_kind() const override { return Kind::BOOL; }
    bool get_value() const { return b; }
    virtual BoolSymValue *copy_cast(Type type) const override { return new BoolSymValue(b, type); }
private:
    const bool b;
};

class ExternSymValue: public SymValue {
public:
    ExternSymValue(std::string_view externName, Type type): SymValue(type), name(externName) {}
    Kind get_kind() const override { return Kind::EXTERN; }
    std::string_view get_name() { return name; }
    virtual ExternSymValue *copy_cast(Type type) const override { return new ExternSymValue(name, type); }
private:
    std::string_view name;
};

class FloatSymValue: public SymValue {
public:
    explicit FloatSymValue(float f, Type type);
    explicit FloatSymValue(double d, Type type);
    FloatSymValue(llvm::APFloat f, Type type): SymValue(type), val(f) {}
    Kind get_kind() const override { return Kind::FLOAT; }
    llvm::APFloat get_value() const { return val; }
    virtual FloatSymValue *copy_cast(Type type) const override { return new FloatSymValue(val, type); }
    std::string toString() const;
private:
    llvm::APFloat val;
};

class FuncRefSymValue: public SymValue {
public:
    FuncRefSymValue(std::string_view name, Type type): SymValue(type), name(name) {}
    Kind get_kind() const override { return Kind::FUNCREF; }
    std::string_view get_name() const { return name; }
    virtual FuncRefSymValue *copy_cast(Type type) const override { return new FuncRefSymValue(name, type); }
private:
    std::string_view name;
};

class IntSymValue: public SymValue {
public:
    IntSymValue(uint64_t value, Type type);
    IntSymValue(llvm::APInt value, Type type);
    Kind get_kind() const override { return Kind::INT; }
    llvm::APInt get_value() const { return val; }
    virtual IntSymValue *copy_cast(Type type) const override;
    std::string toString() const;
private:
    const llvm::APInt val;
};

class UnknownSymValue: public SymValue {
public:
    UnknownSymValue(Type type);
    UnknownSymValue(std::string name, Type type): SymValue(type), name(name) {}
    Kind get_kind() const override { return Kind::UNKNOWN; }
    virtual UnknownSymValue *copy_cast(Type type) const override { return new UnknownSymValue(type); }
    std::string get_name() const { return name; }
private:
    std::string name;
    static unsigned long previousIndex;
};

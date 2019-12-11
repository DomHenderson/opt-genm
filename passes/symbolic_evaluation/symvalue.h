#pragma once

#include <string_view>

#include <llvm/ADT/StringRef.h>

#include "core/type.h"

class SymValue {
public:
    SymValue(Type type): type(type) {}

    enum class Kind {
        ADDR,
        BOOL,
        EXTERN,
        FLOAT,
        FUNCREF,
        INT,
        UNKNOWN
    };

    virtual Kind get_kind() const = 0;
    virtual Type get_type() const { return type; }
    virtual SymValue *copy_cast(Type type) const = 0;
private:
    Type type;
};

class AddrSymValue: public SymValue {
public:
    AddrSymValue(std::string_view atomName, int offset, Type type): SymValue(type), name(atomName), offset(offset) {}
    Kind get_kind() const override { return Kind::ADDR; }
    std::string_view get_name() { return name; }
    int get_offset() { return offset; }
    virtual AddrSymValue *copy_cast(Type type) const override { return new AddrSymValue(name, offset, type); }
private:
    std::string_view name;
    int offset;
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
    FloatSymValue(float f, Type type): SymValue(type), val(f) {}
    Kind get_kind() const override { return Kind::FLOAT; }
    float get_value() const { return val; }
    virtual FloatSymValue *copy_cast(Type type) const override { return new FloatSymValue(val, type); }
private:
    // llvm::APFloat
    const float val;
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
    IntSymValue(int value, Type type): SymValue(type), val(value) {}
    Kind get_kind() const override { return Kind::INT; }
    int get_value() const { return val; }
    virtual IntSymValue *copy_cast(Type type) const override { return new IntSymValue(val, type); }
private:
    // llvm::APInt, llvm:APSInt
    const int val;
};

class UnknownSymValue: public SymValue {
public:
    UnknownSymValue(Type type): SymValue(type) {}
    Kind get_kind() const override { return Kind::UNKNOWN; }
    virtual UnknownSymValue *copy_cast(Type type) const override { return new UnknownSymValue(type); }
};

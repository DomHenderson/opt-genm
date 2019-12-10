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
        STR,
        UNKNOWN
    };

    virtual Kind get_kind() const = 0;
    virtual Type get_type() const { return type; }
private:
    Type type;
};

class AddrSymValue: public SymValue {
public:
    AddrSymValue(std::string_view atomName, int offset, Type type): SymValue(type), name(atomName), offset(offset) {}
    Kind get_kind() const override { return Kind::ADDR; }
    std::string_view get_name() { return name; }
    int get_offset() { return offset; }
private:
    std::string_view name;
    int offset;
};

class BoolSymValue: public SymValue {
public:
    BoolSymValue(bool b, Type type): SymValue (type), b(b) {}
    Kind get_kind() const override { return Kind::BOOL; }
    bool get_value() const { return b; }
private:
    const bool b;
};

class ExternSymValue: public SymValue {
public:
    ExternSymValue(std::string_view externName, Type type): SymValue(type), name(externName) {}
    Kind get_kind() const override { return Kind::EXTERN; }
    std::string_view get_name() { return name; }
private:
    std::string_view name;
};

class FloatSymValue: public SymValue {
public:
    FloatSymValue(float f, Type type): SymValue(type), val(f) {}
    Kind get_kind() const override { return Kind::FLOAT; }
    float get_value() const { return val; }
private:
    // llvm::APFloat
    const float val;
};

class FuncRefSymValue: public SymValue {
public:
    FuncRefSymValue(std::string_view name, Type type): SymValue(type), name(name) {}
    Kind get_kind() const override { return Kind::FUNCREF; }
    std::string_view get_name() const { return name; }
private:
    std::string_view name;
};

class IntSymValue: public SymValue {
public:
    IntSymValue(int value, Type type): SymValue(type), val(value) {}
    Kind get_kind() const override { return Kind::INT; }
    int get_value() const { return val; }
private:
    // llvm::APInt, llvm:APSInt
    const int val;
};

class StringSymValue: public SymValue {
public:
    StringSymValue(llvm::StringRef str, Type type): SymValue(type), str(str) {}
    Kind get_kind() const override { return Kind::STR; }
    llvm::StringRef get_string() { return str; }
private:
    llvm::StringRef str;
};

class UnknownSymValue: public SymValue {
public:
    UnknownSymValue(Type type): SymValue(type) {}
    Kind get_kind() const override { return Kind::UNKNOWN; }
};

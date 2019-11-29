#pragma once

#include <string_view>

class SymValue {
public:
    enum class Kind {
        BOOL,
        INT,
        FLOAT,
        FUNCREF,
        ATOM,
        UNKNOWN
    };

    virtual Kind get_kind() const = 0;
};

class BoolSymValue: public SymValue {
public:
    BoolSymValue(bool b): b(b) {}
    Kind get_kind() const override { return Kind::BOOL; }
    bool get_value() const { return b; }
private:
    const bool b;
};

class FloatSymValue: public SymValue {
public:
    FloatSymValue(float f): val(f) {}
    Kind get_kind() const override { return Kind::FLOAT; }
    float get_value() const { return val; }
private:
    // llvm::APFloat
    const float val;
};

class FuncRefSymValue: public SymValue {
public:
    FuncRefSymValue(std::string_view name): name(name) {}
    Kind get_kind() const override { return Kind::FUNCREF; }
    std::string_view get_name() const { return name; }
private:
    std::string_view name;
};

class IntSymValue: public SymValue {
    public:
    IntSymValue(int value): val(value) {}
    Kind get_kind() const override { return Kind::INT; }
    int get_value() const { return val; }
private:
    // llvm::APInt, llvm:APSInt
    const int val;
};

class UnknownSymValue : public SymValue {
public:
    UnknownSymValue() {}
    Kind get_kind() const override { return Kind::UNKNOWN; }
};

class AtomSymValue {
    Atom *atom;
    usigned Offset;
}
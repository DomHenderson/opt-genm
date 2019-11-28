#pragma once

#include <stack>
#include <unordered_map>
#include <vector>

class SymValue {
public:
    enum class Kind {
        ARG,
        BOOL,
        INT,
        FLOAT,
        FUNCREF,
        UNKNOWN
    };

    virtual Kind get_kind() const = 0;
};

class ArgSymValue : public SymValue {
public:
    ArgSymValue(unsigned id): id(id) {}
    Kind get_kind() const override { return Kind::ARG; }
private:
    const unsigned id;
};

class BoolSymValue: public SymValue {
public:
    BoolSymValue(bool b): b(b) {}
    Kind get_kind() const override { return Kind::BOOL; }
private:
    const bool b;
};

class FloatSymValue: public SymValue {
public:
    FloatSymValue(float f): val(f) {}
    Kind get_kind() const override { return Kind::FLOAT; }
private:
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
private:
    const int val;
};

class UnknownSymValue : public SymValue {
public:
    UnknownSymValue() {}
    Kind get_kind() const override { return Kind::UNKNOWN; }
};

class Frame {
public:
    Frame(unsigned arg_base): arg_base(arg_base) {}
    unsigned get_arg_base() const { return arg_base; }
private:
    unsigned arg_base;
};

class Stack {
public:
    void push_frame(std::vector<SymValue*> new_args);
    void pop_frame();
    Frame &top_frame();

    unsigned get_local_arg(unsigned id);
private:
    std::vector<SymValue*> args;
    std::stack<Frame> stack;
};
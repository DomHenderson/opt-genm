#pragma once

#include <stack>
#include <unordered_map>
#include <vector>

#include "core/inst.h"
#include "symvalue.h"

class Frame {
public:
    Frame(unsigned arg_base, Inst *callee): arg_base(arg_base), callee(callee) {}
    unsigned get_arg_base() const { return arg_base; }
    Inst *get_callee() const { return callee; }
private:
    unsigned arg_base;
    Inst *callee;
};

class Stack {
public:
    void push_frame(std::vector<SymValue*> new_args, Inst *calle = nullptr);
    void pop_frame();
    Frame &top_frame();

    unsigned get_local_arg(unsigned id);
private:
    std::vector<SymValue*> args;
    std::stack<Frame> stack;
};
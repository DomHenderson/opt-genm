#pragma once

#include <optional>
#include <stack>
#include <unordered_map>
#include <vector>

#include "core/inst.h"
#include "symvalue.h"

class Frame {
public:
    using Inst_iterator = llvm::ilist<Inst>::iterator;

    Frame(
        std::vector<SymValue*> args,
        Frame *previous,
        Inst *caller,
        std::optional<Inst_iterator> resumeInst
    ): args(args), previous(previous), caller(caller), resumeInst(resumeInst) {}
    SymValue *get_arg(unsigned idx) { return args[idx]; }
    Frame *get_previous() { return previous; }
    Inst *get_caller() { return caller; }
    std::optional<Inst_iterator> get_resume_inst() { return resumeInst; }
private:
    Frame *previous;
    std::vector<SymValue*> args;
    Inst *caller;
    std::optional<Inst_iterator> resumeInst;
};

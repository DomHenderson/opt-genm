#pragma once

#include <optional>
#include <unordered_map>
#include <vector>

#include "core/insts.h"
#include "core/insts_binary.h"
#include "core/insts_call.h"
#include "core/insts_const.h"
#include "core/pass.h"
#include "core/pass_manager.h"
#include "symbolic_evaluation/flowgraph.h"
#include "symbolic_evaluation/stack.h"

class SymValue;

class SymbolicEvaluation final : public Pass {
public:
    /// Pass identifier.
    static const char *kPassID;

    SymbolicEvaluation(PassManager *passManager);

    void Run(Prog *prog) override;

    const char *GetPassName() const override;

private:
    SymValue *RunFunction(Func *func, std::vector<SymValue*> funcArgs, Prog *prog);
    SymValue *RunBlock(Block &block, Prog *prog);
    SymValue *RunInst(Inst &inst, Prog *prog);

    void Arg(ArgInst *argInst);
    void Call(CallInst *callInst, Prog *prog);
    void Cmp(CmpInst *cmpInst);
    void JumpCond(JumpCondInst *jumpCondInst);
    void Mov(MovInst *movInst);
    void Ret(ReturnInst *returnInst);
    void TCall(TailCallInst *tailCallInst, Prog *prog);

    void AllocateResult(Inst *inst, std::unique_ptr<SymValue> value);
    SymValue *GetResult(Inst *inst);

    void Branch(CmpInst *constraint, Block *trueBlock, Block *falseBlock) {std::cout<<"Branching not implemented"<<std::endl; }
    void InvalidateHeap() { std::cout<<"Implement invalidate heap!"<<std::endl; }

    Stack stack;
    std::unordered_map<Inst *, unsigned> reg_allocs;
    std::vector<std::unique_ptr<SymValue>> results;

    bool carryOn = true;
    unsigned int limit = 100;
};
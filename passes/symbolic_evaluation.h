#pragma once

#include <optional>
#include <queue>
#include <unordered_map>
#include <vector>

#include "core/insts.h"
#include "core/insts_binary.h"
#include "core/insts_call.h"
#include "core/insts_const.h"
#include "core/pass.h"
#include "core/pass_manager.h"
#include "symbolic_evaluation/flownode.h"
#include "symbolic_evaluation/location.h"
#include "symbolic_evaluation/frame.h"
#include "symbolic_evaluation/valuepool.h"

class SymValue;

class SymbolicEvaluation final : public Pass {
public:
    /// Pass identifier.
    static const char *kPassID;

    SymbolicEvaluation(PassManager *passManager);

    void Run(Prog *prog) override;

    const char *GetPassName() const override;

private:
    using Func_iterator = llvm::ilist<Func>::iterator;
    using Block_iterator = llvm::ilist<Block>::iterator;
    using Inst_iterator = llvm::ilist<Inst>::iterator;

    std::unordered_set<FlowNode*> RunInst(Inst &inst, FlowNode *node);

    void StepNode(FlowNode *node);

    void Arg(ArgInst *argInst, FlowNode *node);
    std::unordered_set<FlowNode*> Call(CallInst *callInst, FlowNode *node);
    void Cmp(CmpInst *cmpInst, FlowNode *node);
    std::unordered_set<FlowNode*> JumpCond(JumpCondInst *jumpCondInst, FlowNode *node);
    void Load(LoadInst *loadInst, FlowNode *node);
    void Mov(MovInst *movInst, FlowNode *node);
    std::unordered_set<FlowNode*> Ret(ReturnInst *returnInst, FlowNode *node);
    void Store(StoreInst *storeInst, FlowNode *node);
    std::unordered_set<FlowNode*> TCall(TailCallInst *tailCallInst, FlowNode *node);

    void InvalidateHeap() { std::cout<<"Implement invalidate heap!"<<std::endl; }

    FlowNode *CreateTailCallFlowNode(Func_iterator func, std::vector<SymValue*> args, FlowNode *previous);
    FlowNode *CreateReturnFlowNode(FlowNode *previous);
    FlowNode *CreateBlockFlowNode(Block_iterator block, FlowNode *previous);
    FlowNode *CreateFunctionFlowNode(Func_iterator func, std::vector<SymValue*> args, Inst *return_addr, std::optional<Location> resume_location, FlowNode *previous);
    RootNode *CreateRootNode();
    std::vector<SymValue*> CreateUnknownArgsVector(unsigned size);

    Prog *prog;

    std::queue<FlowNode*> frontier;

    bool carryOn = true;
    unsigned int count = 0;
    unsigned int limit = 100;

    ValuePool valuePool;
};
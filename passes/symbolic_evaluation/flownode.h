#pragma once

#include <iostream>
#include <unordered_map>
#include <vector>

#include <llvm/ADT/ilist.h>

#include "core/block.h"
#include "core/constant.h"
#include "core/func.h"
#include "core/insts_call.h"
#include "datastore.h"
#include "frame.h"
#include "symvalue.h"

class SuccessorFlowNode;
class SymExPool;

class FlowNode {
public:
    using Inst_iterator = llvm::ilist<Inst>::iterator;
    using Block_iterator = llvm::ilist<Block>::iterator;
    using Func_iterator = llvm::ilist<Func>::iterator;

    FlowNode(Frame &frame, SymExPool &pool);
    virtual ~FlowNode() {}

    virtual SuccessorFlowNode *CreateBlockNode(Block &block);
    virtual SuccessorFlowNode *CreateFunctionNode(
        Func &func,
        std::vector<SymValue*> args,
        Inst_iterator caller
    );
    virtual SuccessorFlowNode *CreateReturnNode() = 0;
    virtual SuccessorFlowNode *CreateTailCallNode(
        Func &func,
        std::vector<SymValue*> args
    );

    virtual Block *ResolvePhiBlocks(std::vector<Block*> blocks, bool includeSelf = false) = 0;

    virtual void AllocateResult(Inst *inst, SymValue *value);
    virtual SymValue *GetResult(Inst *inst) = 0;

    virtual DataStore &get_store() = 0;

    virtual Inst_iterator get_starting_inst() = 0;
    virtual Block_iterator get_block() = 0;
    virtual Func_iterator get_func() = 0;

    virtual Frame &get_frame();

    virtual std::string get_name() = 0;

    virtual SymValue *GetRegister(ConstantReg::Kind reg);
    virtual void SetRegister(ConstantReg::Kind reg, SymValue* value);

protected:
    std::unordered_map<Inst*,unsigned> vreg_allocs;
    std::vector<SymValue*> values;
    std::unordered_map<ConstantReg::Kind,SymValue*> registers; 
    Frame &currentFrame;
    SymExPool &pool;
};

class SuccessorFlowNode : public FlowNode {
public:
    SuccessorFlowNode(Inst_iterator startingInst, Frame &frame, FlowNode &previous, SymExPool &pool);
    ~SuccessorFlowNode() = default;

    virtual SuccessorFlowNode *CreateReturnNode() override;

    virtual Block *ResolvePhiBlocks(std::vector<Block*> blocks, bool includeSelf = false) override;

    virtual SymValue *GetResult(Inst *inst) override;

    virtual LogStore &get_store() override;

    virtual Inst_iterator get_starting_inst() override;
    virtual Block_iterator get_block() override;
    virtual Func_iterator get_func() override;

    virtual std::string get_name() override;
private:
    Inst_iterator startingInst;

    FlowNode &previousNode;

    std::unique_ptr<LogStore> dataStore;
};

class RootFlowNode : public FlowNode {
public:
    RootFlowNode(Func &func, Prog &prog, SymExPool &pool);
    ~RootFlowNode() = default;

    virtual SuccessorFlowNode *CreateReturnNode() override;

    virtual Block *ResolvePhiBlocks(std::vector<Block*> blocks, bool includeSelf = false) override;

    virtual SymValue *GetResult(Inst *inst) override;

    virtual LogStore &get_store() override;

    virtual Inst_iterator get_starting_inst() override;
    virtual Block_iterator get_block() override;
    virtual Func_iterator get_func() override;

    virtual std::string get_name() override;
private:
    Func &func;
    
    BaseStore baseStore;
    LogStore dataStore;
};

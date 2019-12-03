#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "core/block.h"
#include "core/constant.h"
#include "core/func.h"
#include "core/global.h"
#include "core/inst.h"
#include "core/insts.h"
#include "core/insts_call.h"
#include "core/insts_const.h"
#include "core/pass_manager.h"
#include "core/prog.h"
#include "core/value.h"
#include "passes/symbolic_evaluation.h"
#include "symbolic_evaluation/stack.h"
#include "symbolic_evaluation/symvalue.h"
#include "symbolic_evaluation/symvaluecomp.h"


llvm::ilist<Func>::iterator FindFuncByName(std::string_view name, Prog *prog);
std::string toString(Inst::Kind k);


// -----------------------------------------------------------------------------
const char *SymbolicEvaluation::kPassID = "symbolic_evaluation";

SymbolicEvaluation::SymbolicEvaluation(PassManager *passManager) : Pass(passManager) {}

const char *SymbolicEvaluation::GetPassName() const
{
  return "Symbolic Evaluation";
}

void SymbolicEvaluation::Run(Prog *program)
{
    // PrintLearningInfo(prog);

    // ---------------------------------------------------------------
    prog = program;
    // auto startFunc = prog->begin();
    std::cout<<"Starting"<<std::endl;
    auto startFunc = FindFuncByName("caml_program", prog);
    auto argCount = startFunc->params().size();
    std::cout<<"argCount: "<<argCount<<std::endl;
    //RunFunction(&*startFunc, std::vector<SymValue*>(argCount, results[0].get()), prog);
    frontier.push(CreateFunctionFlowNode(
        startFunc,
        CreateUnknownArgsVector(argCount),
        nullptr,
        std::nullopt,
        nullptr
    ));

    // while(!frontier.empty() && count < limit) {
    //     FlowNode *node = frontier.front();
    //     frontier.pop();
    //     std::cout<<"Stepping node "<<node->name<<std::endl;
    //     StepNode(node);
    // }

    // if(!frontier.empty()) {
    //     std::cout<<"Finished early"<<std::endl;
    // }
}

void SymbolicEvaluation::StepNode(FlowNode *node)
{
    auto inst = node->location.inst;
    auto block = node->location.block;
    auto func = node->location.func;
    std::unordered_set<FlowNode*> next;

    while(inst != block->end() && next.empty()) {
        next = RunInst(*inst, node);
        ++inst;
    }

    for(auto n: next) {
        frontier.push(n);
    }

    ++count;
}

std::unordered_set<FlowNode*> SymbolicEvaluation::RunInst(Inst &inst, FlowNode *node)
{
    std::unordered_set<FlowNode*> emptySet;

    switch(inst.GetKind()) {
    case Inst::Kind::ARG:
        std::cout<<"Running arg"<<std::endl;
        Arg(static_cast<ArgInst*>(&inst), node);
        return emptySet;
    
    case Inst::Kind::CALL: 
        std::cout<<"Running call"<<std::endl;
        return Call(static_cast<CallInst*>(&inst), node);
    
    case Inst::Kind::CMP:
        std::cout<<"Running cmp"<<std::endl;
        Cmp(static_cast<CmpInst*>(&inst), node);
        return emptySet;
    
    case Inst::Kind::JCC:
        std::cout<<"Running jcc"<<std::endl;
        return JumpCond(static_cast<JumpCondInst*>(&inst), node);

    case Inst::Kind::LD:
        std::cout<<"Running load"<<std::endl;
        Load(static_cast<LoadInst*>(&inst), node);
        return emptySet;

    case Inst::Kind::MOV:
        std::cout<<"Running mov"<<std::endl;
        Mov(static_cast<MovInst*>(&inst), node);
        return emptySet;

    case Inst::Kind::RET:
        std::cout<<"Running ret"<<std::endl;
        return Ret(static_cast<ReturnInst*>(&inst), node);
    
    case Inst::Kind::ST:
        std::cout<<"Running store"<<std::endl;
        Store(static_cast<StoreInst*>(&inst), node);
        return emptySet;

    case Inst::Kind::TCALL:
        std::cout<<"Running tail call"<<std::endl;
        return TCall(static_cast<TailCallInst*>(&inst), node);

    default:
        std::cout<<"Skipping"<<std::endl;
        return emptySet;
    }
}


// -----------------------------------------------------------------------------
FlowNode *SymbolicEvaluation::CreateTailCallFlowNode(
    Func_iterator func,
    std::vector<SymValue*> args,
    FlowNode *previous
) {
    FlowNode *newNode = new FlowNode();
    newNode->previousNode = previous;
    newNode->location.func = func;
    newNode->location.block = func->begin();
    newNode->location.inst = func->begin()->begin();

    Frame *newFrame = new Frame(args);
    newFrame->previous = previous->currentFrame->previous;
    newFrame->return_addr = previous->currentFrame->return_addr;
    newNode->currentFrame = newFrame;

    newNode->name = func->GetName();

    return newNode;
}

FlowNode *SymbolicEvaluation::CreateReturnFlowNode(
    FlowNode *previous
) {
    FlowNode *newNode = new FlowNode();
    newNode->previousNode = previous;
    newNode->currentFrame = previous->currentFrame->previous;
    
    if(newNode->currentFrame->resume_location.has_value() ) {
        newNode->location = newNode->currentFrame->resume_location.value();
    } else {
        std::cout<<"No return location to return to"<<std::endl;
        return nullptr;
    }

    newNode->name = newNode->location.func->GetName();

    return newNode;
}

FlowNode *SymbolicEvaluation::CreateBlockFlowNode(
    Block_iterator block,
    FlowNode *previous
) {
    FlowNode *newNode = new FlowNode();
    newNode->previousNode = previous;
    newNode->location.func = block->getParent()->getIterator();
    newNode->location.block = block;
    newNode->location.inst = block->begin();

    newNode->currentFrame = previous->currentFrame;

    newNode->name = block->GetName();

    return newNode;
}

FlowNode *SymbolicEvaluation::CreateFunctionFlowNode(
    Func_iterator func,
    std::vector<SymValue*> args,
    Inst *return_addr,
    std::optional<Location> resume_location,
    FlowNode *previous
) {
    int i = 0;
    FlowNode *newNode = new FlowNode();
    newNode->previousNode = previous;
    newNode->location.func = func;
    newNode->location.block = func->begin();
    newNode->location.inst = func->begin()->begin();

    Frame *newFrame = new Frame(args);
    newFrame->previous = previous == nullptr 
        ? nullptr
        : previous->currentFrame;
    newFrame->return_addr = return_addr;
    newNode->currentFrame = newFrame;

    if(previous != nullptr ) {
        previous->currentFrame->resume_location = resume_location;
    }

    newNode->name = func->GetName();
    return newNode;
}

std::vector<SymValue*> SymbolicEvaluation::CreateUnknownArgsVector(
    unsigned size
) {
    std::vector<SymValue*> result;
    result.reserve(size);
    for(unsigned i = 0; i < size; ++i) {
        SymValue *value = new UnknownSymValue();
        valuePool.persist(value);
        result.push_back(value);
    }
    return result;
}

llvm::ilist<Func>::iterator FindFuncByName(std::string_view name, Prog *prog)
{
    auto result = std::find_if(
        prog->begin(),
        prog->end(),
        [&name](auto& f) {
            return f.GetName() == name;
        }
    );
    if(result == prog->end()) {
        std::cout<<"Failed to find function "<<name<<std::endl;
    } else {
        std::cout<<"Found "<<name<<std::endl;
    }
    return result;
}


// -----------------------------------------------------------------------------
void SymbolicEvaluation::Arg(ArgInst *argInst, FlowNode *node)
{
    unsigned int idx = argInst->GetIdx();

    node->AllocateResult(
        argInst,
        node->currentFrame->args[idx]
    );
}

std::unordered_set<FlowNode*> SymbolicEvaluation::Call(CallInst *callInst, FlowNode *node)
{
    SymValue *v = node->GetResult(callInst->GetCallee());

    if ( v == nullptr || v->get_kind() != SymValue::Kind::FUNCREF ) {
        std::cout<<"Called something that isn't a func ref"<<std::endl;
        SymValue* unknown = new UnknownSymValue();
        valuePool.persist(unknown);
        node->AllocateResult(
            callInst,
            unknown
        );
        return std::unordered_set<FlowNode*>();
    }

    FuncRefSymValue *fvp = (FuncRefSymValue*)v;
    std::string_view name = fvp->get_name();

    auto func = FindFuncByName(name, prog);

    if ( func == prog->end() ) {
        std::cout<<"No function with name " << name << " found"<<std::endl;
        SymValue* unknown = new UnknownSymValue();
        valuePool.persist(unknown);
        node->AllocateResult(
            callInst,
            unknown
        );
        return std::unordered_set<FlowNode*>();
    }

    std::vector<SymValue*> args;
    std::transform(
        callInst->arg_begin(), callInst->arg_end(),
        std::back_inserter(args),
        [this, node](auto x) { return node->GetResult(x); }
    );

    Location resume_location;
    resume_location.inst = callInst->getIterator();
    ++resume_location.inst;
    resume_location.block = node->location.block;
    resume_location.func = node->location.func;

    //Check if end of block

    std::unordered_set<FlowNode*> result;
    result.insert(CreateFunctionFlowNode(
        func,
        args,
        callInst,
        std::optional<Location>(resume_location),
        node
    ));

    return result;
}

void SymbolicEvaluation::Cmp(CmpInst *cmpInst, FlowNode *node)
{
    Cond c = cmpInst->GetCC();
    Inst *lhs = cmpInst->GetLHS();
    Inst *rhs = cmpInst->GetRHS();
    SymValue *lv = node->GetResult(lhs);
    SymValue *rv = node->GetResult(rhs);
    SymValue *result;

    std::cout<<"lhs: "<<toString(lhs->GetKind())<<" rhs: "<<toString(rhs->GetKind())<<std::endl;

    switch(c) {
    case Cond::EQ:
        std::cout<<"Comparing EQ"<<std::endl;
        result = SymComp::EQ(lv, rv);
        break;
    
    case Cond::NE:
        std::cout<<"Comparing NE"<<std::endl;
        result = SymComp::NEQ(lv, rv);
        break;
    
    case Cond::LT:
        std::cout<<"Comparing LT"<<std::endl;
        result = SymComp::LT(lv, rv);
        break;
    
    case Cond::GT:
        std::cout<<"Comparing GT"<<std::endl;
        result = SymComp::GT(lv, rv);
        break;
    
    case Cond::LE:
        std::cout<<"Comparing LE"<<std::endl;
        result = SymComp::LE(lv, rv);
        break;
    
    case Cond::GE:
        std::cout<<"Comparing GE"<<std::endl;
        result = SymComp::GE(lv, rv);
        break;
    
    default:
        std::cout<<"Comparing "<<(int)c<<std::endl;
        result = new UnknownSymValue();
        break;
    }

    valuePool.persist(result);
    node->AllocateResult(cmpInst, result);
}

std::unordered_set<FlowNode*>  SymbolicEvaluation::JumpCond(JumpCondInst *jumpCondInst, FlowNode *node)
{
    std::unordered_set<FlowNode*> result;

    Inst *cond = jumpCondInst->GetCond();
    Block *trueBlock = jumpCondInst->GetTrueTarget();
    Func *trueFunc = trueBlock->getParent();
    Block *falseBlock = jumpCondInst->GetFalseTarget();
    Func *falseFunc = falseBlock->getParent();

    SymValue *symCond = node->GetResult(cond);
    if(symCond->get_kind() == SymValue::Kind::BOOL) {
        auto b = static_cast<BoolSymValue*>(symCond);
        Block *block;
        Func *func;
        if(b->get_value()) {
            block = trueBlock;
            func = trueFunc;
        } else {
            block = falseBlock;
            func = falseFunc;
        }
        if(func->GetName() != node->location.func->GetName()) {
            std::cout<<"Jumping from "<<node->location.func->GetName()<<" to "<<func->GetName()<<std::endl;
        }
        FlowNode *newNode = CreateBlockFlowNode(block->getIterator(), node);
        result.insert(newNode);
    } else {
        result.insert(
            CreateBlockFlowNode(trueBlock->getIterator(), node)
        );
        result.insert(
            CreateBlockFlowNode(falseBlock->getIterator(), node)
        );
    }
    return result;
}

void SymbolicEvaluation::Load(LoadInst *loadInst, FlowNode *node)
{
    SymValue *addr = node->GetResult(loadInst->GetAddr());
    node->AllocateResult(loadInst,node->Load(addr));
}

void SymbolicEvaluation::Mov(MovInst *movInst, FlowNode *node)
{
    Value *arg = movInst->GetArg();
    SymValue *result;
    switch(arg->GetKind()) {
    case Value::Kind::GLOBAL: {
        auto g = static_cast<Global*>(arg);
        switch(g->GetKind()) {
        case Global::Kind::FUNC:
            std::cout<<"Moving global func"<<std::endl;
            result = new FuncRefSymValue(g->GetName());
            valuePool.persist(result);
            break;
        
        default:
            std::cout<<"Moving unknown global"<<std::endl;
            result = new UnknownSymValue();
            valuePool.persist(result);
            break;
        }
    } break;

    case Value::Kind::INST: {
        std::cout<<"Moving inst"<<std::endl;
        auto i = static_cast<Inst*>(arg);
        result = node->GetResult(i);
    } break;

    case Value::Kind::CONST: {
        Constant *c = static_cast<Constant*>(arg);
        switch(c->GetKind()) {
        case Constant::Kind::INT: {
            ConstantInt *ci = static_cast<ConstantInt*>(c);
            result = new IntSymValue(ci->GetValue());
            valuePool.persist(result);
            std::cout<<"Moving int "<<ci->GetValue()<<std::endl;
        } break;
        
        case Constant::Kind::FLOAT: {
            ConstantFloat *cf = static_cast<ConstantFloat*>(c);
            result = new FloatSymValue(cf->GetValue());
            valuePool.persist(result);
            std::cout<<"Moving float "<<cf->GetValue()<<std::endl;
        } break;

        default:
            result = new UnknownSymValue();
            valuePool.persist(result);
            std::cout<<"Moving const"<<std::endl;
            break;
        }
    } break;

    case Value::Kind::EXPR: {
        result = new UnknownSymValue();
        valuePool.persist(result);
        std::cout<<"Moving expr"<<std::endl;
    }
    }

    node->AllocateResult(movInst, result);
}

std::unordered_set<FlowNode*> SymbolicEvaluation::Ret(ReturnInst *returnInst, FlowNode *node)
{
    Inst *return_addr = node->currentFrame->return_addr;
    SymValue *returnValue = node->GetResult(returnInst->GetValue());
    FlowNode *newNode = CreateReturnFlowNode(node);
    newNode->AllocateResult(return_addr, returnValue);
    std::unordered_set<FlowNode*> nodes;
    nodes.insert(newNode);
    return nodes;
}

void SymbolicEvaluation::Store(StoreInst *storeInst, FlowNode *node)
{
    SymValue *addr = node->GetResult(storeInst->GetAddr());
    SymValue *value = node->GetResult(storeInst->GetVal());
    node->Store(addr, value);
}

std::unordered_set<FlowNode*>  SymbolicEvaluation::TCall(TailCallInst *tailCallInst, FlowNode *node)
{
    SymValue *v = node->GetResult(tailCallInst->GetCallee());

    if ( v == nullptr || v->get_kind() != SymValue::Kind::FUNCREF ) {
        std::cout<<"Called something that isn't a func ref"<<std::endl;
        SymValue* unknown = new UnknownSymValue();
        valuePool.persist(unknown);
        node->AllocateResult(
            tailCallInst,
            unknown
        );
        return std::unordered_set<FlowNode*>();
    }

    FuncRefSymValue *fvp = (FuncRefSymValue*)v;
    std::string_view name = fvp->get_name();

    auto func = FindFuncByName(name, prog);

    if ( func == prog->end() ) {
        std::cout<<"No function with name " << name << " found"<<std::endl;
        SymValue* unknown = new UnknownSymValue();
        valuePool.persist(unknown);
        node->AllocateResult(
            tailCallInst,
            unknown
        );
        return std::unordered_set<FlowNode*>();
    }

    std::vector<SymValue*> args;
    std::transform(
        tailCallInst->arg_begin(), tailCallInst->arg_end(),
        std::back_inserter(args),
        [this, node](auto x) { return node->GetResult(x); }
    );

    std::unordered_set<FlowNode*> result;
    result.insert(CreateTailCallFlowNode(func, args, node));

    return result;
}


// -----------------------------------------------------------------------------
std::string toString(Inst::Kind k)
{
    switch(k) {
        // Control flow.
        case Inst::Kind::CALL: return "call"; break;
        case Inst::Kind::TCALL: return "tcall"; break;
        case Inst::Kind::INVOKE: return "invoke"; break;
        case Inst::Kind::TINVOKE: return "tinvoke"; break;
        case Inst::Kind::RET: return "ret"; break;
        case Inst::Kind::JCC: return "jcc"; break;
        case Inst::Kind::JI: return "ji"; break;
        case Inst::Kind::JMP: return "jmp"; break;
        case Inst::Kind::SWITCH: return "switch"; break;
        case Inst::Kind::TRAP: return "trap"; break;
    // Memory.
        case Inst::Kind::LD: return "ld"; break;
        case Inst::Kind::ST: return "st"; break;
    // Atomic exchange.
        case Inst::Kind::XCHG: return "xchg"; break;
    // Set register.
        case Inst::Kind::SET: return "set"; break;
    // Variable argument lists.
        case Inst::Kind::VASTART: return "vastart"; break;
    // Dynamic stack allcoation.
        case Inst::Kind::ALLOCA: return "alloca"; break;
    // Constants.
        case Inst::Kind::ARG: return "arg"; break;
        case Inst::Kind::FRAME: return "frame"; break;
        case Inst::Kind::UNDEF: return "undef"; break;
    // Conditional.
        case Inst::Kind::SELECT: return "select"; break;
    // Unary instructions.
        case Inst::Kind::ABS: return "abs"; break;
        case Inst::Kind::NEG: return "neg"; break;
        case Inst::Kind::SQRT: return "sqrt"; break;
        case Inst::Kind::SIN: return "sin"; break;
        case Inst::Kind::COS: return "cos"; break;
        case Inst::Kind::SEXT: return "sext"; break;
        case Inst::Kind::ZEXT: return "zext"; break;
        case Inst::Kind::FEXT: return "fext"; break;
        case Inst::Kind::MOV: return "mov"; break;
        case Inst::Kind::TRUNC: return "trunc"; break;
    // Binary instructions.
        case Inst::Kind::ADD: return "add"; break;
        case Inst::Kind::AND: return "and"; break;
        case Inst::Kind::CMP: return "cmp"; break;
        case Inst::Kind::DIV: return "div"; break;
        case Inst::Kind::REM: return "rem"; break;
        case Inst::Kind::MUL: return "mul"; break;
        case Inst::Kind::OR: return "or"; break;
        case Inst::Kind::ROTL: return "rotl"; break;
        case Inst::Kind::SLL: return "sll"; break;
        case Inst::Kind::SRA: return "sra"; break;
        case Inst::Kind::SRL: return "srl"; break;
        case Inst::Kind::SUB: return "sub"; break;
        case Inst::Kind::XOR: return "xor"; break;
        case Inst::Kind::POW: return "pow"; break;
        case Inst::Kind::COPYSIGN: return "copysign"; break;
    // Overflow tests.
        case Inst::Kind::UADDO: return "uaddo"; break;
        case Inst::Kind::UMULO: return "umulo"; break;
    // PHI node.
        case Inst::Kind::PHI: return "phi"; break;
        default: return "default"; break;
    }
}

void PrintLearningInfo(Prog *prog)
{
    std::unordered_map<int, std::optional<int>> registers;
    std::map<Inst::Kind, bool> seen; //TEMP
    for (auto &func : *prog) {
        std::cout<<func.GetName()<<"---";
        std::cout<<func.size()<<std::endl;
        for (auto &block : func) {
            std::cout<<block.GetName()<<"---"<<std::endl;
            for (auto &inst : block) {
                std::cout<<(void*)&inst<<toString(inst.GetKind())<<" ";
                seen[inst.GetKind()] = true;
                auto begin = inst.op_begin();
                auto end = inst.op_end();
                for ( auto i = begin; i < end; ++i ) {
                    auto v = i->get();
                    Value::Kind k = v->GetKind();
                    switch(k) {
                    case Value::Kind::CONST: {
                        Constant *c = (Constant*)v;
                        switch(c->GetKind()) {
                        case Constant::INT: {
                            ConstantInt *ci = (ConstantInt*)c;
                            std::cout<<"Constant"<<ci->GetValue()<<" ";
                        } break;
                        case Constant::FLOAT: {
                            ConstantFloat *cf = (ConstantFloat*)c;
                            std::cout<<"Constant"<<cf->GetValue()<<" ";
                        } break;
                        case Constant::REG:
                            std::cout<<"ConstantReg"<<std::endl;
                            break;
                        }
                    } break;
                    
                    case Value::Kind::EXPR: {
                        std::cout<<"Expr"<<" ";
                    } break;
                    
                    case Value::Kind::GLOBAL: {
                        std::cout<<"Global";
                        Global *g = (Global*)v;
                        switch(g->GetKind()) {
                        case Global::Kind::ATOM:
                            std::cout<<"Atom";
                            break;
                        
                        case Global::Kind::BLOCK:
                            std::cout<<"Block";
                            break;
                        
                        case Global::Kind::EXTERN:
                            std::cout<<"Extern";
                            break;
                        
                        case Global::Kind::FUNC:
                            std::cout<<"Func";
                            break;
                        
                        case Global::Kind::SYMBOL:
                            std::cout<<"Symbol";
                            break;
                        }
                        std::cout<<g->GetName()<<" ";
                    } break;
                    
                    case Value::Kind::INST:
                        Inst *i = (Inst*)v;
                        std::cout<<"Inst"<<(void*)i<<toString(i->GetKind())<<" ";
                        break;
                    }
                }
                std::cout<<std::endl;
            }
        }
    }

    for(auto &entry: seen) {
        std::cout<<toString(entry.first)<<" "<<entry.second<<std::endl;
    }
}
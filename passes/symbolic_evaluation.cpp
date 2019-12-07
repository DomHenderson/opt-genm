#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "core/atom.h"
#include "core/block.h"
#include "core/constant.h"
#include "core/data.h"
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
#include "symbolic_evaluation/frame.h"
#include "symbolic_evaluation/storagepool.h"
#include "symbolic_evaluation/symvalue.h"
#include "symbolic_evaluation/symvaluecomp.h"


llvm::ilist<Func>::iterator FindFuncByName(std::string_view name, Prog *prog);
std::string toString(Inst::Kind k);
void PrintCodeInfo(Prog *prog);
void PrintDataInfo(Prog *prog);


// -----------------------------------------------------------------------------
const char *SymbolicEvaluation::kPassID = "symbolic_evaluation";

SymbolicEvaluation::SymbolicEvaluation(PassManager *passManager) : Pass(passManager) {}

const char *SymbolicEvaluation::GetPassName() const
{
  return "Symbolic Evaluation";
}

void SymbolicEvaluation::Run(Prog *program)
{
    PrintCodeInfo(program);

    // ---------------------------------------------------------------
    prog = program;
    std::cout<<"Starting"<<std::endl;
    frontier.push(CreateRootNode());
    std::cout<<"Done"<<std::endl;

    while(!frontier.empty() && count < limit) {
        FlowNode *node = frontier.front();
        frontier.pop();
        std::cout<<"Stepping node "<<node->get_name()<<std::endl;
        StepNode(node);
    }

    if(!frontier.empty()) {
        std::cout<<"Finished early"<<std::endl;
    }
}

void SymbolicEvaluation::StepNode(FlowNode *node)
{
    auto inst = node->get_starting_inst();
    auto block = node->get_block();
    auto func = node->get_func();
    std::optional<std::unordered_set<FlowNode*>> next;

    while(inst != block->end() && next == std::nullopt) {
        next = RunInst(*inst, node);
        ++inst;
    }

    if(next != std::nullopt) {
        auto nextNodes = next.value();
        for(auto n: nextNodes) {
            frontier.push(n);
        }
    }

    ++count;
}

std::optional<std::unordered_set<FlowNode*>> SymbolicEvaluation::RunInst(
    Inst &inst,
    FlowNode *node
) {
    switch(inst.GetKind()) {
    case Inst::Kind::ADD:
        std::cout<<"Running add"<<std::endl;
        Add(static_cast<AddInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::ARG:
        std::cout<<"Running arg"<<std::endl;
        Arg(static_cast<ArgInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::CALL: 
        std::cout<<"Running call"<<std::endl;
        return Call(static_cast<CallInst*>(&inst), node);
    
    case Inst::Kind::CMP:
        std::cout<<"Running cmp"<<std::endl;
        Cmp(static_cast<CmpInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::JCC:
        std::cout<<"Running jcc"<<std::endl;
        return JumpCond(static_cast<JumpCondInst*>(&inst), node);

    case Inst::Kind::LD:
        std::cout<<"Running load"<<std::endl;
        Load(static_cast<LoadInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::MOV:
        std::cout<<"Running mov"<<std::endl;
        Mov(static_cast<MovInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::RET:
        std::cout<<"Running ret"<<std::endl;
        return Ret(static_cast<ReturnInst*>(&inst), node);
    
    case Inst::Kind::ST:
        std::cout<<"Running store"<<std::endl;
        Store(static_cast<StoreInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::TCALL:
        std::cout<<"Running tail call"<<std::endl;
        return TCall(static_cast<TailCallInst*>(&inst), node);

    default:
        std::cout<<"Skipping"<<std::endl;
        return std::nullopt;
    }
}

//-----------------------------------------------------------------------------

RootFlowNode *SymbolicEvaluation::CreateRootNode()
{
    PrintDataInfo(prog);
    auto startFunc = FindFuncByName("caml_program", prog);
    if(startFunc == prog->end()) {
        std::cout<<"Failed to create root node"<<std::endl;
        return nullptr;
    }

    return storagePool.persist(
        new RootFlowNode(*startFunc, *prog, storagePool)
    );
}

SuccessorFlowNode *SymbolicEvaluation::CreateTailCallFlowNode(
    Func_iterator func,
    std::vector<SymValue*> args,
    FlowNode *previous
) {
    return storagePool.persist(
        previous->CreateTailCallNode(
            *func,
            args,
            storagePool
        )
    );
}

SuccessorFlowNode *SymbolicEvaluation::CreateReturnFlowNode(
    FlowNode *previous
) {
    return storagePool.persist(previous->CreateReturnNode());
}

SuccessorFlowNode *SymbolicEvaluation::CreateBlockFlowNode(
    Block_iterator block,
    FlowNode *previous
) {
    return storagePool.persist(previous->CreateBlockNode(*block));
}

SuccessorFlowNode *SymbolicEvaluation::CreateFunctionFlowNode(
    Func_iterator func,
    std::vector<SymValue*> args,
    Inst_iterator caller,
    FlowNode *previous
) {
    return storagePool.persist(
        previous->CreateFunctionNode(
            *func,
            args,
            caller,
            storagePool
        )
    );
}

std::vector<SymValue*> SymbolicEvaluation::CreateUnknownArgsVector(
    unsigned size
) {
    std::vector<SymValue*> result;
    result.reserve(size);
    for(unsigned i = 0; i < size; ++i) {
        SymValue *value = new UnknownSymValue();
        storagePool.persist(value);
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

void SymbolicEvaluation::Add(
    AddInst *addInst,
    FlowNode *node
) {
    SymValue *LHS = node->GetResult(addInst->GetLHS());
    SymValue *RHS = node->GetResult(addInst->GetRHS());

    SymValue *result;

    switch(LHS->get_kind()) {
    case SymValue::Kind::ADDR: {
        AddrSymValue *laddr = static_cast<AddrSymValue*>(LHS);
        switch(RHS->get_kind()) {
        case SymValue::Kind::INT: {
            IntSymValue *rint = static_cast<IntSymValue*>(RHS);
            result = new AddrSymValue(laddr->get_name(), laddr->get_offset()+rint->get_value());
        } break;
        default:
            std::cout<<"Adding ADDR and not INT"<<std::endl;
            result = new UnknownSymValue();
            break;
        }
    } break;
    case SymValue::Kind::INT: {
        IntSymValue *lint = static_cast<IntSymValue*>(LHS);
        switch(RHS->get_kind()) {
        case SymValue::Kind::ADDR: {
            AddrSymValue *raddr = static_cast<AddrSymValue*>(RHS);
            result = new AddrSymValue(raddr->get_name(), raddr->get_offset()+lint->get_value());
        } break;
        case SymValue::Kind::INT: {
            IntSymValue *rint = static_cast<IntSymValue*>(RHS);
            result = new IntSymValue(lint->get_value()+rint->get_value());
        } break;
        default:
            std::cout<<"Adding INT and not ADDR or INT"<<std::endl;
            result = new UnknownSymValue();
            break;
        }
    } break;
    default:
        std::cout<<"Adding unimplemented combination"<<std::endl;
        result = new UnknownSymValue();
        break;
    }

    storagePool.persist(result);
    node->AllocateResult(addInst, result);
}

void SymbolicEvaluation::Arg(
    ArgInst *argInst,
    FlowNode *node
) {
    unsigned int idx = argInst->GetIdx();

    node->AllocateResult(
        argInst,
        node->get_frame().get_arg(idx)
    );
}

std::unordered_set<FlowNode*> SymbolicEvaluation::Call(
    CallInst *callInst,
    FlowNode *node
) {
    SymValue *v = node->GetResult(callInst->GetCallee());

    if ( v == nullptr || v->get_kind() != SymValue::Kind::FUNCREF ) {
        std::cout<<"Called something that isn't a func ref"<<std::endl;
        node->AllocateResult(callInst,storagePool.persist(new UnknownSymValue()));
        return std::unordered_set<FlowNode*>();
    }

    FuncRefSymValue *funcRef = static_cast<FuncRefSymValue*>(v);

    std::vector<SymValue*> args;
    std::transform(
        callInst->arg_begin(), callInst->arg_end(),
        std::back_inserter(args),
        [this, node](auto x) { return node->GetResult(x); }
    );
    
    SuccessorFlowNode *newNode = storagePool.persist(
        node->CreateFunctionNode(
            *FindFuncByName(funcRef->get_name(), prog),
            args,
            callInst->getIterator(),
            storagePool
        )
    );

    std::unordered_set<FlowNode*> result;
    result.insert(newNode);
    return result;
}

void SymbolicEvaluation::Cmp(
    CmpInst *cmpInst,
    FlowNode *node
) {
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

    storagePool.persist(result);
    node->AllocateResult(cmpInst, result);
}

std::unordered_set<FlowNode*> SymbolicEvaluation::JumpCond(
    JumpCondInst *jumpCondInst,
    FlowNode *node
) {
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
        if(func->GetName() != node->get_func()->GetName()) {
            std::cout<<"Jumping from "<<node->get_func()->GetName()<<" to "<<func->GetName()<<std::endl;
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

void SymbolicEvaluation::Load(
    LoadInst *loadInst,
    FlowNode *node
) {
    SymValue *addr = node->GetResult(loadInst->GetAddr());
    node->AllocateResult(loadInst,node->get_store().read(addr));
}

void SymbolicEvaluation::Mov(
    MovInst *movInst,
    FlowNode *node
) {
    Value *arg = movInst->GetArg();
    SymValue *result;
    switch(arg->GetKind()) {
    case Value::Kind::GLOBAL: {
        auto g = static_cast<Global*>(arg);
        switch(g->GetKind()) {
        case Global::Kind::ATOM:
            std::cout<<"Moving global atom"<<std::endl;
            result = new AddrSymValue(g->GetName(),0);
            storagePool.persist(result);
            break;

        case Global::Kind::FUNC:
            std::cout<<"Moving global func"<<std::endl;
            result = new FuncRefSymValue(g->GetName());
            storagePool.persist(result);
            break;
        
        default:
            std::cout<<"Moving unknown global"<<std::endl;
            result = new UnknownSymValue();
            storagePool.persist(result);
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
            storagePool.persist(result);
            std::cout<<"Moving int "<<ci->GetValue()<<std::endl;
        } break;
        
        case Constant::Kind::FLOAT: {
            ConstantFloat *cf = static_cast<ConstantFloat*>(c);
            result = new FloatSymValue(cf->GetValue());
            storagePool.persist(result);
            std::cout<<"Moving float "<<cf->GetValue()<<std::endl;
        } break;

        default:
            result = new UnknownSymValue();
            storagePool.persist(result);
            std::cout<<"Moving const"<<std::endl;
            break;
        }
    } break;

    case Value::Kind::EXPR: {
        result = new UnknownSymValue();
        storagePool.persist(result);
        std::cout<<"Moving expr"<<std::endl;
    }
    }

    node->AllocateResult(movInst, result);
}

std::unordered_set<FlowNode*> SymbolicEvaluation::Ret(
    ReturnInst *returnInst,
    FlowNode *node
) {
    Inst *caller = node->get_frame().get_caller();
    SymValue *returnValue = node->GetResult(returnInst->GetValue());
    FlowNode *newNode = CreateReturnFlowNode(node);
    newNode->AllocateResult(caller, returnValue);
    std::unordered_set<FlowNode*> nodes;
    nodes.insert(newNode);
    return nodes;
}

void SymbolicEvaluation::Store(
    StoreInst *storeInst,
    FlowNode *node
) {
    SymValue *addr = node->GetResult(storeInst->GetAddr());
    SymValue *value = node->GetResult(storeInst->GetVal());
    node->get_store().write(addr, value);
}

std::unordered_set<FlowNode*> SymbolicEvaluation::TCall(
    TailCallInst *tailCallInst,
    FlowNode *node
) {
    SymValue *v = node->GetResult(tailCallInst->GetCallee());

    if ( v == nullptr || v->get_kind() != SymValue::Kind::FUNCREF ) {
        std::cout<<"Called something that isn't a func ref"<<std::endl;
        node->AllocateResult(tailCallInst,storagePool.persist(new UnknownSymValue()));
        return std::unordered_set<FlowNode*>();
    }

    FuncRefSymValue *funcRef = static_cast<FuncRefSymValue*>(v);

    std::vector<SymValue*> args;
    std::transform(
        tailCallInst->arg_begin(), tailCallInst->arg_end(),
        std::back_inserter(args),
        [this, node](auto x) { return node->GetResult(x); }
    );
    
    SuccessorFlowNode *newNode = storagePool.persist(
        node->CreateTailCallNode(
            *FindFuncByName(funcRef->get_name(), prog),
            args,
            storagePool
        )
    );

    std::unordered_set<FlowNode*> result;
    result.insert(newNode);
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

void PrintCodeInfo(Prog *prog)
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
                        case Constant::REG: {
                            std::cout<<"ConstantReg"<<" ";
                            ConstantReg *cr = static_cast<ConstantReg*>(c);
                            switch(cr->GetValue()) {
                            case ConstantReg::Kind::RAX: std::cout<<"RAX"; break;
                            case ConstantReg::Kind::RBX: std::cout<<"RBX"; break;
                            case ConstantReg::Kind::RCX: std::cout<<"RCX"; break;
                            case ConstantReg::Kind::RDX: std::cout<<"RDX"; break;
                            case ConstantReg::Kind::RSI: std::cout<<"RSI"; break;
                            case ConstantReg::Kind::RDI: std::cout<<"RDI"; break;
                            case ConstantReg::Kind::RSP: std::cout<<"RSP"; break;
                            case ConstantReg::Kind::RBP: std::cout<<"RBP"; break;
                            case ConstantReg::Kind::R8: std::cout<<"R8"; break;
                            case ConstantReg::Kind::R9: std::cout<<"R9"; break;
                            case ConstantReg::Kind::R10: std::cout<<"R10"; break;
                            case ConstantReg::Kind::R11: std::cout<<"R11"; break;
                            case ConstantReg::Kind::R12: std::cout<<"R12"; break;
                            case ConstantReg::Kind::R13: std::cout<<"R13"; break;
                            case ConstantReg::Kind::R14: std::cout<<"R14"; break;
                            case ConstantReg::Kind::R15: std::cout<<"R15"; break;
                            case ConstantReg::Kind::RET_ADDR: std::cout<<"RET_ADDR"; break;
                            case ConstantReg::Kind::FRAME_ADDR: std::cout<<"FRAME_ADDR"; break;
                            case ConstantReg::Kind::PC: std::cout<<"PC"; break;
                            default: std::cout<<"Unknown";
                            }
                            std::cout<<std::endl;
                        } break;
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

    std::cout<<"---------------------------------------"<<std::endl;
}

void PrintDataInfo(Prog *prog)
{
    for(auto &data: prog->data()) {
        std::cout<<"Start data segment: "<<data.GetName()<<std::endl;
        for(auto &atom: data) {
            std::cout<<"Start: "<<atom.GetName()<<std::endl;
            for (auto &item: atom) {
                switch(item->GetKind()) {
                case Item::Kind::ALIGN:
                    std::cout<<"Align "<<item->GetAlign()<<std::endl;
                    break;
                
                case Item::Kind::END:
                    std::cout<<"End"<<std::endl;
                    break;
                
                case Item::Kind::FLOAT64:
                    std::cout<<"Float64 "<<item->GetFloat64()<<std::endl;
                    break;
                
                case Item::Kind::INT16:
                    std::cout<<"Int16 "<<item->GetInt16()<<std::endl;
                    break;

                case Item::Kind::INT32:
                    std::cout<<"Int32 "<<item->GetInt32()<<std::endl;
                    break;
                
                case Item::Kind::INT64:
                    std::cout<<"Int64 "<<item->GetInt64()<<std::endl;
                    break;
                
                case Item::Kind::INT8:
                    std::cout<<"Int8 "<<item->GetInt8()<<std::endl;
                    break;
                
                case Item::Kind::SPACE:
                    std::cout<<"Space "<<item->GetSpace()<<std::endl;
                    break;
                
                case Item::Kind::STRING:
                    std::cout<<"String "<<static_cast<std::string>(item->GetString())<<std::endl;
                    break;
                
                case Item::Kind::SYMBOL:
                    std::cout<<"Symbol "<<item->GetSymbol()<<std::endl;
                    break;
                }
            }
            std::cout<<"End atom"<<std::endl;
        }
    }
}

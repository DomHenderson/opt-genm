#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>

#include "z3++.h"

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
#include "symbolic_evaluation/datastore.h"
#include "symbolic_evaluation/frame.h"
#include "symbolic_evaluation/externstubs.h"
#include "symbolic_evaluation/storagepool.h"
#include "symbolic_evaluation/symvalue.h"
#include "symbolic_evaluation/symvaluecomp.h"
#include "symbolic_evaluation/utilities.h"

constexpr std::string_view start_function = "main";

void Z3Test();

//-----------------------------------------------------------------------------

const char *SymbolicEvaluation::kPassID = "symbolic_evaluation";

SymbolicEvaluation::SymbolicEvaluation(PassManager *passManager) : Pass(passManager) {}

const char *SymbolicEvaluation::GetPassName() const
{
  return "Symbolic Evaluation";
}

void SymbolicEvaluation::Run(Prog *program)
{
    Log<<"Running symbolic execution"<<End();
    z3::set_param("parallel.enable", true);
    Z3Test();

    PrintCodeInfo(program);

    // ---------------------------------------------------------------
    prog = program;
    LogFlow<<"Starting"<<End();
    frontier.insert(CreateRootNode());
    LogFlow<<"Done"<<End();

    LogFlow<<End();
    LogFlow<<"----------------------------------------"<<End();
    LogFlow<<"EXECUTION START"<<End();
    LogFlow<<"----------------------------------------"<<End();
    LogFlow<<End();

    while(!frontier.empty() && count < limit) {
        std::cout<<"Joining"<<std::endl;
        JoinNodes();
        std::cout<<"Choosing"<<std::endl;
        FlowNode *node = ChooseNextNode();
        std::cout<<"Erasing"<<std::endl;
        frontier.erase(node);
        std::cout<<"Printing"<<std::endl;
        LogFlow<<"Stepping node "<<node->get_name()<<End();
        std::cout<<"Stepping"<<std::endl;
        StepNode(node);
        std::cout<<"Looping"<<std::endl;
    }

    LogFlow<<(frontier.empty() ? "Finished" : "Finished early")<<End();

    LogFlow<<End();
    LogFlow<<"----------------------------------------"<<End();
    LogFlow<<"EXECUTION END"<<End();
    LogFlow<<"----------------------------------------"<<End();
    LogFlow<<End();

    if(frontier.empty()) {
        //Optimise();
        Rewrite();
    }
    Log<<"Finished"<<End();
    std::cout<<std::endl;
}

void SymbolicEvaluation::JoinNodes()
{
    switch(joinStrategy) {
    case JoinStrategy::NEVER:
        LogTrace<<"Skipping join pass"<<End(true);
        break;
    case JoinStrategy::ALWAYS:
        LogTrace<<"Running join pass"<<End(true);
        std::unordered_map<Inst*, FlowNode*> map;
        LogTrace<<"Frontier contains"<<End();
        for(auto& node: frontier) {
            LogTrace<<"  "<<node->get_name()<<End();
        }

        while(!frontier.empty()) {
            auto node = *frontier.begin();
            frontier.erase(node);

            Inst &start = *node->get_starting_inst();
            auto iter = map.find(&start);
            if(iter == map.end()) {
                map[&start] = node;
            } else {
                LogTrace<<"Joining "<<node->get_name()<<" and "<<map[&start]->get_name()<<End(true);
                JoinFlowNode *joinNode = storagePool.persist(
                    new JoinFlowNode(
                        node->get_starting_inst(),
                        node->get_frame(), //TODO join frames or only join on nodes with same frame
                        *node,
                        *map[&start],
                        storagePool,
                        context
                    )
                );
                frontier.insert(joinNode);
                map.erase(iter);
            }
        }
        for(auto& [inst, node]: map) {
            LogTrace<<"Inserting "<<node->get_name()<<End();
            frontier.insert(node);
        }
        LogTrace<<"Finished join pass"<<End(true);
    }
}

FlowNode *SymbolicEvaluation::ChooseNextNode()
{
    switch(chooseStrategy) {
    case ChooseStrategy::NAIVE:
        LogTrace<<"Running naive choose"<<End();
        return *frontier.begin();
    case ChooseStrategy::OPTIMAL: {
        LogTrace<<"Running optimal choose"<<End();

        LogTrace<<"Frontier: "<<End();
        for(auto& node: frontier) {
            LogTrace<<"    "<<node->get_name()<<End();
        }

        if(frontier.size() == 1) {
            LogTrace<<"Only 1 option to chose from"<<End();
            return *frontier.begin();
        }

        for(auto& node: frontier) {
            if(node->get_starting_inst() != node->get_block()->begin()) {
                std::cout<<"FlowNode "<<node->get_name()<<" is not at start of block"<<std::endl;
                return node;
            }
        }

        std::unordered_map<Block*, std::unordered_map<Block*, unsigned>> descendants;
        for(auto& node: frontier) {
            Block& block = *node->get_block();
            auto it = descendants.find(&block);
            if(it != descendants.end()) continue;
            descendants[&block] = GetDescendants(&block);
        }

        //Descendants now contains the minimum distance from every block in the frontier to every reachable block

        std::unordered_set<Block*> postDominating;

        for(auto& [block, map]: descendants) {
            for(auto& [otherBlock, _]: descendants) {
                if(otherBlock == block) continue;
                if(PostDominates(block, otherBlock)) {
                    LogTrace<<block->GetName()<<" post dominates "<<otherBlock->GetName()<<End();
                    postDominating.insert(block);
                }
            }
        }

        for(auto& block: postDominating) {
            descendants.erase(block);
        }

        //Descendants now does not contain any blocks which postdominate any other blocks in the frontier

        std::unordered_set<Block*> reachable;

        for(auto& [candidate, map]: descendants) {
            for(auto& [otherBlock, otherMap]: descendants) {
                if(otherBlock == candidate) continue;
                if(otherMap.find(candidate) != otherMap.end()) {
                    reachable.insert(candidate);
                    LogTrace<<candidate->GetName()<<" reachable from "<<otherBlock->GetName()<<End();
                    break;
                }
            }
        }

        for(auto& x: reachable) {
            descendants.erase(x);
        }

        if(descendants.empty()) {
            LogWarning<<"No suitable node can be chosen"<<End();
            return *frontier.begin();
        } else {
            Block* chosenBlock = descendants.begin()->first;
            for(auto& node: frontier) {
                if(&*node->get_block() == chosenBlock) {
                    return node;
                }
            }
            LogError<<"Block chosen with no associated node"<<End();
            return *frontier.begin();
        }


        // LogWarning<<"Optimal choose not implemeted yet"<<End();
        // if(frontier.empty()) {
        //     LogError<<"Attempted to choose node from empty frontier"<<End();
        //     return nullptr;
        // }

        // auto evaluate = [](auto node) {
        //     auto name = node->get_block()->GetName();
        //     if(name == ".START") {
        //         return 0;
        //     } else if (name == ".LOOP_HEADER") {
        //         return 1;
        //     } else if (name == ".LOOP_START") {
        //         return 2;
        //     } else if (name == ".LOOP_END") {
        //         return 3;
        //     } else if (name == ".BAD_RET") {
        //         return 4;
        //     } else if (name == ".GOOD_RET") {
        //         return 5;
        //     } else {
        //         LogError<<"Unexpected block name"<<End();
        //         return 6;
        //     }
        // };

        // auto result = *frontier.begin();
        // unsigned resultValue = evaluate(result);

        // for(auto& node: frontier) {
        //     //TODO implement choose function
        //     unsigned value = evaluate(node);
        //     if(value < resultValue) {
        //         result = node;
        //         resultValue = value;
        //     }
        // }

        // return result;
    } break;
    }
}

std::unordered_map<Block*, unsigned> SymbolicEvaluation::GetDescendants(Block *block)
{
    std::unordered_set<Block*> frontier;
    std::unordered_set<Block*> next;
    unsigned distance = 0;
    std::unordered_map<Block*, unsigned> distances;

    frontier.insert(block);
    while(!frontier.empty()) {
        for(const auto& x: frontier) {
            auto it = distances.find(x);
            if(it == distances.end()) {
                distances[x] = distance;
                for(const auto& s: x->successors()) {
                    auto sIt = distances.find(s);
                    if(sIt == distances.end()) {
                        next.insert(s);
                    }
                }
            }
        }
        frontier = next;
        next.clear();
        ++distance;
    }

    return distances;
}

bool SymbolicEvaluation::PostDominates(Block *dominator, Block *source)
{
    std::unordered_set<Block*> frontier;
    std::unordered_set<Block*> next;
    std::unordered_set<Block*> seen;

    frontier.insert(source);
    while(!frontier.empty()) {
        for(const auto& x: frontier) {
            if(x == dominator) {
                 continue;
            }
            auto it = seen.find(x);
            if(it == seen.end()) { //if x has not yet been seen
                seen.insert(x);
                if(x->succ_size() == 0) {
                    return false;
                }
                for(const auto& s: x->successors()) {
                    auto sIt = seen.find(s);
                    if(s != dominator && sIt == seen.end()) { //TODO check for redundancy
                        next.insert(s);
                    }
                }
            }
        }
        frontier = next;
        next.clear();
    }

    return true;
}

void SymbolicEvaluation::StepNode(FlowNode *node)
{
    auto inst = node->get_starting_inst();
    auto block = node->get_block();
    auto func = node->get_func();
    std::optional<std::unordered_set<FlowNode*>> next;

    while(inst != block->end() && next == std::nullopt) {
        next = RunInst(*inst, node);
        LogDetail<<"Incrementing inst "<<toString(*inst)<<End();
        ++inst;
        if(inst == block->end()) {
            LogDetail<<"Incremented to block end"<<End();
        } else {
            LogDetail<<"Incremented inst "<<toString(*inst)<<End();
        }
    }

    if(next != std::nullopt) {
        auto nextNodes = next.value();
        for(auto n: nextNodes) {
            frontier.insert(n);
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
        LogTrace<<"Running add"<<End();
        return Add(static_cast<AddInst*>(&inst), node);

    case Inst::Kind::AND:
        LogTrace<<"Running and"<<End();
        And(static_cast<AndInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::ARG:
        LogTrace<<"Running arg"<<End();
        Arg(static_cast<ArgInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::CALL: 
        LogTrace<<"Running call"<<End();
        return Call(static_cast<CallInst*>(&inst), node);
    
    case Inst::Kind::CMP:
        LogTrace<<"Running cmp"<<End();
        Cmp(static_cast<CmpInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::JCC:
        LogTrace<<"Running jcc"<<End();
        return JumpCond(static_cast<JumpCondInst*>(&inst), node);
    
    case Inst::Kind::JMP:
        LogTrace<<"Running jmp"<<End();
        return Jump(static_cast<JumpInst*>(&inst), node);

    case Inst::Kind::JI:
        LogTrace<<"Running jump indirect"<<End();
        return JumpIndirect(static_cast<JumpIndirectInst*>(&inst), node);
    
    case Inst::Kind::LD:
        LogTrace<<"Running load"<<End();
        Load(static_cast<LoadInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::MOV:
        LogTrace<<"Running mov"<<End();
        Mov(static_cast<MovInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::MUL:
        LogTrace<<"Running mul"<<End();
        Mul(static_cast<MulInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::PHI:
        LogTrace<<"Running phi"<<End();
        Phi(static_cast<PhiInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::REM:
        LogTrace<<"Running rem"<<End();
        Rem(static_cast<RemInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::RET:
        LogTrace<<"Running ret"<<End();
        return Ret(static_cast<ReturnInst*>(&inst), node);

    case Inst::Kind::SET:
        LogTrace<<"Running set"<<End();
        Set(static_cast<SetInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SEXT:
        LogTrace<<"Running sign extend"<<End();
        SExt(static_cast<SExtInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SLL:
        LogTrace<<"Running sll"<<End();
        LeftLogicalShift(static_cast<SllInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SRA:
        LogTrace<<"Running sra"<<End();
        RightArithmeticShift(static_cast<SraInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SRL:
        LogTrace<<"Running srl"<<End();
        RightLogicalShift(static_cast<SrlInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::ST:
        LogTrace<<"Running store"<<End();
        Store(static_cast<StoreInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SUB:
        LogTrace<<"Running sub"<<End();
        Sub(static_cast<SubInst*>(&inst), node);
        LogTrace<<"Finished sub"<<End();
        return std::nullopt;

    case Inst::Kind::TCALL:
        LogTrace<<"Running tail call"<<End();
        return TCall(static_cast<TailCallInst*>(&inst), node);

    case Inst::Kind::ZEXT:
        LogTrace<<"Running zero extend"<<End();
        ZExt(static_cast<ZExtInst*>(&inst), node);
        return std::nullopt;
    default:
        LogTrace<<"Skipping "<<toString(inst)<<End();
        return std::nullopt;
    }
}

//-----------------------------------------------------------------------------

std::vector<Inst*> SymValueToInsts(SymValue *value, FlowNode *node, Prog *prog)
{
    LogTrace<<"Converting "<<toString(value)<<" to insts"<<End();
    assert(value != nullptr);
    auto result = std::vector<Inst*>();
    switch(value->get_kind()) {
    case SymValue::Kind::BLOCKREF: {
        auto blockref = static_cast<BlockRefSymValue*>(value);
        auto inst = new MovInst(value->get_type(), &*FindBlockByName(blockref->get_name(), prog), AnnotSet());
        result.push_back(inst);
    } break;
    case SymValue::Kind::BOOL: {
        auto boolValue = static_cast<BoolSymValue*>(value);
        auto inst = new MovInst(value->get_type(), new ConstantInt(boolValue->get_value()?1:0), AnnotSet());
        result.push_back(inst);
    } break;
    case SymValue::Kind::EXTERN: {
        auto externValue = static_cast<ExternSymValue*>(value);
        auto inst = new MovInst(value->get_type(), new Extern(externValue->get_name()), AnnotSet());
        result.push_back(inst);
    } break;
    case SymValue::Kind::FLOAT: {
        auto floatValue = static_cast<FloatSymValue*>(value);
        auto inst = new MovInst(value->get_type(), new ConstantFloat(floatValue->get_value().convertToDouble()), AnnotSet());
        result.push_back(inst);
    } break;
    case SymValue::Kind::FUNCREF: {
        auto funcValue = static_cast<FuncRefSymValue*>(value);
        auto inst = new MovInst(value->get_type(), &*FindFuncByName(funcValue->get_name(), prog), AnnotSet());
        auto result = std::vector<Inst*>();
    } break;
    case SymValue::Kind::HEAPPTR: {
        LogError<<"Not implemented yet"<<End(true);
        assert(false);
    } break;
    case SymValue::Kind::INT: {
        LogDetail<<"Int"<<End();
        auto intValue = static_cast<IntSymValue*>(value);
        LogDetail<<toString(intValue)<<End();
        auto inst = new MovInst(value->get_type(), new ConstantInt(intValue->get_value().getLimitedValue()), AnnotSet());
        LogDetail<<toString(inst)<<End();
        result.push_back(inst);
    } break;
    case SymValue::Kind::STATICPTR: {
        LogDetail<<"Static ptr"<<End();
        auto ptrValue = static_cast<StaticPtrSymValue*>(value);
        auto atom = FindAtomByName(ptrValue->get_name(), prog);
        auto addrInst = new MovInst(value->get_type(), atom, AnnotSet());
        result.push_back(addrInst);
        LogDetail<<toString(addrInst)<<End();

        unsigned defaultOffset = node->get_store().getLabel(ptrValue->get_name())->offset;
        unsigned offset = ptrValue->get_offset().getLimitedValue();
        if(offset < defaultOffset) {
            LogDetail<<"Default offset for "<<ptrValue->get_name()<<" is "<<defaultOffset<<End();
            LogDetail<<"Actual offset is "<<ptrValue->get_offset().getLimitedValue()<<End();
            auto diffInst = new MovInst(value->get_type(), new ConstantInt(defaultOffset-offset), AnnotSet());
            result.push_back(diffInst);
            auto subInst = new SubInst(value->get_type(), addrInst, diffInst, AnnotSet());
            result.push_back(subInst);
        } else if(offset > defaultOffset) {
            LogDetail<<"Default offset for "<<ptrValue->get_name()<<" is "<<defaultOffset<<End();
            LogDetail<<"Actual offset is "<<ptrValue->get_offset().getLimitedValue()<<End();
            auto diffInst = new MovInst(value->get_type(), new ConstantInt(offset-defaultOffset), AnnotSet());
            result.push_back(diffInst);
            auto addInst = new AddInst(value->get_type(), addrInst, diffInst, AnnotSet());
            result.push_back(addInst);
        }
    } break;
    case SymValue::Kind::UNKNOWN:
        LogError<<"Not implemented yet"<<End(true);
        assert(false);
        break;
    default:
        LogError<<"Missing case"<<End(true);
        assert(false);
        break;
    }
    LogDetail<<"Returning "<<result.size()<<" insts"<<End();
    return result;
}

std::vector<DataStore::Action*> GetImportantWrites(FlowNode *node)
{
    std::vector<DataStore::Action*> log = node->get_store().getFullLog();
    LogDetail<<"Retrieved log of size "<<log.size()<<End();
    for(int i = log.size()-1; i >= 0; --i) {
        LogDetail<<"i: "<<i<<End();
        auto action = log[i];
        if(action->get_kind() == DataStore::Action::Kind::WRITE) {
            auto write = static_cast<DataStore::Write*>(action);
            for(int j = i-1; j >= 0; --j) {
                if(log[j]->get_kind() == DataStore::Action::Kind::WRITE) {
                    auto write2 = static_cast<DataStore::Write*>(log[j]);
                    if(SymComp::EQ(write->get_addr(), write2->get_addr(), node).first == SymComp::Result::TRUE) {
                        log.erase(log.begin()+j);
                        --i;
                    }
                }
            }
        } else {
            log.erase(log.begin()+i);
        }
    }
    LogDetail<<"Finished getting important writes"<<End();
    return log;
}

void SymbolicEvaluation::Rewrite()
{
    auto& [endNode, returnValue] = *finishedNodes.begin(); //TEMP
    auto insts = SymValueToInsts(returnValue, endNode, prog);
    auto log = GetImportantWrites(endNode);
    auto writeInsts = std::vector<std::pair<Inst*,SymValue*>>();
    std::transform(
        log.begin(), log.end(),
        std::back_inserter(writeInsts),
        [](auto& action) {
            assert(action->get_kind() == DataStore::Action::Kind::WRITE);
            auto write = static_cast<DataStore::Write*>(action);
            return std::make_pair(write->get_inst(), write->get_addr());
        }
    );
    LogDetail<<"Populated write insts"<<End();
    auto writeOperandInsts = std::vector<std::vector<Inst*>>();
    for(auto& [inst, addr]: writeInsts){
        LogDetail<<toString(inst)<<End();
        if(inst->GetKind() == Inst::Kind::ST) {
            auto storeInst = static_cast<StoreInst*>(inst);
            auto op = storeInst->GetVal();
            LogDetail<<toString(op)<<End();
            auto symValue = endNode->GetResult(op);
            LogDetail<<toString(symValue)<<End();
            auto valueInsts = SymValueToInsts(symValue, endNode, prog);
            auto addrInsts = SymValueToInsts(addr, endNode, prog);
            auto allInsts = valueInsts;
            allInsts.insert(allInsts.end(), addrInsts.begin(), addrInsts.end());
            auto newStoreInst = new StoreInst(storeInst->GetStoreSize(), *addrInsts.rbegin(), *valueInsts.rbegin(), AnnotSet());
            LogDetail<<"Created new store inst "<<toString(newStoreInst)<<End();
            allInsts.push_back(newStoreInst);
            writeOperandInsts.push_back(allInsts);
        } else {
            LogError<<"Other writes not implemented yet"<<End(true);
            assert(false);
        }
    }
    LogDetail<<"Creating new block"<<End();
    Block *block = new Block(".rewriteBlock1");
    LogDetail<<"Created new block"<<End();
    LogDetail<<"Adding write operand insts"<<End();
    for(auto& v: writeOperandInsts) {
        for(auto& inst: v) {
            LogDetail<<"Adding "<<toString(inst)<<End();
            block->AddInst(inst);
        }
    }
    LogDetail<<"Adding return insts"<<End();
    for(auto i = insts.rbegin(); i != insts.rend(); ++i) {
        LogDetail<<"Adding "<<toString(*i)<<End();
        block->AddInst(*i);
    }
    auto returnInst = new ReturnInst(*insts.rbegin(), AnnotSet());
    LogDetail<<"Adding final return inst "<<toString(returnInst)<<End();
    block->AddInst(returnInst);
    Func *func = new Func(prog, endNode->get_func()->GetName());
    func->AddBlock(block);
    prog->AddFunc(func);
    endNode->get_func()->replaceAllUsesWith(func);
    endNode->get_func()->eraseFromParent();
    LogFlow<<"Done"<<End();
}

//-----------------------------------------------------------------------------

RootFlowNode *SymbolicEvaluation::CreateRootNode()
{
    PrintDataInfo(prog);
    auto startFunc = FindFuncByName(start_function, prog);
    if(startFunc == prog->end()) {
        LogWarning<<"Failed to create root node"<<End();
        return nullptr;
    }

    return storagePool.persist(
        new RootFlowNode(*startFunc, *prog, storagePool, context)
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
            args
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
    FlowNode *previous,
    std::optional<z3::expr> constraint
) {
    return storagePool.persist(previous->CreateBlockNode(*block, constraint));
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
            caller
        )
    );
}

// -----------------------------------------------------------------------------

std::optional<std::unordered_set<FlowNode*>> SymbolicEvaluation::Add(
    AddInst *addInst,
    FlowNode *node
) {
    auto [lhs, rhs] = getOperandValues(addInst, node);

    SymValue *result;
    Type resultType = addInst->GetType();

    try {
        switch(lhs->get_kind()) {
        case SymValue::Kind::STATICPTR: {
            StaticPtrSymValue *laddr = static_cast<StaticPtrSymValue*>(lhs);
            switch(rhs->get_kind()) {
            case SymValue::Kind::INT: {
                IntSymValue *rint = static_cast<IntSymValue*>(rhs);
                result = new StaticPtrSymValue(laddr->get_name(), laddr->get_offset()+rint->get_value(), laddr->get_max(), resultType);
                LogDetail<<laddr->toString()<<"+"<<rint->toString()<<" = "<<static_cast<StaticPtrSymValue*>(result)->toString()<<End();
            } break;
            default:
                LogWarning<<"Adding STATICPTR and not INT"<<End();
                result = new UnknownSymValue(resultType);
                break;
            }
        } break;
        case SymValue::Kind::INT: {
            LogDetail<<"Adding lhs int"<<End();
            IntSymValue *lint = static_cast<IntSymValue*>(lhs);
            switch(rhs->get_kind()) {
            case SymValue::Kind::STATICPTR: {
                StaticPtrSymValue *raddr = static_cast<StaticPtrSymValue*>(rhs);
                result = new StaticPtrSymValue(raddr->get_name(), raddr->get_offset()+lint->get_value(), raddr->get_max(), resultType);
                LogDetail<<lint->toString()<<"+"<<raddr->toString()<<" = "<<static_cast<StaticPtrSymValue*>(result)->toString()<<End();
            } break;
            case SymValue::Kind::FLOAT: {
                auto rf = static_cast<FloatSymValue*>(rhs);
                if(resultType == Type::F64) {
                    auto resultValue = llvm::APFloat(static_cast<double>(0));
                    resultValue.convertFromAPInt(lint->get_value(), isSigned(lint->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                    resultValue = resultValue + rf->get_value();
                    result = new FloatSymValue(resultValue, resultType);
                    LogDetail<<lint->toString()<<"+"<<rf->toString()<<" = "<<static_cast<FloatSymValue*>(result)->toString()<<End();
                }else if (resultType == Type::F32) {
                    auto resultValue = llvm::APFloat(static_cast<float>(0));
                    resultValue.convertFromAPInt(lint->get_value(), isSigned(lint->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                    resultValue = resultValue + rf->get_value();
                    result = new FloatSymValue(resultValue, resultType);
                    LogDetail<<lint->toString()<<"+"<<rf->toString()<<" = "<<static_cast<FloatSymValue*>(result)->toString()<<End();
                } else {
                    LogWarning<<"Adding int and float and not expecting float as a result"<<End();
                    result = new UnknownSymValue(resultType);
                }
            } break;
            case SymValue::Kind::INT: {
                LogDetail<<"rhs int"<<End();
                IntSymValue *rint = static_cast<IntSymValue*>(rhs);
                result = new IntSymValue(lint->get_value()+rint->get_value(), resultType);
                LogDetail<<lint->toString()<<"+"<<rint->toString()<<" = "<<static_cast<IntSymValue*>(result)->toString()<<End();
            } break;
            default:
                LogDetail<<"Unable to calculate INT plus "<<toString(rhs->get_kind())<<End();
                result = new UnknownSymValue(resultType);
                break;
            }
        } break;
        default:
            LogDetail<<"Unable to calculate "<<toString(lhs->get_kind())<<" plus "<<toString(rhs->get_kind())<<End();
            result = new UnknownSymValue(resultType);
            break;
        }

        storagePool.persist(result);
        node->AllocateResult(addInst, result);
        return std::nullopt;
    } catch (OffsetOutOfBoundsException e) {
        return std::unordered_set<FlowNode*>();
    }
    
}

void SymbolicEvaluation::And(
    AndInst *andInst,
    FlowNode *node
) {
    auto [lhs, rhs] = getOperandValues(andInst, node);
    Type resultType = andInst->GetType();
    switch(lhs->get_kind()) {
    case SymValue::Kind::INT: {
        auto l = static_cast<IntSymValue*>(lhs);
        if(l->get_value().isAllOnesValue()) {
            auto result = rhs->copy_cast(resultType);
            LogDetail<<l->toString()<<" is all ones: result = "<<toString(result)<<End();
            node->AllocateResult(andInst,storagePool.persist(result));
        } else if (l->get_value().getLimitedValue() == 0) {
            auto result = new IntSymValue(0, resultType);
            LogDetail<<l->toString()<<" is all zeros: result = "<<toString(result)<<End();
            node->AllocateResult(andInst,storagePool.persist(result));
        } else {
            LogDetail<<l->toString()<<" and ";
            switch(rhs->get_kind()) {
            case SymValue::Kind::INT: {
                auto r = static_cast<IntSymValue*>(rhs);
                LogDetail<<r->toString()<<" = ";
                auto result = new IntSymValue(l->get_value() & r->get_value(), resultType);
                LogDetail<<result->toString()<<End();
            } break;
            default:
                LogDetail<<toString(rhs)<<" is not calculable"<<End();
                node->AllocateResult(andInst, storagePool.persist(new UnknownSymValue(resultType)));
                break;
            }
        }
    } break;
    default:
        switch(rhs->get_kind()) {
        case SymValue::Kind::INT: {
            auto r = static_cast<IntSymValue*>(rhs);
            if(r->get_value().isAllOnesValue()) {
                auto result = lhs->copy_cast(resultType);
                LogDetail<<r->toString()<<" is all ones: result = "<<toString(result)<<End();
                node->AllocateResult(andInst,storagePool.persist(result));
            } else if (r->get_value().getLimitedValue() == 0) {
                auto result = new IntSymValue(0, resultType);
                LogDetail<<r->toString()<<" is all zeros: result = "<<toString(result)<<End();
                node->AllocateResult(andInst,storagePool.persist(result));
            } else {
                LogDetail<<toString(lhs)<<" and "<<r->toString()<<" is not calculable"<<End();
                node->AllocateResult(andInst, storagePool.persist(new UnknownSymValue(resultType)));
            }
        } break;
        default:
            if(SymComp::EQ(lhs,rhs,node).first == SymComp::Result::TRUE) {
                LogDetail<<toString(lhs)<<" and "<<toString(rhs)<<" are equal"<<End();
                node->AllocateResult(andInst, storagePool.persist(lhs->copy_cast(resultType)));
            } else {
                LogDetail<<toString(lhs)<<" and "<<toString(rhs)<<" is not calculable"<<End();
                node->AllocateResult(andInst, storagePool.persist(new UnknownSymValue(resultType)));
            }
        } break;
    }
}

void SymbolicEvaluation::Arg(
    ArgInst *argInst,
    FlowNode *node
) {
    unsigned int idx = argInst->GetIdx();
    SymValue *result = node->get_frame().get_arg(idx);
    LogDetail<<toString(result)<<" at index "<<idx<<End();

    node->AllocateResult(
        argInst,
        result
    );
}

std::optional<std::unordered_set<FlowNode*>> SymbolicEvaluation::Call(
    CallInst *callInst,
    FlowNode *node
) {
    SymValue *v = node->GetResult(callInst->GetCallee());

    if ( v == nullptr ) {
        LogWarning<<"Attempted to call nullptr"<<End();
        if(!callInst->IsVoid()) {
            node->AllocateResult(callInst,storagePool.persist(new UnknownSymValue(*callInst->GetType())));
        }
        return std::unordered_set<FlowNode*>();
    }

    std::vector<SymValue*> args;
    std::transform(
        callInst->arg_begin(), callInst->arg_end(),
        std::back_inserter(args),
        [this, node](auto x) { return node->GetResult(x); }
    );

    switch(v->get_kind()) {
    case SymValue::Kind::FUNCREF: {
        auto funcRef = static_cast<FuncRefSymValue*>(v);
        
        SuccessorFlowNode *newNode = storagePool.persist(
            node->CreateFunctionNode(
                *FindFuncByName(funcRef->get_name(), prog),
                args,
                callInst->getIterator()
            )
        );

        std::unordered_set<FlowNode*> result;
        result.insert(newNode);
        return result;
    }

    case SymValue::Kind::EXTERN: {
        auto ext = static_cast<ExternSymValue*>(v);
        bool evaluable = false;
        LogDetail<<"Calling extern "<<ext->get_name()<<End();
        if(externHasStub(ext->get_name())) {
            evaluable = true;
            runExtern(ext->get_name(), args, node, callInst->GetType(), storagePool);
        } else if(knownSafeExtern(ext->get_name())) {
            LogDetail<<"Not invalidating store"<<End();
        } else {
            LogDetail<<"Invalidating store"<<End();
            //TODO DO INVALIDATE
            //node->get_store().invalidate();
        }
        
        if(!evaluable && !callInst->IsVoid()){
            node->AllocateResult(callInst, storagePool.persist(new UnknownSymValue(*callInst->GetType())));
        }
        return std::nullopt;
    }

    default:
        LogWarning<<"Attempted to call something of unhandled type"<<End();
        return std::unordered_set<FlowNode*>();
    }
}

void SymbolicEvaluation::Cmp(
    CmpInst *cmpInst,
    FlowNode *node
) {
    Cond c = cmpInst->GetCC();
    auto [lhs, rhs] = getOperandValues(cmpInst, node);
    SymComp::Result result;
    std::optional<z3::expr> constraint;

    LogDetail<<"lhs: "<<toString(lhs)<<" rhs: "<<toString(rhs)<<End();

    if(lhs == nullptr || rhs == nullptr) {
        LogWarning<<"operand nullptr"<<End();
        return;
    }

    switch(c) {
    case Cond::EQ: {
        LogDetail<<"Comparing EQ"<<End();
        auto [r, constr] = SymComp::EQ(lhs, rhs, node);
        result = r;
        constraint = constr;
    } break;
    
    case Cond::NE: {
        LogDetail<<"Comparing NE"<<End();
        auto [r, constr] = SymComp::NEQ(lhs, rhs, node);
        result = r;
        constraint = constr;
    } break;
    
    case Cond::LT:
        LogDetail<<"Comparing LT"<<End();
        result = SymComp::LT(lhs, rhs);
        break;
    
    case Cond::GT: {
        LogDetail<<"Comparing GT"<<End();
        auto [r, constr] = SymComp::GT(lhs, rhs, node);
        result = r;
        constraint = constr;
    } break;
    
    case Cond::LE: {
        LogDetail<<"Comparing LE"<<End();
        auto [r, constr] = SymComp::LE(lhs, rhs, node);
        result = r;
        constraint = constr;
    } break;
    
    case Cond::GE:
        LogDetail<<"Comparing GE"<<End();
        result = SymComp::GE(lhs, rhs);
        break;
    
    default:
        LogDetail<<"Comparing "<<(int)c<<End();
        result = SymComp::Result::UNKNOWN;
        break;
    }

    SymValue *value = SymComp::ToSymValue(result, cmpInst->GetType());
    storagePool.persist(value);
    node->AllocateResult(cmpInst, value);
    if(constraint.has_value() && value->get_kind() == SymValue::Kind::UNKNOWN) {
        auto unknown = static_cast<UnknownSymValue*>(value);
        auto expr = 
            (
                node->get_context().bv_const(unknown->get_name().c_str(), bitLength(unknown->get_type()))
                != 0
            )
            == *constraint;
        LogDetail<<"Adding constraint "<<expr.to_string()<<End();
        node->AddConstraint(expr);
        
    }
}

std::unordered_set<FlowNode*> SymbolicEvaluation::Jump(
    JumpInst *jumpInst,
    FlowNode *node
) {
    auto block = jumpInst->GetTarget()->getIterator();
    std::unordered_set<FlowNode*> result;
    result.insert(
        CreateBlockFlowNode(
            block,
            node
        )
    );
    return result;
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
        LogDetail<<"Condition is bool "<<(b->get_value()?"true":"false")<<End();
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
            LogTrace<<"Jumping from "<<node->get_func()->GetName()<<" to "<<func->GetName()<<End();
        }
        LogTrace<<"Jumping to block "<<block->GetName()<<End();
        FlowNode *newNode = CreateBlockFlowNode(block->getIterator(), node);
        result.insert(newNode);
    } else if (symCond->get_kind() == SymValue::Kind::INT) {
        auto i = static_cast<IntSymValue*>(symCond);
        LogDetail<<"Condition is int "<<i->toString()<<End();
        Block *block;
        Func *func;
        if(i->get_value().getBoolValue()) {
            block = trueBlock;
            func = trueFunc;
        } else {
            block = falseBlock;
            func = falseFunc;
        }
        if(func->GetName() != node->get_func()->GetName()) {
            LogTrace<<"Jumping from "<<node->get_func()->GetName()<<" to "<<func->GetName()<<End();
        }
        LogTrace<<"Jumping to block "<<block->GetName()<<End();
        FlowNode *newNode = CreateBlockFlowNode(block->getIterator(), node);
        result.insert(newNode);
    } else if ( symCond->get_kind() == SymValue::Kind::UNKNOWN) {
        LogDetail<<"Branching based on unknown"<<End();
        auto unknown = static_cast<UnknownSymValue*>(symCond);
        auto &c = node->get_context();
        z3::expr trueConstraint = c.bv_const(unknown->get_name().c_str(), bitLength(unknown->get_type())) != 0;
        bool takeTrue, takeFalse;
        z3::solver s(c);
        node->AssertStateConstraints(s);
        s.add(node->get_path_constraint());
        s.push();
        s.add(!trueConstraint);
        switch(s.check()) {
        case z3::sat: takeFalse = true; break;
        case z3::unsat: takeFalse = false; break;
        case z3::unknown: takeFalse = true; break;
        }
        s.pop();
        s.add(trueConstraint);
        switch(s.check()) {
        case z3::sat: takeTrue = true; break;
        case z3::unsat: takeTrue = false; break;
        case z3::unknown: takeTrue = true; break;
        }
        if(takeTrue) {
            LogDetail<<"Taking true: "<<trueBlock->GetName()<<" with constraint "<<trueConstraint.to_string()<<End();
            result.insert(
                CreateBlockFlowNode(
                    trueBlock->getIterator(),
                    node,
                    trueConstraint
                )
            );
        }
        if(takeFalse) {
            LogDetail<<"Taking false: "<<falseBlock->GetName()<<" with constraint "<<(!trueConstraint).to_string()<<End();
            result.insert(
                CreateBlockFlowNode(
                    falseBlock->getIterator(),
                    node,
                    !trueConstraint
                )
            );
        }
    } else {
        LogDetail<<"Taking both branches"<<End();
        LogTrace<<"Jumping to blocks "<<trueBlock->GetName()<<" and "<<falseBlock->GetName()<<End();
        result.insert(
            CreateBlockFlowNode(trueBlock->getIterator(), node)
        );
        result.insert(
            CreateBlockFlowNode(falseBlock->getIterator(), node)
        );
    }
    return result;
}

std::unordered_set<FlowNode*> SymbolicEvaluation::JumpIndirect(
    JumpIndirectInst *jumpIndirectInst,
    FlowNode *node
) {
    auto target = node->GetResult(jumpIndirectInst->GetTarget());
    switch(target->get_kind()) {
    case SymValue::Kind::BLOCKREF: {
        auto block = static_cast<BlockRefSymValue*>(target);
        auto iter = FindBlockByName(block->get_name(), prog);
        if(iter == prog->end()->end()) {
            LogWarning<<"Block not found"<<End();
            return std::unordered_set<FlowNode*>();
        }
        std::unordered_set<FlowNode*> result;
        result.insert(CreateBlockFlowNode(iter,node));
        return result;
    } break;
    default:
        LogWarning<<"Unable to jump to "<<toString(target)<<End();
        return std::unordered_set<FlowNode*>();
    }
}

void SymbolicEvaluation::LeftLogicalShift(
    SllInst *sllInst,
    FlowNode *node
) {
    auto [lhs, rhs] = getOperandValues(sllInst, node);

    SymValue *result;
    Type resultType = sllInst->GetType();

    switch(rhs->get_kind()) {
    case SymValue::Kind::INT: {
        auto r = static_cast<IntSymValue*>(rhs);
        switch(lhs->get_kind()) {
        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lhs);
            result = new IntSymValue(l->get_value() << r->get_value(), resultType);
            LogDetail<<l->toString()<<"<<"<<r->toString()<<" = "<<static_cast<IntSymValue*>(result)->toString()<<End();
        } break;
        default:
            LogDetail<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted left by "<<r->toString()<<End();
            result = new UnknownSymValue(resultType);
            break;
        }
    } break;
    default :
        LogDetail<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted left by "<<toString(rhs->get_kind())<<End();
        result = new UnknownSymValue(resultType);
        break;
    }

    storagePool.persist(result);
    node->AllocateResult(
        sllInst,
        result
    );
}

void SymbolicEvaluation::Load(
    LoadInst *loadInst,
    FlowNode *node
) {
    SymValue *addr = node->GetResult(loadInst->GetAddr());
    node->AllocateResult(loadInst,node->get_store().read(addr, loadInst->GetLoadSize(), loadInst->GetType(), node, loadInst));
}

void SymbolicEvaluation::Mov(
    MovInst *movInst,
    FlowNode *node
) {
    node->AllocateResult(
        movInst,
        node->CreateSymValue(
            movInst->GetArg(),
            movInst->GetType()
        )
    );
}

void SymbolicEvaluation::Mul(
    MulInst *mulInst,
    FlowNode *node
) {
    auto [lhs, rhs] = getOperandValues(mulInst, node);

    Type resultType = mulInst->GetType();

    switch(lhs->get_kind()) {
    case SymValue::Kind::FLOAT: {
        auto l = static_cast<FloatSymValue*>(lhs);
        LogDetail<<"Multiplying float "<<l->toString();
        switch(rhs->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto r = static_cast<FloatSymValue*>(rhs);
            LogDetail<<" with float "<<r->toString();
            auto result = new FloatSymValue(
                l->get_value() * r->get_value(),
                resultType
            );
            LogDetail<<" to produce "<<result->toString();
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    result
                )
            );
        } break;
        case SymValue::Kind::INT: {
            auto r = static_cast<IntSymValue*>(rhs);
            LogDetail<<" with int "<<r->toString();
            SymValue *result;
            if(resultType == Type::F64) {
                auto resultValue = llvm::APFloat(static_cast<double>(0));
                resultValue.convertFromAPInt(r->get_value(), isSigned(r->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + l->get_value();
                result = new FloatSymValue(resultValue, resultType);
                LogDetail<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            }else if (resultType == Type::F32) {
                auto resultValue = llvm::APFloat(static_cast<float>(0));
                resultValue.convertFromAPInt(r->get_value(), isSigned(r->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + l->get_value();
                result = new FloatSymValue(resultValue, resultType);
                LogDetail<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            } else {
                LogDetail<<End();
                LogWarning<<"Adding int and float and not expecting float as a result"<<End();
                result = new UnknownSymValue(resultType);
            }
            
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    result
                )
            );
        } break;
        default: {
            LogDetail<<" with "<<(rhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    new UnknownSymValue(resultType)
                )
            );
        } break;
        }
    } break;
    case SymValue::Kind::INT: {
        auto l = static_cast<IntSymValue*>(lhs);
        LogDetail<<"Multiplying int "<<l->toString();
        switch(rhs->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto r = static_cast<FloatSymValue*>(rhs);
            LogDetail<<" with float "<<r->toString();
            SymValue *result;
            if(resultType == Type::F64) {
                auto resultValue = llvm::APFloat(static_cast<double>(0));
                resultValue.convertFromAPInt(l->get_value(), isSigned(l->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + r->get_value();
                result = new FloatSymValue(resultValue, resultType);
                LogDetail<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            }else if (resultType == Type::F32) {
                auto resultValue = llvm::APFloat(static_cast<float>(0));
                resultValue.convertFromAPInt(l->get_value(), isSigned(l->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + r->get_value();
                result = new FloatSymValue(resultValue, resultType);
                LogDetail<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            } else {
                LogDetail<<End();
                LogWarning<<"Adding int and float and not expecting float as a result"<<End();
                result = new UnknownSymValue(resultType);
            }
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    result
                )
            );
        } break;
        case SymValue::Kind::INT: {
            auto r = static_cast<IntSymValue*>(rhs);
            LogDetail<<" with int "<<r->toString();
            IntSymValue *result = new IntSymValue(
                l->get_value() * r->get_value(),
                resultType
            );
            LogDetail<<" to produce "<<result->toString();
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    result
                )
            );
        } break;
        default: {
            LogDetail<<" with "<<(rhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    new UnknownSymValue(resultType)
                )
            );
        } break;
        }
    } break;
    default:
        LogDetail<<"Multiplying "<<(lhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
        LogDetail<<" with "<<(rhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
        node->AllocateResult(
            mulInst,
            storagePool.persist(
                new UnknownSymValue(resultType)
            )
        );
        break;
    }
    LogDetail<<End();
}

void SymbolicEvaluation::Rem(
    RemInst *remInst,
    FlowNode *node
) {
    auto [lhs, rhs] = getOperandValues(remInst, node);
    Type resultType = remInst->GetType();

    if(isIntType(resultType)) {
        if(lhs->get_kind() == SymValue::Kind::INT && rhs->get_kind() == SymValue::Kind::INT) {
            auto l = static_cast<IntSymValue*>(lhs);
            auto r = static_cast<IntSymValue*>(rhs);
            LogDetail<<l->toString()<<(isSigned(resultType)?" srem ":" urem ")<<r->toString()<<" ";
            node->AllocateResult(
                remInst,
                storagePool.persist(
                    new IntSymValue(
                        isSigned(resultType) 
                            ? l->get_value().srem(r->get_value())
                            : l->get_value().urem(r->get_value()),
                        resultType
                    )
                )
            );
        } else {
            node->AllocateResult(
                remInst,
                storagePool.persist(
                    new UnknownSymValue(resultType)
                )
            );
        }
    } else {
        if(lhs->get_kind() == SymValue::Kind::FLOAT && rhs->get_kind() == SymValue::Kind::FLOAT) {
            auto l = static_cast<FloatSymValue*>(lhs);
            auto r = static_cast<FloatSymValue*>(rhs);
            LogDetail<<l->toString()<<" frem "<<r->toString()<<" ";
            llvm::APFloat resultValue = l->get_value();
            auto status = resultValue.remainder(r->get_value());
            if(status == llvm::APFloatBase::opOK) {
                node->AllocateResult(
                    remInst,
                    storagePool.persist(
                        new FloatSymValue(
                            resultValue,
                            resultType
                        )
                    )
                );
            } else {
                node->AllocateResult(
                remInst,
                storagePool.persist(
                    new UnknownSymValue(resultType)
                )
            );
            }
        } else {
            node->AllocateResult(
                remInst,
                storagePool.persist(
                    new UnknownSymValue(resultType)
                )
            );
        }
    }
    LogDetail<<toString(*node->GetResult(remInst))<<End();
}

std::unordered_set<FlowNode*> SymbolicEvaluation::Ret(
    ReturnInst *returnInst,
    FlowNode *node
) {
    Inst *caller = node->get_frame().get_caller();
    LogDetail<<"Caller: "<<toString(caller)<<End();
    FlowNode *newNode = CreateReturnFlowNode(node);
    if(newNode == nullptr) {
        SymValue *returnValue = node->GetResult(returnInst->GetValue());
        assert(finishedNodes.find(node) == finishedNodes.end());
        finishedNodes.insert({node, returnValue});
        return std::unordered_set<FlowNode*>();
    } else if(!caller->IsVoid()) {
        LogDetail<<"Caller is not void"<<End();
        SymValue *returnValue = node->GetResult(returnInst->GetValue());
        LogDetail<<"Return value "<<toString(returnValue)<<End();
        newNode->AllocateResult(caller, returnValue);
    }
    std::unordered_set<FlowNode*> nodes;
    nodes.insert(newNode);
    return nodes;
}

void SymbolicEvaluation::RightArithmeticShift(
    SraInst *sraInst,
    FlowNode *node
) {
    auto [lhs, rhs] = getOperandValues(sraInst, node);

    SymValue *result;
    Type resultType = sraInst->GetType();

    switch(rhs->get_kind()) {
    case SymValue::Kind::INT: {
        auto r = static_cast<IntSymValue*>(rhs);
        switch(lhs->get_kind()) {
        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lhs);
            result = new IntSymValue(l->get_value().ashr(r->get_value()), resultType);
            LogDetail<<"Shifted "<<l->toString()<<" right arithmetically by "<<r->toString()<<" to produce "<<static_cast<IntSymValue*>(result)->toString()<<End();
        } break;
        default:
            LogDetail<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<r->toString()<<End();
            result = new UnknownSymValue(resultType);
            break;
        }
    } break;
    default :
        LogDetail<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<toString(rhs->get_kind())<<End();
        result = new UnknownSymValue(resultType);
        break;
    }

    storagePool.persist(result);
    node->AllocateResult(
        sraInst,
        result
    );
}

void SymbolicEvaluation::RightLogicalShift(
    SrlInst *srlInst,
    FlowNode *node
) {
    auto [lhs, rhs] = getOperandValues(srlInst, node);

    SymValue *result;
    Type resultType = srlInst->GetType();

    switch(rhs->get_kind()) {
    case SymValue::Kind::INT: {
        auto r = static_cast<IntSymValue*>(rhs);
        switch(lhs->get_kind()) {
        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lhs);
            result = new IntSymValue(l->get_value().lshr(r->get_value()), resultType);
            LogDetail<<"Shifted "<<l->toString()<<" right logically by "<<r->toString()<<" to produce "<<static_cast<IntSymValue*>(result)->toString()<<End();
        } break;
        default:
            LogDetail<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<r->toString()<<End();
            result = new UnknownSymValue(resultType);
            break;
        }
    } break;
    default :
        LogDetail<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<toString(rhs->get_kind())<<End();
        result = new UnknownSymValue(resultType);
        break;
    }

    storagePool.persist(result);
    node->AllocateResult(
        srlInst,
        result
    );
}

void SymbolicEvaluation::Set(
    SetInst *setInst,
    FlowNode *node
) {
    auto value = node->GetResult(setInst->GetValue());
    LogDetail<<"Setting "<<toString(setInst->GetReg())<<" to "<<toString(value)<<End();
    node->SetRegister(setInst->GetReg()->GetValue(), value);
}

void SymbolicEvaluation::SExt(
    SExtInst *sExtInst,
    FlowNode *node
) {
    auto op = node->GetResult(sExtInst->GetArg());
    Type resultType = sExtInst->GetType();
    unsigned length = bitLength(resultType);
    switch(op->get_kind()) {
    case SymValue::Kind::INT: {
        auto x = static_cast<IntSymValue*>(op);
        auto value = x->get_value().sext(length);
        auto result = new IntSymValue(value, resultType);
        LogDetail<<"Sign extending "<<x->toString()<<" to "<<result->toString()<<End();
        node->AllocateResult(sExtInst, storagePool.persist(result));
    } break;
    default: {
        auto result = new UnknownSymValue(resultType);
        LogDetail<<"Unable to sign extend a non-integer value"<<End();
    } break;
    }
}

void SymbolicEvaluation::Store(
    StoreInst *storeInst,
    FlowNode *node
) {
    SymValue *addr = node->GetResult(storeInst->GetAddr());
    SymValue *value = node->GetResult(storeInst->GetVal());
    node->get_store().write(addr, value, storeInst);
}

std::optional<std::unordered_set<FlowNode*>> SymbolicEvaluation::Sub(
    SubInst *subInst,
    FlowNode *node
) {
    auto [lhs, rhs] = getOperandValues(subInst, node);

    SymValue *result;
    Type resultType = subInst->GetType();

    try {
        switch(lhs->get_kind()) {
        case SymValue::Kind::STATICPTR: {
            StaticPtrSymValue *laddr = static_cast<StaticPtrSymValue*>(lhs);
            switch(rhs->get_kind()) {
            case SymValue::Kind::INT: {
                IntSymValue *rint = static_cast<IntSymValue*>(rhs);
                result = new StaticPtrSymValue(laddr->get_name(), laddr->get_offset()-rint->get_value(), laddr->get_max(), resultType);
                LogDetail<<laddr->toString()<<"+"<<rint->toString()<<" = "<<static_cast<StaticPtrSymValue*>(result)->toString()<<End();
            } break;
            case SymValue::Kind::STATICPTR: {
                auto raddr = static_cast<StaticPtrSymValue*>(rhs);
                if(raddr->get_name() == laddr->get_name()) {
                    result = new IntSymValue(laddr->get_offset()-raddr->get_offset(), resultType);
                } else {
                    LogWarning<<"Subtracting pointers to different atoms"<<End();
                    result = new UnknownSymValue(resultType);
                }
            } break;
            default:
                LogWarning<<"Subtracting "<<toString(rhs->get_kind())<<" from STATICPTR"<<End();
                result = new UnknownSymValue(resultType);
                break;
            }
        } break;
        case SymValue::Kind::INT: {
            IntSymValue *lint = static_cast<IntSymValue*>(lhs);
            switch(rhs->get_kind()) {
            case SymValue::Kind::FLOAT: {
                auto rf = static_cast<FloatSymValue*>(rhs);
                if(resultType == Type::F64) {
                    auto resultValue = llvm::APFloat(static_cast<double>(0));
                    resultValue.convertFromAPInt(lint->get_value(), isSigned(lint->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                    resultValue = resultValue - rf->get_value();
                    result = new FloatSymValue(resultValue, resultType);
                    LogDetail<<lint->toString()<<"-"<<rf->toString()<<" = "<<static_cast<FloatSymValue*>(result)->toString()<<End();
                }else if (resultType == Type::F32) {
                    auto resultValue = llvm::APFloat(static_cast<float>(0));
                    resultValue.convertFromAPInt(lint->get_value(), isSigned(lint->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                    resultValue = resultValue - rf->get_value();
                    result = new FloatSymValue(resultValue, resultType);
                    LogDetail<<lint->toString()<<"-"<<rf->toString()<<" = "<<static_cast<FloatSymValue*>(result)->toString()<<End();
                } else {
                    LogWarning<<"Subtracting float from int and not expecting float as a result"<<End();
                    result = new UnknownSymValue(resultType);
                }
            } break;
            case SymValue::Kind::INT: {
                IntSymValue *rint = static_cast<IntSymValue*>(rhs);
                result = new IntSymValue(lint->get_value()-rint->get_value(), resultType);
                LogDetail<<lint->toString()<<"-"<<rint->toString()<<" = "<<static_cast<IntSymValue*>(result)->toString()<<End();
            } break;
            default:
                LogDetail<<"Unable to calculate INT minus "<<toString(rhs->get_kind())<<End();
                result = new UnknownSymValue(resultType);
                break;
            }
        } break;
        case SymValue::Kind::UNKNOWN: {
            auto lval = static_cast<UnknownSymValue*>(lhs);
            LogDetail<<"Subtracting from "<<lval->get_name()<<End();
            switch(rhs->get_kind()) {
            case SymValue::Kind::INT: {
                auto rval = static_cast<IntSymValue*>(rhs);
                auto unknown = new UnknownSymValue(resultType);
                z3::expr x = context.bv_const(unknown->get_name().c_str(), bitLength(resultType));
                z3::expr l = context.bv_const(lval->get_name().c_str(), bitLength(lval->get_type()));
                z3::expr r = context.bv_val(rval->get_value().getLimitedValue(), bitLength(rval->get_type()));
                z3::expr constraint = (x == (l - r));
                LogDetail<<"Adding constraint"<<End();
                node->AddConstraint(constraint);
                LogDetail<<"Added constraint"<<End();
                result = unknown;
            } break;
            case SymValue::Kind::UNKNOWN: {
                auto rval = static_cast<UnknownSymValue*>(rhs);
                auto unknown = new UnknownSymValue(resultType);
                z3::expr x = context.bv_const(unknown->get_name().c_str(), bitLength(resultType));
                z3::expr l = context.bv_const(lval->get_name().c_str(), bitLength(lval->get_type()));
                z3::expr r = context.bv_const(rval->get_name().c_str(), bitLength(rval->get_type()));
                z3::expr constraint = (x == (l - r));
                node->AddConstraint(constraint);
                result = unknown;
            } break;
            }
        } break;
        default:
            LogDetail<<"Unable to calculate "<<toString(lhs->get_kind())<<" minus "<<toString(rhs->get_kind())<<End();
            result = new UnknownSymValue(resultType);
            break;
        }

        storagePool.persist(result);
        node->AllocateResult(subInst, result);
        return std::nullopt;
    } catch (OffsetOutOfBoundsException e) {
        return std::unordered_set<FlowNode*>();
    }
    
}

std::unordered_set<FlowNode*> SymbolicEvaluation::TCall(
    TailCallInst *tailCallInst,
    FlowNode *node
) {
    SymValue *v = node->GetResult(tailCallInst->GetCallee());

    if ( v == nullptr || v->get_kind() != SymValue::Kind::FUNCREF ) {
        LogWarning<<"Called something that isn't a func ref"<<End();
        if(!tailCallInst->IsVoid()) {
            node->AllocateResult(tailCallInst,storagePool.persist(new UnknownSymValue(*tailCallInst->GetType())));
        }
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
            args
        )
    );

    std::unordered_set<FlowNode*> result;
    result.insert(newNode);
    return result;
}

void SymbolicEvaluation::Phi(
    PhiInst *phiInst,
    FlowNode *node
) {
    node->AllocateResult(
        phiInst,
        node->ResolvePhi(phiInst)
    );
    LogDetail<<"Allocated "<<toString(*node->GetResult(phiInst))<<End();
}

void SymbolicEvaluation::ZExt(
    ZExtInst *zExtInst,
    FlowNode *node
) {
    auto x = node->GetResult(zExtInst->GetArg());
    auto resultType = zExtInst->GetType();
    switch(x->get_kind()) {
    case SymValue::Kind::INT: {
        auto op = static_cast<IntSymValue*>(x);
        auto result = new IntSymValue(op->get_value().zext(bitLength(resultType)),resultType);
        LogDetail<<op->toString()<<" zero extended to "<<result->toString()<<End();
        node->AllocateResult(zExtInst,storagePool.persist(result));
    } break;
    default:
        LogDetail<<toString(*x)<<" zero extended to unknown symvalue"<<End();
        node->AllocateResult(
            zExtInst,
            storagePool.persist(new UnknownSymValue(resultType))
        );
        break;
    }
}

void Z3Test()
{
    LogFlow<<End();
    LogFlow<<"----------------------------------------"<<End();
    LogFlow<<"Z3 TEST START"<<End();
    LogFlow<<"----------------------------------------"<<End();
    LogFlow<<End();

    z3::context c;

    z3::expr x = c.bool_const("x");
    z3::expr y = c.bool_const("y");

    z3::expr conjecture = (!(x && y)) == (!x || !y);
    z3::solver s(c);

    s.add(!conjecture);
    LogDetail<<s<<End();
    LogDetail<<s.to_smt2()<<End();
    switch(s.check()) {
    case z3::unsat: LogFlow<<"De-Morgan is valid. Z3 is working"<<End(); break;
    case z3::sat: LogFlow<<"De-Morgan is not valid. Z3 is not working"<<End(); break;
    case z3::unknown: LogFlow<<"Validity of De-Morgan is unknown. Z3 is not working"<<End(); break;
    }

    z3::expr a1 = c.bool_const("a");
    z3::expr a2 = c.bool_const("a");
    z3::expr assertion = a1 != a2;
    z3::solver s1(c);
    s1.add(assertion);
    LogDetail<<s1<<End();
    LogDetail<<s1.to_smt2()<<End();
    switch(s1.check()) {
    case z3::unsat: LogDetail<<"Equal names mean equal values"<<End(); break;
    case z3::sat: LogDetail<<"Equal names do not mean equal values"<<End(); break;
    case z3::unknown: LogDetail<<"Equal names may or may not mean equal values"<<End(); break;
    }

    LogDetail<<End();
    LogDetail<<"----------------------------------------"<<End();
    LogDetail<<"Z3 TEST END"<<End();
    LogDetail<<"----------------------------------------"<<End();
    LogDetail<<End();
}

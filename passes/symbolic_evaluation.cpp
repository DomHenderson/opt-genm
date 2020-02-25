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
    Z3Test();

    PrintCodeInfo(program);

    // ---------------------------------------------------------------
    prog = program;
    std::cout<<"Starting"<<std::endl;
    frontier.push(CreateRootNode());
    std::cout<<"Done"<<std::endl;

    std::cout<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"EXECUTION START"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;

    while(!frontier.empty() && count < limit) {
        FlowNode *node = frontier.top();
        frontier.pop();
        std::cout<<"Stepping node "<<node->get_name()<<std::endl;
        StepNode(node);
    }

    std::cout<<(frontier.empty() ? "Finished" : "Finished early")<<std::endl;

    std::cout<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"EXECUTION END"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;

    if(frontier.empty()) {
        //Optimise();
        Rewrite();
    }
    std::cout<<"Finished"<<std::endl;
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
        return Add(static_cast<AddInst*>(&inst), node);

    case Inst::Kind::AND:
        std::cout<<"Running and"<<std::endl;
        And(static_cast<AndInst*>(&inst), node);
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
    
    case Inst::Kind::JMP:
        std::cout<<"Running jmp"<<std::endl;
        return Jump(static_cast<JumpInst*>(&inst), node);

    case Inst::Kind::JI:
        std::cout<<"Running jump indirect"<<std::endl;
        return JumpIndirect(static_cast<JumpIndirectInst*>(&inst), node);
    
    case Inst::Kind::LD:
        std::cout<<"Running load"<<std::endl;
        Load(static_cast<LoadInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::MOV:
        std::cout<<"Running mov"<<std::endl;
        Mov(static_cast<MovInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::MUL:
        std::cout<<"Running mul"<<std::endl;
        Mul(static_cast<MulInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::PHI:
        std::cout<<"Running phi"<<std::endl;
        Phi(static_cast<PhiInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::REM:
        std::cout<<"Running rem"<<std::endl;
        Rem(static_cast<RemInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::RET:
        std::cout<<"Running ret"<<std::endl;
        return Ret(static_cast<ReturnInst*>(&inst), node);

    case Inst::Kind::SET:
        std::cout<<"Running set"<<std::endl;
        Set(static_cast<SetInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SEXT:
        std::cout<<"Running sign extend"<<std::endl;
        SExt(static_cast<SExtInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SLL:
        std::cout<<"Running sll"<<std::endl;
        LeftLogicalShift(static_cast<SllInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SRA:
        std::cout<<"Running sra"<<std::endl;
        RightArithmeticShift(static_cast<SraInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SRL:
        std::cout<<"Running srl"<<std::endl;
        RightLogicalShift(static_cast<SrlInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::ST:
        std::cout<<"Running store"<<std::endl;
        Store(static_cast<StoreInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SUB:
        std::cout<<"Running sub"<<std::endl;
        Sub(static_cast<SubInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::TCALL:
        std::cout<<"Running tail call"<<std::endl;
        return TCall(static_cast<TailCallInst*>(&inst), node);

    case Inst::Kind::ZEXT:
        std::cout<<"Running zero extend"<<std::endl;
        ZExt(static_cast<ZExtInst*>(&inst), node);
        return std::nullopt;
    default:
        std::cout<<"Skipping "<<toString(inst)<<std::endl;
        return std::nullopt;
    }
}

//-----------------------------------------------------------------------------

// auto AnalyseLogs(const std::vector<FlowNode*>& finishedNodes)
// {
//     //A mapping from read instructions to all the values they read
//     //If all these values are equal and evaluable at compile time, the instruction can be replaced with a move
//     std::unordered_map<Inst*, std::vector<SymValue*>> readResults;
//     std::unordered_map<FlowNode*, std::unordered_map<DataStore::Write*, std::pair<SymValue*,SymValue*>>> precalculableWrites;

//     for(auto& node: finishedNodes) {
//         auto log = node->get_store().getFullLog();
//         std::cout<<"Log has length "<<log.size()<<std::endl;


//         //Logged writes which come before any read to the given location
//         std::unordered_map<DataStore::Write*,std::pair<SymValue*,SymValue*>> writesBeforeRead;

//         for(auto it = log.begin(); it < log.end(); ++it) {
//             DataStore::Action *action = *it;
//             switch(action->get_kind()) {
//             case DataStore::Action::Kind::READ: {
//                 auto *read = static_cast<DataStore::Read*>(action);
//                 Inst *inst = read->get_inst();
//                 if(readResults.find(inst) == readResults.end()) {
//                     readResults[inst] = std::vector<SymValue*>();
//                 }
//                 readResults[inst].push_back(read->get_value());
//             } break;

//             case DataStore::Action::Kind::WRITE: {
//                 auto write = static_cast<DataStore::Write*>(action);
//                 SymValue *write_addr = write->get_addr();
//                 bool writeBeforeRead = std::all_of(
//                     log.begin(),
//                     it,
//                     [write_addr](auto action) {
//                         if(action->get_kind() == DataStore::Action::Kind::READ) {
//                             auto read = static_cast<DataStore::Read*>(action);
//                             SymValue *read_addr = read->get_addr();
//                             return SymComp::EQ(read_addr, write_addr) == SymComp::Result::FALSE;
//                         } else if (action->get_kind() == DataStore::Action::Kind::INVALIDATE) {
//                             return false;
//                         } else {
//                             return true;
//                         }
//                     }
//                 );

//                 if(writeBeforeRead) {
//                     writesBeforeRead[write] = {write_addr, write->get_value()};
//                 }
//             }
//             }
//         }

//         precalculableWrites[node] = writesBeforeRead;
//     }

//     auto pair = std::make_pair(readResults, precalculableWrites);

//     return pair;
// }

// auto CalculateReadReplacements(const std::unordered_map<Inst*, std::vector<SymValue*>>& readResults)
// {
//     std::unordered_map<Inst*, SymValue*> readReplacements;

//     //Iterate over logged reads and put those which always take the same value in readReplacements
//     for(auto& [inst, values]: readResults) {
//         std::cout<<toString(inst)<<" takes the values "<<std::endl;
//         for(auto& i: values) {
//             std::cout<<"    "<<toString(i)<<std::endl;
//         }
//         auto notEqual = std::adjacent_find(values.begin(), values.end(), [](auto left, auto right) {
//             return SymComp::EQ(left, right) != SymComp::Result::TRUE;
//         });
//         if(notEqual == values.end()) {
//             std::cout<<"Values for "<<toString(inst)<<" are equal"<<std::endl;
//             readReplacements[inst] = values[0];
//         } else {
//             std::cout<<"Values for "<<toString(inst)<<" differ"<<std::endl;
//         }
//     }
//     return readReplacements;
// }

// void EnactReadReplacements(const std::unordered_map<Inst*, SymValue*>& readReplacements)
// {
//     for(auto& [inst, value]: readReplacements) {
//         if(value == nullptr) {
//             std::cout<<"WARNING: value == nullptr"<<std::endl;
//             continue;
//         }
//         if(inst->GetNumRets() == 0 || inst->GetType(0) != value->get_type()) {
//             std::cout<<"WARNING: types don't match"<<std::endl;
//         }
//         switch(value->get_kind()) {
//         case SymValue::Kind::INT: {
//             std::cout<<"Value is int"<<std::endl;
//             auto intSymValue = static_cast<IntSymValue*>(value);
//             switch(value->get_type()) {
//             case Type::U64: {
//                 std::cout<<"Type is U64"<<std::endl;
//                 uint64_t val = intSymValue->get_value().getLimitedValue();
//                 std::cout<<"Numeric value is "<<val<<std::endl;
//                 ConstantInt *replacementValue = new ConstantInt(val);
//                 std::cout<<"Created constant"<<std::endl;
//                 MovInst *replacement = new MovInst(value->get_type(), replacementValue, AnnotSet());
//                 inst->getParent()->AddInst(replacement, inst);
//                 std::cout<<"Created movInst"<<std::endl;
//                 inst->replaceAllUsesWith(replacement);
//                 inst->eraseFromParent();
//                 std::cout<<"Replaced"<<std::endl;
//             }
//             }
//         } break;
//         default:
//             std::cout<<"Not implemented yet"<<std::endl;
//         }
//     }
// }

// auto FilterPrecalculableWrites(const std::unordered_map<FlowNode*, std::unordered_map<DataStore::Write*, std::pair<SymValue*,SymValue*>>>& precalculableWrites)
// {
//     std::vector<std::tuple<SymValue*,SymValue*,Inst*>> result;

//     //Only need to filter when there are multiple flownodes
//     if(precalculableWrites.size() < 2) {
//         for(auto& [node, map]: precalculableWrites) {
//             for(auto& [write, pair]: map) {
//                 auto& [addr, val] = pair;
//                 result.push_back({addr, val, write->get_inst()});
//             }
//         }
//         return result;
//     }

//     //std::unordered_map<std::pair<SymValue*,SymValue*>, Inst*> validWrites;

//     for(auto& [node1, writes]: precalculableWrites) {
//         std::unordered_map<FlowNode*,std::unordered_set<DataStore::Write*>> validWrites;
//         for(auto& [node2, _]: precalculableWrites) {
//             if (node1 == node2) continue;
//             validWrites[node2] = std::unordered_set<DataStore::Write*>();

//             for(auto& [write, pair]: writes) {
//                 auto& [write_address, write_value] = pair;

//                 auto node2Log = node2->get_store().getFullLog();
//                 bool valid = true;
//                 for(auto& action: node2Log) {
//                     if(action->get_kind() == DataStore::Action::Kind::READ) {
//                         auto read = static_cast<DataStore::Read*>(action);
//                         if(SymComp::EQ(read->get_addr(), write_address) != SymComp::Result::FALSE) {
//                             valid = !(SymComp::EQ(read->get_value(), write_value) != SymComp::Result::TRUE);
//                             break;
//                         }
//                     }
//                 }
//                 if(valid) {
//                     validWrites[node2].insert(write);
//                 }
//             }
//         }

//         auto intersection = validWrites.begin()->second;
//         for(auto& [node, set]: validWrites) {
//             auto temp = std::unordered_set<DataStore::Write*>();
//             std::set_intersection(
//                 intersection.begin(), intersection.end(),
//                 set.begin(), set.end(),
//                 std::inserter(temp, temp.begin())
//             );
//             intersection = temp;
//         }
//         for(auto& write: intersection) {
//             auto& [addr, val] = precalculableWrites.at(node1).at(write);
//             result.push_back({addr, val, write->get_inst()});
//         }
//     }

//     return result;
// }

// void updateStaticDataStore(
//     Prog* prog,
//     std::vector<std::tuple<SymValue*, SymValue*, Inst*>> writesBeforeRead,
//     std::function<unsigned(std::string_view)> GetOffset
// ) {
//     for(auto& [addr, value, inst]: writesBeforeRead) {
//         if(value == nullptr) {
//             std::cout<<"Value is a nullptr"<<std::endl;
//             continue;
//         }
//         std::cout<<toString(inst)<<" writes "<<toString(value)<<" at "<<toString(addr)<<std::endl;
//         if(addr->get_kind() == SymValue::Kind::STATICPTR) {
//             auto address = static_cast<StaticPtrSymValue*>(addr);
//             auto name = address->get_name();
//             //TODO bounds check
//             auto offset = address->get_offset().getLimitedValue();
//             for(auto& dataSegment: prog->data()) {
//                 for(auto& atom: dataSegment) {
//                     if(atom.GetName() != name) continue;
//                     std::cout<<"Found "<<name<<std::endl;
//                     unsigned labelOffset = GetOffset(address->get_name());
//                     std::cout<<"Label offset: "<<labelOffset<<std::endl;
//                     Atom::iterator itemIter = atom.begin();
//                     if(labelOffset <= offset) {
//                         unsigned atomOffset = offset - labelOffset;
//                         while(atomOffset > 0 ) {
//                             std::cout<<"TODO: implement search"<<std::endl;
//                             atomOffset = 0;
//                         }
//                         std::cout<<"Found item"<<std::endl;
//                     } else {
//                         std::cout<<"TODO: find item before label"<<std::endl;
//                     }
//                     auto item = *itemIter;
//                     switch(value->get_kind()) {
//                     case SymValue::Kind::INT: {
//                         auto intSymValue = static_cast<IntSymValue*>(value);
//                         std::cout<<"Value is an int"<<std::endl;
//                         switch(item->GetKind()) {
//                         case Item::Kind::ALIGN: std::cout<<"WARNING: Attempting to overwrite align with int"<<std::endl; break;
//                         case Item::Kind::END: std::cout<<"WARNING: Attempting to overwrite end"<<std::endl; break;
//                         case Item::Kind::FLOAT64: std::cout<<"WARNING: Attempting to overwrite float with int"<<std::endl; break;
//                         case Item::Kind::INT8: std::cout<<"WARNING: Not implemented"<<std::endl; break;
//                         case Item::Kind::INT16: std::cout<<"WARNING: Not implemented"<<std::endl; break;
//                         case Item::Kind::INT32: std::cout<<"WARNING: Not implemented"<<std::endl; break;
//                         case Item::Kind::INT64: {
//                             unsigned typeLength = byteLength(value->get_type());
//                             std::cout<<"Type length: "<<typeLength<<std::endl;
//                             if(typeLength != 8) {
//                                 std::cout<<"Attempting to overwrite int of length "<<8<<" with int of length "<<typeLength<<std::endl;
//                             } else {
//                                 //TODO bounds checking
//                                 int64_t writeValue = intSymValue->get_value().getLimitedValue();
//                                 std::cout<<"Write value: "<<writeValue<<std::endl;
//                                 Item *newItem = new (item) Item(Item::Kind::INT64, writeValue);
//                                 std::cout<<"Created new item"<<std::endl;
//                                 inst->eraseFromParent();
//                                 std::cout<<"Erased store"<<std::endl;
//                             }
//                         } break;
//                         case Item::Kind::SPACE: std::cout<<"WARNING: Not implemented"<<std::endl; break;
//                         case Item::Kind::STRING: std::cout<<"WARNING: Not implemented"<<std::endl; break;
//                         case Item::Kind::SYMBOL: std::cout<<"WARNING: Not implemented"<<std::endl; break;
//                         }
//                     } break;
//                     default:
//                         std::cout<<"Unimplemented"<<std::endl;
//                         break;
//                     }
//                 }
//             }
//         }
//     }
// }

// void SymbolicEvaluation::Optimise()
// {
//     std::cout<<"Optimising"<<std::endl;
//     std::cout<<finishedNodes.size()<<" finished nodes"<<std::endl;

//     auto [readResults, precalculableWrites] = AnalyseLogs(finishedNodes);
//     auto readReplacements = CalculateReadReplacements(readResults);
//     EnactReadReplacements(readReplacements);
//     auto filteredWrites = FilterPrecalculableWrites(precalculableWrites);
//     updateStaticDataStore(
//         prog,
//         filteredWrites,
//         [node=finishedNodes[0]](std::string_view name) {
//             return node->get_store().getLabel(name)->offset;
//         }
//     );

// }

//-----------------------------------------------------------------------------

std::vector<Inst*> SymValueToInsts(SymValue *value, FlowNode *node, Prog *prog)
{
    std::cout<<"Converting "<<toString(value)<<" to insts"<<std::endl;
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
        std::cout<<"Not implemented yet"<<std::endl;
        assert(false);
    } break;
    case SymValue::Kind::INT: {
        std::cout<<"Int"<<std::endl;
        auto intValue = static_cast<IntSymValue*>(value);
        std::cout<<toString(intValue)<<std::endl;
        auto inst = new MovInst(value->get_type(), new ConstantInt(intValue->get_value().getLimitedValue()), AnnotSet());
        std::cout<<toString(inst)<<std::endl;
        result.push_back(inst);
    } break;
    case SymValue::Kind::STATICPTR: {
        std::cout<<"Static ptr"<<std::endl;
        auto ptrValue = static_cast<StaticPtrSymValue*>(value);
        auto atom = FindAtomByName(ptrValue->get_name(), prog);
        auto addrInst = new MovInst(value->get_type(), atom, AnnotSet());
        result.push_back(addrInst);
        std::cout<<toString(addrInst)<<std::endl;

        unsigned defaultOffset = node->get_store().getLabel(ptrValue->get_name())->offset;
        unsigned offset = ptrValue->get_offset().getLimitedValue();
        if(offset < defaultOffset) {
            std::cout<<"Default offset for "<<ptrValue->get_name()<<" is "<<defaultOffset<<std::endl;
            std::cout<<"Actual offset is "<<ptrValue->get_offset().getLimitedValue()<<std::endl;
            auto diffInst = new MovInst(value->get_type(), new ConstantInt(defaultOffset-offset), AnnotSet());
            result.push_back(diffInst);
            auto subInst = new SubInst(value->get_type(), addrInst, diffInst, AnnotSet());
            result.push_back(subInst);
        } else if(offset > defaultOffset) {
            std::cout<<"Default offset for "<<ptrValue->get_name()<<" is "<<defaultOffset<<std::endl;
            std::cout<<"Actual offset is "<<ptrValue->get_offset().getLimitedValue()<<std::endl;
            auto diffInst = new MovInst(value->get_type(), new ConstantInt(offset-defaultOffset), AnnotSet());
            result.push_back(diffInst);
            auto addInst = new AddInst(value->get_type(), addrInst, diffInst, AnnotSet());
            result.push_back(addInst);
        }
    } break;
    case SymValue::Kind::UNKNOWN:
        std::cout<<"Not implemented yet"<<std::endl;
        assert(false);
        break;
    default:
        assert(false);
        break;
    }
    std::cout<<"Returning "<<result.size()<<" insts"<<std::endl;
    return result;
}

std::vector<DataStore::Action*> GetImportantWrites(FlowNode *node)
{
    std::vector<DataStore::Action*> log = node->get_store().getFullLog();
    std::cout<<"Retrieved log of size "<<log.size()<<std::endl;
    for(int i = log.size()-1; i >= 0; --i) {
        std::cout<<"i: "<<i<<std::endl;
        auto action = log[i];
        if(action->get_kind() == DataStore::Action::Kind::WRITE) {
            auto write = static_cast<DataStore::Write*>(action);
            for(int j = i-1; j >= 0; --j) {
                if(log[j]->get_kind() == DataStore::Action::Kind::WRITE) {
                    auto write2 = static_cast<DataStore::Write*>(log[j]);
                    if(SymComp::EQ(write->get_addr(), write2->get_addr()) == SymComp::Result::TRUE) {
                        log.erase(log.begin()+j);
                        --i;
                    }
                }
            }
        } else {
            log.erase(log.begin()+i);
        }
    }
    std::cout<<"Finished getting important writes"<<std::endl;
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
    std::cout<<"Populated write insts"<<std::endl;
    auto writeOperandInsts = std::vector<std::vector<Inst*>>();
    for(auto& [inst, addr]: writeInsts){
        std::cout<<toString(inst)<<std::endl;
        if(inst->GetKind() == Inst::Kind::ST) {
            auto storeInst = static_cast<StoreInst*>(inst);
            auto op = storeInst->GetVal();
            std::cout<<toString(op)<<std::endl;
            auto symValue = endNode->GetResult(op);
            std::cout<<toString(symValue)<<std::endl;
            auto valueInsts = SymValueToInsts(symValue, endNode, prog);
            auto addrInsts = SymValueToInsts(addr, endNode, prog);
            auto allInsts = valueInsts;
            allInsts.insert(allInsts.end(), addrInsts.begin(), addrInsts.end());
            auto newStoreInst = new StoreInst(storeInst->GetStoreSize(), *addrInsts.rbegin(), *valueInsts.rbegin(), AnnotSet());
            std::cout<<"Created new store inst "<<toString(newStoreInst)<<std::endl;
            allInsts.push_back(newStoreInst);
            writeOperandInsts.push_back(allInsts);
        } else {
            std::cout<<"Other writes no implemented yet"<<std::endl;
            assert(false);
        }
    }
    std::cout<<"Creating new block"<<std::endl;
    Block *block = new Block(".rewriteBlock1");
    std::cout<<"Created new block"<<std::endl;
    std::cout<<"Adding write operand insts"<<std::endl;
    for(auto& v: writeOperandInsts) {
        for(auto& inst: v) {
            std::cout<<"Adding "<<toString(inst)<<std::endl;
            block->AddInst(inst);
        }
    }
    std::cout<<"Adding return insts"<<std::endl;
    for(auto i = insts.rbegin(); i != insts.rend(); ++i) {
        std::cout<<"Adding "<<toString(*i)<<std::endl;
        block->AddInst(*i);
    }
    auto returnInst = new ReturnInst(*insts.rbegin(), AnnotSet());
    std::cout<<"Adding final return inst "<<toString(returnInst)<<std::endl;
    block->AddInst(returnInst);
    Func *func = new Func(prog, endNode->get_func()->GetName());
    func->AddBlock(block);
    prog->AddFunc(func);
    endNode->get_func()->replaceAllUsesWith(func);
    endNode->get_func()->eraseFromParent();
    std::cout<<"Done"<<std::endl;
}

//-----------------------------------------------------------------------------

RootFlowNode *SymbolicEvaluation::CreateRootNode()
{
    PrintDataInfo(prog);
    auto startFunc = FindFuncByName(start_function, prog);
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
                std::cout<<laddr->toString()<<"+"<<rint->toString()<<" = "<<static_cast<StaticPtrSymValue*>(result)->toString()<<std::endl;
            } break;
            default:
                std::cout<<"WARNING: Adding STATICPTR and not INT"<<std::endl;
                result = new UnknownSymValue(resultType);
                break;
            }
        } break;
        case SymValue::Kind::INT: {
            IntSymValue *lint = static_cast<IntSymValue*>(lhs);
            switch(rhs->get_kind()) {
            case SymValue::Kind::STATICPTR: {
                StaticPtrSymValue *raddr = static_cast<StaticPtrSymValue*>(rhs);
                result = new StaticPtrSymValue(raddr->get_name(), raddr->get_offset()+lint->get_value(), raddr->get_max(), resultType);
                std::cout<<lint->toString()<<"+"<<raddr->toString()<<" = "<<static_cast<StaticPtrSymValue*>(result)->toString()<<std::endl;
            } break;
            case SymValue::Kind::FLOAT: {
                auto rf = static_cast<FloatSymValue*>(rhs);
                if(resultType == Type::F64) {
                    auto resultValue = llvm::APFloat(static_cast<double>(0));
                    resultValue.convertFromAPInt(lint->get_value(), isSigned(lint->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                    resultValue = resultValue + rf->get_value();
                    result = new FloatSymValue(resultValue, resultType);
                    std::cout<<lint->toString()<<"+"<<rf->toString()<<" = "<<static_cast<FloatSymValue*>(result)->toString()<<std::endl;
                }else if (resultType == Type::F32) {
                    auto resultValue = llvm::APFloat(static_cast<float>(0));
                    resultValue.convertFromAPInt(lint->get_value(), isSigned(lint->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                    resultValue = resultValue + rf->get_value();
                    result = new FloatSymValue(resultValue, resultType);
                    std::cout<<lint->toString()<<"+"<<rf->toString()<<" = "<<static_cast<FloatSymValue*>(result)->toString()<<std::endl;
                } else {
                    std::cout<<"WARNING: Adding int and float and not expecting float as a result"<<std::endl;
                    result = new UnknownSymValue(resultType);
                }
            } break;
            case SymValue::Kind::INT: {
                IntSymValue *rint = static_cast<IntSymValue*>(rhs);
                result = new IntSymValue(lint->get_value()+rint->get_value(), resultType);
                std::cout<<lint->toString()<<"+"<<rint->toString()<<" = "<<static_cast<IntSymValue*>(result)->toString()<<std::endl;
            } break;
            default:
                std::cout<<"Unable to calculate INT plus "<<toString(rhs->get_kind())<<std::endl;
                result = new UnknownSymValue(resultType);
                break;
            }
        } break;
        default:
            std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" plus "<<toString(rhs->get_kind())<<std::endl;
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
            std::cout<<l->toString()<<" is all ones: result = "<<toString(result)<<std::endl;
            node->AllocateResult(andInst,storagePool.persist(result));
        } else if (l->get_value().getLimitedValue() == 0) {
            auto result = new IntSymValue(0, resultType);
            std::cout<<l->toString()<<" is all zeros: result = "<<toString(result)<<std::endl;
            node->AllocateResult(andInst,storagePool.persist(result));
        } else {
            std::cout<<l->toString()<<" and ";
            switch(rhs->get_kind()) {
            case SymValue::Kind::INT: {
                auto r = static_cast<IntSymValue*>(rhs);
                std::cout<<r->toString()<<" = ";
                auto result = new IntSymValue(l->get_value() & r->get_value(), resultType);
                std::cout<<result->toString()<<std::endl;
            } break;
            default:
                std::cout<<toString(rhs)<<" is not calculable"<<std::endl;
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
                std::cout<<r->toString()<<" is all ones: result = "<<toString(result)<<std::endl;
                node->AllocateResult(andInst,storagePool.persist(result));
            } else if (r->get_value().getLimitedValue() == 0) {
                auto result = new IntSymValue(0, resultType);
                std::cout<<r->toString()<<" is all zeros: result = "<<toString(result)<<std::endl;
                node->AllocateResult(andInst,storagePool.persist(result));
            } else {
                std::cout<<toString(lhs)<<" and "<<r->toString()<<" is not calculable"<<std::endl;
                node->AllocateResult(andInst, storagePool.persist(new UnknownSymValue(resultType)));
            }
        } break;
        default:
            if(SymComp::EQ(lhs,rhs) == SymComp::Result::TRUE) {
                std::cout<<toString(lhs)<<" and "<<toString(rhs)<<" are equal"<<std::endl;
                node->AllocateResult(andInst, storagePool.persist(lhs->copy_cast(resultType)));
            } else {
                std::cout<<toString(lhs)<<" and "<<toString(rhs)<<" is not calculable"<<std::endl;
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
    std::cout<<toString(result)<<" at index "<<idx<<std::endl;

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
        std::cout<<"Attempted to call nullptr"<<std::endl;
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
        std::cout<<"Calling extern "<<ext->get_name()<<std::endl;
        if(externHasStub(ext->get_name())) {
            evaluable = true;
            runExtern(ext->get_name(), args, node, callInst->GetType(), storagePool);
        } else if(knownSafeExtern(ext->get_name())) {
            std::cout<<"Not invalidating store"<<std::endl;
        } else {
            std::cout<<"Invalidating store"<<std::endl;
            //TODO DO INVALIDATE
            //node->get_store().invalidate();
        }
        
        if(!evaluable && !callInst->IsVoid()){
            node->AllocateResult(callInst, storagePool.persist(new UnknownSymValue(*callInst->GetType())));
        }
        return std::nullopt;
    }

    default:
        std::cout<<"Attempted to call something of unhandled type"<<std::endl;
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

    std::cout<<"lhs: "<<toString(lhs)<<" rhs: "<<toString(rhs)<<std::endl;

    if(lhs == nullptr || rhs == nullptr) {
        std::cout<<"WARNING: operand nullptr"<<std::endl;
        return;
    }

    switch(c) {
    case Cond::EQ:
        std::cout<<"Comparing EQ"<<std::endl;
        result = SymComp::EQ(lhs, rhs);
        break;
    
    case Cond::NE:
        std::cout<<"Comparing NE"<<std::endl;
        result = SymComp::NEQ(lhs, rhs);
        break;
    
    case Cond::LT:
        std::cout<<"Comparing LT"<<std::endl;
        result = SymComp::LT(lhs, rhs);
        break;
    
    case Cond::GT:
        std::cout<<"Comparing GT"<<std::endl;
        result = SymComp::GT(lhs, rhs);
        break;
    
    case Cond::LE:
        std::cout<<"Comparing LE"<<std::endl;
        result = SymComp::LE(lhs, rhs);
        break;
    
    case Cond::GE:
        std::cout<<"Comparing GE"<<std::endl;
        result = SymComp::GE(lhs, rhs);
        break;
    
    default:
        std::cout<<"Comparing "<<(int)c<<std::endl;
        result = SymComp::Result::UNKNOWN;
        break;
    }

    SymValue *value = SymComp::ToSymValue(result, cmpInst->GetType());
    storagePool.persist(value);
    node->AllocateResult(cmpInst, value);
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
        std::cout<<"Condition is bool "<<(b->get_value()?"true":"false")<<std::endl;
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
        std::cout<<"Jumping to block "<<block->GetName()<<std::endl;
        FlowNode *newNode = CreateBlockFlowNode(block->getIterator(), node);
        result.insert(newNode);
    } else if (symCond->get_kind() == SymValue::Kind::INT) {
        auto i = static_cast<IntSymValue*>(symCond);
        std::cout<<"Condition is int "<<i->toString()<<std::endl;
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
            std::cout<<"Jumping from "<<node->get_func()->GetName()<<" to "<<func->GetName()<<std::endl;
        }
        std::cout<<"Jumping to block "<<block->GetName()<<std::endl;
        FlowNode *newNode = CreateBlockFlowNode(block->getIterator(), node);
        result.insert(newNode);
    } else {
        std::cout<<"Taking both branches"<<std::endl;
        std::cout<<"Jumping to blocks "<<trueBlock->GetName()<<" and "<<falseBlock->GetName()<<std::endl;
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
            std::cout<<"Block not found"<<std::endl;
            return std::unordered_set<FlowNode*>();
        }
        std::unordered_set<FlowNode*> result;
        result.insert(CreateBlockFlowNode(iter,node));
        return result;
    } break;
    default:
        std::cout<<"Unable to jump to "<<toString(target)<<std::endl;
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
            std::cout<<l->toString()<<"<<"<<r->toString()<<" = "<<static_cast<IntSymValue*>(result)->toString()<<std::endl;
        } break;
        default:
            std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted left by "<<r->toString()<<std::endl;
            result = new UnknownSymValue(resultType);
            break;
        }
    } break;
    default :
        std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted left by "<<toString(rhs->get_kind())<<std::endl;
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
    node->AllocateResult(loadInst,node->get_store().read(addr, loadInst->GetLoadSize(), loadInst->GetType(), loadInst));
}

void SymbolicEvaluation::Mov(
    MovInst *movInst,
    FlowNode *node
) {
    AllocateValue(
        movInst,
        movInst->GetArg(),
        movInst->GetType(),
        node
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
        std::cout<<"Multiplying float "<<l->toString();
        switch(rhs->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto r = static_cast<FloatSymValue*>(rhs);
            std::cout<<" with float "<<r->toString();
            auto result = new FloatSymValue(
                l->get_value() * r->get_value(),
                resultType
            );
            std::cout<<" to produce "<<result->toString();
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    result
                )
            );
        } break;
        case SymValue::Kind::INT: {
            auto r = static_cast<IntSymValue*>(rhs);
            std::cout<<" with int "<<r->toString();
            SymValue *result;
            if(resultType == Type::F64) {
                auto resultValue = llvm::APFloat(static_cast<double>(0));
                resultValue.convertFromAPInt(r->get_value(), isSigned(r->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + l->get_value();
                result = new FloatSymValue(resultValue, resultType);
                std::cout<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            }else if (resultType == Type::F32) {
                auto resultValue = llvm::APFloat(static_cast<float>(0));
                resultValue.convertFromAPInt(r->get_value(), isSigned(r->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + l->get_value();
                result = new FloatSymValue(resultValue, resultType);
                std::cout<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            } else {
                std::cout<<std::endl;
                std::cout<<"WARNING: Adding int and float and not expecting float as a result"<<std::endl;
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
            std::cout<<" with "<<(rhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
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
        std::cout<<"Multiplying int "<<l->toString();
        switch(rhs->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto r = static_cast<FloatSymValue*>(rhs);
            std::cout<<" with float "<<r->toString();
            SymValue *result;
            if(resultType == Type::F64) {
                auto resultValue = llvm::APFloat(static_cast<double>(0));
                resultValue.convertFromAPInt(l->get_value(), isSigned(l->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + r->get_value();
                result = new FloatSymValue(resultValue, resultType);
                std::cout<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            }else if (resultType == Type::F32) {
                auto resultValue = llvm::APFloat(static_cast<float>(0));
                resultValue.convertFromAPInt(l->get_value(), isSigned(l->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + r->get_value();
                result = new FloatSymValue(resultValue, resultType);
                std::cout<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            } else {
                std::cout<<std::endl;
                std::cout<<"WARNING: Adding int and float and not expecting float as a result"<<std::endl;
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
            std::cout<<" with int "<<r->toString();
            IntSymValue *result = new IntSymValue(
                l->get_value() * r->get_value(),
                resultType
            );
            std::cout<<" to produce "<<result->toString();
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    result
                )
            );
        } break;
        default: {
            std::cout<<" with "<<(rhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
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
        std::cout<<"Multiplying "<<(lhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
        std::cout<<" with "<<(rhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
        node->AllocateResult(
            mulInst,
            storagePool.persist(
                new UnknownSymValue(resultType)
            )
        );
        break;
    }
    std::cout<<std::endl;
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
            std::cout<<l->toString()<<(isSigned(resultType)?" srem ":" urem ")<<r->toString()<<" ";
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
            std::cout<<l->toString()<<" frem "<<r->toString()<<" ";
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
    std::cout<<toString(*node->GetResult(remInst))<<std::endl;
}

std::unordered_set<FlowNode*> SymbolicEvaluation::Ret(
    ReturnInst *returnInst,
    FlowNode *node
) {
    Inst *caller = node->get_frame().get_caller();
    std::cout<<"Caller: "<<toString(caller)<<std::endl;
    FlowNode *newNode = CreateReturnFlowNode(node);
    if(newNode == nullptr) {
        SymValue *returnValue = node->GetResult(returnInst->GetValue());
        assert(finishedNodes.find(node) == finishedNodes.end());
        finishedNodes.insert({node, returnValue});
        return std::unordered_set<FlowNode*>();
    } else if(!caller->IsVoid()) {
        std::cout<<"Caller is not void"<<std::endl;
        SymValue *returnValue = node->GetResult(returnInst->GetValue());
        std::cout<<"Return value "<<toString(returnValue)<<std::endl;
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
            std::cout<<"Shifted "<<l->toString()<<" right arithmetically by "<<r->toString()<<" to produce "<<static_cast<IntSymValue*>(result)->toString()<<std::endl;
        } break;
        default:
            std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<r->toString()<<std::endl;
            result = new UnknownSymValue(resultType);
            break;
        }
    } break;
    default :
        std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<toString(rhs->get_kind())<<std::endl;
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
            std::cout<<"Shifted "<<l->toString()<<" right logically by "<<r->toString()<<" to produce "<<static_cast<IntSymValue*>(result)->toString()<<std::endl;
        } break;
        default:
            std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<r->toString()<<std::endl;
            result = new UnknownSymValue(resultType);
            break;
        }
    } break;
    default :
        std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<toString(rhs->get_kind())<<std::endl;
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
    std::cout<<"Setting "<<toString(setInst->GetReg())<<" to "<<toString(value)<<std::endl;
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
        std::cout<<"Sign extending "<<x->toString()<<" to "<<result->toString()<<std::endl;
        node->AllocateResult(sExtInst, storagePool.persist(result));
    } break;
    default: {
        auto result = new UnknownSymValue(resultType);
        std::cout<<"Unable to sign extend a non-integer value"<<std::endl;
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
                std::cout<<laddr->toString()<<"+"<<rint->toString()<<" = "<<static_cast<StaticPtrSymValue*>(result)->toString()<<std::endl;
            } break;
            case SymValue::Kind::STATICPTR: {
                auto raddr = static_cast<StaticPtrSymValue*>(rhs);
                if(raddr->get_name() == laddr->get_name()) {
                    result = new IntSymValue(laddr->get_offset()-raddr->get_offset(), resultType);
                } else {
                    std::cout<<"WARNING: Subtracting pointers to different atoms"<<std::endl;
                    result = new UnknownSymValue(resultType);
                }
            } break;
            default:
                std::cout<<"WARNING: Subtracting "<<toString(rhs->get_kind())<<" from STATICPTR"<<std::endl;
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
                    std::cout<<lint->toString()<<"-"<<rf->toString()<<" = "<<static_cast<FloatSymValue*>(result)->toString()<<std::endl;
                }else if (resultType == Type::F32) {
                    auto resultValue = llvm::APFloat(static_cast<float>(0));
                    resultValue.convertFromAPInt(lint->get_value(), isSigned(lint->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                    resultValue = resultValue - rf->get_value();
                    result = new FloatSymValue(resultValue, resultType);
                    std::cout<<lint->toString()<<"-"<<rf->toString()<<" = "<<static_cast<FloatSymValue*>(result)->toString()<<std::endl;
                } else {
                    std::cout<<"WARNING: Subtracting float from int and not expecting float as a result"<<std::endl;
                    result = new UnknownSymValue(resultType);
                }
            } break;
            case SymValue::Kind::INT: {
                IntSymValue *rint = static_cast<IntSymValue*>(rhs);
                result = new IntSymValue(lint->get_value()-rint->get_value(), resultType);
                std::cout<<lint->toString()<<"-"<<rint->toString()<<" = "<<static_cast<IntSymValue*>(result)->toString()<<std::endl;
            } break;
            default:
                std::cout<<"Unable to calculate INT minus "<<toString(rhs->get_kind())<<std::endl;
                result = new UnknownSymValue(resultType);
                break;
            }
        } break;
        default:
            std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" minus "<<toString(rhs->get_kind())<<std::endl;
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
        std::cout<<"Called something that isn't a func ref"<<std::endl;
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
    unsigned numPredecessors = phiInst->GetNumIncoming();

    std::vector<Block*> blocks;
    for(int i = 0; i < numPredecessors; ++i) {
        blocks.push_back(phiInst->GetBlock(i));
    }

    Block *block = node->ResolvePhiBlocks(blocks);

    if(block == nullptr) {
        std::cout<<"Phi resolution failed. Not allocating"<<std::endl;
        return;
    }

    AllocateValue(
        phiInst,
        phiInst->GetValue(block),
        phiInst->GetType(),
        node
    );

    std::cout<<"Allocated "<<toString(*node->GetResult(phiInst))<<std::endl;
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
        std::cout<<op->toString()<<" zero extended to "<<result->toString()<<std::endl;
        node->AllocateResult(zExtInst,storagePool.persist(result));
    } break;
    default:
        std::cout<<toString(*x)<<" zero extended to unknown symvalue"<<std::endl;
        node->AllocateResult(
            zExtInst,
            storagePool.persist(new UnknownSymValue(resultType))
        );
        break;
    }
}

void SymbolicEvaluation::AllocateValue(
    Inst *inst,
    Value *value,
    Type type,
    FlowNode *node
) {
    SymValue *result;
    switch(value->GetKind()) {
    case Value::Kind::GLOBAL: {
        auto g = static_cast<Global*>(value);
        switch(g->GetKind()) {
        case Global::Kind::ATOM: {
            std::cout<<"Allocating global atom (label) "<<g->GetName()<<std::endl;
            const Label *label = node->get_store().getLabel(g->GetName());
            if(label == nullptr) {
                std::cout<<"WARNING: Attempted to create address symvalue to unknown label"<<std::endl;
            } else {
                result = storagePool.persist(new StaticPtrSymValue(
                    g->GetName(),
                    llvm::APInt(bitLength(type),label->offset, isSigned(type)),
                    label->atom->getSize(),
                    type
                ));
            }
        } break;
        case Global::Kind::BLOCK:
            std::cout<<"Alocating global block "<<g->GetName()<<std::endl;
            result = storagePool.persist(new BlockRefSymValue(g->GetName(), type));
            break;

        case Global::Kind::EXTERN:
            std::cout<<"Allocating global extern "<<g->GetName()<<std::endl;
            result = storagePool.persist(new ExternSymValue(g->GetName(), type));
            break;

        case Global::Kind::FUNC:
            std::cout<<"Allocating global func "<<g->GetName()<<std::endl;
            result = storagePool.persist(new FuncRefSymValue(g->GetName(), type));
            break;
        
        default:
            std::cout<<"Allocating unknown global "<<g->GetName()<<std::endl;
            result = storagePool.persist(new UnknownSymValue(type));
            break;
        }
    } break;

    case Value::Kind::INST: {
        std::cout<<"Allocating inst"<<std::endl;
        auto i = static_cast<Inst*>(value);
        result = node->GetResult(i);
    } break;

    case Value::Kind::CONST: {
        Constant *c = static_cast<Constant*>(value);
        switch(c->GetKind()) {
        case Constant::Kind::INT: {
            ConstantInt *ci = static_cast<ConstantInt*>(c);
            result = new IntSymValue(ci->GetValue(), type);
            if(!isIntType(type)) { std::cout<<"Allocating constant int of non-int type"<<std::endl; }
            storagePool.persist(result);
            std::cout<<"Allocating int "<<ci->GetValue()<<std::endl;
        } break;
        
        case Constant::Kind::FLOAT: {
            ConstantFloat *cf = static_cast<ConstantFloat*>(c);
            result = new FloatSymValue(cf->GetValue(), type);
            if(isIntType(type)) { std::cout<<"Allocating constant float of non-float type"<<std::endl; }
            storagePool.persist(result);
            std::cout<<"Allocating float "<<cf->GetValue()<<std::endl;
        } break;

        default:
            result = new UnknownSymValue(type);
            storagePool.persist(result);
            std::cout<<"Allocating const"<<std::endl;
            break;
        }
    } break;

    case Value::Kind::EXPR: {
        result = new UnknownSymValue(type);
        storagePool.persist(result);
        std::cout<<"Allocating expr"<<std::endl;
    }
    }

    std::cout<<"Allocating "<<toString(inst)<<" := "<<toString(result)<<std::endl;
    node->AllocateResult(inst, result);
}

void Z3Test()
{
    std::cout<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"Z3 TEST START"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;

    z3::context c;

    z3::expr x = c.bool_const("x");
    z3::expr y = c.bool_const("y");

    z3::expr conjecture = (!(x && y)) == (!x || !y);
    z3::solver s(c);

    s.add(!conjecture);
    std::cout<<s<<std::endl;
    std::cout<<s.to_smt2()<<std::endl;
    switch(s.check()) {
    case z3::unsat: std::cout<<"De-Morgan is valid. Z3 is working"<<std::endl; break;
    case z3::sat: std::cout<<"De-Morgan is not valid. Z3 is not working"<<std::endl; break;
    case z3::unknown: std::cout<<"Validity of De-Morgan is unknown. Z3 is not working"<<std::endl; break;
    }

    std::cout<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"Z3 TEST END"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;
}

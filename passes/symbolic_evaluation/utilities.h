#pragma once

#include <string>
#include <string_view>

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/ilist.h>

#include "core/inst.h"
#include "flownode.h"
#include "symvalue.h"

class Func;
class Prog;

llvm::ilist<Func>::iterator FindFuncByName(std::string_view name, Prog *prog);

std::string toString(Inst::Kind k);
std::string toString(Inst &inst);
std::string toString(SymValue::Kind k);
std::string toString(SymValue &value);
std::string toString(Type type);

bool isIntType(Type type);
bool isSigned(Type type);
unsigned bitLength(Type type);
unsigned byteLength(Type type);
std::optional<Type> unsignedOfLength(unsigned length);

std::pair<SymValue*,SymValue*> getOperandValues(BinaryInst *inst, FlowNode *node);

bool knownSafeExtern(std::string_view name);

void PrintCodeInfo(Prog *prog);

void PrintDataInfo(Prog *prog);

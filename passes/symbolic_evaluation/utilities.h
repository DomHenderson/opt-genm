#pragma once

#include <string>
#include <string_view>

#include <llvm/ADT/ilist.h>

#include "core/inst.h"

class Func;
class Prog;

llvm::ilist<Func>::iterator FindFuncByName(std::string_view name, Prog *prog);

std::string toString(Inst::Kind k);
std::string toString(Inst &inst);
std::string toString(Type type);

unsigned typeLength(Type type);
std::optional<Type> unsignedOfLength(unsigned length);

bool knownSafeExtern(std::string_view name);

void PrintCodeInfo(Prog *prog);

void PrintDataInfo(Prog *prog);

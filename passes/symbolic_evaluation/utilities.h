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

void PrintCodeInfo(Prog *prog);

void PrintDataInfo(Prog *prog);

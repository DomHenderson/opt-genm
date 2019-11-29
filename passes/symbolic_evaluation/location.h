#pragma once

#include <llvm/ADT/ilist.h>

#include "core/block.h"
#include "core/func.h"
#include "core/inst.h"

struct Location {
    llvm::ilist<Inst>::iterator inst;
    llvm::ilist<Block>::iterator block;
    llvm::ilist<Func>::iterator func;
};
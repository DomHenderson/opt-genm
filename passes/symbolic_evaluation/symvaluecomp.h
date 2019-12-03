#pragma once

namespace SymComp {
    SymValue *Invert(SymValue *v);
    SymValue *EQ(SymValue *lv, SymValue *rv);
    SymValue *NEQ(SymValue *lv, SymValue *rv);
    SymValue *LT(SymValue* lv, SymValue *rv);
    SymValue *GT(SymValue *lv, SymValue *rv);
    SymValue *LE(SymValue *lv, SymValue *rv);
    SymValue *GE(SymValue *lv, SymValue *rv);
}
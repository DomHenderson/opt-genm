#include <iostream>

#include "symvalue.h"
#include "symvaluecomp.h"

using SymComp::Result;

Result SymComp::FromBool(bool b)
{
     return b ? Result::TRUE : Result::FALSE;
}

SymValue *SymComp::ToSymValue(Result r)
{
    switch(r) {
    case Result::TRUE: return new BoolSymValue(true);
    case Result::UNKNOWN: return new UnknownSymValue();
    case Result::FALSE: return new BoolSymValue(false);
    }
}

// -----------------------------------------------------------------------------
Result SymComp::Not(Result r)
{
    switch(r) {
    case Result::TRUE: return Result::FALSE;
    case Result::UNKNOWN: return Result::UNKNOWN;
    case Result::FALSE: return Result::TRUE;
    }
}

Result SymComp::EQ(SymValue *lv, SymValue *rv)
{
    if ( lv == rv ) {
        return Result::TRUE;
    } else if (lv->get_kind() == rv->get_kind()) {
        switch(lv->get_kind()) {
        case SymValue::Kind::ADDR: {
            auto l = static_cast<AddrSymValue*>(lv);
            auto r = static_cast<AddrSymValue*>(rv);
            return FromBool(l->get_name() == r->get_name() && l->get_offset() == r->get_offset());
        }
        case SymValue::Kind::BOOL: {
            auto l = static_cast<BoolSymValue*>(lv);
            auto r = static_cast<BoolSymValue*>(rv);
            return FromBool(l->get_value() == r->get_value());
        }

        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            return FromBool(l->get_value() == r->get_value());
        }

        case SymValue::Kind::FUNCREF: {
            auto l = static_cast<FuncRefSymValue*>(lv);
            auto r = static_cast<FuncRefSymValue*>(rv);
            return FromBool(l->get_name() == r->get_name());
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            return FromBool(l->get_value() == r->get_value());
        }

        case SymValue::Kind::STR: {
            auto l = static_cast<StringSymValue*>(lv);
            auto r = static_cast<StringSymValue*>(rv);
            return FromBool(l->get_string() == r->get_string());
        }

        default:
            std::cout<<"Default case used in EQ"<<std::endl;
        case SymValue::Kind::UNKNOWN:
            return Result::UNKNOWN;
        }
    } else {
        return Result::UNKNOWN;
    }
}

Result SymComp::NEQ(SymValue *lv, SymValue *rv)
{
    return SymComp::Not(SymComp::EQ(lv, rv));
}

Result SymComp::LT(SymValue* lv, SymValue *rv)
{
    if ( lv == rv ) {
        return Result::FALSE;
    } else if (lv->get_kind() == rv->get_kind()) {
        switch(lv->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            return FromBool(l->get_value() < r->get_value());
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            return FromBool(l->get_value() < r->get_value());
        }

        default:
            std::cout<<"Default case used in LT"<<std::endl;
        case SymValue::Kind::ADDR:
        case SymValue::Kind::BOOL:
        case SymValue::Kind::FUNCREF:
        case SymValue::Kind::STR:
        case SymValue::Kind::UNKNOWN:
            return Result::UNKNOWN;
        }
    } else {
        return Result::UNKNOWN;
    }
}

Result SymComp::GT(SymValue *lv, SymValue *rv)
{
    if ( lv == rv ) {
        return Result::FALSE;
    } else if (lv->get_kind() == rv->get_kind()) {
        switch(lv->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            return FromBool(l->get_value() > r->get_value());
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            return FromBool(l->get_value() > r->get_value());
        }

        default:
            std::cout<<"Default case used in GT"<<std::endl;
        case SymValue::Kind::ADDR:
        case SymValue::Kind::BOOL:
        case SymValue::Kind::FUNCREF:
        case SymValue::Kind::STR:
        case SymValue::Kind::UNKNOWN:
            return Result::UNKNOWN;
        }
    } else {
        return Result::UNKNOWN;
    }
}

Result SymComp::LE(SymValue *lv, SymValue *rv)
{
    return SymComp::Not(SymComp::GT(lv, rv));
}

Result SymComp::GE(SymValue *lv, SymValue *rv)
{
    return SymComp::Not(SymComp::LT(lv, rv));
}

#include <iostream>

#include "symvalue.h"
#include "symvaluecomp.h"

// -----------------------------------------------------------------------------
SymValue *SymComp::Invert(SymValue *v)
{
    if(v->get_kind() == SymValue::Kind::BOOL) {
        auto b = static_cast<BoolSymValue*>(v);
        delete v;
        return new BoolSymValue(!b->get_value());
    } else { //eq is unknown
        return v;
    }
}

SymValue *SymComp::EQ(SymValue *lv, SymValue *rv)
{
    if ( lv == rv ) {
        return new BoolSymValue(true);
    } else if (lv->get_kind() == rv->get_kind()) {
        switch(lv->get_kind()) {
        case SymValue::Kind::BOOL: {
            auto l = static_cast<BoolSymValue*>(lv);
            auto r = static_cast<BoolSymValue*>(rv);
            return new BoolSymValue(l->get_value() == r->get_value());
        }

        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            return new BoolSymValue(l->get_value() == r->get_value());
        }

        case SymValue::Kind::FUNCREF: {
            auto l = static_cast<FuncRefSymValue*>(lv);
            auto r = static_cast<FuncRefSymValue*>(rv);
            return new BoolSymValue(l->get_name() == r->get_name());
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            return new BoolSymValue(l->get_value() == r->get_value());
        }

        default:
            std::cout<<"Default case used in EQ"<<std::endl;
        case SymValue::Kind::UNKNOWN:
            return new UnknownSymValue();
        }
    } else {
        return new UnknownSymValue();
    }
}

SymValue *SymComp::NEQ(SymValue *lv, SymValue *rv)
{
    return SymComp::Invert(SymComp::EQ(lv, rv));
}

SymValue *SymComp::LT(SymValue* lv, SymValue *rv)
{
    if ( lv == rv ) {
        return new BoolSymValue(false);
    } else if (lv->get_kind() == rv->get_kind()) {
        switch(lv->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            return new BoolSymValue(l->get_value() < r->get_value());
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            return new BoolSymValue(l->get_value() < r->get_value());
        }

        default:
            std::cout<<"Default case used in LT"<<std::endl;
        case SymValue::Kind::BOOL:
        case SymValue::Kind::FUNCREF:
        case SymValue::Kind::UNKNOWN:
            return new UnknownSymValue();
        }
    } else {
        return new UnknownSymValue();
    }
}

SymValue *SymComp::GT(SymValue *lv, SymValue *rv)
{
    if ( lv == rv ) {
        return new BoolSymValue(false);
    } else if (lv->get_kind() == rv->get_kind()) {
        switch(lv->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            return new BoolSymValue(l->get_value() > r->get_value());
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            return new BoolSymValue(l->get_value() > r->get_value());
        }

        default:
            std::cout<<"Default case used in GT"<<std::endl;
        case SymValue::Kind::BOOL:
        case SymValue::Kind::FUNCREF:
        case SymValue::Kind::UNKNOWN:
            return new UnknownSymValue();
        }
    } else {
        return new UnknownSymValue();
    }
}

SymValue *SymComp::LE(SymValue *lv, SymValue *rv)
{
    return SymComp::Invert(SymComp::GT(lv, rv));
}

SymValue *SymComp::GE(SymValue *lv, SymValue *rv)
{
    return SymComp::Invert(SymComp::LT(lv, rv));
}

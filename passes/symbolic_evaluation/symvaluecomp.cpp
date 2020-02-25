#include <iostream>

#include "core/type.h"
#include "flownode.h"
#include "symvalue.h"
#include "symvaluecomp.h"
#include "utilities.h"

using SymComp::Result;

Result SymComp::FromBool(bool b)
{
     return b ? Result::TRUE : Result::FALSE;
}

SymValue *SymComp::ToSymValue(Result r, Type type)
{
    switch(r) {
    case Result::TRUE: return new BoolSymValue(true, type);
    case Result::UNKNOWN: return new UnknownSymValue(type);
    case Result::FALSE: return new BoolSymValue(false, type);
    default: std::cout<<"Unhandled result type to sym value"<<std::endl; return nullptr;
    }
}

// -----------------------------------------------------------------------------
Result SymComp::Not(Result r)
{
    switch(r) {
    case Result::TRUE: return Result::FALSE;
    case Result::UNKNOWN: return Result::UNKNOWN;
    case Result::FALSE: return Result::TRUE;
    default: std::cout<<"Unhandled result type to invert"<<std::endl; return Result::UNKNOWN;
    }
}

Result SymComp::EQ(SymValue *lv, SymValue *rv)
{
    if ( lv == rv ) {
        return Result::TRUE;
    } else if (lv->get_kind() == rv->get_kind()) {
        switch(lv->get_kind()) {
        case SymValue::Kind::STATICPTR: {
            auto l = static_cast<StaticPtrSymValue*>(lv);
            auto r = static_cast<StaticPtrSymValue*>(rv);
            return FromBool(l->get_name() == r->get_name() && l->get_offset() == r->get_offset());
        }

        case SymValue::Kind::BLOCKREF: {
            auto l = static_cast<BlockRefSymValue*>(lv);
            auto r = static_cast<BlockRefSymValue*>(rv);
            return FromBool(l->get_name() == r->get_name());
        }

        case SymValue::Kind::BOOL: {
            auto l = static_cast<BoolSymValue*>(lv);
            auto r = static_cast<BoolSymValue*>(rv);
            return FromBool(l->get_value() == r->get_value());
        }

        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            return FromBool(l->get_value().compare(r->get_value()) == llvm::APFloatBase::cmpEqual);
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
            return FromBool(l->get_value().compare(r->get_value()) == llvm::APFloatBase::cmpLessThan);
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            return FromBool(l->get_value().slt(r->get_value()));
        }

        default:
            std::cout<<"Default case used in LT"<<std::endl;
        case SymValue::Kind::STATICPTR:
        case SymValue::Kind::BOOL:
        case SymValue::Kind::FUNCREF:
        case SymValue::Kind::UNKNOWN:
            return Result::UNKNOWN;
        }
    } else {
        return Result::UNKNOWN;
    }
}

std::pair<Result, z3::expr> SymComp::GT(SymValue *lv, SymValue *rv, FlowNode *node)
{
    auto &c = node->get_context();
    if ( lv == rv ) {
        return {
            Result::FALSE,
            c.bool_val(false)
        };
    } else if (lv->get_kind() == rv->get_kind()) {
        switch(lv->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            bool result = l->get_value().compare(r->get_value()) == llvm::APFloatBase::cmpGreaterThan;
            return {
                FromBool(result),
                c.bool_val(result)
            };
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            bool result = l->get_value().sgt(r->get_value());
            return {
                FromBool(result),
                c.bool_val(result)
            };
        }

        case SymValue::Kind::UNKNOWN: {
            auto l = c.bv_const(
                static_cast<UnknownSymValue*>(lv)->get_name().c_str(),
                bitLength(lv->get_type())
            );
            auto r = c.bv_const(
                static_cast<UnknownSymValue*>(rv)->get_name().c_str(),
                bitLength(rv->get_type())
            );
            z3::solver s(c);
            z3::check_result p,n;
            node->AssertConstraints(s);
            s.push();
            s.add(l > r);
            p = s.check();
            s.pop();
            s.add(!(l > r));
            n = s.check();
            switch(p) {
            case z3::sat: std::cout<<"> sat"<<std::endl; break;
            case z3::unsat: std::cout<<"> unsat"<<std::endl; break;
            case z3::unknown: std::cout<<"> unknown"<<std::endl; break;
            }
            
            switch(n) {
            case z3::sat: std::cout<<"<= sat"<<std::endl; break;
            case z3::unsat: std::cout<<"<= unsat"<<std::endl; break;
            case z3::unknown: std::cout<<"<= unknown"<<std::endl; break;
            }

            switch(p) {
            case z3::sat: //l>r could be true
                switch(n) {
                case z3::sat: //L>r could be false
                case z3::unknown:
                    return {Result::UNKNOWN, l > r};
                case z3::unsat: //l>r can't be false
                    return {Result::TRUE, c.bool_val(true)};
                }
            case z3::unsat: //l>r cannot be true
                switch(n) {
                case z3::sat: //l>r could be false
                case z3::unknown:
                    return {Result::FALSE, c.bool_val(false)};
                case z3::unsat: //l>r cannot be false
                    std::cout<<"l>r and l<=r both unsat"<<std::endl;
                    assert(false);
                }
            }
        }

        default:
            std::cout<<"Default case used in GT"<<std::endl;
        case SymValue::Kind::STATICPTR:
        case SymValue::Kind::BOOL:
        case SymValue::Kind::FUNCREF:
            return {
                Result::UNKNOWN,
                c.bool_val(true) //TODO replace with new symbol
            };
        }
    } else {
        auto &c = node->get_context();
        if(lv->get_kind() == SymValue::Kind::UNKNOWN) {
            auto l = c.bv_const(
                static_cast<UnknownSymValue*>(lv)->get_name().c_str(),
                bitLength(lv->get_type())
            );
            if(rv->get_kind() == SymValue::Kind::INT) {
                auto r = c.bv_val(
                    static_cast<IntSymValue*>(rv)->get_value().getLimitedValue(),
                    bitLength(rv->get_type())
                );
                z3::solver s(c);
                z3::check_result p,n;
                node->AssertConstraints(s);
                s.push();
                s.add(l > r);
                std::cout<<s.to_smt2()<<std::endl;
                std::cout<<"-"<<std::endl;
                p = s.check();
                s.pop();
                s.add(!(l > r));
                std::cout<<s.to_smt2()<<std::endl;
                std::cout<<"-"<<std::endl;
                n = s.check();
                switch(p) {
                case z3::sat: std::cout<<"> sat"<<std::endl; break;
                case z3::unsat: std::cout<<"> unsat"<<std::endl; break;
                case z3::unknown: std::cout<<"> unknown"<<std::endl; break;
                }
                
                switch(n) {
                case z3::sat: std::cout<<"<= sat"<<std::endl; break;
                case z3::unsat: std::cout<<"<= unsat"<<std::endl; break;
                case z3::unknown: std::cout<<"<= unknown"<<std::endl; break;
                }

                switch(p) {
                case z3::sat: //l>r could be true
                    switch(n) {
                    case z3::sat: //L>r could be false
                    case z3::unknown:
                        return {Result::UNKNOWN, l > r};
                    case z3::unsat: //l>r can't be false
                        return {Result::TRUE, c.bool_val(true)};
                    }
                case z3::unsat: //l>r cannot be true
                    switch(n) {
                    case z3::sat: //l>r could be false
                    case z3::unknown:
                        return {Result::FALSE, c.bool_val(false)};
                    case z3::unsat: //l>r cannot be false
                        std::cout<<"l>r and l<=r both unsat"<<std::endl;
                        assert(false);
                    }
                }
            }
        }
    }
}

std::pair<Result, z3::expr> SymComp::LE(SymValue *lv, SymValue *rv, FlowNode *node)
{
    auto [gtResult, gtConstraint] = SymComp::GT(lv, rv, node);
    return {SymComp::Not(gtResult), !gtConstraint};
}

Result SymComp::GE(SymValue *lv, SymValue *rv)
{
    return SymComp::Not(SymComp::LT(lv, rv));
}

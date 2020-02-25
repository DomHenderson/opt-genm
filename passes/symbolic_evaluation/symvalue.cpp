#include <cassert>
#include <cstdint>
#include <sstream>
#include <string>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/StringRef.h>

#include "core/type.h"
#include "symvalue.h"
#include "utilities.h"

StaticPtrSymValue::StaticPtrSymValue(std::string_view atomName, unsigned offsetValue, unsigned max, Type type):
    SymValue(type),
    name(atomName),
    offset(bitLength(type), offsetValue, false),
    max(max)
{
    assert(isIntType(type));
    if(offset.getLimitedValue() > max) {
        throw OffsetOutOfBoundsException();
    }
}

StaticPtrSymValue::StaticPtrSymValue(std::string_view atomName, llvm::APInt offset, unsigned max, Type type):
    SymValue(type),
    name(atomName),
    offset(offset),
    max(max)
{
    assert(isIntType(type));
    offset = offset.sextOrTrunc(bitLength(type));
    if(offset.getLimitedValue() > max) {
        throw OffsetOutOfBoundsException();
    }
}

std::string StaticPtrSymValue::toString() const
{
    std::ostringstream stream;
    stream << name
        <<"["
        << offset.toString(10, isSigned(get_type()))
        <<" of "
        << max
        << "] ("
        << ::toString(get_type())
        << ")";
    return stream.str();
}

FloatSymValue::FloatSymValue(double d, Type type):
    SymValue(type),
    val(d)
{
    assert(type==Type::F64);
}

FloatSymValue::FloatSymValue(float f, Type type):
    SymValue(type),
    val(f)
{
    assert(type==Type::F32);
}

std::string FloatSymValue::toString() const
{
    std::ostringstream stream;
    stream<<val.convertToDouble()<<"("<<::toString(get_type())<<")";
    return stream.str();
}

IntSymValue::IntSymValue(uint64_t value, Type type):
    IntSymValue(llvm::APInt(bitLength(type), value, isSigned(type)), type)
{
}

IntSymValue::IntSymValue(llvm::APInt value, Type type):
    SymValue(type),
    val(value)
{
    assert(isIntType(type));
    value = value.sextOrTrunc(bitLength(type));
}

IntSymValue *IntSymValue::copy_cast(Type type) const
{
    return new IntSymValue(
        val.sextOrTrunc(bitLength(type)),
        type
    );
}

std::string IntSymValue::toString() const
{
    return val.toString(10, isSigned(get_type()))+"("+::toString(get_type())+")";
}

HeapPtrSymValue::HeapPtrSymValue(
    std::string_view atomName,
    int offset,
    Type type
) :
    HeapPtrSymValue(
        atomName,
        llvm::APInt(bitLength(type), offset, true),
        type
    )
{}

HeapPtrSymValue::HeapPtrSymValue(
    std::string_view atomName,
    llvm::APInt offset,
    Type type
):
    SymValue(type),
    name(atomName),
    offset(offset)
{
    assert(isIntType(type));
    offset = offset.sextOrTrunc(bitLength(type));
}

unsigned long UnknownSymValue::previousIndex = 0;

UnknownSymValue::UnknownSymValue(
    Type type
) :
    SymValue(type)
{
    ++previousIndex;
    assert(previousIndex != 0);
    name = "x" + std::to_string(previousIndex);
    std::cout<<"Created unknown sym value "<<name<<std::endl;
}

CondSymValue::CondSymValue(
    SymValue *trueValue,
    SymValue *falseValue,
    z3::expr condition
) :
    SymValue(trueValue->get_type()),
    trueValue(trueValue),
    falseValue(falseValue),
    condition(condition)
{
    if(trueValue->get_type() != falseValue->get_type()) {
        std::cout<<"true value and false value should be of the same type"<<std::endl;
        std::cout<<"  true type: "<<toString(trueValue->get_type())<<std::endl;
        std::cout<<"  false type: "<<toString(falseValue->get_type())<<std::endl;
        assert(trueValue->get_type() == falseValue->get_type());
    }
}

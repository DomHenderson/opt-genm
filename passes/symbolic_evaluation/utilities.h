#pragma once

#include <optional>
#include <string>
#include <string_view>

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/ilist.h>

#include "core/atom.h"
#include "core/constant.h"
#include "core/inst.h"
#include "flownode.h"
#include "symvalue.h"

class Func;
class Prog;

Atom *FindAtomByName(std::string_view name, Prog *prog);
llvm::ilist<Block>::iterator FindBlockByName(std::string_view name, Prog *prog);
llvm::ilist<Func>::iterator FindFuncByName(std::string_view name, Prog *prog);

std::string toString(ConstantReg* reg);
std::string toString(Inst::Kind k);
std::string toString(Inst &inst);
std::string toString(Inst *inst);
std::string toString(SymValue::Kind k);
std::string toString(SymValue &value);
std::string toString(SymValue *value);
std::string toString(Type type);

std::string ptrToString(void *ptr);

bool isIntType(Type type);
bool isSigned(Type type);
unsigned bitLength(Type type);
unsigned byteLength(Type type);
unsigned byteLength(Item &item);
std::optional<Type> unsignedOfLength(unsigned length);

std::pair<SymValue*,SymValue*> getOperandValues(BinaryInst *inst, FlowNode *node);

bool knownSafeExtern(std::string_view name);

void PrintCodeInfo(Prog *prog);

void PrintDataInfo(Prog *prog);

class End {
public:
    End(bool flush = false): flush(flush) {}
    bool flush;
};

class Logger {
public:
    Logger(unsigned level): level(level) {}

    static void SetCutoff(unsigned n) {
        cutoff = n;
    }

    template<typename T>
    Logger &operator<<(T t) {
        if(level <= cutoff) {
            std::cout<<t;
        }
        return *this;
    }

    Logger &operator<<(End e) {
        if(level <= cutoff) {
            if(e.flush || overrideFlush) {
                std::cout<<std::endl;
            } else {
                std::cout<<"\n";
            }
        }
        return *this;
    }
protected:
    static unsigned cutoff;
    unsigned level;
    constexpr static bool overrideFlush = true;
};

class HeaderLogger: public Logger {
public:
    HeaderLogger(unsigned level, std::string header): Logger(level), header(header) {}

    template<typename T>
    HeaderLogger &operator<<(T t) {
        if(level <= cutoff) {
            if(!printedHeader) {
                std::cout<<header;
                printedHeader = true;
            }
            std::cout<<t;
        }
        return *this;
    }

    HeaderLogger &operator<<(End e) {
        if(level <= cutoff) {
            if(e.flush || overrideFlush) {
                std::cout<<std::endl;
            } else {
                std::cout<<"\n";
            }
            printedHeader = false;
        }
        return *this;
    }
private:
    std::string header;
    bool printedHeader = false;
};

extern Logger Log;
extern HeaderLogger LogError;
extern HeaderLogger LogWarning;
extern Logger LogFlow;
extern Logger LogTrace;
extern Logger LogDetail;


/*
LOG LEVELS
0 Pass information
5 Program flow + errors
10 Running inst
20 Details
*/
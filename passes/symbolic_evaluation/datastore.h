#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include <llvm/ADT/APInt.h>

#include "core/atom.h"
#include "core/inst.h"
#include "core/type.h"
#include "mappedatom.h"
#include "symvalue.h"

class SymExPool;

struct Label {
    MappedAtom *atom;
    unsigned offset;
};

struct SymByteRef {
    SymValue *value = nullptr;
    unsigned byte = 0;
};

class FlowNode;

class DataStore {
public:
    DataStore(SymExPool &pool): storagePool(pool) {}
    virtual ~DataStore() = default;
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, FlowNode *node) = 0;
    virtual std::vector<SymValue*> readSequence(SymValue *start, unsigned size) = 0;
protected:
    SymExPool &storagePool;
};

struct DataSegmentInfo {
    std::string_view name;
    bool readOnly;
    unsigned start;
    unsigned end;
};

class BaseStore: public DataStore {
public:
    BaseStore(Prog &prog, SymExPool &storagePool);
    virtual ~BaseStore() override = default;
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, FlowNode *node) override;
    virtual std::vector<SymValue*> readSequence(SymValue *start, unsigned size) override;

    const Label *getLabel(std::string_view name) const;
private:
    std::vector<DataSegmentInfo> dataSegments;

    std::vector<std::unique_ptr<MappedAtom>> atoms;    //Each atom can have multiple labels pointing into it
    std::unordered_map<std::string_view,Label> labels; //So we need to store them separately
};

class LogStore: public DataStore {
public:
    LogStore(SymExPool &pool);
    virtual ~LogStore() override = default;
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, FlowNode *node) override;
    virtual std::vector<SymValue*> readSequence(SymValue *start, unsigned size) override;
    
    class Action;
    std::string_view addHeapAtom(unsigned size);
    std::vector<Action*> getLog() const;
    void invalidate(MappedAtom &startPoint, Inst *inst);
    void recordRead(SymValue *loc, SymValue *value, Inst *inst);
    void write(SymValue *addr, SymValue *value, Inst *inst);

    class Action {
    public:
        Action(Inst *inst): inst(inst) {}
        enum class Kind {INVALIDATE, READ, WRITE};
        virtual Kind get_kind() const = 0;
        Inst *get_inst() const { return inst; }
    private:
        Inst *inst;
    };
    class Invalidate: public Action {
    public:
        Invalidate(MappedAtom &atom, Inst *inst): Action(inst), atom(atom) {}
        virtual Kind get_kind() const override { return Kind::INVALIDATE; }
    private:
        MappedAtom &atom;
    };
    class ReadWrite: public Action {
    public:
        ReadWrite(SymValue *addr, SymValue *value, Inst *inst): Action(inst), addr(addr), value(value) {}
        SymValue *get_addr()const { return addr; }
        SymValue *get_value() const { return value; }
    protected:
        SymValue *addr;
        SymValue *value;
    };
    class Read: public ReadWrite {
    public:
        Read(SymValue *addr, SymValue *value, Inst *inst): ReadWrite(addr, value, inst) {}
        virtual Kind get_kind() const override { return Kind::READ; }
    };
    class Write: public ReadWrite {
    public:
        Write(SymValue *addr, SymValue *value, Inst *inst): ReadWrite(addr, value, inst) {}
        virtual Kind get_kind() const override { return Kind::WRITE; }
    };
    
private:
    std::vector<std::unique_ptr<Action>> actions;

    static std::string getNextHeapName();
    static void incrementNextHeapName();
    static std::string nextHeapName;
    std::unordered_map<std::string, std::unique_ptr<MappedAtom>> heap;
};

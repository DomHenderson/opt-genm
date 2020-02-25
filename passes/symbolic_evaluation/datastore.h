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
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, FlowNode *node, Inst *inst = nullptr, bool record = true, unsigned debugCount = 0) = 0;
    virtual void write(SymValue *addr, SymValue *value, Inst *inst) = 0;
    virtual void invalidate(MappedAtom &startPoint, Inst *inst) = 0;
    virtual const Label *getLabel(std::string_view name) const = 0;
    virtual std::string getNextHeapName() const = 0;
    virtual std::vector<SymValue*> readSequence(SymValue *start, unsigned size) = 0;

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

    virtual std::vector<Action*> getFullLog() const = 0;
protected:
    SymExPool &storagePool;
    // virtual std::vector<MappedAtom*> modelPointers(std::string_view name) = 0;
// private:
    // virtual void fillMissingBytes(std::vector<SymByteRef> &bytes, const MappedAtom *atom) const = 0;
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
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, FlowNode *node, Inst *inst = nullptr, bool record = true, unsigned debugCount = 0) override;
    virtual void write(SymValue *addr, SymValue *value, Inst *inst) override;
    virtual void invalidate(MappedAtom &startPoint, Inst *inst) override;
    virtual const Label *getLabel(std::string_view name) const override;
    virtual std::string getNextHeapName() const override { return "0"; } //BaseStores do not have heaps so the next name is the first name
    virtual std::vector<SymValue*> readSequence(SymValue *start, unsigned size) override;
    virtual std::vector<Action*> getFullLog() const override { return std::vector<Action*>(); }
    // virtual std::vector<MappedAtom*> modelPointers(std::string_view name) override;
private:
    // virtual void fillMissingBytes(std::vector<SymByteRef> &bytes, const MappedAtom *atom) const override;

    std::vector<DataSegmentInfo> dataSegments;

    std::vector<std::unique_ptr<MappedAtom>> atoms;    //Each atom can have multiple labels pointing into it
    std::unordered_map<std::string_view,Label> labels; //So we need to store them separately
};

class LogStore: public DataStore {
public:
    LogStore(DataStore &store, SymExPool &pool);
    virtual ~LogStore() override = default;
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, FlowNode *node, Inst *inst = nullptr, bool record = true, unsigned debugCount = 0) override;
    virtual void write(SymValue *addr, SymValue *value, Inst *inst) override;
    virtual void invalidate(MappedAtom &startPoint, Inst *inst) override;
    virtual const Label *getLabel(std::string_view name) const override;
    virtual std::string_view addHeapAtom(unsigned size);
    virtual std::vector<SymValue*> readSequence(SymValue *start, unsigned size) override;
    // virtual std::vector<MappedAtom*> modelPointers(std::string_view name) override;

    virtual std::vector<Action*> getFullLog() const override;

protected:
    virtual std::string getNextHeapName() const override;
private:
    // virtual void fillMissingBytes(std::vector<SymByteRef> &bytes, const MappedAtom *atom) const override;
    void incrementNextHeapName();

    DataStore &baseStore;
    std::vector<std::unique_ptr<Action>> actions;

    std::unordered_map<std::string, std::unique_ptr<MappedAtom>> heap;
    std::string nextHeapName;
};

class JoinStore: public LogStore {
    JoinStore(DataStore &leftStore, DataStore &rightStore, SymExPool &pool);
    virtual ~JoinStore() override = default;
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, FlowNode *node, Inst *inst = nullptr, bool record = true, unsigned debugCount = 0) override;
    virtual void write(SymValue *addr, SymValue *value, Inst *inst) override;
    virtual void invalidate(MappedAtom &startPoint, Inst *inst) override;
    virtual const Label *getLabel(std::string_view name) const override;
    virtual std::string_view addHeapAtom(unsigned size) override;
    virtual std::vector<SymValue*> readSequence(SymValue *start, unsigned size) override;

    virtual std::vector<Action*> getFullLog() const override;
};

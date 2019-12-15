#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <string_view>
#include <utility>

#include <llvm/ADT/APInt.h>

#include "core/atom.h"
#include "core/type.h"
#include "symvalue.h"

class SymExPool;

class MappedAtom {
public:
    MappedAtom(SymExPool &pool): pool(pool) {} 
    SymValue *get(unsigned offset, size_t loadSize, Type type);
    SymValue *get(llvm::APInt offset, size_t loadSize, Type type);

    void addSymValue(SymValue &symValue);
    void addSpace(unsigned space);

    unsigned getSize() const;
private:
    std::pair<unsigned, unsigned> FindSection(unsigned offset) const;

    std::map<unsigned,SymValue*> items;

    unsigned size = 0;

    SymExPool &pool;
};

struct Label {
    MappedAtom *atom;
    unsigned offset;
};

class DataStore {
public:
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, bool record = true, unsigned debugCount = 0) = 0;
    virtual void write(SymValue *addr, SymValue *value) = 0;
    virtual void invalidate() = 0;
    virtual const Label *getLabel(std::string_view name) const = 0;
};

struct DataSegmentInfo {
    std::string_view name;
    bool readOnly;
};

class BaseStore: public DataStore {
public:
    BaseStore(Prog &prog, SymExPool &storagePool);
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, bool record = true, unsigned debugCount = 0) override;
    virtual void write(SymValue *addr, SymValue *value) override;
    virtual void invalidate() override;
    virtual const Label *getLabel(std::string_view name) const override;
private:
    std::vector<DataSegmentInfo> dataSegments;

    std::vector<std::unique_ptr<MappedAtom>> atoms;    //Each atom can have multiple labels pointing into it
    std::unordered_map<std::string_view,Label> labels; //So we need to store them separately

    SymExPool &storagePool;
};

class LogStore: public DataStore {
public:
    LogStore(DataStore &store);
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, bool record = true, unsigned debugCount = 0) override;
    virtual void write(SymValue *addr, SymValue *value) override;
    virtual void invalidate() override;
    virtual const Label *getLabel(std::string_view name) const override;
private:
    class Action {
    public:
        enum class Kind {INVALIDATE, READ, WRITE};
        virtual Kind get_kind() const = 0;
    };
    class Invalidate: public Action {
    public:
        virtual Kind get_kind() const override { return Kind::INVALIDATE; }
    };
    class Read: public Action {
    public:
        Read(SymValue *addr, SymValue *value): addr(addr), value(value) {}
        virtual Kind get_kind() const override { return Kind::READ; }
        SymValue *get_addr() const { return addr; }
        SymValue *get_value() const { return value; }
    private:
        SymValue *addr;
        SymValue *value;
    };
    class Write: public Action {
    public:
        Write(SymValue *addr, SymValue *value): addr(addr), value(value) {}
        virtual Kind get_kind() const override { return Kind::WRITE; }
        SymValue *get_addr() const { return addr; }
        SymValue *get_value() const { return value; }
    private:
        SymValue *addr;
        SymValue *value;
    };

    DataStore &baseStore;
    std::vector<std::unique_ptr<Action>> actions;
};

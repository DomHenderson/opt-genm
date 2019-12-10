#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <string_view>

#include "core/atom.h"
#include "core/type.h"
#include "symvalue.h"

class SymExPool;

class DataStore {
public:
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, bool record = true) = 0;
    virtual void write(SymValue *addr, SymValue *value) = 0;
    virtual void invalidate() = 0;
};

class MappedItem {
public:
    enum class Kind {
        VALUE,
        END
    };
    MappedItem(): kind(Kind::END), value(nullptr), size(0) {}
    MappedItem(SymValue *value, unsigned size): kind(Kind::VALUE), value(value), size(size) {}
    Kind get_kind() { return kind; }
    SymValue *get_value() { return value; }
    unsigned get_size() { return size; }
private:
    Kind kind;
    SymValue *value;
    unsigned size;
};

class MappedAtom {
public:
    MappedAtom(SymExPool &pool): pool(pool) {}
    void add(Item *item);
    SymValue *get(int offset, Type type);
private:
    std::map<unsigned,MappedItem> items;
    unsigned next = 0;
    unsigned previous;

    bool ended = false;

    SymExPool &pool;
};

class BaseStore: public DataStore {
public:
    BaseStore(Prog &prog, SymExPool &storagePool);
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, bool record = true) override;
    virtual void write(SymValue *addr, SymValue *value) override;
    virtual void invalidate() override;
private:
    std::unordered_map<std::string_view,MappedAtom> atoms;
    SymExPool &storagePool;
};

class LogStore: public DataStore {
public:
    LogStore(DataStore &store);
    virtual SymValue *read(SymValue *loc, size_t loadSize, Type type, bool record = true) override;
    virtual void write(SymValue *addr, SymValue *value) override;
    virtual void invalidate() override;
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

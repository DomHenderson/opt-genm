#include <exception>
#include <functional>
#include <optional>
#include <string_view>
#include <unordered_map>

#include "core/type.h"
#include "externstubs.h"
#include "flownode.h"
#include "storagepool.h"
#include "symvalue.h"
#include "utilities.h"

using ExternStub = std::function<std::optional<SymValue*>(std::vector<SymValue*>, FlowNode*, std::optional<Type>, SymExPool&)>;

std::optional<SymValue*> strlenStub(std::vector<SymValue*> args, FlowNode* node, std::optional<Type> type, SymExPool& pool)
{
    if(args.size() != 1) {
        std::cout<<"WARNING: strlen called with "<<args.size()<<" arguments"<<std::endl;
    }
    SymValue *arg = args[0];
    if(arg->get_kind() == SymValue::Kind::STATICPTR) {
        std::cout<<"Running strlen"<<std::endl;
        auto addr = static_cast<StaticPtrSymValue*>(arg);
        auto label = node->get_store().getLabel(addr->get_name());
        unsigned length = 0;
        StaticPtrSymValue* i;
        while(true) {
            std::cout<<"Reading at length "<<length<<std::endl;
            //All addrsymvalues need to be persisted because they are stored in the logged reads
            i = pool.persist(new StaticPtrSymValue(
                addr->get_name(),
                addr->get_offset() + length,
                addr->get_max(),
                addr->get_type()
            ));
            auto read = node->get_store().read(i, 1, Type::U8, node);
            if(read == nullptr) {
                std::cout<<"WARNING: Read returned nullptr"<<std::endl;
                return std::nullopt;
            } else if (read->get_kind() == SymValue::Kind::INT) {
                auto c = static_cast<IntSymValue*>(read);
                if(c->get_value().getLimitedValue() == 0) {
                    std::cout<<"Found a zero at length "<<length<<std::endl;
                    return pool.persist(
                        new IntSymValue(
                            length,
                            *type
                        )
                    );
                }
            } else {
                std::cout<<"Found a non-int value at "<<length<<std::endl;
                return pool.persist(
                    new UnknownSymValue(*type)
                );
            }
            ++length;
        }
    } else {
        std::cout<<"WARNING: Running strlen with operand of non-address type"<<std::endl;
        return pool.persist(new UnknownSymValue(*type));
    }
}

std::optional<SymValue*> mallocStub(std::vector<SymValue*> args, FlowNode* node, std::optional<Type> type, SymExPool& pool)
{
    if(!type.has_value()) {
        std::cout<<"WARNING: Calling malloc without return type"<<std::endl;
        return std::nullopt;
    }
    if(*type != Type::U64) {
        std::cout<<"WARNING: Attempting to create malloc pointer of type "<<toString(*type)<<std::endl;
    }
    if(args.size() != 1) {
        std::cout<<"WARNING: Calling malloc with "<<args.size()<<" arguments"<<std::endl;
        return pool.persist(new UnknownSymValue(*type));
    }
    switch(args[0]->get_kind()) {
    case SymValue::Kind::INT: {
        auto i = static_cast<IntSymValue*>(args[0]);
        auto name = node->AllocateHeapBlock(i->get_value().getLimitedValue());
        return pool.persist(
            new HeapPtrSymValue(
                name,
                0,
                *type
            )
        );
    }

    default:
        std::cout<<"WARNING: First argument to malloc is "<<toString(args[0])<<std::endl;
        return pool.persist(new UnknownSymValue(*type));
    }
}

std::optional<SymValue*> memcpyStub(std::vector<SymValue*> args, FlowNode* node, std::optional<Type> type, SymExPool& pool)
{
    if(args.size() != 3) {
        std::cout<<"WARNING: Calling memcpy with "<<args.size()<<" arguments"<<std::endl;
    }
    std::cout<<"Memcpy arguments:"<<std::endl;
    for(auto& x: args) {
        std::cout<<"    "<<toString(x)<<std::endl;
    }
    if(args[2]->get_kind() == SymValue::Kind::INT) {
        auto length = static_cast<IntSymValue*>(args[2])->get_value();
        
    } else {
        std::cout<<"Cannot memcpy "<<toString(args[2])<<" bytes"<<std::endl;
    }
    return args[0];
}

std::unordered_map<std::string, ExternStub> stubs = {
    {"strlen", strlenStub},
    {"malloc", mallocStub},
    {"memcpy", memcpyStub}
};

bool externHasStub(std::string_view name) {
    return stubs.find(static_cast<std::string>(name)) != stubs.end();
}

std::optional<SymValue*> runExtern(std::string_view name, std::vector<SymValue*> args, FlowNode *node, std::optional<Type> type, SymExPool &pool) {
    if(!externHasStub(name)) { 
        throw StubNotImplementedException();
    }

    return stubs[static_cast<std::string>(name)](args, node, type, pool);
}

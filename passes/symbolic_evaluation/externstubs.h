#pragma once

#include <exception>
#include <optional>
#include <string_view>

#include "core/type.h"

class FlowNode;
class SymExPool;
class SymValue;

class StubNotImplementedException: public std::exception {};

bool externHasStub(std::string_view name);

std::optional<SymValue*> runExtern(std::string_view name, std::vector<SymValue*> args, FlowNode* node, std::optional<Type> type, SymExPool &pool);

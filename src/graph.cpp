#include <watever/graph.h>
#include <watever/utils.h>

using namespace watever;

Value Graph::getNode(Opcode Op, ValueType RT, Value Arg1) {
  WATEVER_TODO("handle one arg nodes");
}
Value Graph::getNode(Opcode Op, ValueType RT, Value Arg1, Value Arg2) {
  WATEVER_TODO("handle two arg nodes");
  switch (Op) {
  case Opcode::ADD: {

    break;
  }
  default:
    WATEVER_UNREACHABLE("Illegal signature for Opcode {}", Op);
  }
}
Value Graph::getNode(Opcode Op, ValueType RT, Value Arg1, Value Arg2,
                     Value Arg3) {
  WATEVER_TODO("handle one three nodes");
}

Value Graph::getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1) {
  WATEVER_TODO("handle one arg nodes");
}

// See SelectionDAG.cpp:7645
Value Graph::getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1,
                     Value Arg2) {
  WATEVER_TODO("handle two arg nodes");
}

Value Graph::getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1,
                     Value Arg2, Value Arg3) {
  WATEVER_TODO("handle three arg nodes");
}

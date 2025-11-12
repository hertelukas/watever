#include <watever/graph.h>
#include <watever/utils.h>

using namespace watever;

inline ValueType Value::getValueType() const {
  return DefinedBy->getValueType(ResNo);
}

Value Graph::getNode(Opcode Op, ValueType RT, Value Arg1) {
  WATEVER_TODO("handle one arg nodes");
}
Value Graph::getNode(Opcode Op, ValueType RT, Value Arg1, Value Arg2) {
  WATEVER_TODO("handle two arg nodes");
  switch (Op) {
  case Opcode::ADD: {
    assert((RT == ValueType::I32 || RT == ValueType::I64) &&
           "This operator only applies to Integer types!");
    assert(Arg1.getValueType() == Arg2.getValueType() &&
           "Binary operator types must match!");
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


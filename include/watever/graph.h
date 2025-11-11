#ifndef GRAPH_H
#define GRAPH_H

#include <llvm/ADT/ilist.h>
#include <vector>
namespace watever {

enum class ValueType { I32, I64, F32, F64, Other, Glue };

/// Opcodes which are legal (can be lowered) for Wasm
enum class Opcode { EntryToken, ADD, SUB, ADC };

class Node;
class Use;
class Graph;

/// Output of a GraphNode
class Value {
  /// Node, which defines this value.
  Node *DefinedBy;
  /// The index of the return value of the node we are using.
  unsigned ResNo = 0;
};

/// Represents an operation
class Node : public llvm::ilist_node<Node> {
  Opcode Op;

  /// The values used by this operation.
  Use *const Operands;
  unsigned NumOperands;

  /// The types produced by this operation.
  // TODO probably not efficient to allocate such a short vector
  const std::vector<ValueType> ResultTypes;

  /// List to all users of the results of this node.
  Use *UseList;

public:
  Node(Opcode Op, const std::vector<ValueType> RTs, Use *Ops,
       unsigned NumOperands)
      : Op{Op}, ResultTypes{RTs}, Operands{Ops}, NumOperands{NumOperands} {}
};

/// Input to a Node.
class Use {
  Value Val;
  Node *const User;

  /// Linked list to all results of the node which created this value.
  Use **Prev = nullptr;
  Use *Next = nullptr;

public:
  Use(Value V, Node *U) : Val(V), User(U) {}
};

/// Represenets a single basic block.
class Graph {
  Node EntryNode;

  llvm::ilist<Node> AllNodes;

public:
  // TODO store entry node in all nodes?
  Graph()
      : EntryNode(Opcode::EntryToken,
                  std::vector<ValueType>{ValueType::Other, ValueType::Glue},
                  nullptr, 0) {}

  Value getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1);
  Value getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1, Value Arg2);
  Value getNode(Opcode Op, std::vector<ValueType> RTs, Value Arg1, Value Arg2,
                Value Arg3);
};

} // namespace watever

#endif /* GRAPH_H */

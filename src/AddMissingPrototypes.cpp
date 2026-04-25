//===-- WebAssemblyAddMissingPrototypes.cpp - Fix prototypeless functions -===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Add prototypes to prototypes-less functions.
///
/// WebAssembly has strict function prototype checking so we need functions
/// declarations to match the call sites.  Clang treats prototype-less functions
/// as varargs (foo(...)) which happens to work on existing platforms but
/// doesn't under WebAssembly.  This pass will find all the call sites of each
/// prototype-less function, ensure they agree, and then set the signature
/// on the function declaration accordingly.
///
//===----------------------------------------------------------------------===//

#include "watever/AddMissingPrototypes.hpp"
#include "watever/utils.hpp"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include <llvm/IR/Analysis.h>
#include <llvm/IR/PassManager.h>
using namespace llvm;

PreservedAnalyses watever::AddMissingPrototypes::run(Module &M,
                                                     ModuleAnalysisManager &) {
  WATEVER_LOG_TRACE("Add Missing Prototypes");

  std::vector<std::pair<Function *, Function *>> Replacements;

  // Find all the prototype-less function declarations
  for (Function &F : M) {
    if (!F.isDeclaration() || !F.hasFnAttribute("no-prototype"))
      continue;

    WATEVER_LOG_TRACE("Found no-prototype function: {}", F.getName());

    // When clang emits prototype-less C functions it uses (...), i.e. varargs
    // function that take no arguments (have no sentinel).  When we see a
    // no-prototype attribute we expect the function have these properties.
    if (!F.isVarArg())
      WATEVER_UNREACHABLE("Functions with 'no-prototype' attribute must take varargs: {}", F.getName());
    unsigned NumParams = F.getFunctionType()->getNumParams();
    if (NumParams != 0) {
      if (!(NumParams == 1 && F.arg_begin()->hasStructRetAttr()))
      WATEVER_UNREACHABLE("Functions with 'no-prototype' attribute should not have params: {}", F.getName());
    }

    // Find calls of this function, looking through bitcasts.
    SmallVector<CallBase *> Calls;
    SmallVector<Value *> Worklist;
    Worklist.push_back(&F);
    while (!Worklist.empty()) {
      Value *V = Worklist.pop_back_val();
      for (User *U : V->users()) {
        if (auto *BC = dyn_cast<BitCastOperator>(U))
          Worklist.push_back(BC);
        else if (auto *CB = dyn_cast<CallBase>(U))
          if (CB->getCalledOperand() == V)
            Calls.push_back(CB);
      }
    }

    // Create a function prototype based on the first call site that we find.
    FunctionType *NewType = nullptr;
    for (CallBase *CB : Calls) {
      WATEVER_LOG_TRACE("prototype-less call of {}:", F.getName());
      WATEVER_LOG_TRACE(llvmToString(*CB));
      FunctionType *DestType = CB->getFunctionType();
      if (!NewType) {
        // Create a new function with the correct type
        NewType = DestType;
        WATEVER_LOG_TRACE("found function type: {}", llvmToString(*NewType));
      } else if (NewType != DestType) {
        WATEVER_LOG_WARN(
            "prototype-less function used with conflicting signatures: {}",
            F.getName());
        WATEVER_LOG_TRACE(llvmToString(*DestType));
        WATEVER_LOG_TRACE(llvmToString(*NewType));
      }
    }

    if (!NewType) {
      WATEVER_LOG_TRACE("could not derive a function prototype from usage: {}",
                        F.getName());
      // We could not derive a type for this function.  In this case strip
      // the isVarArg and make it a simple zero-arg function.  This has more
      // chance of being correct.  The current signature of (...) is illegal in
      // C since it doesn't have any arguments before the "...", we this at
      // least makes it possible for this symbol to be resolved by the linker.
      NewType = FunctionType::get(F.getFunctionType()->getReturnType(), false);
    }

    Function *NewF =
        Function::Create(NewType, F.getLinkage(), F.getName() + ".fixed_sig");
    NewF->setAttributes(F.getAttributes());
    NewF->removeFnAttr("no-prototype");
    Replacements.emplace_back(&F, NewF);
  }

  if (Replacements.empty()) {
    return PreservedAnalyses::all();
  }

  for (auto &Pair : Replacements) {
    Function *OldF = Pair.first;
    Function *NewF = Pair.second;
    std::string Name = std::string(OldF->getName());
    M.getFunctionList().push_back(NewF);
    OldF->replaceAllUsesWith(
        ConstantExpr::getPointerBitCastOrAddrSpaceCast(NewF, OldF->getType()));
    OldF->eraseFromParent();
    NewF->setName(Name);
  }
  
  return PreservedAnalyses::none().preserveSet<CFGAnalyses>();
}

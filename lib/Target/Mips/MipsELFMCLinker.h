//===- MipsELFMCLinker.h --------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef MIPS_ELF_SECTION_LINKER_H
#define MIPS_ELF_SECTION_LINKER_H
#ifdef ENABLE_UNITTEST
#include <gtest.h>
#endif
#include <mcld/CodeGen/MCLinker.h>

namespace mcld {

class Module;
class MemoryArea;

/** \class MipsELFMCLinker
 *  \brief MipsELFMCLinker sets up the environment for linking.
 */
class MipsELFMCLinker : public MCLinker
{
public:
  MipsELFMCLinker(LinkerConfig& pConfig,
                  mcld::Module& pModule,
                  MemoryArea& pOutput,
                  TargetLDBackend &pLDBackend);

  ~MipsELFMCLinker();
};

} // namespace of mcld

#endif


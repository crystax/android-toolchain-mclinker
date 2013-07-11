//===- InputSectDesc.h ----------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef MCLD_SCRIPT_INPUT_SECTION_DESCRIPTION_INTERFACE_H
#define MCLD_SCRIPT_INPUT_SECTION_DESCRIPTION_INTERFACE_H
#ifdef ENABLE_UNITTEST
#include <gtest.h>
#endif

#include <mcld/Script/ScriptCommand.h>
#include <mcld/Script/StringList.h>
#include <cassert>

namespace mcld
{

class WildcardPattern;
class OutputSectDesc;
class LinkerScript;

/** \class InputSectDesc
 *  \brief This class defines the interfaces to input section description.
 */

class InputSectDesc : public ScriptCommand
{
public:
  enum KeepPolicy {
    Keep,
    NoKeep
  };

  struct Spec {
    bool hasFile() const { return m_pWildcardFile != NULL; }
    const WildcardPattern& file() const {
      assert(hasFile());
      return *m_pWildcardFile;
    }

    bool hasExcludeFiles() const {
      return m_pExcludeFiles != NULL && !m_pExcludeFiles->empty();
    }
    const StringList& excludeFiles() const {
      assert(hasExcludeFiles());
      return *m_pExcludeFiles;
    }

    bool hasSections() const {
      return m_pWildcardSections != NULL && !m_pWildcardSections->empty();
    }
    const StringList& sections() const {
      assert(hasSections());
      return *m_pWildcardSections;
    }

    WildcardPattern* m_pWildcardFile;
    StringList* m_pExcludeFiles;
    StringList* m_pWildcardSections;
  };

public:
  InputSectDesc(KeepPolicy pPolicy,
                const Spec& pSpec,
                const OutputSectDesc& pOutputDesc,
                LinkerScript& pLDScript);
  ~InputSectDesc();

  KeepPolicy policy() const { return m_KeepPolicy; }

  const Spec& spec() const { return m_Spec; }

  void dump() const;

  static bool classof(const ScriptCommand* pCmd)
  {
    return pCmd->getKind() == ScriptCommand::INPUT_SECT_DESC;
  }

  void activate();

private:
  KeepPolicy m_KeepPolicy;
  Spec m_Spec;
  const OutputSectDesc& m_OutputSectDesc;
  LinkerScript& m_LDScript;
};

} // namespace of mcld

#endif

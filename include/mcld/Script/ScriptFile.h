//===- ScriptFile.h -------------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef MCLD_SCRIPT_FILE_INTERFACE_H
#define MCLD_SCRIPT_FILE_INTERFACE_H
#ifdef ENABLE_UNITTEST
#include <gtest.h>
#endif

#include <mcld/Script/Assignment.h>
#include <mcld/Script/OutputSectDesc.h>
#include <mcld/Script/InputSectDesc.h>
#include <vector>
#include <string>

namespace mcld
{

class ScriptCommand;
class Input;
class InputTree;
class InputBuilder;
class GroupReader;
class LinkerConfig;
class LinkerScript;
class RpnExpr;
class StringList;

/** \class ScriptFile
 *  \brief This class defines the interfaces to a linker script file.
 */

class ScriptFile
{
public:
  enum Kind {
    LDScript,      // -T
    Expression,    // --defsym
    VersionScript, // --version-script
    DynamicList,   // --dynamic-list
    Unknown
  };

  typedef std::vector<ScriptCommand*> CommandQueue;
  typedef CommandQueue::const_iterator const_iterator;
  typedef CommandQueue::iterator iterator;
  typedef CommandQueue::const_reference const_reference;
  typedef CommandQueue::reference reference;

public:
  ScriptFile(Kind pKind, Input& pInput, InputBuilder& pBuilder);
  ~ScriptFile();

  const_iterator  begin() const { return m_CommandQueue.begin(); }
  iterator        begin()       { return m_CommandQueue.begin(); }
  const_iterator  end()   const { return m_CommandQueue.end(); }
  iterator        end()         { return m_CommandQueue.end(); }

  const_reference front() const { return m_CommandQueue.front(); }
  reference       front()       { return m_CommandQueue.front(); }
  const_reference back()  const { return m_CommandQueue.back(); }
  reference       back()        { return m_CommandQueue.back(); }

  const Input& input() const { return m_Input; }
  Input&       input()       { return m_Input; }

  size_t size() const { return m_CommandQueue.size(); }

  Kind getKind() const { return m_Kind; }

  const InputTree& inputs() const { return *m_pInputTree; }
  InputTree&       inputs()       { return *m_pInputTree; }

  const std::string& name() const { return m_Name; }
  std::string&       name()       { return m_Name; }

  void dump() const;
  void activate();

  /// ENTRY(symbol)
  void addEntryPoint(const std::string& pSymbol, LinkerScript& pScript);

  /// OUTPUT_FORMAT(bfdname)
  /// OUTPUT_FORMAT(default, big, little)
  void addOutputFormatCmd(const std::string& pFormat);
  void addOutputFormatCmd(const std::string& pDefault,
                          const std::string& pBig,
                          const std::string& pLittle);

  /// GROUP(file, file, ...)
  /// GROUP(file file ...)
  void addGroupCmd(StringList& pStringList,
                   GroupReader& pGroupReader,
                   const LinkerConfig& pConfig,
                   const LinkerScript& pScript);

  /// OUTPUT(filename)
  void addOutputCmd(const std::string& pFileName, LinkerScript& pScript);

  /// SEARCH_DIR(path)
  void addSearchDirCmd(const std::string& pPath, LinkerScript& pScript);

  /// OUTPUT_ARCH(bfdarch)
  void addOutputArchCmd(const std::string& pArch);

  /// ASSERT(exp, message)
  void addAssertCmd(RpnExpr& pRpnExpr, const std::string& pMessage);

  /// assignment
  void addAssignment(LinkerScript& pLDScript,
                     const std::string& pSymbol,
                     RpnExpr& pRpnExpr,
                     Assignment::Type pType = Assignment::DEFAULT);

  bool hasSectionsCmd() const;

  void enterSectionsCmd();

  void leaveSectionsCmd();

  void enterOutputSectDesc(const std::string& pName,
                           const OutputSectDesc::Prolog& pProlog);

  void leaveOutputSectDesc(const OutputSectDesc::Epilog& pEpilog);

  void addInputSectDesc(InputSectDesc::KeepPolicy pPolicy,
                        const InputSectDesc::Spec& pSpec,
                        LinkerScript& pScript);

  static const std::string& createParserStr(const char* pText, size_t pLength);

  static void clearParserStrPool();

private:
  Kind m_Kind;
  Input& m_Input;
  std::string m_Name;
  InputTree* m_pInputTree;
  InputBuilder& m_Builder;
  CommandQueue m_CommandQueue;
  bool m_HasSectionsCmd;
  bool m_InSectionsCmd;
  bool m_InOutputSectDesc;
};

} // namespace of mcld

#endif


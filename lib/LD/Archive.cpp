//===- Archive.cpp --------------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include <mcld/LD/Archive.h>
#include <mcld/MC/InputFactory.h>

using namespace mcld;

//===----------------------------------------------------------------------===//
// Archive
Archive::Archive(Input& pInputFile, InputFactory& pInputFactory)
 : m_ArchiveFile(pInputFile),
   m_pInputTree(NULL),
   m_SymTabEntryFactory(32)
{
  m_pInputTree = new InputTree(pInputFactory);
}

Archive::~Archive()
{
  delete m_pInputTree;
}

/// getARFile - get the Input& of the archive file
Input& Archive::getARFile()
{
  return m_ArchiveFile;
}

/// getARFile - get the Input& of the archive file
const Input& Archive::getARFile() const
{
  return m_ArchiveFile;
}

/// inputs - get the input tree built from this archive 
InputTree& Archive::inputs()
{
  return *m_pInputTree;
}

/// inputs - get the input tree built from this archive 
const InputTree& Archive::inputs() const
{
  return *m_pInputTree;
}

/// getObjectMemberMap - get the map that contains the included object files
Archive::ObjectMemberMapType& Archive::getObjectMemberMap()
{
  return m_ObjectMemberMap;
}

/// getObjectMemberMap - get the map that contains the included object files
const Archive::ObjectMemberMapType& Archive::getObjectMemberMap() const
{
  return m_ObjectMemberMap;
}

/// numOfObjectMember - return the number of included object files
size_t Archive::numOfObjectMember() const
{
  return m_ObjectMemberMap.numOfEntries();
}

/// addObjectMember - add a object in the object member map
/// @param pFileOffset - file offset in symtab represents a object file
/// @param pIter - the iterator in the input tree built from this archive
bool Archive::addObjectMember(uint32_t pFileOffset, InputTree::iterator pIter)
{
  bool exist;
  ObjectMemberEntryType* entry = m_ObjectMemberMap.insert(pFileOffset, exist);
  if (!exist)
    entry->setValue(pIter);
  return !exist;
}

/// hasObjectMember - check if a object file is included or not
/// @param pFileOffset - file offset in symtab represents a object file
bool Archive::hasObjectMember(uint32_t pFileOffset) const
{
  return (m_ObjectMemberMap.find(pFileOffset) != m_ObjectMemberMap.end());
}

/// getArchiveMemberMap - get the map that contains the included archive files
Archive::ArchiveMemberMapType& Archive::getArchiveMemberMap()
{
  return m_ArchiveMemberMap;
}

/// getArchiveMemberMap - get the map that contains the included archive files
const Archive::ArchiveMemberMapType& Archive::getArchiveMemberMap() const
{
  return m_ArchiveMemberMap;
}

/// addArchiveMember - add an archive in the archive member map
/// @param pName    - the name of the new archive member
/// @param pLastPos - this records the point to insert the next node in the
///                   subtree of this archive member
/// @param pMove    - this records the direction to insert the next node in the
///                   subtree of this archive member
bool Archive::addArchiveMember(const llvm::StringRef& pName,
                               InputTree::iterator pLastPos,
                               InputTree::Mover* pMove)
{
  bool exist;
  ArchiveMemberEntryType* entry = m_ArchiveMemberMap.insert(pName, exist);
  if (!exist) {
    ArchiveMember& ar = entry->value();
    ar.lastPos = pLastPos;
    ar.move = pMove;
  }
  return !exist;
}

/// hasArchiveMember - check if an archive file is included or not
bool Archive::hasArchiveMember(const llvm::StringRef& pName) const
{
  return (m_ArchiveMemberMap.find(pName) != m_ArchiveMemberMap.end());
}

/// getArchiveMember - get a archive member
Archive::ArchiveMemberType*
Archive::getArchiveMember(const llvm::StringRef& pName)
{
  ArchiveMemberMapType::iterator it = m_ArchiveMemberMap.find(pName);
  if (it != m_ArchiveMemberMap.end())
    return &(it.getEntry()->value());
  return NULL;
}

/// getSymbolTable - get the symtab
Archive::SymTabType& Archive::getSymbolTable()
{
  return m_SymTab;
}

/// getSymbolTable - get the symtab
const Archive::SymTabType& Archive::getSymbolTable() const
{
  return m_SymTab;
}

/// setSymTabSize - set the memory size of symtab
void Archive::setSymTabSize(size_t pSize)
{
  m_SymTabSize = pSize;
}

/// getSymTabSize - get the memory size of symtab
size_t Archive::getSymTabSize() const
{
  return m_SymTabSize;
}

/// numOfSymbols - return the number of symbols in symtab
size_t Archive::numOfSymbols() const
{
  return m_SymTab.size();
}

/// addSymbol - add a symtab entry to symtab
/// @param pName - symbol name
/// @param pFileOffset - file offset in symtab represents a object file
void Archive::addSymbol(const char* pName,
                        uint32_t pFileOffset,
                        enum Archive::Status pStatus)
{
  SymTabEntry* entry = m_SymTabEntryFactory.allocate();
  new (entry) SymTabEntry(pName, pFileOffset, pStatus);
  m_SymTab.push_back(entry);
}

/// getSymbolName - get the symbol name with the given index
const std::string& Archive::getSymbolName(size_t pSymIdx) const
{
  assert(pSymIdx < numOfSymbols());
  return m_SymTab[pSymIdx]->name;
}

/// getObjFileOffset - get the file offset that represent a object file
uint32_t Archive::getObjFileOffset(size_t pSymIdx) const
{
  assert(pSymIdx < numOfSymbols());
  return m_SymTab[pSymIdx]->fileOffset;
}

/// getSymbolStatus - get the status of a symbol
enum Archive::Status Archive::getSymbolStatus(size_t pSymIdx) const
{
  assert(pSymIdx < numOfSymbols());
  return m_SymTab[pSymIdx]->status;
}

/// setSymbolStatus - set the status of a symbol
void Archive::setSymbolStatus(size_t pSymIdx,
                              enum Archive::Status pStatus)
{
  assert(pSymIdx < numOfSymbols());
  m_SymTab[pSymIdx]->status = pStatus;
}

/// getStrTable - get the extended name table
std::string& Archive::getStrTable()
{
  return m_StrTab;
}

/// getStrTable - get the extended name table
const std::string& Archive::getStrTable() const
{
  return m_StrTab;
}


//===- Path.h -------------------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This file declares the mcld::sys::fs:: namespace. It follows TR2/boost
// filesystem (v3), but modified to remove exception handling and the
// path class.
//===----------------------------------------------------------------------===//
#ifndef MCLD_PATH_H
#define MCLD_PATH_H
#ifdef ENABLE_UNITTEST
#include <gtest.h>
#endif

#include <llvm/Support/raw_ostream.h>
#include <mcld/Config/Config.h>

#include <iosfwd>
#include <functional>
#include <string>
#include <locale>

namespace mcld {
namespace sys  {
namespace fs   {

#if defined(MCLD_ON_WIN32) && !defined(__MINGW32__)
const wchar_t preferred_separator = L'\\';
const wchar_t separator = L'\\';
const wchar_t colon = L':';
const wchar_t dot = L'.';
#else
const char    preferred_separator = '/';
const char    separator = '/';
const char    colon = ':';
const char    dot = L'.';
#endif

/** \class Path
 *  \brief Path provides an abstraction for the path to a file or directory in
 *   the operating system's filesystem.
 *
 *  FIXME: current Path library only support UTF-8 chararcter set.
 *
 */
class Path
{
public:
#if defined(MCLD_ON_WIN32) && !defined(__MINGW32__)
  typedef wchar_t                            ValueType;
  typedef std::wstring                       StringType;
#else
  typedef char                               ValueType;
  typedef std::string                        StringType;
#endif
  typedef std::codecvt<wchar_t, char, mbstate_t> CodeCVT;

public:
  Path();
  Path(const ValueType* s);
  Path(const StringType &s);
  Path(const Path& pCopy);
  virtual ~Path();

  // -----  assignments  ----- //
  template <class InputIterator>
  Path& assign(InputIterator begin, InputIterator end);
  Path& assign(const StringType &s);
  Path& assign(const ValueType* s, unsigned int length);

  //  -----  appends  ----- //
  template <class InputIterator>
  Path& append(InputIterator begin, InputIterator end);
  Path& append(const Path& pPath);

  //  -----  observers  ----- //
  bool empty() const;

  bool isFromRoot() const;
  bool isFromPWD() const;

  const StringType &native() const
  { return m_PathName; }

  StringType &native()
  { return m_PathName; }

  const ValueType* c_str() const
  { return m_PathName.c_str(); }

  std::string string() const;
#if !defined(__MINGW32__)
  std::wstring wstring() const;
#endif

  // -----  decomposition  ----- //
  Path parent_path() const;
  Path filename() const;
  Path stem() const;
  Path extension() const;

  // -----  generic form observers  ----- //
  StringType generic_string() const;
  bool canonicalize();

public:
  StringType::size_type m_append_separator_if_needed();
  void m_erase_redundant_separator(StringType::size_type sep_pos);

protected:
  StringType m_PathName;
};

bool operator==(const Path& pLHS,const Path& pRHS);
bool operator!=(const Path& pLHS,const Path& pRHS);
Path operator+(const Path& pLHS, const Path& pRHS);

//===----------------------------------------------------------------------===//
// Non-member Functions
//===----------------------------------------------------------------------===//
bool exists(const Path &pPath);

bool is_directory(const Path &pPath);

template <class Char, class Traits>
inline std::basic_ostream<Char, Traits>&
operator<<(std::basic_ostream<Char, Traits>& pOS, const Path& pPath)
{
  return pOS << pPath.native();
}

template <class Char, class Traits>
inline std::basic_istream<Char, Traits>&
operator>>(std::basic_istream<Char, Traits>& pOS, Path& pPath)
{
  return pOS >> pPath.native();
}

inline llvm::raw_ostream&
operator<<(llvm::raw_ostream& pOS, const Path& pPath)
{
  return pOS << pPath.native();
}

//===----------------------------------------------------------------------===//
// class path member template implementation
//===----------------------------------------------------------------------===//
template <class InputIterator>
Path& Path::assign(InputIterator begin, InputIterator end)
{
  m_PathName.clear();
  if (begin != end)
    m_PathName.append<InputIterator>(begin, end);
  return *this;
}

template <class InputIterator>
Path& Path::append(InputIterator begin, InputIterator end)
{
  if (begin == end)
    return *this;
  StringType::size_type sep_pos(m_append_separator_if_needed());
  m_PathName.append<InputIterator>(begin, end);
  if (sep_pos)
    m_erase_redundant_separator(sep_pos);
  return *this;
}

} // namespace of fs
} // namespace of sys
} // namespace of mcld

//===----------------------------------------------------------------------===//
// STL compatible functions
//===----------------------------------------------------------------------===//
namespace std {

template<>
struct less<mcld::sys::fs::Path> : public binary_function<mcld::sys::fs::Path,
                                                         mcld::sys::fs::Path,
                                                         bool>
{
  bool operator() (const mcld::sys::fs::Path& pX,const mcld::sys::fs::Path& pY) const {
    if (pX.generic_string().size() < pY.generic_string().size())
      return true;
    return (pX.generic_string() < pY.generic_string());
  }
};

} // namespace of std

#endif


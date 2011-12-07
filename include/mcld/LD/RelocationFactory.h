/*****************************************************************************
 *   The MCLinker Project, Copyright (C), 2011 -                             *
 *   Embedded and Web Computing Lab, National Taiwan University              *
 *   MediaTek, Inc.                                                          *
 *                                                                           *
 *   Diana Chen <diana.chen@mediatek.com>                                    *
 ****************************************************************************/
#ifndef LD_RELOCATION_FACTORY_H
#define LD_RELOCATION_FACTORY_H
#ifdef ENABLE_UNITTEST
#include <gtest.h>
#endif
#include <mcld/LD/Relocation.h>

namespace mcld
{

class LDSymbol;
class ResolveInfo;
class MCFragmentRef;

/** \class RelocationFactory
 *  \brief RelocationFactory provides the interface for generating target
 *  relocation
 *
 */
class RelocationFactory
{
public:
  typedef Relocation::Type Type;
  typedef Relocation::Address Address;
  typedef Relocation::DWord DWord;

public:
  RelocationFactory();
  virtual ~RelocationFactory();

  // ----- production ----- //
  virtual Relocation* produce(Type pType,
                              const LDSymbol& pSymbol,
                              MCFragmentRef& pFragRef,
                              DWord* pTargetData = NULL,
                              Address pAddend = 0) = 0;

  virtual void destroy(Relocation* pRelocation) = 0;

};

} // namespace of mcld

#endif


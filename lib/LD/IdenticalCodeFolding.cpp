//===- IndenticalCodeFolding.cpp ------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include <mcld/LD/IdenticalCodeFolding.h>

#include <mcld/GeneralOptions.h>
#include <mcld/Module.h>
#include <mcld/Fragment/RegionFragment.h>
#include <mcld/LD/LDContext.h>
#include <mcld/LD/LDSection.h>
#include <mcld/LD/RelocData.h>
#include <mcld/LD/Relocator.h>
#include <mcld/LD/ResolveInfo.h>
#include <mcld/LD/SectionData.h>
#include <mcld/LinkerConfig.h>
#include <mcld/MC/Input.h>
#include <mcld/Support/Demangle.h>
#include <mcld/Support/MsgHandling.h>
#include <mcld/Target/GNULDBackend.h>

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/Format.h>

#include <cassert>
#include <map>
#include <set>

#if !defined(__MINGW32__)
#include <zlib.h>
#else
static uint32_t crc32_tab[] = {
	0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419, 0x706af48f,
	0xe963a535, 0x9e6495a3,	0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988,
	0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91, 0x1db71064, 0x6ab020f2,
	0xf3b97148, 0x84be41de,	0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
	0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec,	0x14015c4f, 0x63066cd9,
	0xfa0f3d63, 0x8d080df5,	0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172,
	0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,	0x35b5a8fa, 0x42b2986c,
	0xdbbbc9d6, 0xacbcf940,	0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
	0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423,
	0xcfba9599, 0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
	0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,	0x76dc4190, 0x01db7106,
	0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
	0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x086d3d2d,
	0x91646c97, 0xe6635c01, 0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e,
	0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
	0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
	0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7,
	0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0,
	0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa,
	0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
	0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81,
	0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a,
	0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683, 0xe3630b12, 0x94643b84,
	0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
	0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
	0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc,
	0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5, 0xd6d6a3e8, 0xa1d1937e,
	0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
	0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55,
	0x316e8eef, 0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
	0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe, 0xb2bd0b28,
	0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
	0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9, 0xeb0e363f,
	0x72076785, 0x05005713, 0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38,
	0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
	0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
	0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69,
	0x616bffd3, 0x166ccf45, 0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2,
	0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc,
	0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
	0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693,
	0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
	0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
};

uint32_t
crc32(uint32_t crc, const void *buf, size_t size)
{
	const uint8_t *p;

	p = (const uint8_t *)buf;
	crc = crc ^ ~0U;

	while (size--)
		crc = crc32_tab[(crc ^ *p++) & 0xFF] ^ (crc >> 8);

	return crc ^ ~0U;
}
#endif

namespace mcld {

static bool isSymCtorOrDtor(const ResolveInfo& pSym) {
  // We can always fold ctors and dtors since accessing function pointer in C++
  // is forbidden.
  llvm::StringRef name(pSym.name(), pSym.nameSize());
  if (!name.startswith("_ZZ") && !name.startswith("_ZN")) {
    return false;
  }
  return isCtorOrDtor(pSym.name(), pSym.nameSize());
}

IdenticalCodeFolding::IdenticalCodeFolding(const LinkerConfig& pConfig,
                                           const TargetLDBackend& pBackend,
                                           Module& pModule)
    : m_Config(pConfig), m_Backend(pBackend), m_Module(pModule) {
}

void IdenticalCodeFolding::foldIdenticalCode() {
  // 1. Find folding candidates.
  FoldingCandidates candidate_list;
  findCandidates(candidate_list);

  // 2. Initialize constant section content
  for (size_t i = 0; i < candidate_list.size(); ++i) {
    candidate_list[i].initConstantContent(m_Backend, m_KeptSections);
  }

  // 3. Find identical code until convergence
  bool converged = false;
  size_t iterations = 0;
  while (!converged && (iterations < m_Config.options().getICFIterations())) {
    converged = matchCandidates(candidate_list);
    ++iterations;
  }
  if (m_Config.options().printICFSections()) {
    debug(diag::debug_icf_iterations) << iterations;
  }

  // 4. Fold the identical code
  typedef std::set<Input*> FoldedObjects;
  FoldedObjects folded_objs;
  KeptSections::iterator kept, keptEnd = m_KeptSections.end();
  size_t index = 0;
  for (kept = m_KeptSections.begin(); kept != keptEnd; ++kept, ++index) {
    LDSection* sect = (*kept).first;
    Input* obj = (*kept).second.first;
    size_t kept_index = (*kept).second.second;
    if (index != kept_index) {
      sect->setKind(LDFileFormat::Folded);
      folded_objs.insert(obj);

      if (m_Config.options().printICFSections()) {
        KeptSections::iterator it = m_KeptSections.begin() + kept_index;
        LDSection* kept_sect = (*it).first;
        Input* kept_obj = (*it).second.first;
        debug(diag::debug_icf_folded_section) << sect->name() << obj->name()
                                              << kept_sect->name()
                                              << kept_obj->name();
      }
    }
  }

  // Adjust the fragment reference of the folded symbols.
  FoldedObjects::iterator fobj, fobjEnd = folded_objs.end();
  for (fobj = folded_objs.begin(); fobj != fobjEnd; ++fobj) {
    LDContext::sym_iterator sym, symEnd = (*fobj)->context()->symTabEnd();
    for (sym = (*fobj)->context()->symTabBegin(); sym != symEnd; ++sym) {
      if ((*sym)->hasFragRef() && ((*sym)->type() == ResolveInfo::Function)) {
        LDSymbol* out_sym = (*sym)->resolveInfo()->outSymbol();
        FragmentRef* frag_ref = out_sym->fragRef();
        LDSection* sect = &(frag_ref->frag()->getParent()->getSection());
        if (sect->kind() == LDFileFormat::Folded) {
          size_t kept_index = m_KeptSections[sect].second;
          LDSection* kept_sect = (*(m_KeptSections.begin() + kept_index)).first;
          frag_ref->assign(kept_sect->getSectionData()->front(),
                           frag_ref->offset());
        }
      }
    }  // for each symbol
  }    // for each folded object
}

void IdenticalCodeFolding::findCandidates(FoldingCandidates& pCandidateList) {
  Module::obj_iterator obj, objEnd = m_Module.obj_end();
  for (obj = m_Module.obj_begin(); obj != objEnd; ++obj) {
    std::set<const LDSection*> funcptr_access_set;
    typedef std::map<LDSection*, LDSection*> CandidateMap;
    CandidateMap candidate_map;
    LDContext::sect_iterator sect, sectEnd = (*obj)->context()->sectEnd();
    for (sect = (*obj)->context()->sectBegin(); sect != sectEnd; ++sect) {
      switch ((*sect)->kind()) {
        case LDFileFormat::TEXT: {
          candidate_map.insert(
              std::make_pair(*sect, reinterpret_cast<LDSection*>(NULL)));
          break;
        }
        case LDFileFormat::Relocation: {
          LDSection* target = (*sect)->getLink();
          if (target->kind() == LDFileFormat::TEXT) {
            candidate_map[target] = *sect;
          }

          // Safe icf
          if (m_Config.options().getICFMode() == GeneralOptions::ICF_Safe) {
            RelocData::iterator rel, relEnd = (*sect)->getRelocData()->end();
            for (rel = (*sect)->getRelocData()->begin(); rel != relEnd; ++rel) {
              LDSymbol* sym = rel->symInfo()->outSymbol();
              if (sym->hasFragRef() && (sym->type() == ResolveInfo::Function)) {
                const LDSection* def =
                    &sym->fragRef()->frag()->getParent()->getSection();
                if (!isSymCtorOrDtor(*rel->symInfo()) &&
                    m_Backend.mayHaveUnsafeFunctionPointerAccess(*target) &&
                    m_Backend.getRelocator()
                        ->mayHaveFunctionPointerAccess(*rel)) {
                  funcptr_access_set.insert(def);
                }
              }
            }  // for each reloc
          }

          break;
        }
        default: {
          // skip
          break;
        }
      }  // end of switch
    }    // for each section

    CandidateMap::iterator candidate, candidateEnd = candidate_map.end();
    for (candidate = candidate_map.begin(); candidate != candidateEnd;
         ++candidate) {
      if ((m_Config.options().getICFMode() == GeneralOptions::ICF_All) ||
          (funcptr_access_set.count(candidate->first) == 0)) {
        size_t index = m_KeptSections.size();
        m_KeptSections[candidate->first] = ObjectAndId(*obj, index);
        pCandidateList.push_back(
            FoldingCandidate(candidate->first, candidate->second, *obj));
      }
    }  // for each possible candidate
  }  // for each obj
}

bool IdenticalCodeFolding::matchCandidates(FoldingCandidates& pCandidateList) {
  typedef std::multimap<uint32_t, size_t> ChecksumMap;
  ChecksumMap checksum_map;
  std::vector<std::string> contents(pCandidateList.size());
  bool converged = true;

  for (size_t index = 0; index < pCandidateList.size(); ++index) {
    contents[index] = pCandidateList[index].getContentWithVariables(
        m_Backend, m_KeptSections);
    uint32_t checksum = ::crc32(0xFFFFFFFF,
                                (const uint8_t*)contents[index].c_str(),
                                contents[index].length());

    size_t count = checksum_map.count(checksum);
    if (count == 0) {
      checksum_map.insert(std::make_pair(checksum, index));
    } else {
      std::pair<ChecksumMap::iterator, ChecksumMap::iterator> ret =
          checksum_map.equal_range(checksum);
      for (ChecksumMap::iterator it = ret.first; it != ret.second; ++it) {
        size_t kept_index = (*it).second;
        if (contents[index].compare(contents[kept_index]) == 0) {
          m_KeptSections[pCandidateList[index].sect].second = kept_index;
          converged = false;
          break;
        }
      }
    }
  }

  return converged;
}

void IdenticalCodeFolding::FoldingCandidate::initConstantContent(
    const TargetLDBackend& pBackend,
    const IdenticalCodeFolding::KeptSections& pKeptSections) {
  // Get the static content from text.
  assert(sect != NULL && sect->hasSectionData());
  SectionData::const_iterator frag, fragEnd = sect->getSectionData()->end();
  for (frag = sect->getSectionData()->begin(); frag != fragEnd; ++frag) {
    switch (frag->getKind()) {
      case Fragment::Region: {
        const RegionFragment& region = llvm::cast<RegionFragment>(*frag);
        content.append(region.getRegion().begin(), region.size());
        break;
      }
      default: {
        // FIXME: Currently we only take care of RegionFragment.
        break;
      }
    }
  }

  // Get the static content from relocs.
  if (reloc_sect != NULL && reloc_sect->hasRelocData()) {
    RelocData::iterator rel, relEnd = reloc_sect->getRelocData()->end();
    for (rel = reloc_sect->getRelocData()->begin(); rel != relEnd; ++rel) {
      llvm::format_object4<Relocation::Type,
                           Relocation::Address,
                           Relocation::Address,
                           Relocation::Address> rel_info("%x%llx%llx%llx",
                                                         rel->type(),
                                                         rel->symValue(),
                                                         rel->addend(),
                                                         rel->place());
      char rel_str[48];
      rel_info.print(rel_str, sizeof(rel_str));
      content.append(rel_str);

      // Handle the recursive call.
      LDSymbol* sym = rel->symInfo()->outSymbol();
      if ((sym->type() == ResolveInfo::Function) && sym->hasFragRef()) {
        LDSection* def = &sym->fragRef()->frag()->getParent()->getSection();
        if (def == sect) {
          continue;
        }
      }

      if (!pBackend.isSymbolPreemptible(*rel->symInfo()) && sym->hasFragRef() &&
          (pKeptSections.find(
               &sym->fragRef()->frag()->getParent()->getSection()) !=
           pKeptSections.end())) {
        // Mark this reloc as a variable.
        variable_relocs.push_back(rel);
      } else {
        // TODO: Support inlining merge sections if possible (target-dependent).
        if ((sym->binding() == ResolveInfo::Local) ||
            (sym->binding() == ResolveInfo::Absolute)) {
          // ABS or Local symbols.
          content.append(sym->name()).append(obj->name()).append(
              obj->path().native());
        } else {
          content.append(sym->name());
        }
      }
    }
  }
}

std::string IdenticalCodeFolding::FoldingCandidate::getContentWithVariables(
    const TargetLDBackend& pBackend,
    const IdenticalCodeFolding::KeptSections& pKeptSections) {
  std::string result(content);
  // Compute the variable content from relocs.
  std::vector<Relocation*>::const_iterator rel, relEnd = variable_relocs.end();
  for (rel = variable_relocs.begin(); rel != relEnd; ++rel) {
    LDSymbol* sym = (*rel)->symInfo()->outSymbol();
    LDSection* def = &sym->fragRef()->frag()->getParent()->getSection();
    // Use the kept section index.
    KeptSections::const_iterator it = pKeptSections.find(def);
    llvm::format_object1<size_t> kept_info("%x", (*it).second.second);
    char kept_str[8];
    kept_info.print(kept_str, sizeof(kept_str));
    result.append(kept_str);
  }

  return result;
}

} // namespace mcld

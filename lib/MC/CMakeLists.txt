add_mcld_library(MCLDMC
  Attribute.cpp
  AttributeSet.cpp
  CommandAction.cpp
  ContextFactory.cpp
  FileAction.cpp
  Input.cpp
  InputAction.cpp
  InputBuilder.cpp
  InputFactory.cpp
  MCLDDirectory.cpp
  SearchDirs.cpp
  SymbolCategory.cpp
  ZOption.cpp
  )

target_link_libraries(MCLDMC ${cmake_2_8_12_PRIVATE}
  MCLDSupport
  )

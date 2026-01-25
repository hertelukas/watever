set(APP_SOURCES
  binary.cpp
  color.cpp
  ir.cpp
  legalization.cpp
  utils.cpp
  opcode.cpp
  opcode-code-table.c
  printer.cpp
  symbol.cpp
  FixFunctionBitcasts.cpp)
  
add_library(${PROJECT_NAME}lib ${APP_SOURCES})

target_include_directories(${PROJECT_NAME}lib PUBLIC
  ${PROJECT_SOURCE_DIR}/include
)
target_include_directories(${PROJECT_NAME}lib SYSTEM PUBLIC
  ${LLVM_INCLUDE_DIRS}
)

if (LLVM_LINK_LLVM_DYLIB)
  message(STATUS "Dynamically linking against LLVM")
  target_link_libraries(${PROJECT_NAME}lib PUBLIC LLVM)
else()
  message(STATUS "Statically linking LLVM")
  target_link_libraries(${PROJECT_NAME}lib PUBLIC
    LLVMCore
    LLVMPasses
    LLVMSupport)
endif()

set(WATEVER_LOGGING "DebugOnly" CACHE STRING "Enable logging with spdlog (DebugOnly(default)/ON/OFF)")
set_property(CACHE WATEVER_LOGGING PROPERTY STRINGS DebugOnly ON OFF)

# spdlog
if (((WATEVER_LOGGING STREQUAL "DebugOnly") AND (CMAKE_BUILD_TYPE STREQUAL "Debug")) OR (WATEVER_LOGGING STREQUAL "ON"))
  set(SPDLOG_NO_EXCEPTIONS ON CACHE BOOL "WATEVER compiles without exceptions")
  set(SPDLOG_USE_STD_FORMAT ON CACHE BOOL "WATEVER uses C++23")
  add_subdirectory(../deps/spdlog ${CMAKE_CURRENT_BINARY_DIR}/deps/spdlog)
  
  target_compile_definitions(${PROJECT_NAME}lib PUBLIC WATEVER_LOGGING)
  target_link_libraries(${PROJECT_NAME}lib PUBLIC spdlog::spdlog)
endif ()

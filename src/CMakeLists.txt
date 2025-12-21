set(APP_SOURCES
  binary.cpp
  ir.cpp
  legalization.cpp
  utils.cpp
  opcode.cpp
  opcode-code-table.c
  printer.cpp
  symbol.cpp)
  
add_library(${PROJECT_NAME}lib ${APP_SOURCES})

target_include_directories(${PROJECT_NAME}lib PUBLIC
  ${PROJECT_SOURCE_DIR}/include
)
target_include_directories(${PROJECT_NAME}lib SYSTEM PUBLIC
  ${LLVM_INCLUDE_DIRS}
)

target_link_libraries(${PROJECT_NAME}lib PUBLIC LLVM)

set(WATEVER_LOGGING "ON" CACHE STRING "Enable logging with spdlog (DebugOnly/ON(default)/OFF)")
set_property(CACHE WATEVER_LOGGING PROPERTY STRINGS DebugOnly ON OFF)

# spdlog
if (((WATEVER_LOGGING STREQUAL "DebugOnly") AND (CMAKE_BUILD_TYPE STREQUAL "Debug")) OR (WATEVER_LOGGING STREQUAL "ON"))
  set(SPDLOG_NO_EXCEPTIONS ON CACHE BOOL "WATEVER compiles without exceptions")
  set(SPDLOG_USE_STD_FORMAT ON CACHE BOOL "WATEVER uses C++23")
  add_subdirectory(../deps/spdlog ${CMAKE_CURRENT_BINARY_DIR}/deps/spdlog)
  
  target_compile_definitions(${PROJECT_NAME}lib PUBLIC WATEVER_LOGGING)
  target_link_libraries(${PROJECT_NAME}lib PUBLIC spdlog::spdlog)
endif ()

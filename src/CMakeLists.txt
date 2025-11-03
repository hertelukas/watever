set(APP_SOURCES
  utils.cpp
  opcode.cpp
  opcode-code-table.c)
  
add_library(${PROJECT_NAME}lib ${APP_SOURCES})

target_include_directories(${PROJECT_NAME}lib
  PUBLIC
    ${PROJECT_SOURCE_DIR}/include
    ${LLVM_INCLUDE_DIRS}
)

target_link_libraries(${PROJECT_NAME}lib PUBLIC LLVM)

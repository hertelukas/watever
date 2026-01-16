if (NOT LLVM_EXTERNAL_LIT)
    find_program(LLVM_EXTERNAL_LIT NAMES llvm-lit lit)
endif()

if (NOT LLVM_EXTERNAL_LIT)
    message(WARNING "lit not found, tests will not be available.")
    return()
else ()
    message(STATUS "Found lit: ${LLVM_EXTERNAL_LIT}")
endif()

configure_file(lit.site.cfg.py.in lit.site.cfg.py @ONLY)

add_custom_target(check-${PROJECT_NAME}
    COMMAND ${LLVM_EXTERNAL_LIT} -sv ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${PROJECT_NAME}
)

add_custom_target(check-benchmark
    COMMAND ${LLVM_EXTERNAL_LIT} -sv ${CMAKE_CURRENT_BINARY_DIR}/benchmark
    DEPENDS ${PROJECT_NAME}
)

add_custom_target(check-all
    DEPENDS check-${PROJECT_NAME} check-benchmark
)

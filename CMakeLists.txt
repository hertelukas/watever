cmake_minimum_required(VERSION 3.25)
project(watever)

include(CheckLinkerFlag)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# warnings
if (MSVC)
    add_compile_options(/W4 /WX)
    # disable RTTI and exceptions
    add_compile_options(/GR- /EHsc /D_HAS_EXCEPTIONS=0)
else ()
    add_compile_options(-Wall -Wextra -Wpedantic)
    # disable RTTI and exceptions
    add_compile_options("$<$<COMPILE_LANGUAGE:CXX>:-fno-rtti;-fno-exceptions>")

    # this is annoying
    add_compile_options(-Wno-missing-field-initializers)

    if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
        add_compile_options(-Wno-nested-anon-types)
        add_compile_options(-Wno-old-style-cast)
        add_compile_options(-Wno-covered-switch-default)
        add_compile_options(-Wsign-compare)
    endif ()

    check_linker_flag(CXX "LINKER:--gc-sections" HAVE_GC_SECTIONS)
    if (HAVE_GC_SECTIONS)
        add_compile_options(-ffunction-sections -fdata-sections)
        add_link_options(LINKER:--gc-sections)
    endif ()

    # for development builds use -Werror and enable sanitizers
    if (PROJECT_IS_TOP_LEVEL)
        add_compile_options(-Werror -Wno-error=deprecated-declarations)
        if (CMAKE_BUILD_TYPE STREQUAL "Debug")
            add_compile_options(-fsanitize=address -D_GLIBCXX_ASSERTIONS)
            add_link_options(-fsanitize=address)
        endif ()
    endif ()
endif ()

find_package(LLVM CONFIG REQUIRED)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

add_subdirectory(src)
add_subdirectory(app)
add_subdirectory(test)

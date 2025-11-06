add_executable(${PROJECT_NAME} main.cpp)

target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_NAME}lib)
target_include_directories(${PROJECT_NAME} PRIVATE ../deps/)

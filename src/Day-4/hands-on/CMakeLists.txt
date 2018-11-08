cmake_minimum_required(VERSION 3.12)

project(day_4_prepared_codes LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)
include_directories(${CMAKE_BINARY_DIR}/mod)

set(directories_list
  utilities
  abstract-calculus
  tests
)
foreach(directory ${directories_list})
  add_subdirectory( "${directory}" )
endforeach()

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/grid-parameters.nml"
  "${CMAKE_CURRENT_BINARY_DIR}/grid-parameters.nml"
  COPYONLY
)
enable_testing()

#---------------------------------------------------------------------------------------
# Define macro for adding CAF tests, and ensuring proper flags are passed to MPI runtime
#---------------------------------------------------------------------------------------
function(add_caf_test name num_caf_img test_target)
  add_test(NAME ${name}
    COMMAND cafrun -n ${num_caf_img} .tests/${test_target})
  set_tests_properties(${name}
    PROPERTIES
    PASS_REGULAR_EXPRESSION "Test passed."
    PROCESSORS ${num_caf_img})
endfunction()

add_caf_test(abstract-calculus-test 4 test-abstract-calculus)

cmake_minimum_required(VERSION 3.12)

project(day_4_prepared_codes LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)
include_directories(${CMAKE_BINARY_DIR}/mod)

add_library( utilities
  object_interface.f90
  assertion-interface.f90
  assertion-implementation.f90
)

add_executable( test-person
  person-implementation.f90
  person-interface.f90
  test-person.f90
)
target_link_libraries( test-person
  utilities
)

add_executable( test-abstract-calculus
  test-abstract-calculus.f90
  scalar-field-interface.f90
  spectral-field-module.f90
  spectral-field-submodule.f90
)

target_link_libraries( test-abstract-calculus
  utilities
)

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
    COMMAND cafrun -n ${num_caf_img} ./${test_target})
  set_tests_properties(${name}
    PROPERTIES
    PASS_REGULAR_EXPRESSION "Test passed."
    PROCESSORS ${num_caf_img})
endfunction()

#add_caf_test(test-abstract-calculus 4 test-abstract-calculus)

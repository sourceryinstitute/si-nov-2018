cmake_minimum_required(VERSION 3.12)

project(day_3_hands_on LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)
include_directories(${CMAKE_BINARY_DIR}/mod)

add_library( utilities
  Day-1/code-jams/02-assertion-interface.f90
  Day-1/code-jams/03-assertion-implementation.f90
)

add_executable( test-dimensional-grid
  test-dimensional-grid.f90
  structured-grid-implementation.f90
  structured-grid-interface.f90
  dimensional-grid-interface.f90
)

target_link_libraries( test-dimensional-grid
  utilities
)

add_executable( test-structured-grid
  test-structured-grid.f90
  structured-grid-implementation.f90
  structured-grid-interface.f90
)

target_link_libraries( test-structured-grid
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

add_caf_test(test-dimensional-grid 4 test-dimensional-grid)
add_caf_test(test-structured-grid 4 test-structured-grid)

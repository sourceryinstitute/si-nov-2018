add_executable( test-scalar-field
  test-scalar-field.f90
)
add_executable( test-emulated-co_sum
  test-emulated-co_sum.f90
)
set( tests_list
  scalar-field
  emulated-co_sum
)
foreach(test ${tests_list})
  target_link_libraries( test-${test}
    utilities
    abstractcalculus
  )
endforeach()

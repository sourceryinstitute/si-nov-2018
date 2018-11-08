add_executable( test-scalar-field
  test-scalar-field.f90
)
target_link_libraries( test-scalar-field
  utilities
  abstractcalculus
)

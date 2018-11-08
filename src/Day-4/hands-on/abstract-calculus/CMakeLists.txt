add_library( abstractcalculus
  scalar-field-interface.f90
  spectral-field-module.f90
  spectral-field-submodule.f90
)
target_link_libraries( abstractcalculus
  utilities
)

program ud_dtio_demo
  use iso_fortran_env, only : output_unit
  use assertion_interface, only : assert
  use person_interface, only : person
  implicit none

  call assert(0==1,"never happens",person)

  write (unit=output_unit,fmt=*) person()
    !! List-directed output of the result of an intrinsic structure constructor invocation

  write (unit=output_unit,fmt='(a,i4)') "Hello, world from ",this_image()
   !! Example of formatted output
end program

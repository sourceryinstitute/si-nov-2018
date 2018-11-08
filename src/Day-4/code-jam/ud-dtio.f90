MODULE person_interface
  implicit none

  private
  public :: person

  type person
    private
    character (len=7) :: name="fortran"
    integer :: age=61
    contains
      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
  end type person

  interface
    module subroutine write_formatted(dtv,unit,iotype,vlist,iostat,iomsg)
      class(person), intent(in) :: dtv
      integer, intent(in) :: unit
      character (len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
    end subroutine
  end interface

end module

submodule(person_interface) person_implementation
contains

    module subroutine write_formatted(dtv,unit,iotype,vlist,iostat,iomsg)
      class(person), intent(in) :: dtv
      integer, intent(in) :: unit
      character (len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
      if (iotype/="LISTDIRECTED") error stop "unsupported I/O type"
      write(unit,fmt=*) "User-defined derived type I/O: ",dtv%name,dtv%age
    end subroutine

end submodule

program ud_dtio_demo
  use iso_fortran_env, only : output_unit
  use person_interface, only : person
  implicit none

  write (unit=output_unit,fmt=*) person()
    !! List-directed output of the result of an intrinsic structure constructor invocation

  write (unit=output_unit,fmt='(a,i4)') "Hello, world from ",this_image()
   !! Example of formatted output
end program

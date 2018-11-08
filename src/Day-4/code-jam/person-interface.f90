module person_interface
  use object_interface, only : object
  implicit none

  private
  public :: person

  type, extends(object) :: person
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

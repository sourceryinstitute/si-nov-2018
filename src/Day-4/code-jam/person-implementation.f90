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

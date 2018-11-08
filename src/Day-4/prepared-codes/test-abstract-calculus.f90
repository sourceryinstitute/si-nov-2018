program main
  !! author: Damian Rouson
  !!
  !! Check addition and real-scalar pre-multiplication via abstract calculus
  use assertion_interface, only : assert, assertions
  use field_abstract_interface, only : field
  implicit none
  real, parameter :: a=1.
  class(field), allocatable :: x[:],y[:],z[:]
  integer, allocatable :: nx(:)

  nx = get_grid_resolution("grid-parameters.nml")

  associate(space_dimension=>size(nx))
    if (assertions) call assert(space_dimension==3,"3D grid")

   call x%initialize()
   call y%initialize()
   call z%initialize()

  end associate

contains

  function get_grid_resolution(file_name) result(resolution)

    character(len=*), intent(in) :: file_name
    integer, parameter :: success=0, reader=1
    integer, allocatable :: resolution(:)
    integer nx, ny, nz, file_unit, stat
    namelist/grid_resolution/ nx, ny, nz

    if (this_image()==reader) then
      open(newunit=file_unit,file=file_name,iostat=stat)
      call assert(stat==success,"file opened sucessfully")

      read(unit=file_unit,nml=grid_resolution,iostat=stat)
      call assert(stat==success,"file read sucessfully")

      resolution = [nx,ny,nz]
    end if

    call co_broadcast(resolution,source_image=reader)

  end function

end program

module dimensional_grid_interface
  use structured_grid_interface, only : structured_grid, space_dimension
  implicit none

  private
  public :: dimensional_grid
  public :: m, kg, sec
  public :: space_dimension

  enum, bind(C)
    enumerator m,kg,sec
  end enum

  integer, parameter :: num_fundamental = size([m,kg,sec])

  type, extends(structured_grid) :: dimensional_grid
    integer :: units(num_fundamental)
  end type

end module

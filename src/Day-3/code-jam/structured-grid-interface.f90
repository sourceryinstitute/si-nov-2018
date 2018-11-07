module structured_grid_interface
  implicit none

  private
  public :: structured_grid
  public :: space_dimension

  integer, parameter :: space_dimension=3

  type structured_grid
    real, allocatable :: x(:,:,:,:)
      !! grid point locations
  contains
    procedure, nopass :: get_grid_resolution
    procedure allocate_my_partition
    procedure my_partition_size
    procedure, private :: subtract
    generic :: operator(-)=>subtract ! ,negate (not implemented)
   !procedure, private, pass(rhs) :: subtract  ! use this to invoke a procedure on the right-hand-side operand
  end type

 ! Fortran 90 style operator uses static (compile-time) polymorphism and is not heritable
 !interface operator(-)
 !   module procedure subtract,negate, subtract_dimensional_grid
 !end interface

  interface

    module function subtract(lhs,rhs) result(difference)
      !! Return the component-wise difference between the two operands
      class(structured_grid), intent(in) :: lhs,rhs
      class(structured_grid), allocatable :: difference
    end function

    module function get_grid_resolution(file_name) result(resolution)
      !! Read and return grid size parameters
      character(len=*), intent(in) :: file_name
      integer :: resolution(space_dimension)
    end function

    pure module subroutine allocate_my_partition(this,resolution)
      !! Establish a coarray storing grid locations
      class(structured_grid), intent(inout) :: this
      integer, intent(in) :: resolution(space_dimension)
    end subroutine

    pure module function my_partition_size(this) result(my_num_points)
      !! Return the number of grid points owned by this image
      class(structured_grid), intent(in) :: this
      integer my_num_points
    end function

  end interface

end module

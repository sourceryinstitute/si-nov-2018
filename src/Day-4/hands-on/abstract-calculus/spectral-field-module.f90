module spectral_field_interface
  !! author: Damian Rouson
  !!
  !! Demonstrate the implementation of the deferred bindings in the
  !! abstract scalar_field

  use scalar_field_interface, only : scalar_field
  implicit none

  private
  public :: spectral_field

  type, extends(scalar_field) :: spectral_field
    private
    real, allocatable :: f(:,:,:)
  contains
    procedure :: zero
    procedure :: subtract
  end type

  interface

    module function zero(this) result(zero_object)
      !! Factory method: the result is a scalar_field with all field values to zero
      implicit none
      class(spectral_field), intent(in) :: this
      class(scalar_field), allocatable :: zero_object
    end function

    module function subtract(lhs,rhs) result(difference)
      !! The result is the numerical difference between the two operands
      implicit none
      class(spectral_field), intent(in) :: lhs
      class(scalar_field), intent(in) :: rhs
      class(scalar_field), allocatable :: difference
    end function

  end interface

end module

module scalar_field_interface
  !! author: Damian Rouson
  !!
  !! Demonstrate Template Method and Factory Method patterns
  implicit none

  private
  public :: scalar_field

  type, abstract :: scalar_field
  contains
    procedure(zero_interface), deferred :: zero
    procedure(binary_operator_interface), deferred :: subtract
    procedure :: negate
    generic :: operator(-)=>subtract, negate
  end type

  abstract interface

    module function zero_interface(this) result(zero_object)
      !! Factory method: the result is a scalar_field with all field values to zero
      implicit none
      class(scalar_field), intent(in) :: this
      class(scalar_field), allocatable :: zero_object
    end function

    module function binary_operator_interface(lhs,rhs) result(difference)
      !! The result is the numerical difference between the two operands
      implicit none
      class(scalar_field), intent(in) :: lhs, rhs
      class(scalar_field), allocatable :: difference
    end function

  end interface

contains

    module function negate(rhs) result(negative_rhs)
      !! Template method: invoke subtraction with a zero left-hand side
      class(scalar_field), intent(in) :: rhs
      class(scalar_field), allocatable :: negative_rhs
      negative_rhs = rhs%zero() - rhs
    end function

end module

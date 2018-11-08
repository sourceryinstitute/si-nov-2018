module vector_interface
  !! author: Damian Rouson
  !!
  !! Check whether non-abstract extended type must implement both versions of
  !! a generic binding
  implicit none

  private
  public :: scalar_field

  type, abstract :: scalar_field
    private
    real, allocatable  :: x(:,:,:)
  contains
    procedure(binary_operator_interface), deferred :: subtract
    procedure :: negate
      !! invoke subtraction with a zero left-hand size
    generic :: operator(-)=>subtract, negate
  end type
end module

module field_interface
  !! author: Damian Rouson
  !!
  !! Define a scalar field abstract class


  implicit none

  private
  public :: field

  type, abstract :: field
    !! Express and enforce functionality that is useful for writing expressions on scalar fields
  contains
     procedure(default_mutator), deferred ::  initialize
  end type

  abstract interface
    !! Interfaces for deferred bindings

    module subroutine default_mutator(this)
      !! Allocate and set initial values
      implicit none
      class(field), intent(inout) :: this
    end module

  end interface

end module

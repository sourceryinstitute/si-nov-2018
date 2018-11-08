submodule(spectral_field_interface) spectral_field_implementation
  use assertion_interface, only : assert, assertions

  implicit none

contains

    module function zero(this) result(zero_object)
      class(spectral_field), intent(in) :: this
      class(scalar_field), allocatable :: zero_object
      integer alloc_status
      integer, parameter :: success=0

      allocate(zero_object,mold=this,stat=alloc_status)
      if (assertions) call assert(alloc_status==success,"zero: allocation succeeded")

      select type(zero_object)
        class is(spectral_field)
          allocate(zero_object%f,mold=this%f)
          zero_object%f = 0.
        class default
          error stop "zero: unrecognized class"
      end select

    end function

    module function subtract(lhs,rhs) result(difference)
      class(spectral_field), intent(in) :: lhs
      class(scalar_field), intent(in) :: rhs
      class(scalar_field), allocatable :: difference
      integer alloc_status
      integer, parameter :: success=0

      call assert(same_type_as(lhs,rhs),"subtract: consistent operand types")

      allocate(difference,mold=lhs, stat=alloc_status)
      if (assertions) call assert(alloc_status==success,"zero: allocation succeeded")

      select type(difference)
        class is(spectral_field)
          select type(rhs)
             class is(spectral_field)
               call assert( all(shape(lhs%f)==shape(rhs%f)), "conformable operand array components")
               allocate(difference%f,mold=lhs%f)
               difference%f = lhs%f - rhs%f
             class default
                error stop "subtract: unrecognized rhs class"
           end select
         class default
           error stop "subtract: unrecognized difference class"
      end select
    end function

end submodule

program main
  !! author: Damian Rouson
  !!
  !! Demonstrate asynchronous communication of greetings created on multiple images
  use assertion_interface, only : assert
  use iso_fortran_env, only : event_type
  implicit none
  type(event_type), allocatable :: greeting_ready(:)[:]
  integer, parameter :: max_greeting_length=64, success=0
  character(len=max_greeting_length) :: greeting[*]
  integer alloc_stat

  associate(me=>this_image(),ni=>num_images())

    allocate(greeting_ready(2:ni)[*],stat=alloc_stat)
    call assert(alloc_stat==success,"allocation succeeded")

    if (me/=1) then
      write(greeting,*) "Hello from image",me,"of",ni
      event post( greeting_ready(me)[1] )
    else

      write(greeting,*) "Hello from image",me,"of",ni
      print *,greeting

      spin_query_work: block
        logical greeting_not_printed(2:ni)
        integer image, counter

        greeting_not_printed=.true.

        spin: do while(any(greeting_not_printed))
          query_work: do concurrent(image=2:ni)
            call event_query(greeting_ready(image),counter)
            select case(counter)
              case(0)
                !! nothing to do
              case(1)
                if (greeting_not_printed(image)) print *,greeting[image]
                greeting_not_printed(image) = .false.
              case default
                error stop "invalid event count"
            end select
          end do query_work
        end do spin

      end block spin_query_work
    end if

  end associate
end program

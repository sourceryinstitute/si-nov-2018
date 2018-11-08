submodule(assertion_interface) assertion_implementation
  implicit none

contains

    module procedure assert
      use object_interface, only : object
      integer, parameter :: max_digits=32, max_object_length=1024
      character(len=max_digits) my_image
      character(len=max_object_length) object_data
      character(len=:), allocatable :: error_message
      if (.not. assertion) then
          write(my_image,*) this_image()
          error_message = "Assertion '" // description // "' failed on image " // my_image
          if (present(diagnostic_data)) then
            select type(diagnostic_data)
              type is(character(len=*))
                 error_message = error_message // diagnostic_data
              class is(object)
                 write(object_data,*) diagnostic_data
                 error_message = error_message // "with diagnostic data " // object_data
              class default
                 error_message = error_message // "with unrecognized diagnostic_data type"
            end select
          end if
          error stop error_message
      end if
    end procedure

end submodule

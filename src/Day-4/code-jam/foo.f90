program main
  integer, allocatable :: a(:,:)
  integer, parameter :: b(*,*) = reshape([1,2,3,4],[2,2])
  allocate(a,mold=b)
end program

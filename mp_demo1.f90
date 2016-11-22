!> simple dense matrix solver Ax=b module. 
Module matrix_solver

   use MemoryProfiler

   implicit none
   
   public

   !> dense matrix 
   real(kind(1.d0)), allocatable :: A(:,:)
   
   !> unknown vector 
   real(kind(1.d0)), allocatable :: x(:)

   !> right-hand side vector 
   real(kind(1.d0)), allocatable :: b(:)

   !> permutation matrix 
   integer, allocatable :: P(:,:)
   
   !> this module memory profiler 
   type (mem_profiler_t) :: mp
   
   logical :: mp_empty = .true.
   

   interface init
      module procedure :: initialize_mod
   end interface 
   
   interface del 
      module procedure :: delete_mod
   end interface 

contains 
   

!> Initialize all module variables 
subroutine initialize_mod(n)
   
   !> problem size 
   integer, intent(in) :: n
   
   integer :: err
   
   
   ! make a new memory profiler instance
   ! only in the first call to this routine
   if (mp_empty) then 
      mp = mem_profiler("mp_matrix_solver.txt")
      mp_empty = .false.
   end if
   
   ! assign module & routine names 
   err = mp%add_module('matrix_solver')
   err = mp%add_routine("initialize_mod")
   
   ! push (add) all module allocatable variables 
   err = mp%add_array(var=A, d=[n,n], name="A")
   err = mp%add_array(x, n, "x")
   err = mp%add_array(b, n, "b")
   err = mp%add_array(P, [n,n], "P")

   return 
end subroutine initialize_mod


subroutine delete_mod() 

   integer :: err

   ! assign current routine     
   err = mp%add_routine("delete_mod")

   ! remove (del) all module allocatable variables
   err = mp%del_array(A,"A")
   err = mp%del_array(x,"x")
   err = mp%del_array(b,"b")
   err = mp%del_array(P,"P")

   return 
end subroutine delete_mod
   

end module matrix_solver
   
   
!> simple toy example of the memory profiler module 
program mp_demo1

   use matrix_solver
   
   implicit none 
   
   integer, dimension(6), parameter :: n = [10, 100, 500, 1000, 5000, 10000]
   integer :: i
   
   
   do i = 1, size(n)
      call init(n(i))
      call del()
   end do
   
   print *, "number of allocations   in matrix_solver module = ", mp%get_n_alloc()
   print *, "number of deallocations in matrix_solver module = ", mp%get_n_dealloc()


end program mp_demo1
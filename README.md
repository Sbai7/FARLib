# FARLib
This is an Array collection library which is designed according to modern Fortran 2003+ standards. It supports: 

* In-place transparent redimensioning of multidimensional arrays.

* Simple memory profiling of multidimensional allocatable arrays.  

We support integer, real, double, complex, and logical arrays up to rank 4. 

In this second release, available Fortran modules are:

* ResizableArray which simply increases or shrinks an allocatable array size at runtime. The only exposed subroutine call by this module is '*reallocate*' which is type and rank independent.

* MemoryProfiler which helps in tracing and debugging memory leaks from allocation/deallocation statements in a large Fortran based code project. 

## How to use ResizableArray module in your code

* Include the following statement in the body of your main program, function, subroutine, or module:
```
use ResizableArray
``` 

* For a one-dimensional array '*A*' make the following call where '*new_size*' is the target new array size. When the latter is higher than the actual array size it's going to grow. Otherwise, the array size shrinks. 
```
call reallocate(A,new_size)
``` 

* For a two-dimensional array '*B*' we should do: 
```
call reallocate(B,new_isize,new_jsize)
``` 

Hence, additional arguments are obviously expected for higher rank arrays.  

### Examples 

Compile and link the provided demonstration example in '*demo1.f90*' using the following command (i.e. with gfortran):
```
gfortran ResizableArray.f90 demo1.f90 -o demo1
``` 

The first example shows how to use the same resizable array, '*int_ar*', to calculate a simple geometric sum of increasing and then decreasing way as shown in the following code snippet:
```
   ! allocate and fill int_ar 
   if (.not.allocated(int_ar)) allocate(int_ar(10)) 
   old_size = size(int_ar)
   do i = 1, size(int_ar)
      int_ar(i) = i
   end do
   write (*,'(5x,a11,i4,a4,i5)') "Sum of 1 to",old_size,"is:", sum(int_ar)
   
   ! increase ar_size by another 10 elements 9 times 
   do it = 1, 9
      new_size = old_size + 10
      call reallocate(int_ar,new_size)
      do i = old_size+1, new_size
         int_ar(i) = i
      end do
      write (*,'(5x,a11,i4,a4,i5)') "Sum of 1 to",new_size,"is:", sum(int_ar)
      old_size = new_size
   end do
   .
   .
   .
   ! then decrease ar_size by 25 elements 4 times 
   do it = 1, 4
      new_size = old_size - 25
      call reallocate(int_ar,new_size)
      write (*,'(5x,a11,i4,a4,i5)') "Sum of 1 to",new_size,"is:", sum(int_ar)
      old_size = new_size
   end do
   write (*,*)    
```
this produces the following output:
```
     Sum of 1 to  10 is:   55
     Sum of 1 to  20 is:  210
     Sum of 1 to  30 is:  465
     Sum of 1 to  40 is:  820
     Sum of 1 to  50 is: 1275
     Sum of 1 to  60 is: 1830
     Sum of 1 to  70 is: 2485
     Sum of 1 to  80 is: 3240
     Sum of 1 to  90 is: 4095
     Sum of 1 to 100 is: 5050
 ...
     Sum of 1 to  75 is: 2850
     Sum of 1 to  50 is: 1275
     Sum of 1 to  25 is:  325
     Sum of 1 to   0 is:    0
```

Next, the second example just fills a dense matrix, A, and reposition this one into a higher rank matrix, A', such that:
```
   
                A  |  0
         A' = -----------
                0  |  I
```
This is easily done with the following code snippet:
```
   .
   .
   .
   new_isize = old_isize + 5
   new_jsize = old_jsize + 5
   
   call reallocate(matrix,new_isize,new_jsize) 
   
   matrix(6:10,1:10) = 0.d0
   matrix(1:10,6:10) = 0.d0
   
   do i = 1, 5
      matrix(old_isize+i,old_jsize+i) = 1.d0
   end do 
```
Here is a printout of the matrix before and after this transformation:
```
       3.5  1.3  7.9  0.0  4.3
       0.7  3.3  0.0  1.5  0.0
 A =   2.5  0.0  3.9  0.0  4.5
       0.0  2.1  0.0  9.0  0.0
       0.0  0.0  8.7  7.1  5.5

       3.5  1.3  7.9  0.0  4.3  0.0  0.0  0.0  0.0  0.0
       0.7  3.3  0.0  1.5  0.0  0.0  0.0  0.0  0.0  0.0
       2.5  0.0  3.9  0.0  4.5  0.0  0.0  0.0  0.0  0.0
       0.0  2.1  0.0  9.0  0.0  0.0  0.0  0.0  0.0  0.0
       0.0  0.0  8.7  7.1  5.5  0.0  0.0  0.0  0.0  0.0
 A'=   0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0
       0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0
       0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0
       0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0
       0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0
```

## How to use MemoryProfiler module in your code

* Include the following statement at the top of the module you want to profile:
```
use MemoryProfiler 
```

* Create a new variable of type '*mem_profiler_t*' as a global variable in your module: 
```
type (mem_profiler_t) :: mp  
```

* You need to initialize it (only once) somewhere in you module. This could be, perhaps, your module/class initializer or constructor. The supplied argument to '*mem_profiler*' function is the file where all module's allocation/deallocation calls are monitored:
```
mp = mem_profiler("mp_some_module.txt")  
```

* Next, inform which module is being monitored. This has to be called only once: 
```
err = mp%add_module('module_name')  
``` 

* At the top of each module subroutine/function we want to monitor, place a statement like this:
```
err = mp%add_routine("routine_name")  
``` 

* Instead of calling the '*allocate*' statement to reserve memory for an array A let's say of rank 2, replace it with the following statement, where '*d*' holds the desired array size along each dimension. This call has the double effect of allocating the array plus monitoring its memory status:  
```
err = mp%add_array(var=A, d=[n,n], name="A") 
``` 

* Likewise, instead of calling the '*deallocate*' statement to free memory reserved by array A, replace it with the following statement: 
```
err = mp%del_array(A,"A") 
``` 

* Once you finish with it you can reset the memory profiling variable: 
```
call mp%reset() 
``` 

For realistic fortran based codes, a single memory profiler variable could be placed in each module if the number of modules to monitor for memory leaks are small enough. Otherwise, for large projects one program-wide memory profiler container could be used to monitor the overall program allocation/deallocation behavior. 

### Examples 

Compile and link the provided demonstration example in '*mp_demo1.f90*' using the following command (i.e. with gfortran):
```
gfortran MemoryProfiler.f90 mp_demo1.f90 -o mp_demo1
``` 

This example shows for a simple demonstration case how to profile a module's allocatable arrays through an internal '*mem_profiler_t*' variable: 
```
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
```

Next, a toy example program using the last module with built-in memory tracing capability is constructed:   
```
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
```

The first section of the output file '*mp_matrix_solver.txt*' is shown below. It monitors the sequence of allocations (+ sign in first column) and deallocations (- sign in first column) and where they occur. Hence, this straightforwardly accelerates detection of memory leaks in large fortran based software projects. 
```
 Mode                                   Array               Subroutine                   Module
 ----                                --------               ----------                   ------
    +                                       A           initialize_mod            matrix_solver
    +                                       x           initialize_mod            matrix_solver
    +                                       b           initialize_mod            matrix_solver
    +                                       P           initialize_mod            matrix_solver
    -                                       A               delete_mod            matrix_solver
    -                                       x               delete_mod            matrix_solver
    -                                       b               delete_mod            matrix_solver
    -                                       P               delete_mod            matrix_solver
.
.
.
```

## Plan for next releases 

* Extend '*mem_profiler_t*' type to detect other gotchas (such as used memory, etc).
* Design extended types of '*mem_profiler_t*' to perform more sophisticated memory tracing capabilities.
* New module for dynamic array manipulation (aka Fortran 2003+ equivalent to std::vector in C++ STL).
* Special modules for dynamic vectors, dense and sparse matrices.
* Design patterns for Resizable arrays of derived types.

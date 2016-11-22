# FARLib
This is an Array collection library which is designed according to modern Fortran 2003+ standards. It supports: 
* in-place transparent redimensioning of multidimensional arrays,
* ...  
up to 4th rank, of integer, real, double, complex, and logical types. 

In this first release, the only Fortran module available is ResizableArray which simply increases or shrinks an allocatable array size at runtime. The only available subroutine call is '*reallocate*' which is type and rank independent. 

## How to use ResizableArray module in your code

- Include '*use ResizableArray*' statement in the body of your main program, function, subroutine, or module. 
- '*call reallocate(ar,new_size)*' it's all what's needed to resize a previously filled one-dimensional array '*ar*'. Obviously, when '*new_size*' is higher than the actual array size it's going to grow. Otherwise, the array shrinks. Additional arguments are needed when manipulating higher rank arrays. 

## Examples 
- Compile and link the provided demonstration example in '*demo1.f90*' using the following command (i.e. with gfortran)
```
gfortran ResizableArray.f90 demo1.f90 -o demo1
``` 
The first example shows how to use the same resizable array, '*int_ar*', to calculate a simple geometric sum of increasing and then decreasing way as exemplified in the following code snippet:
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
Which produces the following output:
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

## Plan for next releases 

- New module for dynamic array manipulation (aka Fortran 2003+ equivalent to std::vector in C++ STL).
- Special modules for dynamic vectors, dense and sparse matrices.
- Design patterns for Resizable arrays of derived types.

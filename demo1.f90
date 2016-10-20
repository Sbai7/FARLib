!******************************************************************************
!*
!*    FAR-LIB: Fortran Array collection library.
!*    Copyright (C) 2016  by M. A. Sbai 
!*
!*    This program is free software: you can redistribute it and/or modify
!*    it under the terms of the GNU General Public License as published by
!*    the Free Software Foundation, either version 3 of the License, or
!*    any later version.
!*
!*    This program is distributed in the hope that it will be useful,
!*    but WITHOUT ANY WARRANTY; without even the implied warranty of
!*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!*    GNU General Public License for more details.
!*
!*    You should have received a copy of the GNU General Public License
!*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!*
!*    Redistribution and use in source and binary  forms, with or without
!*    modification, are permitted provided that the following conditions are met:
!*
!*  - Redistributions of  source code must retain the above copyright notice,
!*    this list of conditions and the disclaimer below.
!*
!*  - Redistributions in binary form must reproduce the above copyright notice,
!*    this list of  conditions and the disclaimer (as noted below) in the
!*    documentation and/or other materials provided with the distribution.
!*
!******************************************************************************

program demo1
! Simple demonstration of FAR-LIB's ResizableArray module 

   use ResizableArray 
   
   implicit none 

   integer(kind=4) i,j,it                  ! iteration indices 
   integer(kind=4) old_size, new_size      ! arrays size counters
   integer(kind=4) old_isize, new_isize    ! arrays size counters
   integer(kind=4) old_jsize, new_jsize    ! arrays size counters
   
   ! allocatable integer array
   integer, allocatable, dimension(:) :: int_ar 
   
   ! dense matrix
   real(kind=8), allocatable, dimension(:,:) :: matrix 
   
   
   !********** Example 1: calculate a simple geometric sum 
   
   write (*,'(30x,a)') "###########################################"
   write (*,'(30x,a)') "Example 1: calculate a simple geometric sum"
   write (*,'(30x,a)') "###########################################" 
   write (*,*)   

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
   
   write (*,*) "..."
   
   ! then decrease ar_size by 25 elements 4 times 
   do it = 1, 4
      new_size = old_size - 25
      call reallocate(int_ar,new_size)
      write (*,'(5x,a11,i4,a4,i5)') "Sum of 1 to",new_size,"is:", sum(int_ar)
      old_size = new_size
   end do
   write (*,*) 
   
   ! deallocate int_ar 
   if (allocated(int_ar)) deallocate(int_ar) 


   !********** Example 2: manipulation of a dense matrix 
   
   write (*,'(30x,a)') "#########################################"
   write (*,'(30x,a)') "Example 2: manipulation of a dense matrix"
   write (*,'(30x,a)') "#########################################"   
   write (*,*) 

   ! memory allocation & initialization
   if (.not.allocated(matrix)) allocate(matrix(5,5)) 
   
   old_isize = ubound(matrix,1)
   old_jsize = ubound(matrix,2)
   
   matrix = 0.d0 
  
   ! fill matrix 
   !               1     2     3     4     5
   !             ----------------------------
   !              3.5   1.3   7.9   0.0   4.3 | 1 
   !              0.7   3.3   0.0   1.5   0.0 | 2
   !      A =     2.5   0.0   3.9   0.0   4.5 | 3
   !              0.0   2.1   0.0   9.0   0.0 | 4
   !              0.0   0.0   8.7   7.1   5.5 | 5
   matrix(1,1) = 3.5d0            
   matrix(1,2) = 1.3d0            
   matrix(1,3) = 7.9d0            
   matrix(1,5) = 4.3d0            
   matrix(2,1) = 0.7d0            
   matrix(2,2) = 3.3d0            
   matrix(2,4) = 1.5d0            
   matrix(3,1) = 2.5d0            
   matrix(3,3) = 3.9d0            
   matrix(3,5) = 4.5d0            
   matrix(4,2) = 2.1d0            
   matrix(4,4) = 9.0d0            
   matrix(5,3) = 8.7d0            
   matrix(5,4) = 7.1d0            
   matrix(5,5) = 5.5d0  
   
   ! print matrix 
   do i = 1, old_isize 
      if (i==3) then 
         write (*,'(a5)',advance='no') " A = "
         write (*,'(5f5.1)') (matrix(i,j), j=1,old_jsize) 
      else 
         write (*,'(5x,5f5.1)') (matrix(i,j), j=1,old_jsize) 
      end if         
   end do 
   write (*,*) 
   
   ! matrix A becomes A' where 
   !
   !             A  |  0
   !      A' = -----------
   !             0  |  I
   ! 
   ! This manipulation could be simply done as follows 
   
   new_isize = old_isize + 5
   new_jsize = old_jsize + 5
   
   call reallocate(matrix,new_isize,new_jsize) 
   
   matrix(6:10,1:10) = 0.d0
   matrix(1:10,6:10) = 0.d0
   
   do i = 1, 5
      matrix(old_isize+i,old_jsize+i) = 1.d0
   end do 
   
   ! print new matrix 
   do i = 1, new_isize 
      if (i==6) then 
         write (*,'(a5)',advance='no') " A'= "
         write (*,'(10f5.1)') (matrix(i,j), j=1,new_jsize) 
      else 
         write (*,'(5x,10f5.1)') (matrix(i,j), j=1,new_jsize) 
      end if         
   end do 
   write (*,*) 

   !
   if (allocated(matrix)) deallocate(matrix) 

end program demo1
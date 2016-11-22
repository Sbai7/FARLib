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

module MemoryProfiler

implicit none 

private

type, public :: mem_profiler_t
   
   !> name of last entered fortran module 
   character(:), allocatable :: last_module  
   
   !> name of the last caller subroutine/function
   character(:), allocatable :: last_routine

   !> name of last allocated/deallocated array 
   character(:), allocatable :: last_array
   
   !> true if the array will be allocated, false otherwise
   logical :: to_alloc 

   !> log file 
   character(:), allocatable :: flog
   
   !> logging logical file unit
   integer :: funit
   
   !> true if logging is allowed, false otherwise 
   logical :: is_log
   
   !> number of allocations in object's life 
   integer :: n_alloc 
   
   !> number of dealloctions in object's life 
   integer :: n_dealloc 
   
contains 

   procedure, public :: add_module 
   procedure, public :: add_routine
   procedure, public :: get_n_alloc 
   procedure, public :: get_n_dealloc
   procedure, public :: reset
   procedure :: add_array_int_1d
   procedure :: add_array_int_2d
   procedure :: add_array_int_3d
   procedure :: add_array_int_4d
   procedure :: add_array_real_1d
   procedure :: add_array_real_2d
   procedure :: add_array_real_3d
   procedure :: add_array_real_4d
   procedure :: add_array_double_1d
   procedure :: add_array_double_2d
   procedure :: add_array_double_3d
   procedure :: add_array_double_4d
   procedure :: add_array_complex_1d
   procedure :: add_array_complex_2d
   procedure :: add_array_complex_3d
   procedure :: add_array_complex_4d
   procedure :: add_array_dcomplex_1d
   procedure :: add_array_dcomplex_2d
   procedure :: add_array_dcomplex_3d
   procedure :: add_array_dcomplex_4d 
   procedure :: add_array_logical_1d
   procedure :: add_array_logical_2d
   procedure :: add_array_logical_3d
   procedure :: add_array_logical_4d 
   procedure :: del_array_int_1d
   procedure :: del_array_int_2d
   procedure :: del_array_int_3d
   procedure :: del_array_int_4d
   procedure :: del_array_real_1d
   procedure :: del_array_real_2d
   procedure :: del_array_real_3d
   procedure :: del_array_real_4d
   procedure :: del_array_double_1d
   procedure :: del_array_double_2d
   procedure :: del_array_double_3d
   procedure :: del_array_double_4d
   procedure :: del_array_complex_1d
   procedure :: del_array_complex_2d
   procedure :: del_array_complex_3d
   procedure :: del_array_complex_4d
   procedure :: del_array_dcomplex_1d
   procedure :: del_array_dcomplex_2d
   procedure :: del_array_dcomplex_3d
   procedure :: del_array_dcomplex_4d 
   procedure :: del_array_logical_1d
   procedure :: del_array_logical_2d
   procedure :: del_array_logical_3d
   procedure :: del_array_logical_4d 

   !> type destructor 
   final :: delete_mem_profiler 
   
   generic, public :: add_array =>  add_array_int_1d,       &
                                    add_array_int_2d,       &
                                    add_array_int_3d,       &
                                    add_array_int_4d,       &
                                    add_array_real_1d,      &
                                    add_array_real_2d,      &
                                    add_array_real_3d,      &
                                    add_array_real_4d,      &
                                    add_array_double_1d,    &
                                    add_array_double_2d,    &
                                    add_array_double_3d,    &
                                    add_array_double_4d,    &
                                    add_array_complex_1d,   &
                                    add_array_complex_2d,   &
                                    add_array_complex_3d,   &
                                    add_array_complex_4d,   &
                                    add_array_dcomplex_1d,  &
                                    add_array_dcomplex_2d,  &
                                    add_array_dcomplex_3d,  &
                                    add_array_dcomplex_4d,  &
                                    add_array_logical_1d,   &
                                    add_array_logical_2d,   &
                                    add_array_logical_3d,   &
                                    add_array_logical_4d 

   generic, public :: del_array =>  del_array_int_1d,       &
                                    del_array_int_2d,       &
                                    del_array_int_3d,       &
                                    del_array_int_4d,       &
                                    del_array_real_1d,      &
                                    del_array_real_2d,      &
                                    del_array_real_3d,      &
                                    del_array_real_4d,      &
                                    del_array_double_1d,    &
                                    del_array_double_2d,    &
                                    del_array_double_3d,    &
                                    del_array_double_4d,    &
                                    del_array_complex_1d,   &
                                    del_array_complex_2d,   &
                                    del_array_complex_3d,   &
                                    del_array_complex_4d,   &
                                    del_array_dcomplex_1d,  &
                                    del_array_dcomplex_2d,  &
                                    del_array_dcomplex_3d,  &
                                    del_array_dcomplex_4d,  &
                                    del_array_logical_1d,   &
                                    del_array_logical_2d,   &
                                    del_array_logical_3d,   &
                                    del_array_logical_4d

end type mem_profiler_t


!> type constructor
interface mem_profiler
   module procedure :: new_mem_profiler 
end interface

! array allocation helpers 
interface allocate_array
   module procedure :: allocate_int_1d
   module procedure :: allocate_int_2d
   module procedure :: allocate_int_3d
   module procedure :: allocate_int_4d
   module procedure :: allocate_real_1d 
   module procedure :: allocate_real_2d 
   module procedure :: allocate_real_3d 
   module procedure :: allocate_real_4d 
   module procedure :: allocate_double_1d 
   module procedure :: allocate_double_2d 
   module procedure :: allocate_double_3d 
   module procedure :: allocate_double_4d 
   module procedure :: allocate_complex_1d 
   module procedure :: allocate_complex_2d 
   module procedure :: allocate_complex_3d 
   module procedure :: allocate_complex_4d 
   module procedure :: allocate_dcomplex_1d
   module procedure :: allocate_dcomplex_2d
   module procedure :: allocate_dcomplex_3d
   module procedure :: allocate_dcomplex_4d
   module procedure :: allocate_logical_1d
   module procedure :: allocate_logical_2d
   module procedure :: allocate_logical_3d
   module procedure :: allocate_logical_4d
end interface

! array deallocation helpers 
interface deallocate_array 
   module procedure :: deallocate_int_1d
   module procedure :: deallocate_int_2d
   module procedure :: deallocate_int_3d
   module procedure :: deallocate_int_4d
   module procedure :: deallocate_real_1d 
   module procedure :: deallocate_real_2d 
   module procedure :: deallocate_real_3d 
   module procedure :: deallocate_real_4d 
   module procedure :: deallocate_double_1d 
   module procedure :: deallocate_double_2d 
   module procedure :: deallocate_double_3d 
   module procedure :: deallocate_double_4d 
   module procedure :: deallocate_complex_1d 
   module procedure :: deallocate_complex_2d 
   module procedure :: deallocate_complex_3d 
   module procedure :: deallocate_complex_4d 
   module procedure :: deallocate_dcomplex_1d
   module procedure :: deallocate_dcomplex_2d
   module procedure :: deallocate_dcomplex_3d
   module procedure :: deallocate_dcomplex_4d
   module procedure :: deallocate_logical_1d
   module procedure :: deallocate_logical_2d
   module procedure :: deallocate_logical_3d
   module procedure :: deallocate_logical_4d
end interface

public :: mem_profiler, allocate_array, deallocate_array

contains


!> Initialize a memory profiler object 
type(mem_profiler_t) function new_mem_profiler(log_file) result(mp)

   !> name of file logger 
   character(len=*), intent(in), optional :: log_file
   
   logical :: opened
   integer :: free
   
   
   if (present(log_file)) then 
      
      mp%is_log = .true.
      mp%flog   = log_file
      
      ! search for a 'free' io file unit
      opened = .true. 
      free   = 9
      do while (opened) 
         free = free + 1
         inquire (free, opened=opened) 
      end do
      mp%funit = free 

      open (unit=mp%funit, file=mp%flog, status='unknown')
         
      write (mp%funit, 10) "Mode", "Array", "Subroutine", "Module"
      write( mp%funit, 10) "----", "--------", "----------", "------"
   
   else
      mp%is_log = .false.
      
   end if
   
   mp%n_alloc   = 0 
   mp%n_dealloc = 0

   mp%last_routine  = ""
   mp%last_array    = ""
   mp%last_module   = ""
   
10 FORMAT(A5,A40,A25,A25)

end function new_mem_profiler
   
   
!> Reset the components of memory profiler type. 
elemental subroutine reset(this)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this

   
   this%last_module   = ""
   this% last_routine = ""
   this%last_array    = ""
   this%flog          = ""
   
   this%funit     = 0
   this%n_alloc   = 0
   this%n_dealloc = 0
   
   return
end subroutine reset
   
   
!> Automatic destructor 
elemental subroutine delete_mem_profiler(this)

   !> memory profiler object 
   type (mem_profiler_t), intent(inout) :: this

   
   call this%reset()

   return 
end subroutine delete_mem_profiler
   
   
!> Return number of successful allocations.
integer function get_n_alloc(this) result(nalloc)

   !> memory profiler object 
   class (mem_profiler_t), intent(in) :: this 
   
   nalloc = this%n_alloc 
   
   return 
end function get_n_alloc
   
   
!> Return number of successful deallocations.
integer function get_n_dealloc(this) result(ndealloc)

   !> memory profiler object 
   class (mem_profiler_t), intent(in) :: this 
   
   ndealloc = this%n_dealloc 
   
   return 
end function get_n_dealloc
   
   
!> Sets the last entered module by this 
integer function add_module(this, name) result(ok) 

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> last module name 
   character(len=*), intent(in) :: name

   
   this%last_module = name
   ok = 0

   return
end function add_module 


!> Sets the last entered routine by this 
integer function add_routine(this, name) result(ok)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> last routine name 
   character(len=*), intent(in) :: name

   
   this%last_routine = name
   ok = 0

   return
end function add_routine 


!> Sets the last 1D integer array 
integer function add_array_int_1d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   integer, allocatable, intent(inout) :: var(:)
   
   !> array dimensions
   integer, intent(in) :: d
   
   !> last array name 
   character(len=*), intent(in) :: name
      
   
   include "mp_add_array_core.fi"
   
   return
end function add_array_int_1d 
   

!> Sets the last 2D integer array 
integer function add_array_int_2d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   integer, allocatable, intent(inout) :: var(:,:)
   
   !> array dimensions
   integer, intent(in) :: d(2)
   
   !> last array name 
   character(len=*), intent(in) :: name
   
   
   include "mp_add_array_core.fi"
   
   return
end function add_array_int_2d 
   

!> Sets the last 3D integer array 
integer function add_array_int_3d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   integer, allocatable, intent(inout) :: var(:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(3)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_int_3d 
   

!> Sets the last 4D integer array 
integer function add_array_int_4d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   integer, allocatable, intent(inout) :: var(:,:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(4)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_int_4d 
   


!> Sets the last 1D real array 
integer function add_array_real_1d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.0)), allocatable, intent(inout) :: var(:)
   
   !> array dimensions
   integer, intent(in) :: d
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_real_1d 
   

!> Sets the last 2D real array 
integer function add_array_real_2d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:)
   
   !> array dimensions
   integer, intent(in) :: d(2)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_real_2d 
   

!> Sets the last 3D real array 
integer function add_array_real_3d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(3)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_real_3d 
   

!> Sets the last 4D real array 
integer function add_array_real_4d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(4)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_real_4d 
   


!> Sets the last 1D double array 
integer function add_array_double_1d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.d0)), allocatable, intent(inout) :: var(:)
   
   !> array dimensions
   integer, intent(in) :: d
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_double_1d 
   

!> Sets the last 2D double array 
integer function add_array_double_2d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:)
   
   !> array dimensions
   integer, intent(in) :: d(2)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_double_2d 
   

!> Sets the last 3D double array 
integer function add_array_double_3d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(3)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_double_3d 
   

!> Sets the last 4D double array 
integer function add_array_double_4d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(4)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_double_4d 
   


!> Sets the last 1D complex array 
integer function add_array_complex_1d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex, allocatable, intent(inout) :: var(:)
   
   !> array dimensions
   integer, intent(in) :: d
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_complex_1d 
   

!> Sets the last 2D complex array 
integer function add_array_complex_2d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex, allocatable, intent(inout) :: var(:,:)
   
   !> array dimensions
   integer, intent(in) :: d(2)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_complex_2d 
   

!> Sets the last 3D complex array 
integer function add_array_complex_3d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex, allocatable, intent(inout) :: var(:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(3)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_complex_3d 
   

!> Sets the last 4D complex array 
integer function add_array_complex_4d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex, allocatable, intent(inout) :: var(:,:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(4)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_complex_4d 
   


!> Sets the last 1D double complex array 
integer function add_array_dcomplex_1d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex*16, allocatable, intent(inout) :: var(:)
   
   !> array dimensions
   integer, intent(in) :: d
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_dcomplex_1d 
   

!> Sets the last 2D double complex array 
integer function add_array_dcomplex_2d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex*16, allocatable, intent(inout) :: var(:,:)
   
   !> array dimensions
   integer, intent(in) :: d(2)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_dcomplex_2d 
   

!> Sets the last 3D double complex array 
integer function add_array_dcomplex_3d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex*16, allocatable, intent(inout) :: var(:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(3)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_dcomplex_3d 
   

!> Sets the last 4D double complex array 
integer function add_array_dcomplex_4d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex*16, allocatable, intent(inout) :: var(:,:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(4)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_dcomplex_4d 
   


!> Sets the last 1D logical array 
integer function add_array_logical_1d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   logical, allocatable, intent(inout) :: var(:)
   
   !> array dimensions
   integer, intent(in) :: d
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_logical_1d 
   

!> Sets the last 2D logical array 
integer function add_array_logical_2d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   logical, allocatable, intent(inout) :: var(:,:)
   
   !> array dimensions
   integer, intent(in) :: d(2)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_logical_2d 
   

!> Sets the last 3D logical array 
integer function add_array_logical_3d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   logical, allocatable, intent(inout) :: var(:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(3)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_logical_3d 
   

!> Sets the last 4D logical array 
integer function add_array_logical_4d(this, var, d, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   logical, allocatable, intent(inout) :: var(:,:,:,:)
   
   !> array dimensions
   integer, intent(in) :: d(4)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_add_array_core.fi"
   
   return
end function add_array_logical_4d 
   


!> Removes the last 1D integer array 
integer function del_array_int_1d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   integer, allocatable, intent(inout) :: var(:)
   
   !> last array name 
   character(len=*), intent(in) :: name
      
   
   include "mp_del_array_core.fi"
   
   return
end function del_array_int_1d 
   

!> Removes the last 2D integer array 
integer function del_array_int_2d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   integer, allocatable, intent(inout) :: var(:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name
   
   
   include "mp_del_array_core.fi"
   
   return
end function del_array_int_2d 
   

!> Removes the last 3D integer array 
integer function del_array_int_3d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   integer, allocatable, intent(inout) :: var(:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_int_3d 
   

!> Removes the last 4D integer array 
integer function del_array_int_4d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   integer, allocatable, intent(inout) :: var(:,:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_int_4d 
   


!> Removes the last 1D real array 
integer function del_array_real_1d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.0)), allocatable, intent(inout) :: var(:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_real_1d 
   

!> Removes the last 2D real array 
integer function del_array_real_2d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_real_2d 
   

!> Removes the last 3D real array 
integer function del_array_real_3d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_real_3d 
   

!> Removes the last 4D real array 
integer function del_array_real_4d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_real_4d 
   


!> Removes the last 1D double array 
integer function del_array_double_1d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.d0)), allocatable, intent(inout) :: var(:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_double_1d 
   

!> Removes the last 2D double array 
integer function del_array_double_2d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_double_2d 
   

!> Removes the last 3D double array 
integer function del_array_double_3d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_double_3d 
   

!> Removes the last 4D double array 
integer function del_array_double_4d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_double_4d 
   


!> Removes the last 1D complex array 
integer function del_array_complex_1d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex, allocatable, intent(inout) :: var(:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_complex_1d 
   

!> Removes the last 2D complex array 
integer function del_array_complex_2d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex, allocatable, intent(inout) :: var(:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_complex_2d 
   

!> Removes the last 3D complex array 
integer function del_array_complex_3d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex, allocatable, intent(inout) :: var(:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_complex_3d 
   

!> Removes the last 4D complex array 
integer function del_array_complex_4d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex, allocatable, intent(inout) :: var(:,:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_complex_4d 
   


!> Removes the last 1D double complex array 
integer function del_array_dcomplex_1d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex*16, allocatable, intent(inout) :: var(:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_dcomplex_1d 
   

!> Removes the last 2D double complex array 
integer function del_array_dcomplex_2d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex*16, allocatable, intent(inout) :: var(:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_dcomplex_2d 
   

!> Removes the last 3D double complex array 
integer function del_array_dcomplex_3d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex*16, allocatable, intent(inout) :: var(:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_dcomplex_3d 
   

!> Removes the last 4D double complex array 
integer function del_array_dcomplex_4d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   complex*16, allocatable, intent(inout) :: var(:,:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_dcomplex_4d 
   


!> Removes the last 1D logical array 
integer function del_array_logical_1d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   logical, allocatable, intent(inout) :: var(:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_logical_1d 
   

!> Removes the last 2D logical array 
integer function del_array_logical_2d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   logical, allocatable, intent(inout) :: var(:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_logical_2d 
   

!> Removes the last 3D logical array 
integer function del_array_logical_3d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   logical, allocatable, intent(inout) :: var(:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_logical_3d 
   

!> Removes the last 4D logical array 
integer function del_array_logical_4d(this, var, name) result(err)

   !> memory profiler object 
   class (mem_profiler_t), intent(inout) :: this
   
   !> the array to add
   logical, allocatable, intent(inout) :: var(:,:,:,:)
   
   !> last array name 
   character(len=*), intent(in) :: name

   
   include "mp_del_array_core.fi"
   
   return
end function del_array_logical_4d 
   


!> Allocation of a 1D integer array.
integer function allocate_int_1d(var, d) result(ret) 

   !> array to be allocated 
   integer, allocatable, intent(inout) :: var(:) 
   
   !> array dimensions
   integer, intent(in) :: d
      
   
   if (.not.allocated(var)) allocate ( var(d), stat=ret )   

   return 
end function allocate_int_1d
   
   
!> Allocation of a 2D integer array.
integer function allocate_int_2d(var, d) result(ret) 

   !> array to be allocated 
   integer, allocatable, intent(inout) :: var(:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(2)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2)), stat=ret )   

   return 
end function allocate_int_2d
   
   
!> Allocation of a 3D integer array.
integer function allocate_int_3d(var, d) result(ret) 

   !> array to be allocated 
   integer, allocatable, intent(inout) :: var(:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(3)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3)), stat=ret )   

   return 
end function allocate_int_3d
   
   
!> Allocation of a 4D integer array.
integer function allocate_int_4d(var, d) result(ret) 

   !> array to be allocated 
   integer, allocatable, intent(inout) :: var(:,:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(4)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3),d(4)), stat=ret )   

   return 
end function allocate_int_4d
   

   
!> Allocation of a 1D real array.
integer function allocate_real_1d(var, d) result(ret) 

   !> array to be allocated 
   real(kind(1.0)), allocatable, intent(inout) :: var(:) 
   
   !> array dimensions
   integer, intent(in) :: d
      
   
   if (.not.allocated(var)) allocate ( var(d), stat=ret )   

   return 
end function allocate_real_1d
   
   
!> Allocation of a 2D real array.
integer function allocate_real_2d(var, d) result(ret) 

   !> array to be allocated 
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(2)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2)), stat=ret )   

   return 
end function allocate_real_2d
   
   
!> Allocation of a 3D real array.
integer function allocate_real_3d(var, d) result(ret) 

   !> array to be allocated 
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(3)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3)), stat=ret )   

   return 
end function allocate_real_3d
   
   
!> Allocation of a 4D real array.
integer function allocate_real_4d(var, d) result(ret) 

   !> array to be allocated 
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(4)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3),d(4)), stat=ret )   

   return 
end function allocate_real_4d
   
   
   
!> Allocation of a 1D double array.
integer function allocate_double_1d(var, d) result(ret) 

   !> array to be allocated 
   real(kind(1.d0)), allocatable, intent(inout) :: var(:) 
   
   !> array dimensions
   integer, intent(in) :: d
      
   
   if (.not.allocated(var)) allocate ( var(d), stat=ret )   

   return 
end function allocate_double_1d
   
   
!> Allocation of a 2D double array.
integer function allocate_double_2d(var, d) result(ret) 

   !> array to be allocated 
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(2)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2)), stat=ret )   

   return 
end function allocate_double_2d
   
   
!> Allocation of a 3D double array.
integer function allocate_double_3d(var, d) result(ret) 

   !> array to be allocated 
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(3)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3)), stat=ret )   

   return 
end function allocate_double_3d
   
   
!> Allocation of a 4D double array.
integer function allocate_double_4d(var, d) result(ret) 

   !> array to be allocated 
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(4)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3),d(4)), stat=ret )   

   return 
end function allocate_double_4d
   
   
   
!> Allocation of a 1D complex array.
integer function allocate_complex_1d(var, d) result(ret) 

   !> array to be allocated 
   complex, allocatable, intent(inout) :: var(:) 
   
   !> array dimensions
   integer, intent(in) :: d
      
   
   if (.not.allocated(var)) allocate ( var(d), stat=ret )   

   return 
end function allocate_complex_1d
   
   
!> Allocation of a 2D complex array.
integer function allocate_complex_2d(var, d) result(ret) 

   !> array to be allocated 
   complex, allocatable, intent(inout) :: var(:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(2)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2)), stat=ret )   

   return 
end function allocate_complex_2d
   
   
!> Allocation of a 3D complex array.
integer function allocate_complex_3d(var, d) result(ret) 

   !> array to be allocated 
   complex, allocatable, intent(inout) :: var(:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(3)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3)), stat=ret )   

   return 
end function allocate_complex_3d
   
   
!> Allocation of a 4D complex array.
integer function allocate_complex_4d(var, d) result(ret) 

   !> array to be allocated 
   complex, allocatable, intent(inout) :: var(:,:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(4)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3),d(4)), stat=ret )   

   return 
end function allocate_complex_4d
   
   
   
!> Allocation of a 1D double complex array.
integer function allocate_dcomplex_1d(var, d) result(ret) 

   !> array to be allocated 
   complex*16, allocatable, intent(inout) :: var(:) 
   
   !> array dimensions
   integer, intent(in) :: d
      
   
   if (.not.allocated(var)) allocate ( var(d), stat=ret )   

   return 
end function allocate_dcomplex_1d
   
   
!> Allocation of a 2D double complex array.
integer function allocate_dcomplex_2d(var, d) result(ret) 

   !> array to be allocated 
   complex*16, allocatable, intent(inout) :: var(:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(2)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2)), stat=ret )   

   return 
end function allocate_dcomplex_2d
   
   
!> Allocation of a 3D double complex array.
integer function allocate_dcomplex_3d(var, d) result(ret) 

   !> array to be allocated 
   complex*16, allocatable, intent(inout) :: var(:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(3)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3)), stat=ret )   

   return 
end function allocate_dcomplex_3d
   
   
!> Allocation of a 4D double complex array.
integer function allocate_dcomplex_4d(var, d) result(ret) 

   !> array to be allocated 
   complex*16, allocatable, intent(inout) :: var(:,:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(4)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3),d(4)), stat=ret )   

   return 
end function allocate_dcomplex_4d


   
!> Allocation of a 1D logical array.
integer function allocate_logical_1d(var, d) result(ret) 

   !> array to be allocated 
   logical, allocatable, intent(inout) :: var(:) 
   
   !> array dimensions
   integer, intent(in) :: d
      
   
   if (.not.allocated(var)) allocate ( var(d), stat=ret )   

   return 
end function allocate_logical_1d
   
   
!> Allocation of a 2D logical array.
integer function allocate_logical_2d(var, d) result(ret) 

   !> array to be allocated 
   logical, allocatable, intent(inout) :: var(:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(2)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2)), stat=ret )   

   return 
end function allocate_logical_2d
   
   
!> Allocation of a 3D logical array.
integer function allocate_logical_3d(var, d) result(ret) 

   !> array to be allocated 
   logical, allocatable, intent(inout) :: var(:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(3)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3)), stat=ret )   

   return 
end function allocate_logical_3d
   
   
!> Allocation of a 4D logical array.
integer function allocate_logical_4d(var, d) result(ret) 

   !> array to be allocated 
   logical, allocatable, intent(inout) :: var(:,:,:,:) 
   
   !> array dimensions
   integer, intent(in) :: d(4)
      
   
   if (.not.allocated(var)) allocate ( var(d(1),d(2),d(3),d(4)), stat=ret )   

   return 
end function allocate_logical_4d


   
!> Deallocation of a 1D integer array.
integer function deallocate_int_1d(var) result(ret) 

   !> array to be deallocated 
   integer, allocatable, intent(inout) :: var(:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_int_1d
   
   
!> Deallocation of a 2D integer array.
integer function deallocate_int_2d(var) result(ret) 

   !> array to be deallocated 
   integer, allocatable, intent(inout) :: var(:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_int_2d
   
   
!> Deallocation of a 3D integer array.
integer function deallocate_int_3d(var) result(ret) 

   !> array to be deallocated 
   integer, allocatable, intent(inout) :: var(:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_int_3d
   
   
!> Deallocation of a 4D integer array.
integer function deallocate_int_4d(var) result(ret) 

   !> array to be deallocated 
   integer, allocatable, intent(inout) :: var(:,:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_int_4d
   
   
   
!> Deallocation of a 1D real array.
integer function deallocate_real_1d(var) result(ret) 

   !> array to be deallocated 
   real(kind(1.0)), allocatable, intent(inout) :: var(:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_real_1d
   
   
!> Deallocation of a 2D real array.
integer function deallocate_real_2d(var) result(ret) 

   !> array to be deallocated 
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_real_2d
   
   
!> Deallocation of a 3D real array.
integer function deallocate_real_3d(var) result(ret) 

   !> array to be deallocated 
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_real_3d
   
   
!> Deallocation of a 4D real array.
integer function deallocate_real_4d(var) result(ret) 

   !> array to be deallocated 
   real(kind(1.0)), allocatable, intent(inout) :: var(:,:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_real_4d
   
   
   
!> Deallocation of a 1D double array.
integer function deallocate_double_1d(var) result(ret) 

   !> array to be deallocated 
   real(kind(1.d0)), allocatable, intent(inout) :: var(:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_double_1d
   
   
!> Deallocation of a 2D double array.
integer function deallocate_double_2d(var) result(ret) 

   !> array to be deallocated 
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_double_2d
   
   
!> Deallocation of a 3D double array.
integer function deallocate_double_3d(var) result(ret) 

   !> array to be deallocated 
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_double_3d
   
   
!> Deallocation of a 4D double array.
integer function deallocate_double_4d(var) result(ret) 

   !> array to be deallocated 
   real(kind(1.d0)), allocatable, intent(inout) :: var(:,:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_double_4d
   
   
   
!> Deallocation of a 1D complex array.
integer function deallocate_complex_1d(var) result(ret) 

   !> array to be deallocated 
   complex, allocatable, intent(inout) :: var(:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_complex_1d
   
   
!> Deallocation of a 2D complex array.
integer function deallocate_complex_2d(var) result(ret) 

   !> array to be deallocated 
   complex, allocatable, intent(inout) :: var(:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_complex_2d
   
   
!> Deallocation of a 3D complex array.
integer function deallocate_complex_3d(var) result(ret) 

   !> array to be deallocated 
   complex, allocatable, intent(inout) :: var(:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_complex_3d
   
   
!> Deallocation of a 4D complex array.
integer function deallocate_complex_4d(var) result(ret) 

   !> array to be deallocated 
   complex, allocatable, intent(inout) :: var(:,:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_complex_4d
   
   
   
!> Deallocation of a 1D double complex array.
integer function deallocate_dcomplex_1d(var) result(ret) 

   !> array to be deallocated 
   complex*16, allocatable, intent(inout) :: var(:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_dcomplex_1d
   
   
!> Deallocation of a 2D double complex array.
integer function deallocate_dcomplex_2d(var) result(ret) 

   !> array to be deallocated 
   complex*16, allocatable, intent(inout) :: var(:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_dcomplex_2d
   
   
!> Deallocation of a 3D double complex array.
integer function deallocate_dcomplex_3d(var) result(ret) 

   !> array to be deallocated 
   complex*16, allocatable, intent(inout) :: var(:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_dcomplex_3d
   
   
!> Deallocation of a 4D double complex array.
integer function deallocate_dcomplex_4d(var) result(ret) 

   !> array to be deallocated 
   complex*16, allocatable, intent(inout) :: var(:,:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_dcomplex_4d
   
         
   
!> Deallocation of a 1D logical array.
integer function deallocate_logical_1d(var) result(ret) 

   !> array to be deallocated 
   logical, allocatable, intent(inout) :: var(:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_logical_1d
   
   
!> Deallocation of a 2D logical array.
integer function deallocate_logical_2d(var) result(ret) 

   !> array to be deallocated 
   logical, allocatable, intent(inout) :: var(:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_logical_2d
   
   
!> Deallocation of a 3D logical array.
integer function deallocate_logical_3d(var) result(ret) 

   !> array to be deallocated 
   logical, allocatable, intent(inout) :: var(:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_logical_3d
   
   
!> Deallocation of a 4D logical array.
integer function deallocate_logical_4d(var) result(ret) 

   !> array to be deallocated 
   logical, allocatable, intent(inout) :: var(:,:,:,:) 
      
   if (allocated(var)) deallocate ( var, stat=ret )   

   return 
end function deallocate_logical_4d
   
         
end module MemoryProfiler
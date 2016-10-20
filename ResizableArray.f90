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

MODULE ResizableArray
! Module which increase or shrink allocatable array sizes at runtime. 

	IMPLICIT NONE
	
	PUBLIC :: reallocate

	INTERFACE reallocate
	
	   ! Integer type subroutines
	   module procedure reallocate_int_1d
	   module procedure reallocate_int_2d
	   module procedure reallocate_int_3d
	   module procedure reallocate_int_4d
	   
	   ! Single precision real type subroutines
	   module procedure reallocate_real_1d 
	   module procedure reallocate_real_2D
	   module procedure reallocate_real_3D
	   module procedure reallocate_real_4D
	   
	   ! Double precision real type subroutines
	   module procedure reallocate_double_1d 
	   module procedure reallocate_double_2D
	   module procedure reallocate_double_3D
	   module procedure reallocate_double_4D
       
	   ! Single precision complex type subroutines
	   module procedure reallocate_complex_1d 
	   module procedure reallocate_complex_2D
	   module procedure reallocate_complex_3D
	   module procedure reallocate_complex_4D
	   
	   ! Double precision complex type subroutines
	   module procedure reallocate_dcomplex_1d 
	   module procedure reallocate_dcomplex_2D
	   module procedure reallocate_dcomplex_3D
	   module procedure reallocate_dcomplex_4D
       
       ! Logical type subroutines
	   module procedure reallocate_logical_1d 
	   module procedure reallocate_logical_2D
	   module procedure reallocate_logical_3D
	   module procedure reallocate_logical_4D
	   
	END INTERFACE reallocate
	
	
CONTAINS


!*** Integer type subroutines *************************************************


	SUBROUTINE reallocate_int_1d(a,ni_new)

		INTEGER,DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: a
		INTEGER,DIMENSION(:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new
		INTEGER :: ni_old

		ni_old = SIZE(a)
		IF (ni_old == ni_new) RETURN

		ALLOCATE(temp(ni_new))
        
		temp(1:min(ni_old,ni_new)) = a(1:min(ni_old,ni_new))
		
		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_int_1d


	SUBROUTINE reallocate_int_2d(a,ni_new,nj_new)

		INTEGER,DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT) :: a
		INTEGER,DIMENSION(:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new
		INTEGER :: ni_old,nj_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		IF (ni_old == ni_new .AND. nj_old == nj_new) RETURN

		ALLOCATE(temp(ni_new,nj_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new)) = &
		   a(1:min(ni_old,ni_new),1:min(nj_old,nj_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_int_2d


	SUBROUTINE reallocate_int_3d(a,ni_new,nj_new,nk_new)

		INTEGER,DIMENSION(:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		INTEGER,DIMENSION(:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new
		INTEGER :: ni_old,nj_old,nk_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. nk_old == nk_new) &
		RETURN

		ALLOCATE(temp(ni_new,nj_new,nk_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_int_3d


	SUBROUTINE reallocate_int_4d(a,ni_new,nj_new,nk_new,nl_new)

		INTEGER,DIMENSION(:,:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		INTEGER,DIMENSION(:,:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new,nl_new
		INTEGER :: ni_old,nj_old,nk_old,nl_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		nl_old = UBOUND(a,4)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. & 
		    nk_old == nk_new .AND. nl_old == nl_new) RETURN

		ALLOCATE(temp(ni_new,nj_new,nk_new,nl_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_int_4d
	
	
!*** Single precision real type subroutines ***********************************


	SUBROUTINE reallocate_real_1d(a,ni_new)

		REAL*4,DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: a
		REAL*4,DIMENSION(:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new
		INTEGER :: ni_old

		ni_old = SIZE(a)
		IF (ni_old == ni_new) RETURN

		ALLOCATE(temp(ni_new))

		temp(1:min(ni_old,ni_new)) = a(1:min(ni_old,ni_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_real_1d


	SUBROUTINE reallocate_real_2d(a,ni_new,nj_new)

		REAL*4,DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT) :: a
		REAL*4,DIMENSION(:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new
		INTEGER :: ni_old,nj_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		IF (ni_old == ni_new .AND. nj_old == nj_new) RETURN

		ALLOCATE(temp(ni_new,nj_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new)) = &
		   a(1:min(ni_old,ni_new),1:min(nj_old,nj_new))
		   
		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_real_2d


	SUBROUTINE reallocate_real_3d(a,ni_new,nj_new,nk_new)

		REAL*4,DIMENSION(:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		REAL*4,DIMENSION(:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new
		INTEGER :: ni_old,nj_old,nk_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. nk_old == nk_new) &
		RETURN

		ALLOCATE(temp(ni_new,nj_new,nk_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_real_3d


	SUBROUTINE reallocate_real_4d(a,ni_new,nj_new,nk_new,nl_new)

		REAL*4,DIMENSION(:,:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		REAL*4,DIMENSION(:,:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new,nl_new
		INTEGER :: ni_old,nj_old,nk_old,nl_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		nl_old = UBOUND(a,4)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. & 
		    nk_old == nk_new .AND. nl_old == nl_new) RETURN

		ALLOCATE(temp(ni_new,nj_new,nk_new,nl_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_real_4d
	
	
!*** Double precision real type subroutines ***********************************
	
	
	SUBROUTINE reallocate_double_1d(a,ni_new)

		REAL*8,DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: a
		REAL*8,DIMENSION(:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new
		INTEGER :: ni_old

		ni_old = SIZE(a)
		IF (ni_old == ni_new) RETURN

		ALLOCATE(temp(ni_new))

		temp(1:min(ni_old,ni_new)) = a(1:min(ni_old,ni_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN
	END SUBROUTINE reallocate_double_1d


	SUBROUTINE reallocate_double_2d(a,ni_new,nj_new)

		REAL*8,DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT) :: a
		REAL*8,DIMENSION(:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new
		INTEGER :: ni_old,nj_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		IF (ni_old == ni_new .AND. nj_old == nj_new) RETURN

		ALLOCATE(temp(ni_new,nj_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new)) = &
		   a(1:min(ni_old,ni_new),1:min(nj_old,nj_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_double_2d


	SUBROUTINE reallocate_double_3d(a,ni_new,nj_new,nk_new)

		REAL*8,DIMENSION(:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		REAL*8,DIMENSION(:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new
		INTEGER :: ni_old,nj_old,nk_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. nk_old == nk_new) &
		RETURN
		
		ALLOCATE(temp(ni_new,nj_new,nk_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new))
		
		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_double_3d


	SUBROUTINE reallocate_double_4d(a,ni_new,nj_new,nk_new,nl_new)

		REAL*8,DIMENSION(:,:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		REAL*8,DIMENSION(:,:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new,nl_new
		INTEGER :: ni_old,nj_old,nk_old,nl_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		nl_old = UBOUND(a,4)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. & 
		    nk_old == nk_new .AND. nl_old == nl_new) RETURN
		    
		ALLOCATE(temp(ni_new,nj_new,nk_new,nl_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_double_4d


!*** Single precision complex type subroutines ********************************


	SUBROUTINE reallocate_complex_1d(a,ni_new)

		COMPLEX*8,DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: a
		COMPLEX*8,DIMENSION(:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new
		INTEGER :: ni_old

		ni_old = SIZE(a)
		IF (ni_old == ni_new) RETURN

		ALLOCATE(temp(ni_new))

		temp(1:min(ni_old,ni_new)) = a(1:min(ni_old,ni_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_complex_1d


	SUBROUTINE reallocate_complex_2d(a,ni_new,nj_new)

		COMPLEX*8,DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT) :: a
		COMPLEX*8,DIMENSION(:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new
		INTEGER :: ni_old,nj_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		IF (ni_old == ni_new .AND. nj_old == nj_new) RETURN

		ALLOCATE(temp(ni_new,nj_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new)) = &
		   a(1:min(ni_old,ni_new),1:min(nj_old,nj_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_complex_2d


	SUBROUTINE reallocate_complex_3d(a,ni_new,nj_new,nk_new)

		COMPLEX*8,DIMENSION(:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		COMPLEX*8,DIMENSION(:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new
		INTEGER :: ni_old,nj_old,nk_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. nk_old == nk_new) &
		RETURN

		ALLOCATE(temp(ni_new,nj_new,nk_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_complex_3d


	SUBROUTINE reallocate_complex_4d(a,ni_new,nj_new,nk_new,nl_new)

		COMPLEX*8,DIMENSION(:,:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		COMPLEX*8,DIMENSION(:,:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new,nl_new
		INTEGER :: ni_old,nj_old,nk_old,nl_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		nl_old = UBOUND(a,4)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. & 
		    nk_old == nk_new .AND. nl_old == nl_new) RETURN

		ALLOCATE(temp(ni_new,nj_new,nk_new,nl_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_complex_4d


!*** Double precision complex type subroutines ********************************


	SUBROUTINE reallocate_dcomplex_1d(a,ni_new)

		COMPLEX*16,DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: a
		COMPLEX*16,DIMENSION(:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new
		INTEGER :: ni_old

		ni_old = SIZE(a)
		IF (ni_old == ni_new) RETURN

		ALLOCATE(temp(ni_new))

		temp(1:min(ni_old,ni_new)) = a(1:min(ni_old,ni_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_dcomplex_1d


	SUBROUTINE reallocate_dcomplex_2d(a,ni_new,nj_new)

		COMPLEX*16,DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT) :: a
		COMPLEX*16,DIMENSION(:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new
		INTEGER :: ni_old,nj_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		IF (ni_old == ni_new .AND. nj_old == nj_new) RETURN

		ALLOCATE(temp(ni_new,nj_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new)) = &
		   a(1:min(ni_old,ni_new),1:min(nj_old,nj_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_dcomplex_2d


	SUBROUTINE reallocate_dcomplex_3d(a,ni_new,nj_new,nk_new)

		COMPLEX*16,DIMENSION(:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		COMPLEX*16,DIMENSION(:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new
		INTEGER :: ni_old,nj_old,nk_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. nk_old == nk_new) &
		RETURN

		ALLOCATE(temp(ni_new,nj_new,nk_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_dcomplex_3d


	SUBROUTINE reallocate_dcomplex_4d(a,ni_new,nj_new,nk_new,nl_new)

		COMPLEX*16,DIMENSION(:,:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		COMPLEX*16,DIMENSION(:,:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new,nl_new
		INTEGER :: ni_old,nj_old,nk_old,nl_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		nl_old = UBOUND(a,4)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. & 
		    nk_old == nk_new .AND. nl_old == nl_new) RETURN

		ALLOCATE(temp(ni_new,nj_new,nk_new,nl_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_dcomplex_4d


!*** Logical type subroutines *************************************************


	SUBROUTINE reallocate_logical_1d(a,ni_new)

		LOGICAL,DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: a
		LOGICAL,DIMENSION(:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new
		INTEGER :: ni_old

		ni_old = SIZE(a)
		IF (ni_old == ni_new) RETURN

		ALLOCATE(temp(ni_new))

		temp(1:min(ni_old,ni_new)) = a(1:min(ni_old,ni_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_logical_1d


	SUBROUTINE reallocate_logical_2d(a,ni_new,nj_new)

		LOGICAL,DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT) :: a
		LOGICAL,DIMENSION(:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new
		INTEGER :: ni_old,nj_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		IF (ni_old == ni_new .AND. nj_old == nj_new) RETURN

		ALLOCATE(temp(ni_new,nj_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new)) = &
		   a(1:min(ni_old,ni_new),1:min(nj_old,nj_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_logical_2d


	SUBROUTINE reallocate_logical_3d(a,ni_new,nj_new,nk_new)

		LOGICAL,DIMENSION(:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		LOGICAL,DIMENSION(:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new
		INTEGER :: ni_old,nj_old,nk_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. nk_old == nk_new) &
		RETURN

		ALLOCATE(temp(ni_new,nj_new,nk_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new),1:min(nk_old,nk_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_logical_3d


	SUBROUTINE reallocate_logical_4d(a,ni_new,nj_new,nk_new,nl_new)

		LOGICAL,DIMENSION(:,:,:,:),ALLOCATABLE,INTENT(INOUT) :: a
		LOGICAL,DIMENSION(:,:,:,:),ALLOCATABLE :: temp
		INTEGER,INTENT(IN) :: ni_new,nj_new,nk_new,nl_new
		INTEGER :: ni_old,nj_old,nk_old,nl_old

		ni_old = UBOUND(a,1)
		nj_old = UBOUND(a,2)
		nk_old = UBOUND(a,3)
		nl_old = UBOUND(a,4)
		IF (ni_old == ni_new .AND. nj_old == nj_new .AND. & 
		    nk_old == nk_new .AND. nl_old == nl_new) RETURN

		ALLOCATE(temp(ni_new,nj_new,nk_new,nl_new))

		temp(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new)) &
		=  a(1:min(ni_old,ni_new),1:min(nj_old,nj_new), &
		     1:min(nk_old,nk_new),1:min(nl_old,nl_new))

		CALL MOVE_ALLOC(temp,a)

        RETURN 
	END SUBROUTINE reallocate_logical_4d


END MODULE ResizableArray
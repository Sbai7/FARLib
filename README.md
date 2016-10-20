# FARLib
This is an Array collection library which is designed according to modern Fortran 2003+ standard. It supports in-place transparent redimensioning of multidimensional arrays, up to 4th rank, of Integer, real, double, complex, and logical types. 

In this first release, the only Fortran module available is ResizableArray which simply increases or shrinks an allocatable array size at runtime. The only available subroutine call is '*reallocate*' which is type and rank independent. 

cmake_minimum_required ( VERSION 3.21 )
set ( CMAKE_Fortran_COMPILER gfortran )
project ( fortran-calendar-jp LANGUAGES Fortran VERSION 1.0.0 )
enable_language ( Fortran )
enable_testing ()
add_subdirectory ( app )
add_subdirectory ( test )

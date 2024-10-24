set ( EXE fortran-calendar-jp )
set ( SRCS 
  ~/2_tools/fortran-logger/src/logger_mo.f90
  ~/2_tools/fortran-datetime/src/dt_mo.f90
  ../app/cli_mo.f90
  ../app/main.f90
  ../src/calendar_mo.f90
  )
set ( CMAKE_Fortran_FLAGS 
  "-ffree-line-length-none -Wno-maybe-uninitialized -fcoarray=single")

if ( CMAKE_BUILD_TYPE MATCHES "Release" )
  set ( CMAKE_Fortran_FLAGS_RELEASE
    "-O3 -Drelease ${CMAKE_Fortran_FLAGS}" )
endif()

add_executable ( ${EXE} ${SRCS} )
target_include_directories ( ${EXE} PUBLIC "../include" )
install ( TARGETS ${EXE} )

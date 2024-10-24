set ( EXE unit_test )
set ( ARGS
  --dir "../../data"
  --date_fr "2010-01-01"
  --date_to "2021-12-31"
  )
set ( SRCS 
  ~/2_tools/fortran-logger/src/logger_mo.f90
  ~/2_tools/fortran-datetime/src/dt_mo.f90
  ../app/cli_mo.f90
  ../app/main.f90
  ../src/calendar_mo.f90
  )

set ( CMAKE_Fortran_FLAGS
  "-ffree-line-length-none -Wno-maybe-uninitialized -fcoarray=single")

if ( CMAKE_BUILD_TYPE MATCHES "Debug" )
  set ( CMAKE_Fortran_FLAGS_DEBUG
    "-Ddebug -O0 -fbounds-check -fbacktrace -Wall" )
endif()

add_executable ( ${EXE} ${SRCS} )
target_include_directories ( ${EXE} PUBLIC "../include" )
add_test ( NAME ${EXE} COMMAND ${EXE} ${ARGS} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )

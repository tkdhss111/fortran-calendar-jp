set ( EXE unit_test )
set ( ARGS
  --dir "${MYHOME}/2_tools/fortran-calendar-jp/data"
  #--date_fr "2010-01-01"
  #--date_to "2021-12-31"
  )

set ( SRCS 
  ${MYHOME}/2_tools/fortran-logger/src/logger_mo.f90
  ${MYHOME}/2_tools/fortran-datetime/src/dt_mo.f90
  ../app/cli_mo.f90
  ../app/main.f90
  ../src/calendar_mo.f90
  )

set ( CMAKE_Fortran_FLAGS
  "-ffree-line-length-none -fcoarray=single")
set ( CMAKE_Fortran_FLAGS_DEBUG
  "-Ddebug -O0 -fbounds-check -fbacktrace -Wall -Wno-uninitialized -Wno-maybe-uninitialized ${CMAKE_Fortran_FLAGS}" )

add_executable ( ${EXE} ${SRCS} )
target_include_directories ( ${EXE} PUBLIC "../include" )
add_test ( NAME ${EXE} COMMAND ${EXE} ${ARGS} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )

#include "macro.fi"

program main

  use cli_mo
  use dt_mo
  use calendar_mo

  implicit none

  character(255) :: dir     ! Working directory
  character(10)  :: date_fr ! Start date
  character(10)  :: date_to ! End date
  character(19)  :: datetime

  type(cli_ty) :: cli
  type(ca_ty)  :: ca
  type(ho_ty)  :: ho
  type(dt_ty)  :: t
  integer      :: i = 1

  cli%title     = 'Japanese Calendar Server'
  cli%exe       = 'fortran-calendar-jp'
  cli%author    = 'Hisashi Takeda, Ph.D.'
  cli%copyright = '2020 Copyright(C) All Rights Reserved.'
  cli%version   = '1.0'
  cli%usage(i)  = '===================================================';i=i+1
  cli%usage(i)  = 'Usage: '//trim(cli%exe)//' [OPTIONS]'               ;i=i+1
  cli%usage(i)  = ''                                                   ;i=i+1
  cli%usage(i)  = 'Example: '//trim(cli%exe)//' --dir ./data'          ;i=i+1
  cli%usage(i)  = '                             --date_fr 2000-01-01'  ;i=i+1
  cli%usage(i)  = '                             --date_to 2020-12-31'  ;i=i+1
  cli%usage(i)  = ''                                                   ;i=i+1
  cli%usage(i)  = 'Program options:'                                   ;i=i+1
  cli%usage(i)  = '  --dir      followed by path of working directory' ;i=i+1
  cli%usage(i)  = '  --date_fr  followed by start date as yyyy-mm-dd'  ;i=i+1
  cli%usage(i)  = '  --date_to  followed by   end date as yyyy-mm-dd'  ;i=i+1
  cli%usage(i)  = ''                                                   ;i=i+1
  cli%usage(i)  = '  -v, --version print version information and exit' ;i=i+1
  cli%usage(i)  = '  -h, --help    print usage information and exit'   ;i=i+1
  cli%usage(i)  = '===================================================';i=i+1
  cli%n_usage   = i-1

  ! Default
  date_fr = '2000-01-01'

  t = t%now()
  if ( t%mo < 3 ) then ! New calendar will be released on Feb. 15
    datetime = get_datetime ( yr0 = t%yr, mo0 = 12, dy0 = 31 )
  else
    datetime = get_datetime ( yr0 = t%yr + 1, mo0 = 12, dy0 = 31 )
  end if

  date_to = datetime(1:10) 

  call cli%get_args ( dir, date_fr, date_to )

  call logger%init ( file  = trim(dir)//'/fortran-calendar-jp.log', &
                     app   = trim(cli%exe)//' @ HP-Z840', &
                     email = 'dsbiztiu@gmail.com' )

  call ho%download ( dir )
  call ho%init ( dir )
  call ca%init ( date_fr, date_to )
  call ca%make ( dir, ho )
  call ca%write ( trim(dir)//'/calendar.csv' )
  call csv2parquet ( trim(dir)//'/calendar.csv', trim(dir)//'/calendar.parquet' )

end program

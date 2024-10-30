#include "macro.fi"

module calendar_mo

  use logger_mo, only: logger_ty, paste
  use dt_mo,     only: dt_ty, seq_dt, strptime, get_datetime, &
                       day_of_the_week, LAB_WDAY, LAB_WDAY_JP

  implicit none

  type ho_ty
    type(dt_ty), allocatable :: ts(:) ! Datetimes
    integer                  :: nts   ! Number of datetimes
  contains
    procedure, nopass :: download => download_holidays 
    procedure         :: init     => init_holidays 
  end type

  ! Calendar type
  type ca_ty
    type(dt_ty)                :: t_fr      ! Start date
    type(dt_ty)                :: t_to      ! End date
    type(dt_ty),   allocatable :: ts(:)     ! Datetimes
    character(80), allocatable :: event(:)  ! Event description for special term 
    character(5),  allocatable :: md(:)     ! mm-dd (month and day) 
    integer,       allocatable :: dow(:, :) ! Day of the week dummy 
    integer,       allocatable :: wk(:)     ! Week number within a year 
    integer,       allocatable :: gr(:)     ! Group of special term (Sun:1--Sat:7 on April 1st )
    integer,       allocatable :: wd(:)     ! Weekday dummy
    integer,       allocatable :: ho(:)     ! Holiday dummy
    integer,       allocatable :: bw(:)     ! Weekday between day-offs dummry
    integer,       allocatable :: af(:)     ! Weekday after day-off dummy 
    integer,       allocatable :: sp(:)     ! Special day dummy
    integer,       allocatable :: ab(:)     ! Abnormal day dummy
    integer                    :: nts       ! Number of datetimes
  contains
    procedure :: init => init_calendar
    procedure :: make => make_calendar
  end type

  ! Special days type
  type spd_ty
    character(80) :: nm
    character(10) :: date ! Use 1:5 for mm-dd
  end type

  type(logger_ty) :: logger

contains

  function read_special_days ( file ) result ( spd )

    character(*), intent(in)  :: file
    type(spd_ty), allocatable :: spd(:)
    integer u, i, nr

    __LOG__( 'S: read_special_days' )

    call logger%open ( __FILE__, __LINE__, newunit = u, file = file, status = 'old' )
    nr = count_rows ( u ) - 1 
    allocate ( spd(nr) )
    read ( u, '()' ) ! Skip header line
    do i = 1, nr
      read ( u, * ) spd(i)%nm, spd(i)%date 
    end do
    close ( u )

    __LOG__( 'E: read_special_days' )

  end function read_special_days

  subroutine init_calendar ( this, date_fr, date_to )

    class(ca_ty), intent(inout) :: this
    character(*), intent(in)    :: date_fr, date_to 

    __LOG__( 'S: init_calendar' )

    this%t_fr = strptime ( date_fr, '%Y-%m-%d' )
    this%t_to = strptime ( date_to, '%Y-%m-%d' )
    this%ts = seq_dt ( this%t_fr, this%t_to, '1 day' )
    this%nts = size(this%ts)

    __LOG__( paste ( 'nts: ', this%nts, 'date_fr: ', date_fr, ', date_to: ', date_to ) )

    allocate ( this%event(this%nts) )
    allocate ( this%md   (this%nts) )
    allocate ( this%dow(this%nts, 7), source = 0 )
    allocate ( this%wk(this%nts),     source = 0 )
    allocate ( this%gr(this%nts),     source = 0 )
    allocate ( this%wd(this%nts),     source = 0 )
    allocate ( this%ho(this%nts),     source = 0 )
    allocate ( this%bw(this%nts),     source = 0 )
    allocate ( this%af(this%nts),     source = 0 )
    allocate ( this%sp(this%nts),     source = 0 )
    allocate ( this%ab(this%nts),     source = 0 )

    this%md = this%ts%mm//'-'//this%ts%dd
    this%event = ''

    __LOG__( 'E: init_calendar' )

  end subroutine init_calendar

  subroutine write_csv ( ca, file )

    type(ca_ty),  intent(in) :: ca
    character(*), intent(in) :: file
    character(2000)          :: csv, csnm
    integer i, j, u

    __LOG__( 'S: write_csv' )

    ! カラム名 | 内容
    ! ---------|-------
    ! date     |日時（yyyy-mm-dd形式）
    ! dow      |曜日
    ! name     |日種別名称（平日／休日／祝日）
    ! event    |特異期間名称（年末年始，GW，お盆） 
    ! md       |月日（mm-dd形式）
    ! gr       |曜日回り同一年度グループ（3月1日の曜日番号； 日：１〜土：７）
    ! year     |年（yyyy形式） 
    ! month    |月（m形式）
    ! day      |日（d形式）
    ! wk       |週番号
    ! su       |日曜ダミー
    ! mo       |月曜ダミー
    ! tu       |火曜ダミー
    ! we       |水曜ダミー
    ! th       |木曜ダミー
    ! fr       |金曜ダミー
    ! sa       |土曜ダミー
    ! wd       |平日ダミー
    ! ho       |祝日ダミー
    ! bw       |休日間ダミー
    ! af       |休日明ダミー
    ! sp       |特異日ダミー
    ! ab       |異常日ダミー

    csnm = 'date,dow,name,event,md,gr,year,month,day,wk,su,mo,tu,we,th,fr,sa,wd,ho,bw,af,sp,ab'   

    call logger%open ( __FILE__, __LINE__, newunit = u, file = file, status = 'replace' )
    write (  u, '(a)' ) csnm
    do i = 1, ca%nts
      write ( csv, '(a)' ) '"'//ca%ts(i)%date//'"'
      write ( csv, '(a)' ) trim(csv)//',"'//LAB_WDAY_JP(ca%ts(i)%dow)//'"'
      write ( csv, '(a)' ) trim(csv)//',"'//trim(ca%ts(i)%nm)//'"'
      write ( csv, '(a)' ) trim(csv)//',"'//trim(ca%event(i))//'"'
      write ( csv, '(a)' ) trim(csv)//',"'//ca%md(i)//'"'
      write ( csv, '(a, i1)' ) trim(csv)//',', ca%gr(i)
      write ( csv, '(a, i4)' ) trim(csv)//',', ca%ts(i)%yr
      write ( csv, '(a, i2)' ) trim(csv)//',', ca%ts(i)%mo
      write ( csv, '(a, i2)' ) trim(csv)//',', ca%ts(i)%dy
      write ( csv, '(a, i2)' ) trim(csv)//',', ca%wk(i)
      do j = 1, 7
        write ( csv, '(a, i1)' ) trim(csv)//',', ca%dow(i, j)
      end do
      write ( csv, '(a, i1)' ) trim(csv)//',', ca%wd(i)
      write ( csv, '(a, i1)' ) trim(csv)//',', ca%ho(i)
      write ( csv, '(a, i1)' ) trim(csv)//',', ca%bw(i)
      write ( csv, '(a, i1)' ) trim(csv)//',', ca%af(i)
      write ( csv, '(a, i1)' ) trim(csv)//',', ca%sp(i)
      write ( csv, '(a, i1)' ) trim(csv)//',', ca%ab(i)
      write( u, '(a)' ) csv
    end do
    close ( u )

    __LOG__( 'E: write_csv' )

  end subroutine write_csv

  subroutine make_week_number ( this )

    ! Increment by Monday

    class(ca_ty), intent(inout) :: this
    type(dt_ty) :: t
    integer wk  ! Week number
    integer pyr ! Present year
    integer nts, i, k

    nts = size(this%ts)
    pyr = this%ts(1)%yr

    t = strptime ( this%ts(1)%yyyy//'-01-01', '%Y-%m-%d' )

    wk = 1
    k = 1

    do while ( k <= nts )

      if ( this%ts(k)%datetime == t%datetime ) then
        this%wk(k) = wk
        k = k + 1
      end if

      t = t%plus ( days = 1 )

      ! Count Mondays
      if ( t%dow == 2 ) then ! 2: Monday
        wk = wk + 1
      end if

      if ( t%yr > pyr ) then
        pyr = t%yr
        wk = 1
      end if

      !__LOG__( paste( 't%datetime: ', t%datetime, ', dow:', t%dow, ', wk:', wk, ', pyr:', pyr ) )
    end do

  end subroutine

  subroutine make_calendar ( this, dir, ho )

    class(ca_ty), intent(inout) :: this
    character(*), intent(in)    :: dir 
    type(ho_ty),  intent(inout) :: ho
    type(spd_ty), allocatable   :: abds(:)
    integer i, j, i_d, i_y, it

    __LOG__( 'S: make_calendar' )

    !
    ! Set week number
    !
    call make_week_number ( this )

    !
    ! Set name of a day
    !
    where ( this%ts%dow == 7 )
      this%ts%nm = '休日' 
    end where

    where ( this%ts%dow == 1 )
      this%ts%nm = '休日' 
    end where

    !
    ! Merge holidays
    !
    do i = 1, ho%nts
      it = findloc( this%ts%date, ho%ts(i)%date, dim = 1 )
      if ( it > 0 ) then
        this%ho(it) = 1
        if ( ho%ts(i)%nm == '休日' ) then ! Make nameless holiday unique
          this%ts(it)%nm = '祝日（'//this%md(it)//'）'
        else
          this%ts(it)%nm = '祝日（'//trim(ho%ts(i)%nm)//'）'
        end if
      end if
    end do

    !
    ! Make day of the week dummies
    !
    do i = 1, 7
      where ( this%ts%dow == i )
        this%dow(:, i) = 1
      end where
    end do

    !
    ! Make year group of special term 
    !
    do i_y = this%ts(1)%yr, this%ts(this%nts)%yr
      ! From March 1 to December 31
      where ( this%ts%yr == i_y .and. this%ts%mo >= 3 )
        this%gr = day_of_the_week ( i_y, 3, 1 ) 
      end where
      ! From January 1 to February 30
      where ( this%ts%yr == i_y .and. this%ts%mo <= 2 )
        this%gr = day_of_the_week ( i_y - 1, 3, 1 ) 
      end where
    end do

    !
    ! Weekday
    !
    do concurrent ( i = 1:this%nts )
      if ( this%ho(i)     == 0 .and. & ! Not holiday
           this%dow(i, 7) == 0 .and. & ! Not Saturday
           this%dow(i, 1) == 0       & ! Not Sunday
          ) then
        this%wd(i) = 1
        this%ts(i)%nm = '平日' 
      end if
    end do

    !
    ! Weekday between day-offs 
    !
    do concurrent ( i = 2:this%nts - 1 )
      if ( this%wd(i) == 1 .and. & ! Weekday 
            ( this%ho(i - 1) * this%ho(i + 1)     == 1 .or. & ! Between holidays
              this%ho(i - 1) * this%dow(i + 1, 7) == 1 .or. & ! Between holiday and Saturday
              this%ho(i + 1) * this%dow(i - 1, 1) == 1 )    & ! Between Sunday and holiday
          ) then
        this%bw(i) = 1
        this%ts(i)%nm = '平日（休日間）' 
      end if
    end do

    !
    ! Weekday after day-off 
    !
    do concurrent ( i = 2:this%nts - 1 )
      if ( this%wd(i) == 1 .and. & ! Weekday 
            this%bw(i) == 0 .and. & ! Not weekday between holidays
            ( this%ho(i - 1) == 1 .or. &
              this%dow(i - 1, 1) == 1 ) & ! Previous day is holiday or Sunday
          ) then
        this%af(i) = 1
        this%ts(i)%nm = '平日（休日明）'!（除：休日間平日）
      end if
    end do

    !
    ! Make abnorml day dummy
    !
    abds = read_special_days ( trim(dir)//'/abnormaldays.csv' )
    do i_d = 1, size(abds) ! Abnormal dates loop
      where ( this%ts%date == abds(i_d)%date )
        this%ab = 1
        this%event = trim(abds(i_d)%nm)
      end where
    end do

    !
    ! Make special day dummy
    !

    ! Golden Week
    where ( '05-03' <= this%md .and. this%md <= '05-05' )
      this%sp = 1
      this%event = 'GW（'//this%md//'）'
    end where

    where ( (this%md == '04-30' .or. this%md == '05-01' .or. this%md == '05-02' ) .and. this%wd == 1 )
      this%sp = 1
      this%event = 'GW（'//this%md//'）'
    end where

    ! Obon Week
    !where ( '08-13' <= this%md .and. this%md <= '08-16' )
    where ( '08-12' <= this%md .and. this%md <= '08-18' )
      this%sp = 1
      this%event = 'お盆（'//this%md//'）'
    end where

    !where ( this%md == '08-12' .and. LAB_WDAY_JP(this%ts%dow) == '月' )
    !  this%sp = 1
    !  this%event = 'お盆'
    !end where

    !where ( this%md == '08-17' .and. LAB_WDAY_JP(this%ts%dow) == '金' )
    !  this%sp = 1
    !  this%event = 'お盆'
    !end where

    ! Year-end and New Year Days
    !where ( '12-29' <= this%md .and. this%md <= '12-31' )
    where ( '12-28' <= this%md .and. this%md <= '12-31' )
      this%sp = 1
      this%event = '年末年始（'//this%md//'）'
    end where

    !where ( '01-01' <= this%md .and. this%md <= '01-03' )
    where ( '01-01' <= this%md .and. this%md <= '01-05' )
      this%sp = 1
      this%event = '年末年始（'//this%md//'）'
    end where

block
    integer, allocatable :: dows(:)
    dows = this%ts%dow ! Workaround
    !where ( this%md == '12-28' .and. LAB_WDAY_JP(this%ts%dow) == '月' ) ! Bug report is required?
    where ( this%md == '12-28' .and. LAB_WDAY_JP(dows) == '月' )
      this%sp = 1
      this%event = '年末年始（'//this%md//'）'
    end where

    ! ToDo: Investigate the effectiveness
    !where ( this%md == '01-04' .and. LAB_WDAY_JP(this%ts%dow) == '金' ) ! Bug report is required?
    where ( this%md == '01-04' .and. LAB_WDAY_JP(dows) == '金' )
      this%sp = 1
      this%event = '年末年始（'//this%md//'）'
    end where
end block

#ifdef debug
    do i = 7, this%nts - 6
      if ( this%ho(i) == 1 ) then
        do j = i - 6, i + 6 
          print '(a, a, i1, a, i1, a, i1, a)', &
            this%ts(j)%date//' ('//LAB_WDAY(this%ts(j)%dow)//')', &
            ', ho:', this%ho(j), ', bw:', this%bw(j), ', af:', this%af(j), ' '//trim(this%ts(j)%nm)
        end do
        print *, repeat('-', 30)
      end if
    end do
#endif

    __LOG__( 'E: make_calendar' )

  end subroutine make_calendar

  subroutine download_holidays ( dir )

    character(*), intent(in) :: dir

    __LOG__( 'S: download_holidays' )

    !
    ! Change encoding SHIFT-JIS to UTF-8
    !
    ! [Cabinet Office] https://www8.cao.go.jp/chosei/shukujitsu/syukujitsu.csv

#ifdef release
    __EXEC__( 'curl https://www8.cao.go.jp/chosei/shukujitsu/syukujitsu.csv -o '//trim(dir)//'/syukujitsu.csv' )
    __INFO__( paste( 'National Holidays Calendar has been successfully downloaded.', 'sendmail' ) )
#endif

    __EXEC__( 'iconv -f sjis -t utf-8 '//trim(dir)//'/syukujitsu.csv --output '//trim(dir)//'/syukujitsu_utf8.csv' )

    !
    ! Make '/' to space so that program can read them with *
    !
    __EXEC__('sed -i "s!/! !g" '//trim(dir)//'/syukujitsu_utf8.csv' )

    __LOG__( 'E: download_holidays' )

  end subroutine download_holidays

  subroutine init_holidays ( this, dir )

    class(ho_ty), intent(inout) :: this
    character(*), intent(in)    :: dir 
    character(50)               :: cnms(2)
    character(50), allocatable  :: nms(:)
    integer                     :: i, u

    __LOG__( 'S: init_holidays' )

    !
    ! Read calendar data
    !
    call logger%open ( __FILE__, __LINE__, newunit = u, &
      file = trim(dir)//'/syukujitsu_utf8.csv', status = 'old' )
    this%nts = count_rows ( u ) - 1
    allocate ( this%ts(this%nts), nms(this%nts) )
    read ( u, '(a, a)' ) cnms
    do i = 1, this%nts
      read ( u, * ) this%ts(i)%yr, this%ts(i)%mo, this%ts(i)%dy, nms(i)
    end do
    close ( u )

    this%ts = strptime ( get_datetime ( yr0 = this%ts%yr, &
                                        mo0 = this%ts%mo, &
                                        dy0 = this%ts%dy ) )
    this%ts%nm = nms

    ! Until 2019, Gym Day (体育の日)
    do i = 1, this%nts
      if ( index( this%ts(i)%nm, '体育の日' ) > 0 ) then
        __LOG__( paste ( 'Renamed: 体育の日 -> スポーツの日 on ', this%ts(i)%date ) )
        this%ts(i)%nm = 'スポーツの日'
      end if 
    end do

    __LOG__( 'E: init_holidays' )

  end subroutine init_holidays

  function count_rows ( u ) result ( nr )
    integer, intent(in) :: u
    integer             :: nr
    character(255)      :: iomsg
    integer             :: iostat
    nr = 0
    rewind(u)
    do
      read ( u, '()', end = 10, iostat = iostat, iomsg = iomsg )
      if ( iostat /= 0 ) then
        nr = 0
        __ERROR__( paste ( 'count_rows, iostat=', iostat, ', iomsg: ', trim(iomsg) ) )
        return
      end if
      nr = nr + 1
    end do
    10 rewind (u)
  end function

  pure function make_csv ( vals ) result ( csv )
    character(*), intent(in) :: vals(:) 
    character(80*size(vals)) :: csv
    integer i, n
    n = size(vals)
    csv = trim(vals(1))
    do i = 2, n
      csv = trim(csv)//','//trim(vals(i))
    end do
  end function

end module

module time_utilities

contains

subroutine date_from_since(since_date, sec_since, current_date)

! create date string from since_date date string and number of seconds since
! offset_ss can be used to adjust for time zone

implicit none
character*19 :: since_date, current_date  ! format: yyyy-mm-dd hh:nn:ss
double precision      :: sec_since
!li xu over the integer32 limit, change to integer64

integer*8     :: count_sec, count_sav
integer*8     :: since_yyyy, since_mm, since_dd, since_hh, since_nn, since_ss
integer      :: current_yyyy, current_mm, current_dd, current_hh, current_nn, current_ss
logical      :: leap_year = .false.
integer      :: num_leap = 0
integer*8      :: iyyyy, imm, limit
integer, dimension(12), parameter :: days_in_mm = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  current_date = "xxxx-xx-xx xx:xx:xx"

  limit = huge(1)
  if(sec_since > limit .or. sec_since < 0) stop "not capable of dealing with sec_since"

  read(since_date( 1: 4),  '(i4)') since_yyyy
  read(since_date( 6: 7),  '(i2)') since_mm
  read(since_date( 9:10),  '(i2)') since_dd
  read(since_date(12:13),  '(i2)') since_hh
  read(since_date(15:16),  '(i2)') since_nn
  read(since_date(18:19),  '(i2)') since_ss
  
! reset to beginning of since_year

  count_sec = 0
  if(mod(since_yyyy,4) == 0) leap_year = .true.
  
  do imm = 1,since_mm-1
    count_sec = count_sec - days_in_mm(imm)*86400
  end do

  if(leap_year .and. since_mm > 2) count_sec = count_sec - 86400
  count_sec = count_sec - (since_dd - 1) * 86400
  count_sec = count_sec - (since_hh) * 3600
  count_sec = count_sec - (since_nn) * 60
  count_sec = count_sec - (since_ss)
  
! not worrying about the complexity of non-recent leap years 

! find the year

  current_yyyy = since_yyyy

  do while (count_sec <= sec_since)
    count_sav = count_sec
    if(mod(current_yyyy,4) == 0) then
      count_sec = count_sec + 366*86400
    else
      count_sec = count_sec + 365*86400
    end if
    current_yyyy = current_yyyy + 1
  end do
  
  count_sec = count_sav
  current_yyyy = current_yyyy - 1
  
! find the month

  current_mm = 0!since_mm

  leap_year = .false.
  if(mod(current_yyyy,4) == 0) leap_year = .true.

  do while (count_sec <= sec_since)
    count_sav = count_sec
    current_mm = current_mm + 1
    if(leap_year .and. current_mm == 2) then
      count_sec = count_sec + 29*86400
    else
      count_sec = count_sec + days_in_mm(current_mm)*86400
    end if
  end do
  
  count_sec = count_sav
  
! find the day

  current_dd = 0!since_dd

  do while (count_sec <= sec_since)
    current_dd = current_dd + 1
    count_sav = count_sec
    count_sec = count_sec + 86400
  end do
  
  count_sec = count_sav
  
! find the hour

  current_hh = -1!since_hh

  do while (count_sec <= sec_since)
    current_hh = current_hh + 1
    count_sav = count_sec
    count_sec = count_sec + 3600
  end do
  
  count_sec = count_sav
  
! find the minute

  current_nn = -1!since_nn

  do while (count_sec <= sec_since)
    current_nn = current_nn + 1
    count_sav = count_sec
    count_sec = count_sec + 60
  end do
  
  count_sec = count_sav
  
  current_ss = sec_since - count_sec

  write(current_date( 1: 4),  '(i4.4)') current_yyyy
  write(current_date( 6: 7),  '(i2.2)') current_mm
  write(current_date( 9:10),  '(i2.2)') current_dd
  write(current_date(12:13),  '(i2.2)') current_hh
  write(current_date(15:16),  '(i2.2)') current_nn
  write(current_date(18:19),  '(i2.2)') current_ss

end subroutine date_from_since
  
subroutine calc_sec_since(since_date, current_date, offset_ss, sec_since)

! calculate number of seconds between since_date and current_date

double precision      :: sec_since
character*19 :: since_date, current_date  ! format: yyyy-mm-dd hh:nn:ss
integer      :: offset_ss
integer      :: since_yyyy, since_mm, since_dd, since_hh, since_nn, since_ss
integer      :: current_yyyy, current_mm, current_dd, current_hh, current_nn, current_ss
logical      :: leap_year = .false.
integer      :: iyyyy, imm
integer, dimension(12), parameter :: days_in_mm = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  sec_since = 0

  read(since_date( 1: 4),  '(i4)') since_yyyy
  read(since_date( 6: 7),  '(i2)') since_mm
  read(since_date( 9:10),  '(i2)') since_dd
  read(since_date(12:13),  '(i2)') since_hh
  read(since_date(15:16),  '(i2)') since_nn
  read(since_date(18:19),  '(i2)') since_ss

  read(current_date( 1: 4),  '(i4)') current_yyyy
  read(current_date( 6: 7),  '(i2)') current_mm
  read(current_date( 9:10),  '(i2)') current_dd
  read(current_date(12:13),  '(i2)') current_hh
  read(current_date(15:16),  '(i2)') current_nn
  read(current_date(18:19),  '(i2)') current_ss

! not worrying about the complexity of non-recent leap years 

! calculate number of seconds in all years  
  do iyyyy = since_yyyy, current_yyyy
    if(mod(iyyyy,4) == 0) then
      sec_since = sec_since + 366*86400
    else
      sec_since = sec_since + 365*86400
    end if
  end do
  
! remove seconds from since_year 
  if(mod(since_yyyy,4) == 0) leap_year = .true.
  
  do imm = 1,since_mm-1
    sec_since = sec_since - days_in_mm(imm)*86400
  end do

  if(leap_year .and. since_mm > 2) sec_since = sec_since - 86400
  
  sec_since = sec_since - (since_dd - 1) * 86400
  
  sec_since = sec_since - (since_hh) * 3600

  sec_since = sec_since - (since_nn) * 60

  sec_since = sec_since - (since_ss)
  
! remove seconds in current_year 
  leap_year = .false.
  if(mod(current_yyyy,4) == 0) leap_year = .true.
  
  do imm = current_mm+1, 12
    sec_since = sec_since - days_in_mm(imm)*86400
  end do
  if(leap_year .and. current_mm < 3) sec_since = sec_since - 86400
  
  sec_since = sec_since - (days_in_mm(current_mm) - current_dd) * 86400
  
  sec_since = sec_since - (23 - current_hh) * 3600

  sec_since = sec_since - (59 - current_nn) * 60

  sec_since = sec_since - (60 - current_ss)
  
  sec_since = sec_since + offset_ss
  
end subroutine calc_sec_since

end module time_utilities

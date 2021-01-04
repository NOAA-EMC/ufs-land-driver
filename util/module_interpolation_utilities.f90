module interpolation_utilities

contains

subroutine interpolate_monthly(now_time, vector_length, monthly_var, interp_var)

use time_utilities

implicit none

double precision                  :: now_time
integer                           :: vector_length
real, dimension(vector_length,12) :: monthly_var
real, dimension(vector_length)    :: interp_var

character*19        :: before_date, after_date, current_date  ! format: yyyy-mm-dd hh:nn:ss
double precision    :: before_time, after_time
real                :: before_weight, after_weight
integer             :: before_mm, after_mm, iloop
integer             :: current_yyyy, current_mm, current_dd

! get the current date string assuming reference_date

call date_from_since("1970-01-01 00:00:00", now_time, current_date)

! assume the monthly data are valid on the 15th

before_date = current_date(1:7)//"-15 00:00:00"
after_date  = current_date(1:7)//"-15 00:00:00"

read(current_date( 1: 4),  '(i4)') current_yyyy
read(current_date( 6: 7),  '(i2)') current_mm
read(current_date( 9:10),  '(i2)') current_dd

if(current_dd < 15) then
  if(current_mm == 1) then
    write(before_date( 1: 4),  '(i4.4)') current_yyyy - 1
    write(before_date( 6: 7),  '(i2.2)') 12
  else
    write(before_date( 6: 7),  '(i2.2)') current_mm - 1
  end if
else
  if(current_mm == 12) then
    write(after_date( 1: 4),  '(i4.4)') current_yyyy + 1
    write(after_date( 6: 7),  '(i2.2)') 1
  else
    write(after_date( 6: 7),  '(i2.2)') current_mm + 1
  end if
end if

! get the time the month before and after assuming reference date

call calc_sec_since("1970-01-01 00:00:00",before_date,0,before_time)
call calc_sec_since("1970-01-01 00:00:00", after_date,0, after_time)

if(before_time > now_time .or. after_time < now_time) &
   stop "problem with time in interpolate_monthly"

 after_weight = (now_time - before_time) / (after_time - before_time)
before_weight = 1.0 - after_weight

read(before_date( 6: 7),  '(i2)') before_mm
read( after_date( 6: 7),  '(i2)')  after_mm

do iloop = 1, vector_length

  interp_var(iloop) = before_weight * monthly_var(iloop,before_mm) + &
                       after_weight * monthly_var(iloop,after_mm)

end do

end subroutine interpolate_monthly

subroutine interpolate_linear(now_time, last_time, next_time, vector_length, &
                              last_var, next_var, interp_var)

implicit none

double precision                  :: now_time, last_time, next_time
integer                           :: vector_length
real, dimension(vector_length)    :: last_var, next_var, interp_var

real                :: last_weight, next_weight

if(last_time > now_time .or. next_time < now_time) &
   stop "problem with time in interpolate_monthly"

 next_weight = (now_time - last_time) / (next_time - last_time)
 last_weight = 1.0 - next_weight

  interp_var = last_weight * last_var + next_weight * next_var

end subroutine interpolate_linear

end module interpolation_utilities


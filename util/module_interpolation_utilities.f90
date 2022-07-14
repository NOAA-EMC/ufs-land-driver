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

call date_from_since("1800-01-01 00:00:00", now_time, current_date)

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

call calc_sec_since("1800-01-01 00:00:00",before_date,0,before_time)
call calc_sec_since("1800-01-01 00:00:00", after_date,0, after_time)

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

real                              :: last_weight, next_weight


if(last_time > now_time .or. next_time < now_time) &
   stop "problem with time in interpolate_linear"

 next_weight = (now_time - last_time) / (next_time - last_time)
 last_weight = 1.0 - next_weight

  interp_var = last_weight * last_var + next_weight * next_var

end subroutine interpolate_linear

subroutine interpolate_gswp3_zenith(now_time, last_time, vector_length, &
                                    latitude, longitude, timestep,      &
                                    last_var, interp_var)

use cosine_zenith

implicit none

double precision                  :: now_time, last_time
integer                           :: vector_length, timestep
real, dimension(vector_length)    :: latitude, longitude, last_var, interp_var

double precision                  :: calc_time
real, dimension(vector_length)    :: cosz1, cosz2, cosz3, coszavg, cosz

real                              :: julian
real, parameter                   :: critical_energy = 50.0
real, parameter                   :: critical_cosz   = 0.0001

  if(now_time < last_time) &
     stop "problem with time in interpolate_zenith_angle"

! calculate zenith angle between the model timesteps for this forcing average

  calc_time = last_time + 0.5*timestep
  call calc_cosine_zenith(calc_time, vector_length, latitude, longitude, cosz1, julian)

  calc_time = last_time + 0.5*timestep + timestep
  call calc_cosine_zenith(calc_time, vector_length, latitude, longitude, cosz2, julian)

  calc_time = last_time + 0.5*timestep + 2*timestep
  call calc_cosine_zenith(calc_time, vector_length, latitude, longitude, cosz3, julian)

  where(cosz1 <= critical_cosz) cosz1 = 0.0
  where(cosz2 <= critical_cosz) cosz2 = 0.0
  where(cosz3 <= critical_cosz) cosz3 = 0.0
  
  coszavg = (cosz1 + cosz2 + cosz3) / 3.0
  
  interp_var = 0.0

! find which time to interpolate

  if(now_time == last_time) then
    cosz = cosz1
  elseif(now_time == last_time + real(timestep)) then
    cosz = cosz2
  elseif(now_time == last_time + real(2*timestep)) then
    cosz = cosz3
  else
    stop "problem interpolating in gswp3_zenith"
  end if

  where(coszavg > 0.0) interp_var = cosz/coszavg*last_var

!  print*,'------interpolate_gswp3_zenith------'
!  print*,'now_time,last_time,now-last'
!  print*,now_time/3600,last_time/3600,(now_time-last_time)/3600
!  print*,'cosz1,cosz2,cosz3,coszavg,cosz,last_var,interp_var'
!  print*,cosz1(1700),cosz2(1700),cosz3(1700),coszavg(1700),&
!	cosz(1700),last_var(1700),interp_var(1700)

! final data check

where(interp_var < 0.0 .or. cosz <= 0.0) interp_var = 0.0


end subroutine interpolate_gswp3_zenith

subroutine interpolate_gswp3_zenith1(now_time, last_time, next_time, vector_length, &
                                    latitude, longitude, timestep,      &
                                    last_var, next_var, interp_var)

use cosine_zenith

implicit none

double precision                  :: now_time, last_time, next_time
integer                           :: vector_length, timestep, num_timestep
real, dimension(vector_length)    :: latitude, longitude, last_var,next_var, interp_var

double precision                  :: calc_time, tdiff
real, dimension(vector_length)    :: cosz_last, cosz_now, cosz_next

real                              :: julian
real, parameter                   :: critical_energy = 50.0
real, parameter                   :: critical_cosz   = 0.0001

!  print*,'in SUB2, now_time,last_time:',now_time,last_time
  if(now_time < last_time) &
     stop "in SUB2: problem with time in interpolate_zenith_angle"

! Hailan Wang 6/17/2022
! num_timestep: check temporal resolution of atmospheric forcing (e.g., 3-hourly, 6-hourly)
! tdiff: time difference between now_time and last_time / timestep 
  num_timestep = ( next_time - last_time ) / timestep
  tdiff = ( now_time - last_time ) / timestep

! calculate zenith angle at now_time (0.5 hour forward)

  calc_time = last_time + 0.5*timestep
  call calc_cosine_zenith(calc_time, vector_length, latitude, longitude, cosz_last, julian)

  calc_time = last_time + (0.5 + tdiff ) *timestep
  call calc_cosine_zenith(calc_time, vector_length, latitude, longitude, cosz_now, julian)

  calc_time = last_time + (0.5 + num_timestep) *timestep 
  call calc_cosine_zenith(calc_time, vector_length, latitude, longitude, cosz_next, julian)

  where(cosz_last <= critical_cosz) cosz_last = 0.0
  where(cosz_now  <= critical_cosz) cosz_now = 0.0
  where(cosz_next <= critical_cosz) cosz_next = 0.0
  
  interp_var = 0.0

! for interpolation, only use max(last_var, next_var), scaled by cosz/cosz_last(or next)
!
  where(last_var>next_var.and.last_var>0) interp_var=cosz_now/cosz_last*last_var
  where(last_var<next_var.and.next_var>0) interp_var=cosz_now/cosz_next*next_var

!  print*,'------interpolate_gswp3_zenith------'
!  print*,'now_time,last_time,now-last'
!  print*,now_time/3600,last_time/3600,(now_time-last_time)/3600
!  print*,'cosz_last,cosz_now,cosz_next,last_var,next_var,interp_var'
!  print*,cosz_last(1700),cosz_now(1700),cosz_next(1700),last_var(1700),&
!	next_var(1700),interp_var(1700)

! final data check

where(interp_var < 0.0 .or. cosz_now <= 0.0) interp_var = 0.0

end subroutine interpolate_gswp3_zenith1

end module interpolation_utilities


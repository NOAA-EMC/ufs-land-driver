module cosine_zenith

contains

subroutine calc_cosine_zenith(reference_date, now_time, im, latitude, longitude, cosz, julian)

use time_utilities

implicit none

double precision    :: now_time, sec_since
integer             :: im
real, dimension(im) :: latitude, longitude
real, dimension(im) :: cosz
real                :: julian
real                :: obecl, sinob, sxlong, arg, tloctim, hrang, declin
integer             :: ihour, iminute, isecond, iloc
character*19        :: now_date  ! format: yyyy-mm-dd hh:nn:ss
character*19        :: reference_date

real, parameter :: degrad = 3.14159265/180.
real, parameter :: dpd    = 360./365.

call date_from_since(reference_date, now_time, now_date)

call calc_sec_since(now_date(1:4)//"-01-01 00:00:00", now_date, 0, sec_since)

read(now_date(12:13), *) ihour
read(now_date(15:16), *) iminute
read(now_date(18:19), *) isecond

julian = sec_since/86400.0

!-----obecl : obliquity = 23.5 degree.

obecl = 23.5*degrad
sinob = sin(obecl)

!-----calculate longitude of the sun from vernal equinox:

if(julian >= 80.) sxlong = dpd*(julian -80.)*degrad
if(julian <  80.) sxlong = dpd*(julian+285.)*degrad
arg = sinob*sin(sxlong)
declin = asin(arg)

do iloc = 1, im
  tloctim = real(ihour) + real(iminute)/60.0 + real(isecond)/3600.0 + longitude(iloc)/15.0 ! local time in hours
  tloctim = amod(tloctim+24.0, 24.0)
  hrang = 15.*(tloctim-12.)*degrad
  cosz(iloc) = sin(latitude(iloc)*degrad)*sin(declin)+cos(latitude(iloc)*degrad)*cos(declin)*cos(hrang)
end do

end subroutine calc_cosine_zenith

end module cosine_zenith

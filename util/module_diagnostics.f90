module diagnostics

contains

subroutine calc_dewpoint(vector_length,specific_humidity, surface_pressure, dewpoint)

implicit none

integer                        :: vector_length
real, dimension(vector_length) :: specific_humidity
real, dimension(vector_length) :: surface_pressure
real, dimension(vector_length) :: dewpoint
real, dimension(vector_length) :: vapor_pressure      ! [mb]

real, parameter     :: eps = 0.622
real, parameter     :: vapor_pressure_sat_0C = 6.112  ! [mb]

vapor_pressure = surface_pressure / 100.0 * specific_humidity / (specific_humidity*(1-eps) + eps)

dewpoint = 243.5 * log(vapor_pressure/vapor_pressure_sat_0C) / (17.67 - log(vapor_pressure/vapor_pressure_sat_0C))
dewpoint = dewpoint + 273.15

end subroutine calc_dewpoint

end module diagnostics

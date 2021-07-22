module error_handling

contains

subroutine handle_err(status)
  use netcdf
  integer, intent ( in) :: status
 
  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    stop "Stopped"
  end if
end subroutine handle_err

end module error_handling

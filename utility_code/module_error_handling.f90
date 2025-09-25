module error_handling

contains

subroutine handle_err(status,sometext)
  use netcdf
  integer, intent ( in) :: status
  character(len=*), optional :: sometext
 
  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    if(present(sometext)) print *, "Working on: ",trim(sometext)
    stop "Stopped"
  end if
end subroutine handle_err

end module error_handling

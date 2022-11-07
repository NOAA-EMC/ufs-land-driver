module ufsLandNetcdf

use mpi
use netcdf

contains

  subroutine Define1dReal(indata,ncid,vartype,dim_id1,dim_id2)
  
  use ufsLandGenericType, only : real1d
  use error_handling, only : handle_err

  type(real1d) :: indata

  integer :: ncid, varid, status, vartype, dim_id1, dim_id2

  status = nf90_def_var(ncid, indata%name, vartype, (/dim_id1,dim_id2/), varid)
   if (status /= nf90_noerr) call handle_err(status,indata%name)
    status = nf90_put_att(ncid, varid, "long_name", trim(indata%long_name))
     if (status /= nf90_noerr) call handle_err(status,indata%name)
    status = nf90_put_att(ncid, varid, "units", trim(indata%units))
     if (status /= nf90_noerr) call handle_err(status,indata%name)

  end subroutine Define1dReal
  
  subroutine Define2dReal(indata,ncid,vartype,dim_id1,dim_id2,dim_id3)
  
  use ufsLandGenericType, only : real2d
  use error_handling, only : handle_err

  type(real2d) :: indata

  integer :: ncid, varid, status, vartype, dim_id1, dim_id2, dim_id3

  status = nf90_def_var(ncid, indata%name, vartype, (/dim_id1,dim_id2,dim_id3/), varid)
   if (status /= nf90_noerr) call handle_err(status,indata%name)
    status = nf90_put_att(ncid, varid, "long_name", trim(indata%long_name))
     if (status /= nf90_noerr) call handle_err(status,indata%name)
    status = nf90_put_att(ncid, varid, "units", trim(indata%units))
     if (status /= nf90_noerr) call handle_err(status,indata%name)

  end subroutine Define2dReal
  
  subroutine Define1dInt(indata,ncid,vartype,dim_id1,dim_id2)
  
  use ufsLandGenericType, only : int1d
  use error_handling, only : handle_err

  type(int1d) :: indata

  integer :: ncid, varid, status, vartype, dim_id1, dim_id2

  status = nf90_def_var(ncid, indata%name, vartype, (/dim_id1,dim_id2/), varid)
   if (status /= nf90_noerr) call handle_err(status,indata%name)
    status = nf90_put_att(ncid, varid, "long_name", trim(indata%long_name))
     if (status /= nf90_noerr) call handle_err(status,indata%name)
    status = nf90_put_att(ncid, varid, "units", trim(indata%units))
     if (status /= nf90_noerr) call handle_err(status,indata%name)

  end subroutine Define1dInt
  
  subroutine Read1dReal(indata,ncid,start,count)
  
  use ufsLandGenericType, only : real1d
  use error_handling, only : handle_err

  type(real1d) :: indata

  integer :: ncid, varid, status, start(2), count(2)

  status = nf90_inq_varid(ncid, trim(indata%name), varid)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  status = nf90_get_var(ncid, varid, indata%data,start = start, count = count)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  end subroutine Read1dReal
  
  subroutine Write1dReal(indata,ncid,start,count)
  
  use ufsLandGenericType, only : real1d
  use error_handling, only : handle_err

  type(real1d) :: indata

  integer :: ncid, varid, status, start(2), count(2)

  status = nf90_inq_varid(ncid, trim(indata%name), varid)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  status = nf90_var_par_access(ncid, varid, NF90_COLLECTIVE)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  status = nf90_put_var(ncid, varid, indata%data,start = start, count = count)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  end subroutine Write1dReal
  
  subroutine Read2dReal(indata,ncid,start,count)
  
  use ufsLandGenericType, only : real2d
  use error_handling, only : handle_err

  type(real2d) :: indata

  integer :: ncid, varid, status, start(3), count(3)

  status = nf90_inq_varid(ncid, trim(indata%name), varid)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  status = nf90_get_var(ncid, varid, indata%data,start = start, count = count)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  end subroutine Read2dReal
  
  subroutine Write2dReal(indata,ncid,start,count)
  
  use ufsLandGenericType, only : real2d
  use error_handling, only : handle_err

  type(real2d) :: indata

  integer :: ncid, varid, status, start(3), count(3)

  status = nf90_inq_varid(ncid, trim(indata%name), varid)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  status = nf90_var_par_access(ncid, varid, NF90_COLLECTIVE)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  status = nf90_put_var(ncid, varid, indata%data,start = start, count = count)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  end subroutine Write2dReal
  
  subroutine Read1dInt(indata,ncid,start,count)
  
  use ufsLandGenericType, only : int1d
  use error_handling, only : handle_err

  type(int1d) :: indata

  integer :: ncid, varid, status, start(2), count(2)

  status = nf90_inq_varid(ncid, trim(indata%name), varid)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  status = nf90_get_var(ncid, varid, indata%data,start = start, count = count)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  end subroutine Read1dInt
  
  subroutine Write1dInt(indata,ncid,start,count)
  
  use ufsLandGenericType, only : int1d
  use error_handling, only : handle_err

  type(int1d) :: indata

  integer :: ncid, varid, status, start(2), count(2)

  status = nf90_inq_varid(ncid, trim(indata%name), varid)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  status = nf90_var_par_access(ncid, varid, NF90_COLLECTIVE)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  status = nf90_put_var(ncid, varid, indata%data,start = start, count = count)
   if (status /= nf90_noerr) call handle_err(status,indata%name)

  end subroutine Write1dInt
  
end module ufsLandNetcdf

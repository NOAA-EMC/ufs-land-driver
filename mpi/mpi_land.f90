module module_mpi_land

  use mpi

  implicit none
  
  type mpi_land_type
    integer :: my_id
    integer :: numprocs
    integer :: global_nlocations
    integer :: local_nlocations
    integer :: location_start
    integer :: location_end
  end type
  
  contains

  subroutine mpi_land_init(global_nlocations_in, mpiland)

    implicit none
 
    integer             :: global_nlocations_in
    type(mpi_land_type) :: mpiland
    integer             :: ierr
    logical             :: mpi_inited
    integer             :: i, overlap, location_start_shift

    call mpi_initialized( mpi_inited, ierr )
    if ( .not. mpi_inited ) then
      call mpi_init( ierr )  
    endif
    
    call mpi_comm_rank( MPI_COMM_WORLD, mpiland%my_id, ierr )
    call mpi_comm_size( MPI_COMM_WORLD, mpiland%numprocs, ierr )

    mpiland%global_nlocations = global_nlocations_in

    mpiland%local_nlocations = int(mpiland%global_nlocations / mpiland%numprocs)
    mpiland%location_start = mpiland%local_nlocations * mpiland%my_id + 1 

    overlap = mod(mpiland%global_nlocations, mpiland%numprocs)

    location_start_shift = 0

    if(overlap /= 0) then
      do i = 0, overlap - 1

        if(mpiland%my_id == i ) then  ! for the overlap procs add 1 to the number of locations
          mpiland%local_nlocations = mpiland%local_nlocations + 1
        end if
 
        if(mpiland%my_id > i ) then  ! for the overlap procs shift the start locations
          location_start_shift = location_start_shift + 1
        end if

      end do
    end if

    mpiland%location_start = mpiland%location_start + location_start_shift
        
    mpiland%location_end = mpiland%location_start + mpiland%local_nlocations - 1

    call mpi_land_sync()

    return

  end subroutine mpi_land_init
  
  subroutine mpi_land_sync()
    implicit none
    integer ierr
    
    call mpi_barrier( MPI_COMM_WORLD ,ierr)
      if(ierr .ne. 0) call mpi_land_abort()
    
    return
  end subroutine mpi_land_sync

  subroutine mpi_land_abort()
    implicit none
    integer ierr
    
    call mpi_abort(MPI_COMM_WORLD,1,ierr)
  
  end subroutine mpi_land_abort

  subroutine mpi_land_finalize()
    implicit none
    integer ierr
    
    call mpi_finalize(ierr)
  
  end subroutine mpi_land_finalize

end module module_mpi_land




module module_mpi_land

  use mpi

  implicit none
  
  type mpi_land_type
    integer :: group_id     
    integer :: comm_group    !ids below are within this group
    integer :: my_id
    integer :: numprocs
    integer :: global_nlocations
    integer :: local_nlocations
    integer :: location_start
    integer :: location_end
  end type
  
  contains

  subroutine mpi_land_init(num_groups, global_nlocations_in, mpiland)

    implicit none

    integer             :: num_groups
    integer             :: global_nlocations_in
    type(mpi_land_type) :: mpiland

    integer             :: ierr
    logical             :: mpi_inited
    integer             :: i, overlap, location_start_shift
    integer             :: tot_num_procs, group_size, my_global_rank, extra_proc

    call mpi_initialized( mpi_inited, ierr )
    if ( .not. mpi_inited ) then
      call mpi_init( ierr )  
    endif
    
    call mpi_comm_rank( MPI_COMM_WORLD, tot_num_procs, ierr )
    call mpi_comm_size( MPI_COMM_WORLD, my_global_rank, ierr )
 
    group_size = tot_num_procs / num_groups  ! num procs in a group 
    extra_proc = MOD(tot_num_procs, num_groups) 

    mpiland%group_id = my_global_rank / group_size  ! group for proc.  
    if (my_global_rank >  tot_num_procs - extra_proc - 1) then  
        mpiland%group_id = MOD(my_global_rank, group_size)     !add extra processes to first np_ext groups
        group_size = group_size + 1                            !first np_ext groups have an extra 1 proc
    endif

    call mpi_comm_split(MPI_COMM_WORLD, mpiland%group_id, my_global_rank, mpiland%comm_group, ierr)
    call mpi_comm_rank(mpiland%comm_group, mpiland%my_id, ierr )
    call mpi_comm_size(mpiland%comm_group, mpiland%numprocs, ierr )

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




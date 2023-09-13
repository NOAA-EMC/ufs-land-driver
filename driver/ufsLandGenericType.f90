module ufsLandGenericType

use machine , only : kind_phys

implicit none

type :: int1d

  integer             , allocatable :: data (:)                    ! 
  integer             , allocatable :: daily_mean (:)              ! 
  integer             , allocatable :: monthly_mean (:)            ! 
  integer             , allocatable :: solar_noon (:)              ! 
  logical                           :: output_flag       = .false. ! 
  logical                           :: restart_flag      = .false. ! 
  logical                           :: daily_mean_flag   = .false. ! 
  logical                           :: monthly_mean_flag = .false. ! 
  logical                           :: solar_noon_flag   = .false. ! 
  logical                           :: diurnal_flag      = .false. ! 
  character*128                     :: name                        ! 
  character*128                     :: long_name                   ! 
  character*128                     :: units                       ! 
  
end type int1d

type :: real1d

  real(kind=kind_phys), allocatable :: data (:)                    ! 
  real(kind=kind_phys), allocatable :: daily_mean (:)              ! 
  real(kind=kind_phys), allocatable :: monthly_mean (:)            ! 
  real(kind=kind_phys), allocatable :: solar_noon (:)              ! 
  logical                           :: output_flag       = .false. ! 
  logical                           :: restart_flag      = .false. ! 
  logical                           :: daily_mean_flag   = .false. ! 
  logical                           :: monthly_mean_flag = .false. ! 
  logical                           :: solar_noon_flag   = .false. ! 
  logical                           :: diurnal_flag      = .false. ! 
  character*128                     :: name                        ! 
  character*128                     :: long_name                   ! 
  character*128                     :: units                       ! 
  
end type real1d

type :: int2d

  integer             , allocatable :: data (:,:)                  ! 
  integer             , allocatable :: daily_mean (:,:)            ! 
  integer             , allocatable :: monthly_mean (:,:)          ! 
  integer             , allocatable :: solar_noon (:,:)            ! 
  logical                           :: output_flag       = .false. ! 
  logical                           :: restart_flag      = .false. ! 
  logical                           :: daily_mean_flag   = .false. ! 
  logical                           :: monthly_mean_flag = .false. ! 
  logical                           :: solar_noon_flag   = .false. ! 
  logical                           :: diurnal_flag      = .false. ! 
  character*128                     :: name                        ! 
  character*128                     :: long_name                   ! 
  character*128                     :: units                       ! 
  
end type int2d

type :: real2d

  real(kind=kind_phys), allocatable :: data (:,:)                  ! 
  real(kind=kind_phys), allocatable :: daily_mean (:,:)            ! 
  real(kind=kind_phys), allocatable :: monthly_mean (:,:)          ! 
  real(kind=kind_phys), allocatable :: solar_noon (:,:)            ! 
  logical                           :: output_flag       = .false. ! 
  logical                           :: restart_flag      = .false. ! 
  logical                           :: daily_mean_flag   = .false. ! 
  logical                           :: monthly_mean_flag = .false. ! 
  logical                           :: solar_noon_flag   = .false. ! 
  logical                           :: diurnal_flag      = .false. ! 
  character*128                     :: name                        ! 
  character*128                     :: long_name                   ! 
  character*128                     :: units                       ! 
  
end type real2d

end module ufsLandGenericType

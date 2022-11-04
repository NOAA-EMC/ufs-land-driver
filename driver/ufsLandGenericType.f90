module ufsLandGenericType

use machine , only : kind_phys

implicit none

type :: int1d

  integer             , allocatable :: data (:)               ! 
  logical                           :: output_flag  = .false. ! 
  logical                           :: restart_flag = .false. ! 
  character*128                     :: name                   ! 
  character*128                     :: long_name              ! 
  character*128                     :: units                  ! 
  
end type int1d

type :: real1d

  real(kind=kind_phys), allocatable :: data (:)               ! 
  logical                           :: output_flag  = .false. ! 
  logical                           :: restart_flag = .false. ! 
  character*128                     :: name                   ! 
  character*128                     :: long_name              ! 
  character*128                     :: units                  ! 
  
end type real1d

type :: int2d

  integer             , allocatable :: data (:,:)             ! 
  logical                           :: output_flag  = .false. ! 
  logical                           :: restart_flag = .false. ! 
  character*128                     :: name                   ! 
  character*128                     :: long_name              ! 
  character*128                     :: units                  ! 
  
end type int2d

type :: real2d

  real(kind=kind_phys), allocatable :: data (:,:)             ! 
  logical                           :: output_flag  = .false. ! 
  logical                           :: restart_flag = .false. ! 
  character*128                     :: name                   ! 
  character*128                     :: long_name              ! 
  character*128                     :: units                  ! 
  
end type real2d

end module ufsLandGenericType

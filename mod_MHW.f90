!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : mod_MHWs                                                         !
!                                                                              !
!   PURPOSE : Calculate the MHW's various properties                           !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        MODULE mod_MHWs

            USE mod_netCDF_IO
            USE netcdf

            IMPLICIT NONE

            INTEGER  :: window 
            REAL(KIND=8)  :: percent

            REAL(KIND=8),ALLOCATABLE,DIMENSION(:)      ::  lat, lon, time  
            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  sst_data, sst_tmp
            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  sst_clim, sst_percentile

            SAVE

          CONTAINS

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : MHW_setup                                                     !
!                                                                              !
!   PURPOSE : Basic setup for the MHWs calculation                             !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE MHW_setup(Nx,Ny)

              IMPLICIT NONE            
              INTEGER, INTENT(IN)  ::  Nx, Ny
              ALLOCATE( sst_clim(1:Nx,1:Ny,1:365) ) 
              ALLOCATE( sst_percentile(1:Nx,1:Ny,1:365) )

              window   =  5
              percent  = 90.0

          END SUBROUTINE MHW_setup

        END MODULE mod_MHWs

!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : mod_netCDF_IO.f90                                                !
!                                                                              !
!   PURPOSE : Combine the PALM generated particle binary to each time file     !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        MODULE mod_netCDF_IO

            USE netcdf

            IMPLICIT NONE

            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  data_var_3d
            CHARACTER(LEN=128)  ::  path_name, file_name, dir_name, var_name

            SAVE

          CONTAINS

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : netCDF_setup                                                  !
!                                                                              !
!   PURPOSE : Initial setup for reading/writing netCDF files                   !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE netCDF_setup

              IMPLICIT NONE

          END SUBROUTINE netCDF_setup

        END MODULE mod_netCDF_IO

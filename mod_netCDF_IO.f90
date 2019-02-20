!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : mod_netCDF_IO.f90                                                !
!                                                                              !
!   PURPOSE : Read and write the netCDF files                                  !
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

              dir_name   = "DATA/OISST_v2"
              file_name  = "sst_daily_mean.1982-1984.v2.nc"
              path_name  = "./"//TRIM(dir_name)//"/"//TRIM(file_name)

              var_name   = "sst"

          END SUBROUTINE netCDF_setup

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : CHECK                                                         !
!                                                                              !
!   PURPOSE : Checking up the netcdf module's function & subroutine            !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE CHECK(status)

              IMPLICIT NONE
                  INTEGER,INTENT(IN)  ::  status
                  
                  IF (status /= NF90_NOERR) THEN
                      WRITE(*,*) TRIM(NF90_STRERROR(status))
                      STOP "STOPPED"
                  END IF

          END SUBROUTINE CHECK

        END MODULE mod_netCDF_IO

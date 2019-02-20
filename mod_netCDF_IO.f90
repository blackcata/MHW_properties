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

            INTEGER  ::  N1, N2, N3, N
            INTEGER  ::  varid, ncid
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

          END SUBROUTINE netCDF_setup

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : netCDF_read_1d                                                !
!                                                                              !
!   PURPOSE : Reading the netCDF files                                         !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE netCDF_read_1d(data_input)

              IMPLICIT NONE            
              
              REAL(KIND=8),INTENT(INOUT)  ::  data_input(1:N)

              path_name  = "./"//TRIM(dir_name)//"/"//TRIM(file_name)

              !< Open the file, NF90_NOWRITE : read-only access to files
              CALL CHECK( NF90_OPEN(path_name, NF90_NOWRITE, ncid) )

              !< Get the varid of the data variable, based on its name
              CALL CHECK( NF90_INQ_VARID(ncid, TRIM(var_name), varid) )

              !< Read the data
              CALL CHECK( NF90_GET_VAR(ncid, varid, data_input) )

              !< Close the file
              CALL CHECK( NF90_CLOSE(ncid) )

          END SUBROUTINE netCDF_read_1d

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : netCDF_read_3d                                                !
!                                                                              !
!   PURPOSE : Reading the netCDF files                                         !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE netCDF_read_3d(data_input)

              IMPLICIT NONE            
             
              REAL(KIND=8),INTENT(INOUT)  ::  data_input(1:N1,1:N2,1:N3)

              path_name  = "./"//TRIM(dir_name)//"/"//TRIM(file_name)

              !< Open the file, NF90_NOWRITE : read-only access to files
              CALL CHECK( NF90_OPEN(path_name, NF90_NOWRITE, ncid) )

              !< Get the varid of the data variable, based on its name
              CALL CHECK( NF90_INQ_VARID(ncid, TRIM(var_name), varid) )

              !< Read the data
              CALL CHECK( NF90_GET_VAR(ncid, varid, data_input) )

              !< Close the file
              CALL CHECK( NF90_CLOSE(ncid) )

          END SUBROUTINE netCDF_read_3d

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

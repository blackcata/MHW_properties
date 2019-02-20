!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : MHW_main.f90                                                     !
!                                                                              !
!   PURPOSE : Calculate the MHWs(Marine Heat Waves) various properties         !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        PROGRAM MHWs_main

            USE netcdf

            IMPLICIT NONE

            INTEGER   :: Nx, Ny, Nt, ncid, varid, i, j 
            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  data_var
            CHARACTER(LEN=128) :: path_name, file_name, dir_name, var_name

            !-------------------------------------------------------------------!
            !                         READING PROCESS                           !
            !-------------------------------------------------------------------!
            dir_name   = "DATA/OISST_v2"
            file_name  = "sst_daily_mean.1982-1984.v2.nc"
            path_name  = "./"//TRIM(dir_name)//"/"//TRIM(file_name)
            var_name   = "sst"

            Nx  =  1440
            Ny  =  720
            Nt  =  1095 

            !ALLOCATE( data_var(1:Nt,1:Ny,1:Nx) ) 
            ALLOCATE( data_var(1:Nx,1:Ny,1:Nt) ) 
            CALL CHECK( nf90_open(path_name, NF90_NOWRITE, ncid) )

            CALL CHECK( nf90_inq_varid(ncid, TRIM(var_name), varid) ) 

            CALL CHECK( nf90_get_var(ncid, varid, data_var) ) 

            CALL CHECK( nf90_close(ncid) )

            DO i = 1,Nx
              DO j = 1,Ny
                  WRITE(*,"(7(E17.10))") data_var(i,j,1:7)
              END DO 
            END DO 

            WRITE(*,*) path_name


        CONTAINS

            SUBROUTINE CHECK(status)
                INTEGER,INTENT(IN)  ::  status

                IF (status /= nf90_noerr) THEN 
                    PRINT*, TRIM(nf90_strerror(status))
                    STOP "STOPPED"
                END IF
            END SUBROUTINE CHECK
        END PROGRAM MHWs_main

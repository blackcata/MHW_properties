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
            USE mod_netCDF_IO
            USE mod_MHWs

            IMPLICIT NONE
            INTEGER :: it, i,j, yr
            REAL(KIND=8) :: N_tmp
            REAL(KIND=8),DIMENSION(:,:,:),ALLOCATABLE :: chl_clim, sst_tmp
            WRITE(*,*) " "

            !-----------------------------------------------------------------!
            !                     Read the file's dimension                   !
            !-----------------------------------------------------------------!
            N1 = 360 ; N2 = 180 ; N3 = 828
            missing    =  9.96921e+36

            dir_name   =  "DATA/ESA_CCI_V3_8day/TOTAL_REGRID"
            file_name  =  "CHL_8DAY_DAILY_1440x720.nc"

            WRITE(*,*)  "------------BASIC SETUP COMPLETED------------"
            WRITE(*,*)  " "
            
            !<Read the time dimension
            N  =  N3
            var_name   =  "time"
            ALLOCATE(  time(1:N) ) 
            CALL netCDF_read_1d( time , 1 ) 

            !<Read the latitude dimension
            N  =  N2 
            var_name   =  "lat"
            ALLOCATE(  lat(1:N) ) 
            CALL netCDF_read_1d( lat , 1 ) 

            !<Read the longitude dimension
            N  =  N1 
            var_name   =  "lon"
            ALLOCATE(  lon(1:N) ) 
            CALL netCDF_read_1d( lon , 1 ) 

            !-----------------------------------------------------------------!
            !                         Read the SST data                       !
            !-----------------------------------------------------------------!
            var_name   =  "chlor_a"

            ALLOCATE( sst_data(1:N1, 1:N2, 1:N3) ) 
            !<Read the CHL data
            CALL netCDF_read_3d(sst_data, 1, 1, 1)
            
            WRITE(*,*)  "----------READING PROCESS COMPLETED----------"
            WRITE(*,*)  " "

            !-----------------------------------------------------------------!
            !                 Calculate the 11 day window mean                !
            !-----------------------------------------------------------------!
            ALLOCATE( chl_clim(1:N1, 1:N2, 1:46) ) 
            chl_clim = 0.0

            DO j = 1,N2
              DO i = 1,N1
              print*,i,j

                  DO it = 1,46
                      N_tmp = 0.0
                      DO yr = 1,18

                          IF ( abs(sst_data(i,j,(yr-1)*46+it) - missing) > 1.0e+1 ) THEN
                              N_tmp = N_tmp + 1.0
                              chl_clim(i,j,it) =  chl_clim(i,j,it) + sst_data(i,j,(yr-1)*46 + it)

IF(i==904) WRITE(*,"(4I10.3,2X,3E17.10)") i,j,it,yr, N_tmp, sst_data(i,j,(yr-1)*46 + it),chl_clim(i,j,it)

                          ENDIF

                      END DO

                      IF (N_tmp > 1.0) chl_clim(i,j,it) = chl_clim(i,j,it) / N_tmp
                  END DO 

              END DO 
            END DO 

            DO j = 1,N2
                DO i = 1,N1

                   IF ( abs( sst_tmp(i,j,1) -  missing ) < 1.0e+1 ) THEN
                       chl_clim(i,j,:)  =  missing
                       CONTINUE
                   END IF

                END DO 
            END DO

            DO j = 1,N2
                DO i = 1,N1
                   
                   DO it = 1,46
                       IF ( chl_clim(i,j,it)  < 1.0e-5 ) THEN
                           chl_clim(i,j,it)  =  missing
                           CONTINUE
                       END IF
                   END DO 

                END DO 
            END DO

            WRITE(*,*)  "--------CALCULATING PROCESS COMPLETED--------"
            WRITE(*,*)  " "
            DEALLOCATE(sst_data)

            !-----------------------------------------------------------------!
            !                        Write the SST data                       !
            !-----------------------------------------------------------------!
            !<Basic settings for the each variables & directory 
            dir_name   =  "RESULT"
            dim1_name  =  "lon" ; dim2_name = "lat" ; dim3_name = "time"

            dim1_unit  =  "degrees_east" 
            dim2_unit  =  "degrees_north"
            dim3_unit  =  "days since 1800-01-01 00:00:00"
            
            DEALLOCATE(time)  ;  ALLOCATE(time(1:365))
            DO it = 1,46 ; time(it) = 1 + 18*(it-1) ; END DO 
            N3 = 46

            !<Write the climatological mean 
            file_name  =  "CHL_CLIM.nc"
            var_name   =  "chl_clim"
            CALL netCDF_write_3d(chl_clim,lon,lat,time)

            WRITE(*,*)  "--------WRITING PROCESS COMPLETED--------"

        END PROGRAM MHWs_main

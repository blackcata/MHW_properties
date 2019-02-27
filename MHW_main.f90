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
            INTEGER :: it
            WRITE(*,*) " "

            !-----------------------------------------------------------------!
            !                     Read the file's dimension                   !
            !-----------------------------------------------------------------!
            N1 = 1440 ; N2 = 720 ; N3 = 1095
            missing    =  -9.96921e+36

            dir_name   =  "DATA/OISST_v2"
            file_name  =  "sst_daily_mean.1982-1984.v2.nc"
            WRITE(*,*)  "------------BASIC SETUP COMPLETED------------"
            
            !<Read the time dimension
            N  =  N3
            var_name   =  "lon"
            ALLOCATE(  time(1:N) ) 
            CALL netCDF_read_1d( time ) 

            !<Read the latitude dimension
            N  =  N2 
            var_name   =  "lat"
            ALLOCATE(  lat(1:N) ) 
            CALL netCDF_read_1d( lat ) 

            !<Read the longitude dimension
            N  =  N1 
            var_name   =  "lon"
            ALLOCATE(  lon(1:N) ) 
            CALL netCDF_read_1d( lon ) 

            !-----------------------------------------------------------------!
            !                         Read the SST data                       !
            !-----------------------------------------------------------------!
            var_name   =  "sst"

            ALLOCATE( sst_data(1:N1, 1:N2, 1:N3) ) 
            !<Read the SST data
            CALL netCDF_read_3d(sst_data)
            
            WRITE(*,*)  "----------READING PROCESS COMPLETED----------"

            !-----------------------------------------------------------------!
            !                 Calculate the 11 day window mean                !
            !-----------------------------------------------------------------!
            CALL MHW_setup(N1,N2,N3)
            CALL MHW_clim_percent(N1,N2)
            CALL MHW_intensity(N1,N2)
            CALL MHW_duration(N1,N2)

            WRITE(*,*)  "--------CALCULATING PROCESS COMPLETED--------"

            !-----------------------------------------------------------------!
            !                        Write the SST data                       !
            !-----------------------------------------------------------------!
            !<Basic settings for the each variables & directory 
            dir_name   =  "RESULT"
            dim1_name  =  "lon" ; dim2_name = "lat" ; dim3_name = "time"

            !<Write the intensity contour of MHWs
            file_name  =  "OISST_v2_win_11_daily_intensity.1982-1984.nc"
            var_name   =  "sst_anom"
            CALL netCDF_write_3d(sst_anom,lon,lat,time)
            
            !<Write the duration contour of MHWs
            file_name  =  "OISST_v2_win_11_daily_duration.1982-1984.nc"
            var_name   =  "MHWs_dur"
            CALL netCDF_write_3d(MHWs_dur,lon,lat,time)
            
            DEALLOCATE(time)  ;  ALLOCATE(time(1:365))
            DO it = 1,365 ; time(it) = it ; END DO 
            N3 = 365

            !<Write the climatological mean 
            file_name  =  "OISST_v2_win_11_daily_clim_mean.1982-1984.nc"
            var_name   =  "sst_clim"
            CALL netCDF_write_3d(sst_clim,lon,lat,time)

            !<Write the specific percentile data
            file_name  =  "OISST_v2_win_11_daily_percent.1982-1984.nc"
            var_name   =  "sst_percentile"
            CALL netCDF_write_3d(sst_percentile,lon,lat,time)

            WRITE(*,*)  "--------WRITING PROCESS COMPLETED--------"

        END PROGRAM MHWs_main

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
            N1 = 360 ; N2 = 180 ; N3 = 16790
            missing    =  -1.e+34

            dir_name   =  "DATA"
       file_name  =  "SST.1971-2016yr.chlon_SST_restore_E1_t1.CM2.1.nc"

            WRITE(*,*)  "------------BASIC SETUP COMPLETED------------"
            WRITE(*,*)  " "
            
            !<Read the time dimension
            N  =  N3
            var_name   =  "time"
            ALLOCATE(  time(1:N) ) 
            CALL netCDF_read_1d( time ) 

            !<Read the latitude dimension
            N  =  N2 
            var_name   =  "yt"
            ALLOCATE(  lat(1:N) ) 
            CALL netCDF_read_1d( lat ) 

            !<Read the longitude dimension
            N  =  N1 
            var_name   =  "xt"
            ALLOCATE(  lon(1:N) ) 
            CALL netCDF_read_1d( lon ) 

            !-----------------------------------------------------------------!
            !                         Read the SST data                       !
            !-----------------------------------------------------------------!
            var_name   =  "SST"

            ALLOCATE( sst_data(1:N1, 1:N2, 1:N3) ) 
            !<Read the SST data
            CALL netCDF_read_3d(sst_data)
            
            WRITE(*,*)  "----------READING PROCESS COMPLETED----------"
            WRITE(*,*)  " "

            !-----------------------------------------------------------------!
            !                 Calculate the 11 day window mean                !
            !-----------------------------------------------------------------!
            CALL MHW_setup(N1,N2,N3)
            CALL MHW_clim_percent(N1,N2)
            CALL MHW_intensity(N1,N2)
            CALL MHW_duration(N1,N2)

            WRITE(*,*)  "--------CALCULATING PROCESS COMPLETED--------"
            WRITE(*,*)  " "
            DEALLOCATE(sst_data)

            !-----------------------------------------------------------------!
            !                        Write the SST data                       !
            !-----------------------------------------------------------------!
            !<Basic settings for the each variables & directory 
            dir_name   =  "RESULT"
            dim1_name  =  "lon" ; dim2_name = "lat" ; dim3_name = "time"

            dim1_unit  =  "degrees_E" 
            dim2_unit  =  "degrees_N"
            dim3_unit  =  "days since 0001-01-01 00:00:00"

            !<Write the intensity contour of MHWs
            file_name  =  "SST_intensity.1971-2016yr.chlon_SST_restore_E1_t1.CM2.1.nc"
            var_name   =  "sst_anom"
            N1 = 360 ; N2 = 180 ; N3 = 16790
            CALL netCDF_write_3d(sst_anom,lon,lat,time)
            
            !<Write the duration contour of MHWs
            file_name  =  "SST_duration.1971-2016yr.chlon_SST_restore_E1_t1.CM2.1.nc"
            var_name   =  "MHWs_dur"
            N1 = 360 ; N2 = 180 ; N3 = 16790
            CALL netCDF_write_3d(MHWs_dur,lon,lat,time)
            
            DEALLOCATE(time)  ;  ALLOCATE(time(1:365))
            DO it = 1,365 ; time(it) = it ; END DO 
            N3 = 365

            !<Write the climatological mean 
            file_name  =  "SST_clim_mean.1971-2016yr.chlon_SST_restore_E1_t1.CM2.1.nc"
            var_name   =  "sst_clim"
            CALL netCDF_write_3d(sst_clim,lon,lat,time)

            !<Write the specific percentile data
            file_name  =  "SST_percent.1971-2016yr.chlon_SST_restore_E1_t1.CM2.1.nc"
            var_name   =  "sst_percentile"
            CALL netCDF_write_3d(sst_percentile,lon,lat,time)

            WRITE(*,*)  "--------WRITING PROCESS COMPLETED--------"

        END PROGRAM MHWs_main

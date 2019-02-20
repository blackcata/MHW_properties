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

            !-----------------------------------------------------------------!
            !                     Read the file's dimension                   !
            !-----------------------------------------------------------------!
            !<Read the time dimension
            N  =  N3 
            var_name   =  "time"
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
            N1 = 1440 ; N2 = 720 ; N3 = 1095 
            dir_name   =  "DATA/OISST_v2"
            file_name  =  "sst_daily_mean.1982-1984.v2.nc"
            var_name   =  "sst"

            ALLOCATE( sst_data(1:N1, 1:N2, 1:N3) ) 

            CALL netCDF_read_3d(sst_data)
           
        END PROGRAM MHWs_main

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
            INTEGER :: it, ind_str1, ind_str2, ind_str3, N_dim, N_dim1, N_dim2
            INTEGER :: CASE_dim1, CASE_dim2, dN_dim1, dN_dim2 
            CHARACTER(LEN=10)  :: char_str, char_end, char_per1, char_per2
            CHARACTER(LEN=10)  :: char_dim1, char_dim2

            WRITE(*,*) " "

            !-----------------------------------------------------------------!
            !                     Read the file's dimension                   !
            !-----------------------------------------------------------------!
            yr_str  =  1982!year_MHW
            yr_end  =  2017!year_MHW

            N_dim1   =  4              ;  N_dim2   =  2
            dN_dim1  =  1440 / N_dim1  ;  dN_dim2  =  720 / N_dim2
            N_dim  =  N_dim1 * N_dim2

            Nt_yr   =  yr_end - yr_str + 1

            CASE_dim1  =  CASE1 ;  CASE_dim2  = CASE2
            ind_str1   =  ( CASE_dim1-1 ) * dN_dim1 + 1 
            ind_str2   =  ( CASE_dim2-1 ) * dN_dim2 + 1 
            ind_str3   = 365*(yr_str-1982) + 1

            WRITE(char_str,"(I4.4)")  yr_str  ;  WRITE(char_end,"(I4.4)") yr_end

            N1 = dN_dim1 ; N2 = dN_dim2 ; N3 = Nt_yr * 365
            missing    =  -9.96921e+36

            dir_name   =  "DATA/OISST_v2"
            file_name  =  "sst_daily_mean.1982-2018.v2.nc"
            WRITE(*,*)  "------------BASIC SETUP COMPLETED------------"
            WRITE(*,*)  " "
            
            !<Read the time dimension
            N  =  N3
            var_name   =  "time"
            ALLOCATE(  time(1:N) ) 
            CALL netCDF_read_1d(time, ind_str3) 

            !<Read the latitude dimension
            N  =  N2
            var_name   =  "lat"
            ALLOCATE(  lat(1:N) ) 
            CALL netCDF_read_1d(lat, ind_str2) 

            !<Read the longitude dimension
            N  =  N1
            var_name   =  "lon"
            ALLOCATE(  lon(1:N) ) 
            CALL netCDF_read_1d(lon, ind_str1) 

            !-----------------------------------------------------------------!
            !                         Read the SST data                       !
            !-----------------------------------------------------------------!
            var_name   =  "sst"

            ALLOCATE( sst_data(1:N1, 1:N2, 1:N3) ) 
            !<Read the SST data
            CALL netCDF_read_3d(sst_data, ind_str1, ind_str2, ind_str3)
            
            WRITE(*,*)  "----------READING PROCESS COMPLETED----------"
            WRITE(*,*)  " "

            !-----------------------------------------------------------------!
            !                 Calculate the 11 day window mean                !
            !-----------------------------------------------------------------!
            CALL MHW_setup(N1,N2,N3)


      !-----------------------------------------------------------------!
      N3 = 365
      missing    =  -9.96921e+36

      dir_name   =  "DATA"
      file_name  =  "OISST_v2_win_11_daily_clim_mean.1982-2013.nc"
      var_name   =  "sst_clim"
      CALL netCDF_read_3d(sst_clim,ind_str1,ind_str2,1)
      !-----------------------------------------------------------------!

      !-----------------------------------------------------------------!
      N3 = 365
      missing    =  -9.96921e+36

      dir_name   =  "DATA"
!      file_name  =  "OISST_v2_win_11_daily_90_percent.1982-2018.nc"
      file_name  =  "OISST_v2_win_11_daily_percent.90_1982-2013.nc"
      var_name   =  "sst_percentile"
      CALL netCDF_read_3d(sst_percentile_1,ind_str1,ind_str2,1)
      !-----------------------------------------------------------------!

      sst_percentile_2  =  sst_percentile_1

!      !-----------------------------------------------------------------!
!      N3 = 365
!      missing    =  -9.96921e+36
!
!      dir_name   =  "DATA"
!!      file_name  =  "OISST_v2_win_11_daily_75_percent.1982-2018.nc"
!      file_name  =  "OISST_v2_win_11_daily_percent.75_1982-2013.nc"
!!      file_name  =  "OISST_v2_win_11_daily_percent.90_1982-2013.nc"
!      var_name   =  "sst_percentile"
!      CALL netCDF_read_3d(sst_percentile_2,ind_str1,ind_str2,1)
!      !-----------------------------------------------------------------!
      
            CALL MHW_intensity(N1,N2)
            CALL MHW_duration(N1,N2)

            WRITE(*,*)  "--------CALCULATING PROCESS COMPLETED--------"
            WRITE(*,*)  " "
            DEALLOCATE(sst_data)

            !-----------------------------------------------------------------!
            !                        Write the SST data                       !
            !-----------------------------------------------------------------!
            WRITE(char_per1,"(I2.2)") INT(percent_1)
            WRITE(char_per2,"(I2.2)") INT(percent_2)
            WRITE(char_dim1,"(I2.2)") CASE_dim1
            WRITE(char_dim2,"(I2.2)") CASE_dim2
            
            !<Basic settings for the each variables & directory 
            dir_name   =  "RESULT"
            dim1_name  =  "lon" ; dim2_name = "lat" ; dim3_name = "time"

            dim1_unit  =  "degrees_east" 
            dim2_unit  =  "degrees_north"
            dim3_unit  =  "days since 1800-01-01 00:00:00"

!            !<Write the intensity contour of MHWs
!            file_name  =  "OISST_v2_win_11_daily_intensity."//                   &
!                          "CASE_"//TRIM(char_dim1)//"-"//TRIM(char_dim2)//"."// &
!                          TRIM(char_str)//"-"//TRIM(char_end)//".nc"
!            var_name   =  "sst_anom"
!            N3 = Nt_yr * 365
!
!            CALL netCDF_write_3d(sst_anom,lon,lat,time)
            
            !<Write the duration contour of MHWs
            file_name  =  "OISST_v2_win_11_daily_duration."//                   &
                          "CASE_"//TRIM(char_dim1)//"-"//TRIM(char_dim2)//"."// &
                          TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "MHWs_dur"
            N3 = Nt_yr * 365

            CALL netCDF_write_3d(MHWs_dur,lon,lat,time)
!            
!            DEALLOCATE(time)  ;  ALLOCATE(time(1:365))
!            DO it = 1,365 ; time(it) = it ; END DO 
!            N3 = 365
!
!            !<Write the climatological mean 
!            file_name  =  "OISST_v2_win_11_daily_clim_mean."//                   &
!                          TRIM(char_str)//"-"//TRIM(char_end)//".nc"
!            var_name   =  "sst_clim"
!            CALL netCDF_write_3d(sst_clim,lon,lat,time)
!
!            !<Write the specific percentile data
!            file_name  =  "OISST_v2_win_11_daily_percent."//TRIM(char_per1)//"_"&
!                          //TRIM(char_str)//"-"//TRIM(char_end)//".nc"
!            var_name   =  "sst_percentile"
!            CALL netCDF_write_3d(sst_percentile_1,lon,lat,time)
!
!            file_name  =  "OISST_v2_win_11_daily_percent."//TRIM(char_per2)//"_"&
!                          //TRIM(char_str)//"-"//TRIM(char_end)//".nc"
!            var_name   =  "sst_percentile"
!            CALL netCDF_write_3d(sst_percentile_2,lon,lat,time)

            WRITE(*,*)  "--------WRITING PROCESS COMPLETED--------"

        END PROGRAM MHWs_main

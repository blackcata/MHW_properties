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
            INTEGER :: it, ind_str
            CHARACTER(LEN=25)  :: char_str, char_end, char_per1, char_per2, model
            CHARACTER(LEN=100) :: tmp1, tmp2

            WRITE(*,*) " "

            !-----------------------------------------------------------------!
            !                     Read the file's dimension                   !
            !-----------------------------------------------------------------!
            yr_str  =  1850
            yr_end  =  2014
            Nt_yr   =  yr_end - yr_str + 1

            WRITE(char_str,"(I4.4)")  yr_str  ;  WRITE(char_end,"(I4.4)") yr_end

            N1 = 360 ; N2 = 180 ; N3 = Nt_yr * 365
            missing    =  1.e+20

            model      =  "model_name"
            dir_name   =  "DATA/CMIP6_hist_SST_regrid"
            tmp1       =  "regrid_360x180_tos_Oday_"
            tmp2       =  "_historical_r1i1p1f1_gn_18500101-20150101.nc"
            file_name  =  TRIM(tmp1)//TRIM(model)//TRIM(tmp2)

            WRITE(*,*) model
            WRITE(*,*) file_name 

            WRITE(*,*)  "------------BASIC SETUP COMPLETED------------"
            WRITE(*,*)  " "
            
            !<Read the time dimension
            N  =  N3   ;  ind_str  = 365*(yr_str-1850) + 1 
            var_name   =  "time"
            ALLOCATE(  time(1:N) ) 
            CALL netCDF_read_1d(time, ind_str) 

            !<Read the latitude dimension
            N  =  N2 
            var_name   =  "lat"
            ALLOCATE(  lat(1:N) ) 
            CALL netCDF_read_1d(lat, 1) 

            !<Read the longitude dimension
            N  =  N1 
            var_name   =  "lon"
            ALLOCATE(  lon(1:N) ) 
            CALL netCDF_read_1d(lon, 1) 

            !-----------------------------------------------------------------!
            !                         Read the SST data                       !
            !-----------------------------------------------------------------!
            var_name   =  "tos"

            ALLOCATE( sst_data(1:N1, 1:N2, 1:N3) ) 
            !<Read the SST data
            CALL netCDF_read_3d(sst_data, ind_str, ind_str, ind_str)
            
            WRITE(*,*)  "----------READING PROCESS COMPLETED----------"
            WRITE(*,*)  " "

            !-----------------------------------------------------------------!
            !                 Calculate the 11 day window mean                !
            !-----------------------------------------------------------------!
            CALL MHW_setup(N1,N2,N3)

            percent_1  =  90.0
            CALL MHW_clim_percent(N1,N2,sst_percentile_1,percent_1)

            percent_2  =  75.0
            CALL MHW_clim_percent(N1,N2,sst_percentile_2,percent_2)

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
            
            !<Basic settings for the each variables & directory 
            dir_name   =  "RESULT"
            dim1_name  =  "lon" ; dim2_name = "lat" ; dim3_name = "time"

            dim1_unit  =  "degrees_E" 
            dim2_unit  =  "degrees_N"
            dim3_unit  =  "days since 0001-01-01 00:00:00"

            !<Write the intensity contour of MHWs
            file_name  =  TRIM(model)//"_win_11_daily_intensity."//           &
                          TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "sst_anom"
            N1 = 360 ; N2 = 180 ; N3 = Nt_yr * 365
            CALL netCDF_write_3d(sst_anom,lon,lat,time)
            
            !<Write the duration contour of MHWs
            file_name  =  TRIM(model)//"_win_11_daily_duration."//            &
                          TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "MHWs_dur"
            N1 = 360 ; N2 = 180 ; N3 = Nt_yr * 365
            CALL netCDF_write_3d(MHWs_dur,lon,lat,time)
            
            DEALLOCATE(time)  ;  ALLOCATE(time(1:365))
            DO it = 1,365 ; time(it) = it ; END DO 
            N3 = 365

            !<Write the climatological mean 
            file_name  =  TRIM(model)//"_win_11_daily_clim_mean."//           &
                          TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "sst_clim"
            CALL netCDF_write_3d(sst_clim,lon,lat,time)

            !<Write the specific percentile data
            file_name  =  TRIM(model)//"_win_11_daily_percent."//TRIM(char_per1)//"_"&
                          //TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "sst_percentile"
            CALL netCDF_write_3d(sst_percentile_1,lon,lat,time)

            file_name  =  TRIM(model)//"_win_11_daily_percent."//TRIM(char_per2)//"_"&
                          //TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "sst_percentile"
            CALL netCDF_write_3d(sst_percentile_2,lon,lat,time)

            WRITE(*,*)  "--------WRITING PROCESS COMPLETED--------"

        END PROGRAM MHWs_main

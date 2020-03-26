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
            INTEGER :: CASE_dim1, CASE_dim2, dN_dim1, dN_dim2, CASE_num
            CHARACTER(LEN=10)  :: char_str, char_end, char_per1, char_per2
            CHARACTER(LEN=10)  :: char_CASE

            WRITE(*,*) " "

            !-----------------------------------------------------------------!
            !                     Read the file's dimension                   !
            !-----------------------------------------------------------------!
            yr_str  =  1982
            yr_end  =  2019

            CASE_num =  CASE_LATLON
            WRITE(char_CASE,"(I2.2)") CASE_num

            Nt_yr   =  yr_end - yr_str + 1

            ind_str1   =  1
            ind_str2   =  1
            ind_str3   =  365*(yr_str-1982) + 1

            WRITE(char_str,"(I4.4)")  yr_str  ;  WRITE(char_end,"(I4.4)") yr_end

            N1 = 1 ; N2 = 1 ; N3 = Nt_yr * 365
            missing    =  -9.96921e+36

            dir_name   =  "DATA/MHWs_OISST"
            file_name  =  "TS_CASE_"//TRIM(char_CASE) & 
                                    //"_SST_index.1982-2019.nc"
            WRITE(*,*)  TRIM(dir_name)//"/"//TRIM(file_name)

            WRITE(*,*)  "------------BASIC SETUP COMPLETED------------"
            WRITE(*,*)  " "
            
            !<Read the time dimension
            N  =  N3
            var_name   =  "time"
            ALLOCATE(  time(1:N) ) 
            CALL netCDF_read_1d(time, ind_str3) 

            !-----------------------------------------------------------------!
            !                         Read the SST data                       !
            !-----------------------------------------------------------------!
            var_name   =  "SST_index"

            ALLOCATE( sst_data(1:N1, 1:N2, 1:N3) ) 
            !<Read the SST data
            CALL netCDF_read_1d(sst_data, ind_str3)
            
            WRITE(*,*)  "----------READING PROCESS COMPLETED----------"
            WRITE(*,*)  " "

            !-----------------------------------------------------------------!
            !                 Calculate the 11 day window mean                !
            !-----------------------------------------------------------------!
            CALL MHW_setup(N1,N2,N3)

            percent_1  =  90.0
            CALL MHW_clim_percent(N1,N2,sst_percentile_1,percent_1)

            percent_2  =  90.0
            CALL MHW_clim_percent(N1,N2,sst_percentile_2,percent_2)

            CALL MHW_intensity(N1,N2)
            CALL MHW_duration(N1,N2)
            CALL MHW_find_peak(N1,N2)

            WRITE(*,*)  "--------CALCULATING PROCESS COMPLETED--------"
            WRITE(*,*)  " "
            DEALLOCATE(sst_data)

            !-----------------------------------------------------------------!
            !                        Write the SST data                       !
            !-----------------------------------------------------------------!
            WRITE(char_per1,"(I2.2)") INT(percent_1)
            WRITE(char_per2,"(I2.2)") INT(percent_2)
            
            !<Basic settings for the each variables & directory 
            dir_name   =  "RESULT/1982_2019"
            dim1_name  =  "lon" ; dim2_name = "lat" ; dim3_name = "time"

            dim1_unit  =  "degrees_east" 
            dim2_unit  =  "degrees_north"
            dim3_unit  =  "days since 1800-01-01 00:00:00"

            !<Write the intensity time-series of MHWs
            file_name  =  "TS_CASE_"//TRIM(char_case)//"_"//                    &
                          "intensity."//TRIM(char_str)//"-"                     &
                                      //TRIM(char_end)//".nc"
            var_name   =  "sst_anom"
            N3 = Nt_yr * 365

            CALL netCDF_write_1d(sst_anom,time)

            !<Write the duration time-series of MHWs 
            SELECT CASE (result_type)
                CASE (1)
                var_name   =  "MHWs_dur"
                file_name  =  "TS_CASE_"//TRIM(char_case)//"_"//                &
                              "MHWs_date."//TRIM(char_str)//"-"                 &
                                          //TRIM(char_end)//".nc"
                CASE (2)
                var_name   =  "MHWs_start"
                file_name  =  "TS_CASE_"//TRIM(char_case)//"_"//                &
                              "start_day."//TRIM(char_str)//"-"                 &
                                          //TRIM(char_end)//".nc"
                CASE (3)
                var_name   =  "MHWs_end"
                file_name  =  "TS_CASE_"//TRIM(char_case)//"_"//                &
                              "end_day."//TRIM(char_str)//"-"                   &
                                        //TRIM(char_end)//".nc"
                CASE (4)
                var_name   =  "MHWs_cum"
                file_name  =  "TS_CASE_"//TRIM(char_case)//"_"//                &
                              "cum."//TRIM(char_str)//"-"                       &
                                    //TRIM(char_end)//".nc"
            END SELECT

            N3 = Nt_yr * 365
            CALL netCDF_write_1d(MHWs_dur,time)

            !<Write the peak contour of MHWs
            file_name  =  "TS_CASE_"//TRIM(char_case)//"_"// &
                          "peak."//TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "MHWs_peak"
            N3 = Nt_yr * 365

            CALL netCDF_write_1d(MHWs_peak,time)

            DEALLOCATE(time)  ;  ALLOCATE(time(1:365))
            DO it = 1,365 ; time(it) = it ; END DO
            N3 = 365

            !<Write the climatological mean
            file_name  =  "TS_CASE_"//TRIM(char_case)//"_"// &
                          "clim."//TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "sst_clim"
            CALL netCDF_write_1d(sst_clim,time)

            !<Write the specific percentile data
            file_name  =  "TS_CASE_"//TRIM(char_case)//"_"// &
                          TRIM(char_per1)//"_percent."//      &
                          TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "sst_percentile1"
            CALL netCDF_write_1d(sst_percentile_1,time)

            file_name  =  "TS_CASE_"//TRIM(char_case)//"_"// &
                          TRIM(char_per2)//"_percent."//      &
                          TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "sst_percentile2"
            CALL netCDF_write_1d(sst_percentile_2,time)

            WRITE(*,*)  "--------WRITING PROCESS COMPLETED--------"

        END PROGRAM MHWs_main

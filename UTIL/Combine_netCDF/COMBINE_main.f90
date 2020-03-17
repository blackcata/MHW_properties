!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : Combine_main.f90                                                 !
!                                                                              !
!   PURPOSE : Combine each small lat/lon domain netCDF files to 1 file         !
!                                                                              !
!                                                             2019.05.08.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        PROGRAM Combine_main

            USE netcdf
            USE mod_netCDF_IO

            IMPLICIT NONE

            INTEGER            ::  ind_1, ind_2, yr_str, yr_end, Nt_yr, it
            INTEGER            ::  dim1_str, dim1_end, dim2_str, dim2_end
            INTEGER            ::  N_dim1, N_dim2, dN_dim1, dN_dim2, N_dim 
            CHARACTER(LEN=10)  ::  char_dim1, char_dim2, char_str, char_end
            CHARACTER(LEN=40)  ::  char_prefix, output_var, output_path, input_path

            LOGICAL            ::  climatology 

            REAL(KIND=8),ALLOCATABLE,DIMENSION(:)      ::  time, lat, lon
            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  sst_loc, sst_tot  

            WRITE(*,*) " "
            WRITE(*,*)  "--------SETUP PROCESS STARTED--------"

                !<Option for climatology (365) or all time series (Nt_year * 365)
                climatology  =  .FALSE.

                !<Start / End SST years
                yr_str   =  1982
                yr_end   =  2019
                Nt_yr    =  yr_end  -  yr_str  +  1

                !<Longitude / Latitude slice and the number of grid resolution
                N_dim1   =  4              ;  N_dim2   =  2
                dN_dim1  =  1440 / N_dim1  ;  dN_dim2  =  720 / N_dim2
                N_dim    =  N_dim1 * N_dim2

                !<Dimension of each dimension (longitude,latitude,time)
                IF  ( climatology ) THEN
                    N1 = dN_dim1 ; N2 = dN_dim2 ; N3 = 365
                ELSE
                    N1 = dN_dim1 ; N2 = dN_dim2 ; N3 = Nt_yr * 365
                END IF

                !<Prefix / output variable of combining files
                char_prefix  =  "OISST_v2_win_11_daily_duration."
                output_var   =  "MHWs_dur"

                !<Path of input / output files 
                input_path    =  "DATA/1982_2019_Criteria_9090"
                output_path   =  "RESULT"

            WRITE(*,*) " "
            WRITE(*,*)  "--------READING COORDINATES STARTED--------"

            missing    =  -9.96921e+36

            dir_name   =  "DATA/OISST_v2/remove_leap"
            file_name  =  "sst_daily_mean.1982-2019.v2.nc"
            
            !<Read the time dimension
            N  =  N3
            var_name   =  "time"
            ALLOCATE(  time(1:N) )

            IF  ( climatology ) THEN
                DO it = 1,365
                    time(it)  =  it-1
                END DO 
            ELSE
                CALL netCDF_read_1d(time, 1)
            END IF

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

            WRITE(char_str,"(I4.4)")  yr_str
            WRITE(char_end,"(I4.4)")  yr_end
            
            ALLOCATE( sst_tot(1:N1*N_dim1,1:N2*N_dim2,1:N3) )

            WRITE(*,*) " "
            WRITE(*,*)  "--------READING PROCESS STARTED--------"
            DO ind_1  =  1,N_dim1
                DO ind_2  =  1,N_dim2

                    ALLOCATE( sst_loc(1:N1, 1:N2, 1:N3) )
                    WRITE(char_dim1,"(I2.2)") ind_1
                    WRITE(char_dim2,"(I2.2)") ind_2

                    dir_name   =  TRIM(input_path)
                    file_name  =  TRIM(char_prefix)//        &
                                  "CASE_"//TRIM(char_dim1)//                    &
                                      "-"//TRIM(char_dim2)//"."//               &
                                      TRIM(char_str)//"-"//TRIM(char_end)//".nc"
                    var_name   =  TRIM(output_var)
                    
                    WRITE(*,*) ""
                    WRITE(*,*) "READING FILE : ",file_name
                    WRITE(*,*) N1, N2, N3
                    CALL netCDF_read_3d(sst_loc,1,1,1)

                    dim1_str  =  dN_dim1 * (ind_1-1) + 1
                    dim1_end  =  dim1_str + dN_dim1  - 1
                    dim2_str  =  dN_dim2 * (ind_2-1) + 1 
                    dim2_end  =  dim2_str + dN_dim2  - 1

                    WRITE(*,*) dim1_str, dim1_end, dim2_str, dim2_end

                    sst_tot(dim1_str:dim1_end,dim2_str:dim2_end,1:N3) =  sst_loc

                    DEALLOCATE( sst_loc ) 

              END DO 
          END DO 

          WRITE(*,*) " "
          WRITE(*,*)  "--------WRITIING PROCESS STARTED--------"

          !<Basic settings for the each variables & directory
          dir_name   =  TRIM(output_path)
          dim1_name  =  "lon" ; dim2_name = "lat" ; dim3_name = "time"

          dim1_unit  =  "degrees_east"
          dim2_unit  =  "degrees_north"
          dim3_unit  =  "days since 1800-01-01 00:00:00"

          !<Write the duration contour of MHWs
          file_name  =  TRIM(char_prefix)//                                     &
                        TRIM(char_str)//"-"//TRIM(char_end)//".nc"
          var_name   =  TRIM(output_var)

          N1  =  dN_dim1 * N_dim1  ;  N2  =  dN_dim2 * N_dim2 
          CALL netCDF_write_3d(sst_tot,lon,lat,time)

          DEALLOCATE( sst_tot ) 

      END PROGRAM Combine_main

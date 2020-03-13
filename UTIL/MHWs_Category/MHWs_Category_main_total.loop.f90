!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : MHWs_Category_main.f90                                           !
!                                                                              !
!   PURPOSE : Make MHWs each category (Moderate,Strong,Severe,Extreme) files   !
!                                                                              !
!                                                             2020.03.04.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        PROGRAM MHWs_Category_main

            USE netcdf
            USE mod_netCDF_IO

            IMPLICIT NONE

            INTEGER            ::  day, year, i, j, it 
            INTEGER            ::  ind_1, ind_2, yr_str, yr_end, Nt_yr
            INTEGER            ::  dim1_str, dim1_end, dim2_str, dim2_end
            INTEGER            ::  N_dim1, N_dim2, dN_dim1, dN_dim2, N_dim
            INTEGER            ::  ind_str1, ind_str2, ind_str3
            INTEGER            ::  CASE_dim1, CASE_dim2
            CHARACTER(LEN=10)  ::  char_dim1, char_dim2, char_str, char_end
            CHARACTER(LEN=10)  ::  char_per1, char_per2

            REAL(KIND=8),ALLOCATABLE,DIMENSION(:)      ::  time, lat, lon
            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  MHWs_data, dsst,     &
                                                           sst_clim, sst_thres, & 
                                                           sst_data
            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  MHWs_CATG

            WRITE(*,*) " "
            WRITE(*,*)  "-----------SETUP PROCESS STARTED-----------"

            yr_str   =  1982
            yr_end   =  2017
            Nt_yr    =  yr_end  -  yr_str  +  1

            N_dim1   =  4              ;  N_dim2   =  2
            dN_dim1  =  1440 / N_dim1  ;  dN_dim2  =  720 / N_dim2
            N_dim    =  N_dim1 * N_dim2

            CASE_dim1  =  CASE1 ;  CASE_dim2  = CASE2
            ind_str1   =  ( CASE_dim1-1 ) * dN_dim1 + 1
            ind_str2   =  ( CASE_dim2-1 ) * dN_dim2 + 1
            ind_str3   = 365*(yr_str-1982) + 1

            N1 = dN_dim1 ; N2 = dN_dim2 ; N3 = Nt_yr * 365

            WRITE(*,*)  "------------SETUP PROCESS ENDED------------"
            WRITE(*,*) " "

            WRITE(*,*) " "
            WRITE(*,*)  "--------READING COORDINATES STARTED--------"

            missing    =  -9.96921e+36

      dir_name   =  "DATA"
      file_name  =  "OISST_v2_win_11_daily_9075_MHWs_date.1982-2017.nc"

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

            WRITE(char_str,"(I4.4)")  yr_str
            WRITE(char_end,"(I4.4)")  yr_end

            WRITE(*,*)  "---------READING COORDINATES ENDED---------"
            WRITE(*,*) " "

            !-----------------------------------------------------------------!
            !                       Read the MHWs data                        !
            !-----------------------------------------------------------------!
            WRITE(*,*) " "
            WRITE(*,*)  "----------READING PROCESS STARTED----------"
            
            ALLOCATE( dsst(1:N1,1:N2,1:365)  ) 
            ALLOCATE( sst_clim(1:N1,1:N2,1:365)  ) 
            ALLOCATE( sst_thres(1:N1,1:N2,1:365) ) 
            ALLOCATE( sst_data(1:N1,1:N2,1:N3)  ) 
            ALLOCATE( MHWs_data(1:N1,1:N2,1:N3)  )
            ALLOCATE( MHWs_CATG(1:dN_dim1,1:dN_dim2,1:N3) )

            !<Initialization to zero 
            MHWs_data(:,:,:)    =  0.0;  dsst(:,:,:)         =  0.0
            sst_clim(:,:,:)     =  0.0;  sst_thres(:,:,:)    =  0.0 
            MHWs_CATG(:,:,:)  =  0.0

            N3 = 365
            !<Read the SST climatology 
            dir_name   = "DATA/Climatology"
            file_name  = "OISST_v2_win_11_daily_clim_mean.1982-2013.nc"
            var_name   = "sst_clim"
            CALL netCDF_read_3d(sst_clim, ind_str1, ind_str2, ind_str3)

            !<Read the SST 90 percentile
            dir_name   = "DATA/Climatology"
            file_name  = "OISST_v2_win_11_daily_percent.90_1982-2013.nc"
            var_name   = "sst_percentile"
            CALL netCDF_read_3d(sst_thres, ind_str1, ind_str2, ind_str3)

        N3 = Nt_yr * 365
        !<Read the MHWs date 
        dir_name   = "DATA/OISST_v2/remove_leap"
        file_name  = "sst_daily_mean.1982-2018.v2.nc"
        var_name   = "sst"
        CALL netCDF_read_3d(sst_data, ind_str1, ind_str2, ind_str3)

        !<Read the MHWs date 
        dir_name   = "DATA/"
        file_name  = "OISST_v2_win_11_daily_9075_MHWs_date.1982-2017.nc"
        var_name   = "MHWs_dur"
        CALL netCDF_read_3d(MHWs_data, ind_str1, ind_str2, ind_str3)

            WRITE(*,*)  "---------READING PROCESS COMPLETED---------"
            WRITE(*,*)  " "

            !-----------------------------------------------------------------!
            !             Category Classification of the MHWs data            !
            !-----------------------------------------------------------------!
            WRITE(*,*) " "
            WRITE(*,*)  "-------CATEGORY CALCULATING STARTED--------"

            !<Calculate the delta SST between 90 percentile & climatlogy mean 
            dSST(:,:,:)  =  sst_thres(:,:,:) - sst_clim(:,:,:)

            DO j = 1,N2
            WRITE(*,*) j, lat(j)
                DO i = 1,N1
                    IF ( abs(MHWs_data(i,j,1) - missing) < 1.e+1) THEN

                        dSST(i,j,:)       =  missing
                        MHWs_CATG(i,j,:)  =  missing

                    ELSE

                      it = 0
                      DO year = 1,Nt_yr 
                          DO day = 1,365
                              it  =  it + 1
                              IF ( sst_data(i,j,it) >  sst_thres(i,j,day) .AND.&
                                   sst_data(i,j,it) <= sst_thres(i,j,day) +    &
                                                        1*dsst(i,j,day)    )THEN
                                  MHWs_CATG(i,j,it)  =  1
                              END IF 
                              IF ( sst_data(i,j,it) >  sst_thres(i,j,day) +    &
                                                        1*dsst(i,j,day)   .AND. &
                                   sst_data(i,j,it) <= sst_thres(i,j,day) +    &
                                                        2*dsst(i,j,day)    )THEN
                                  MHWs_CATG(i,j,it)  =  2
                              END IF 
                              IF ( sst_data(i,j,it) >  sst_thres(i,j,day) +    &
                                                        2*dsst(i,j,day)   .AND. &
                                   sst_data(i,j,it) <= sst_thres(i,j,day) +    &
                                                        3*dsst(i,j,day)    )THEN
                                  MHWs_CATG(i,j,it)  =  3
                              END IF 
                              IF ( sst_data(i,j,it) >  sst_thres(i,j,day) +    &
                                                        3*dsst(i,j,day)  ) THEN
                                  MHWs_CATG(i,j,it)  =  4
                              END IF 
                          END DO 
                      END DO 

                    END IF

                END DO 
            END DO 

            WRITE(*,*)  "--------CATEGORY CALCULATING ENDED---------"
            WRITE(*,*) " "

            !-----------------------------------------------------------------!
            !                 Write the MHWs categorized data                 !
            !-----------------------------------------------------------------!
            WRITE(char_dim1,"(I2.2)") CASE_dim1
            WRITE(char_dim2,"(I2.2)") CASE_dim2

            !<Basic settings for the each variables & directory
            dir_name   =  "RESULT"
            dim1_name  =  "lon" ; dim2_name = "lat" ; dim3_name = "time"

            dim1_unit  =  "degrees_east"
            dim2_unit  =  "degrees_north"
            dim3_unit  =  "days since 1800-01-01 00:00:00"

            !<Write the duration contour of MHWs
            file_name  =  "OISST_v2_win_11_daily_thres."//                     &
                          "CASE_"//TRIM(char_dim1)//"-"//TRIM(char_dim2)//"."// &
                          TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "sst_percentile"
            N1 = dN_dim1 ; N2 = dN_dim2 ; N3 = 365
            CALL netCDF_write_3d(sst_thres,lon,lat,time) 

            !<Write the category 1 (moderate) contour of MHWs
            file_name  =  "OISST_v2_win_11_daily_MHWs_CATG."//                &
                          "CASE_"//TRIM(char_dim1)//"-"//TRIM(char_dim2)//"."// &
                          TRIM(char_str)//"-"//TRIM(char_end)//".nc"
            var_name   =  "MHWs_CATG"
            N1 = dN_dim1 ; N2 = dN_dim2 ; N3 = Nt_yr * 365
            CALL netCDF_write_3d(MHWs_CATG,lon,lat,time) 

            WRITE(*,*)  "---------WRITING PROCESS COMPLETED---------"

            DEALLOCATE( time, lat, lon )
            DEALLOCATE( sst_clim, sst_thres, dsst, MHWs_data ) 

      END PROGRAM MHWs_Category_main

!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : mod_MHWs                                                         !
!                                                                              !
!   PURPOSE : Calculate the MHW's various properties                           !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        MODULE mod_MHWs

            USE netcdf
            USE qsort_c_module
            USE mod_netCDF_IO

            IMPLICIT NONE

            INTEGER  :: window, Nt_yr, N_percent, thres, window_sm
            REAL(KIND=8)  :: percent

            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  MHWs_dur

            REAL(KIND=8),ALLOCATABLE,DIMENSION(:)      ::  lat, lon, time  
            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  sst_data
            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  sst_clim, sst_percentile
            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  sst_anom

            SAVE

          CONTAINS

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : MHW_setup                                                     !
!                                                                              !
!   PURPOSE : Basic setup for the MHWs calculation                             !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE MHW_setup(Nx,Ny,Nt)

              IMPLICIT NONE            
              INTEGER, INTENT(IN)  ::  Nx, Ny, Nt

              window     =  5
              thres      =  5
              window_sm  =  15
              percent    =  90.0
              Nt_yr      =  Nt / 365
              N_percent  =  INT( (2*window+1)*(Nt_yr-2)*(percent/100.0) )

              ALLOCATE( sst_clim(1:Nx,1:Ny,1:365) ) 
              ALLOCATE( sst_percentile(1:Nx,1:Ny,1:365) )
              ALLOCATE( sst_anom(1:Nx,1:Ny,1:Nt) ) 
              ALLOCATE( MHWs_dur(1:Nx,1:Ny,1:Nt) ) 
              
          END SUBROUTINE MHW_setup

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : MHW_clim_percent                                              !
!                                                                              !
!   PURPOSE : Calculate the specific range window climatologic mean            !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE MHW_clim_percent(Nx,Ny)

              IMPLICIT NONE            

              INTEGER  ::  i, j, it, yr, tmp_ind, time_ind
              INTEGER,INTENT(IN)  :: Nx, Ny
              REAL(KIND=8)  :: ts_tmp( 1:(2*window+1)*(Nt_yr-2) )
              REAL(KIND=8)  :: sst_tmp( 1:Nx,1:Ny,1:(2*window+1)*(Nt_yr-2) )

              DO it = 1,365
              WRITE(*,*) it,"DAY IS PROCESSING"
                  sst_tmp(:,:,:)  =  0.0 
                  ts_tmp(:)       =  0.0

                  !<Gather the time series of each day to sst_tmp
                  !$OMP PARALLEL DO private(yr,tmp_ind,time_ind)
                  DO  yr = 1, Nt_yr-2
                      tmp_ind   =  (2*window+1) * (yr-1) + 1
                      time_ind  =  yr*365 + it  

                      sst_tmp(:,:,tmp_ind:tmp_ind+2*window)  =                  &
                                  sst_data(:,:,time_ind-window:time_ind+window)
                  END DO 
                  !OMP END PARALLEL
                 
                  !<Caculate the climatological mean with specific windows
                  sst_clim(:,:,it)  =  SUM(sst_tmp,DIM=3) /                     &
                                                        ((2*window+1)*(Nt_yr-2))

                  !<Calculate the specific percentile of each day's time series 
                  !$OMP PARALLEL DO private(i,j,ts_tmp)
                  DO i = 1,Nx
                      DO j = 1,Ny
                          ts_tmp  =  sst_tmp(i,j,:) 
                          CALL QsortC(ts_tmp)
                          sst_percentile(i,j,it)  = ts_tmp(N_percent) 
                      END DO
                  END DO 
                  !OMP END PARALLEL

              END DO 

              CALL MHW_moving_average(sst_clim,Nx,Ny,365) 
              CALL MHW_moving_average(sst_percentile,Nx,Ny,365) 

          END SUBROUTINE MHW_clim_percent

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : MHW_moving_average                                            !
!                                                                              !
!   PURPOSE : Smoothing with specific window moving average                    !
!                                                                              !
!                                                             2019.02.28.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
         SUBROUTINE MHW_moving_average(data_input,N1,N2,N3)

              IMPLICIT NONE            
              INTEGER,INTENT(IN)  ::  N1, N2, N3 
              REAL(KIND=8),INTENT(INOUT)  :: data_input(1:N1, 1:N2, 1:N3)

              INTEGER :: it, i, j, tt, ind_str, ind_end, ind_tmp
              REAL(KIND=8)  ::  data_tmp(1:N1, 1:N2, 1:N3), tmp_sum

              DO j = 1,N2
                DO i = 1,N1

                    IF ( abs( data_input(i,j,1) - missing ) < 1.0e+1 ) THEN 
                        data_tmp(i,j,:)  =  missing
                        CYCLE
                    END IF

                    DO it = 1,N3
                        ind_str  =  it - window_sm 
                        ind_end  =  it + window_sm 

                        tmp_sum  =  0.0
                        DO tt = ind_str, ind_end
                            IF     ( tt < 1 ) THEN   ; ind_tmp  =  365 + tt 
                            ELSEIF ( tt > 365 ) THEN ; ind_tmp  =  tt - 365 
                            ELSE                     ; ind_tmp  =  tt
                            END IF

                            tmp_sum = tmp_sum + data_input(i,j,ind_tmp)
                        END DO 

                        data_tmp(i,j,it)  =  tmp_sum / (2*window_sm+1)
                    END DO 

                END DO 
              END DO 
              
              data_input  =  data_tmp

          END SUBROUTINE MHW_moving_average

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : MHW_intensity                                                 !
!                                                                              !
!   PURPOSE : Calculate the MHWs' intensity                                    !
!                                                                              !
!                                                             2019.02.26.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE MHW_intensity(Nx,Ny)

              IMPLICIT NONE            
              INTEGER,INTENT(IN)  ::  Nx, Ny
              INTEGER  :: i, j, yr, ind_str

              !<Calculating the intensity of the MHWs
              !$OMP PARALLEL DO private(yr,ind_str)
              DO yr = 1,Nt_yr 
              WRITE(*,*) yr,"YEAR IS PROCESSING"
                 ind_str  =  (yr-1)*365 + 1
                 sst_anom(:,:,ind_str:ind_str+365-1)  =                         &
                 sst_data(:,:,ind_str:ind_str+365-1)  -  sst_clim
              END DO 
              !OMP END PARALLEL

              !<Missing value treatments
              !$OMP PARALLEL DO private(i,j)
              DO j = 1,Ny
                DO i = 1,Nx
                    IF ( abs( sst_data(i,j,1) - missing ) < 1.0e+1 ) THEN 
                        sst_anom(i,j,:)  =  missing
                    END IF
                END DO 
              END DO 
              !OMP END PARALLEL

          END SUBROUTINE MHW_intensity

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : MHW_duration                                                  !
!                                                                              !
!   PURPOSE : Calculate the MHWs' duration                                     !
!                                                                              !
!                                                             2019.02.26.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE MHW_duration(Nx,Ny)

              IMPLICIT NONE            
              INTEGER,INTENT(IN)  ::  Nx, Ny
              INTEGER  :: i, j, it,  yr, tmp, ind_str, ind_end, day_ind
              REAL(KIND=8) :: diff, diff_pre

              DO j = 1,Ny
                WRITE(*,*) j,"th LATITUDE IS PROCESSING"
                !$OMP PARALLEL DO private(i,yr,it,ind_str,ind_end,tmp,diff,diff_pre)
                DO i = 1,Nx

                    IF ( abs( sst_data(i,j,1) - missing ) < 1.0e+1 ) THEN 
                        MHWs_dur(i,j,:)  =  missing
                        CONTINUE
                    END IF

                    ind_str = 0  ;  ind_end = 0 
                    DO yr = 1,Nt_yr
                      DO it = 1,365
                          tmp  =  (yr-1) * 365 

                          !<Calculate diffrence between percentile and time series
                          diff  = sst_data(i,j,tmp+it) - sst_percentile(i,j,it)

                          IF (tmp + it > 1) THEN
                              !<Periodicity of the 1 year
                              IF (it == 1) THEN ; day_ind = 365 
                              ELSE              ; day_ind = it - 1 
                              END IF  
                              diff_pre  = sst_data(i,j,tmp+it-1)                &
                                                  - sst_percentile(i,j,day_ind)
                          ELSE
                              diff_pre = 0.0
                          END IF

                          !<T_s and T_e criteria : More than specific percentile
                          IF( diff >= 0 .AND. diff_pre <= 0 ) ind_str = tmp + it
                          IF( diff <= 0 .AND. diff_pre >= 0 ) THEN
                              ind_end = tmp + it 
                              
                              !<T_s and T_e criteria : Persist 5 days
                              IF (ind_end - ind_str >= thres) THEN 
                                  !MHWs_dur(i,j,ind_str:ind_end-1) = 1.0
                                  MHWs_dur(i,j,ind_str:ind_end-1) =             &
                                                   sst_anom(i,j,ind_str:ind_end-1)
                              END IF
                          END IF 

                      END DO
                    END DO 

                END DO 
                !OMP END PARALLEL
              END DO 

          END SUBROUTINE MHW_duration

        END MODULE mod_MHWs

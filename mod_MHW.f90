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

            IMPLICIT NONE

            INTEGER  :: window, Nt_yr, N_percent
            REAL(KIND=8)  :: percent

            REAL(KIND=8),ALLOCATABLE,DIMENSION(:)      ::  lat, lon, time  
            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  sst_data
            REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)  ::  sst_clim, sst_percentile

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
              percent    =  90.0
              Nt_yr      =  Nt / 365
              N_percent  =  INT( (2*window+1)*(Nt_yr-2)*(percent/100.0) )

              ALLOCATE( sst_clim(1:Nx,1:Ny,1:365) ) 
              ALLOCATE( sst_percentile(1:Nx,1:Ny,1:365) )
              
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

          END SUBROUTINE MHW_clim_percent

        END MODULE mod_MHWs

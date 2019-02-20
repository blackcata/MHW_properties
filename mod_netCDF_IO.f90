!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : mod_netCDF_IO.f90                                                !
!                                                                              !
!   PURPOSE : Combine the PALM generated particle binary to each time file     !
!                                                                              !
!                                                             2019.02.20.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
MODULE mod_netCDF_IO

    USE netcdf

    IMPLICIT NONE

    INTEGER  :: total_particle_number, class_num 
    REAL(KIND=8)  ::  target_time, eps_t, target_z, eps_z, target_x, eps_x

    SAVE

  CONTAINS

END MODULE mod_netCDF_IO

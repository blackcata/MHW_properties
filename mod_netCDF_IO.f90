!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : mod_netCDF_IO.f90                                                !
!                                                                              !
!   PURPOSE : Read and write the netCDF files                                  !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        MODULE mod_netCDF_IO

            USE netcdf

            IMPLICIT NONE

            INTEGER  ::  N1, N2, N3, N
            INTEGER  ::  varid, ncid, id1, id2, id3, dim1, dim2, dim3
            REAL(KIND=8)  ::  missing
            CHARACTER(LEN=128)  ::  path_name, file_name, dir_name, var_name
            CHARACTER(LEN=128)  ::  dim1_name, dim2_name, dim3_name
            CHARACTER(LEN=128)  ::  dim1_unit, dim2_unit, dim3_unit

            SAVE

          CONTAINS

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : netCDF_setup                                                  !
!                                                                              !
!   PURPOSE : Initial setup for reading/writing netCDF files                   !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE netCDF_setup

              IMPLICIT NONE

              dir_name   = "DATA/OISST_v2"

          END SUBROUTINE netCDF_setup

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : netCDF_read_1d                                                !
!                                                                              !
!   PURPOSE : Reading the netCDF files                                         !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE netCDF_read_1d(data_input,ind_str)

              IMPLICIT NONE            
              
              INTEGER,INTENT(IN)          ::  ind_str
              REAL(KIND=8),INTENT(INOUT)  ::  data_input(1:N)

              path_name  = "./"//TRIM(dir_name)//"/"//TRIM(file_name)

              !< Open the file, NF90_NOWRITE : read-only access to files
              CALL CHECK( NF90_OPEN(path_name, NF90_NOWRITE, ncid) )

              !< Get the varid of the data variable, based on its name
              CALL CHECK( NF90_INQ_VARID(ncid, TRIM(var_name), varid) )

              !< Read the data
              CALL CHECK( NF90_GET_VAR(ncid, varid, data_input,                 &
                                       start = (/ind_str/)) )

              !< Close the file
              CALL CHECK( NF90_CLOSE(ncid) )

          END SUBROUTINE netCDF_read_1d

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : netCDF_read_3d                                                !
!                                                                              !
!   PURPOSE : Reading the netCDF files                                         !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE netCDF_read_3d(data_input, ind_str1, ind_str2, ind_str3)

              IMPLICIT NONE            
             
              INTEGER,INTENT(IN)          ::  ind_str1, ind_str2, ind_str3
              REAL(KIND=8),INTENT(INOUT)  ::  data_input(1:N1,1:N2,1:N3)

              path_name  = "./"//TRIM(dir_name)//"/"//TRIM(file_name)

              !< Open the file, NF90_NOWRITE : read-only access to files
              CALL CHECK( NF90_OPEN(path_name, NF90_NOWRITE, ncid) )

              !< Get the varid of the data variable, based on its name
              CALL CHECK( NF90_INQ_VARID(ncid, TRIM(var_name), varid) )

              !< Read the data
              CALL CHECK( NF90_GET_VAR(ncid, varid, data_input,                 &
                                       start = (/ind_str1, ind_str2, ind_str3/)) )

              !< Close the file
              CALL CHECK( NF90_CLOSE(ncid) )

          END SUBROUTINE netCDF_read_3d

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : netCDF_write_1d                                               !
!                                                                              !
!   PURPOSE : Writing the 1D netCDF files                                      !
!                                                                              !
!                                                             2019.05.13.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE netCDF_write_1d(data_output,data_dim3)

              IMPLICIT NONE            
             
              REAL(KIND=8),INTENT(IN)  ::  data_dim3(1:N3)
              REAL(KIND=8),INTENT(IN)  ::  data_output(1:N3)

              path_name  = "./"//TRIM(dir_name)//"/"//TRIM(file_name)
              
              !<Open the output file
              CALL CHECK( NF90_CREATE(path_name, NF90_SHARE, ncid)  )
              
              !<Define dimensions
              CALL CHECK( NF90_DEF_DIM(ncid, TRIM(dim3_name),  N3, dim3) )

              !<Define the coordinate variables & output variable
              CALL CHECK( NF90_DEF_VAR(ncid,TRIM(dim3_name),NF90_DOUBLE,dim3,id3))
              CALL CHECK( NF90_DEF_VAR(ncid, var_name, NF90_DOUBLE,             &
                                                      (/dim3/), varid) )
              !<Assign missing values
              CALL CHECK( NF90_PUT_ATT(ncid,varid,'_FillValue', missing) )

              !<Add dimension's units
              CALL CHECK( NF90_PUT_ATT(ncid,id3,"units",TRIM(dim3_unit)) )

              !<End define mode
              CALL CHECK( NF90_ENDDEF(ncid) )

              !<Write the coordinate variable & output variable
              CALL CHECK( NF90_PUT_VAR(ncid, id3,   data_dim3) )
              CALL CHECK( NF90_PUT_VAR(ncid, varid, data_output) )

              !<Close the file
              CALL CHECK( NF90_CLOSE(ncid) )

          END SUBROUTINE netCDF_write_1d

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : netCDF_write_3d                                               !
!                                                                              !
!   PURPOSE : Writing the 3D netCDF files                                      !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE netCDF_write_3d(data_output,data_dim1,data_dim2,data_dim3)

              IMPLICIT NONE            
             
              REAL(KIND=8),INTENT(IN)  ::  data_dim1(1:N1)
              REAL(KIND=8),INTENT(IN)  ::  data_dim2(1:N2)
              REAL(KIND=8),INTENT(IN)  ::  data_dim3(1:N3)
              REAL(KIND=8),INTENT(IN)  ::  data_output(1:N1,1:N2,1:N3)

              path_name  = "./"//TRIM(dir_name)//"/"//TRIM(file_name)
              
              !<Open the output file
              CALL CHECK( NF90_CREATE(path_name, NF90_SHARE, ncid)  )
              
              !<Define dimensions
              CALL CHECK( NF90_DEF_DIM(ncid, TRIM(dim1_name),  N1, dim1) )
              CALL CHECK( NF90_DEF_DIM(ncid, TRIM(dim2_name),  N2, dim2) )
              CALL CHECK( NF90_DEF_DIM(ncid, TRIM(dim3_name),  N3, dim3) )


              !<Define the coordinate variables & output variable
              CALL CHECK( NF90_DEF_VAR(ncid,TRIM(dim1_name),NF90_DOUBLE,dim1,id1))
              CALL CHECK( NF90_DEF_VAR(ncid,TRIM(dim2_name),NF90_DOUBLE,dim2,id2))
              CALL CHECK( NF90_DEF_VAR(ncid,TRIM(dim3_name),NF90_DOUBLE,dim3,id3))
              CALL CHECK( NF90_DEF_VAR(ncid, var_name, NF90_DOUBLE,             &
                                                      (/dim1,dim2,dim3/), varid) )
              !<Assign missing values
              CALL CHECK( NF90_PUT_ATT(ncid,varid,'_FillValue', missing) )

              !<Add dimension's units
              CALL CHECK( NF90_PUT_ATT(ncid,id1,"units",TRIM(dim1_unit)) )
              CALL CHECK( NF90_PUT_ATT(ncid,id2,"units",TRIM(dim2_unit)) )
              CALL CHECK( NF90_PUT_ATT(ncid,id3,"units",TRIM(dim3_unit)) )

              !<End define mode
              CALL CHECK( NF90_ENDDEF(ncid) )

              !<Write the coordinate variable & output variable
              CALL CHECK( NF90_PUT_VAR(ncid, id1,   data_dim1) )
              CALL CHECK( NF90_PUT_VAR(ncid, id2,   data_dim2) )
              CALL CHECK( NF90_PUT_VAR(ncid, id3,   data_dim3) )
              CALL CHECK( NF90_PUT_VAR(ncid, varid, data_output) )

              !<Close the file
              CALL CHECK( NF90_CLOSE(ncid) )

          END SUBROUTINE netCDF_write_3d

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : CHECK                                                         !
!                                                                              !
!   PURPOSE : Checking up the netcdf module's function & subroutine            !
!                                                                              !
!                                                             2019.02.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE CHECK(status)

              IMPLICIT NONE

              INTEGER,INTENT(IN)  ::  status
              
              IF (status /= NF90_NOERR) THEN
                  WRITE(*,*) TRIM(NF90_STRERROR(status))
                  STOP "STOPPED"
              END IF

          END SUBROUTINE CHECK

        END MODULE mod_netCDF_IO

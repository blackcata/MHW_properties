! Recursive Fortran 95 quicksort routine
! sorts real numbers into ascending numerical order
! Author: Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03
! Based on algorithm from Cormen et al., Introduction to Algorithms,
! 1997 printing

! Made F conformant by Walt Brainerd

        MODULE qsort_c_module

            IMPLICIT NONE
            PUBLIC :: QsortC
            PRIVATE :: Partition

            CONTAINS

            RECURSIVE SUBROUTINE QsortC(A)
                REAL(KIND=8), INTENT(INOUT), DIMENSION(:) :: A
                INTEGER :: iq

                IF (SIZE(A) > 1) THEN
                    CALL Partition(A, iq)
                    CALL QsortC(A(:iq-1))
                    CALL QsortC(A(iq:))
                END IF

            END SUBROUTINE QsortC

            SUBROUTINE Partition(A, marker)
                REAL(KIND=8), INTENT(INOUT), DIMENSION(:) :: A
                INTEGER, INTENT(OUT) :: marker
                INTEGER :: i, j
                REAL(KIND=8) :: temp
                REAL(KIND=8) :: x      ! pivot point

                x  = A(1)
                i = 0
                j = SIZE(A) + 1

                DO
                    j = j-1
                    DO
                        IF (A(j) <= x) EXIT
                        j = j-1
                    END DO
                    i = i+1
                    DO
                        IF (A(i) >= x) EXIT
                        i = i + 1
                    END DO
                    IF (i < j) THEN
                        ! exchange A(i) and A(j)
                        temp = A(i)
                        A(i) = A(j)
                        A(j) = temp
                    ELSEIF (i == j) THEN
                        marker = i + 1
                        RETURN
                    ELSE
                        marker = i
                        RETURN
                    ENDIF
                END DO

            END SUBROUTINE Partition

        END MODULE qsort_c_module

PROGRAM DIPOLE_MOMENT_CALCULATOR
    IMPLICIT NONE
        CHARACTER(100) :: F
        REAL :: DIPOLE_MOMENT

        F = 'ammonia.pdb'
        DIPOLE_MOMENT = CALC_DIPOLE_MOMENT(F)
    CONTAINS
        REAL FUNCTION CALC_DIPOLE_MOMENT(F)
            TYPE COORDINATES
                REAL :: X
                REAL :: Y
                REAL :: Z
            END TYPE COORDINATES
            
            CHARACTER(*), INTENT(IN) :: F
            CHARACTER(80) :: LINE
            CHARACTER(6) :: STR
            INTEGER :: N, IOS, I
            TYPE(COORDINATES), DIMENSION(:), ALLOCATABLE :: C

            N = 0

            OPEN(UNIT=10, FILE=F, IOSTAT=IOS, STATUS='OLD', ACTION='READ')

            DO
                READ(10, '(A)', IOSTAT=IOS) LINE
                IF (IOS.NE.0) THEN
                    EXIT
                END IF
                READ(LINE(1:6), '(A6)') STR
                IF (STR.EQ.'ATOM  '.OR.STR.EQ.'HETATM') THEN
                    N = N + 1
                END IF
            END DO

            ALLOCATE(C(N))
            REWIND(10)

            DO I = 1, N
                READ(10, '(A)', IOSTAT=IOS) LINE
                READ(LINE(31:38), '(F8.3)') C(I)%X ! EXPECT 3 FP DIGITS
                READ(LINE(39:46), '(F8.3)') C(I)%Y ! EXPECT 3 FP DIGITS
                READ(LINE(47:54), '(F8.3)') C(I)%Z ! EXPECT 3 FP DIGITS
                PRINT *, C(I)%X
                PRINT *, C(I)%Y
                PRINT *, C(I)%Z
            END DO

            CALC_DIPOLE_MOMENT = 0.0
            CLOSE(10)
            RETURN
        END

END PROGRAM DIPOLE_MOMENT_CALCULATOR
MODULE MERGE_SORT
    IMPLICIT NONE
CONTAINS
    RECURSIVE FUNCTION M_SORT(ARR) RESULT(RES)
        INTEGER, INTENT(IN) :: ARR(:)
        INTEGER, ALLOCATABLE :: LEFT(:), RIGHT(:), RES(:)
        INTEGER :: MIDPOINT

        IF (SIZE(ARR).LE.1) THEN ! BASE CASE
            RES = ARR
            RETURN
        END IF
        
        MIDPOINT = SIZE(ARR) / 2
        ! ASSIGN SUBARRAYS
        ALLOCATE(LEFT(MIDPOINT))
        ALLOCATE(RIGHT(SIZE(ARR) - MIDPOINT))
        LEFT = ARR(1:MIDPOINT)
        RIGHT = ARR(MIDPOINT + 1:SIZE(ARR))

        ! GO BACK UP CALL STACK ONCE SIZE OF SUBARRAYS REACHES 1
        LEFT = M_SORT(LEFT)
        RIGHT = M_SORT(RIGHT)

        ! MERGE ARRAYS IN ASCENDING ORDER AND GO UP CALL STACK AGAIN
        RES = MERGE_ARRAYS(LEFT, RIGHT)
        RETURN
    END FUNCTION

    FUNCTION MERGE_ARRAYS(LEFT, RIGHT) RESULT(RES)
        INTEGER, INTENT(IN) :: LEFT(:), RIGHT(:)
        INTEGER, ALLOCATABLE :: RES(:)
        INTEGER :: LI, RI, I ! ITERATION VARIABLES FOR SUBARRAYS AND OUTPUT

        LI = 1
        RI = 1
        I = 1

        ALLOCATE(RES(SIZE(LEFT) + SIZE(RIGHT)))

        DO WHILE (LI.LE.SIZE(LEFT).AND.RI.LE.SIZE(RIGHT)) ! ADD SMALLER ELEMENTS TO OUTPUT
            IF (LEFT(LI).LT.RIGHT(RI)) THEN
                RES(I) = LEFT(LI)
                I = I + 1
                LI = LI +  1
            ELSE
                RES(I) = RIGHT(RI)
                I = I + 1
                RI = RI + 1
            END IF
        END DO

        ! ADD REMAINING ELEMENTS TO OUTPUT
        DO WHILE (LI.LE.SIZE(LEFT))
            RES(I) = LEFT(LI)
            I = I + 1
            LI = LI + 1
        END DO

        DO WHILE (RI.LE.SIZE(RIGHT))
            RES(I) = RIGHT(RI)
            I = I + 1
            RI = RI + 1
        END DO
        RETURN
    END FUNCTION
END MODULE MERGE_SORT
        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 21 21:43:16 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SUBS_AS_ARGUMENTS__genmod
          INTERFACE 
            SUBROUTINE SUBS_AS_ARGUMENTS(X,Y,SUB,RESULT1)
              REAL(KIND=4), INTENT(IN) :: X
              REAL(KIND=4), INTENT(IN) :: Y
              EXTERNAL SUB
              REAL(KIND=4), INTENT(OUT) :: RESULT1
            END SUBROUTINE SUBS_AS_ARGUMENTS
          END INTERFACE 
        END MODULE SUBS_AS_ARGUMENTS__genmod

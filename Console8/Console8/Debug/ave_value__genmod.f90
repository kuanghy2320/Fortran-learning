        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 21 17:34:51 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE AVE_VALUE__genmod
          INTERFACE 
            FUNCTION AVE_VALUE(FUNC,FIRST_NAME,LAST_NAME,N)
              REAL(KIND=4) :: FUNC
              EXTERNAL FUNC
              REAL(KIND=4), INTENT(IN) :: FIRST_NAME
              REAL(KIND=4), INTENT(IN) :: LAST_NAME
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4) :: AVE_VALUE
            END FUNCTION AVE_VALUE
          END INTERFACE 
        END MODULE AVE_VALUE__genmod

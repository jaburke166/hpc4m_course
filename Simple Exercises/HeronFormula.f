!-----------------------------------------------------
! Compute the area of a triangle using Heron's formula
!-----------------------------------------------------
      PROGRAM HeronFormula
            IMPLICIT NONE

            REAL      :: a, b, c              ! INPUT: three sides
            REAL      :: s                    ! LOCAL: half of perim
            REAL      :: Area                 ! OUTPUT: triangle area
            LOGICAL   :: Cond_1, Cond_2       ! LOCAL: logical conds
            LOGICAL   :: RightAng1, RightAng2, RightAng3  

            WRITE(*,*) "Enter the three sides of the triangle please"
            
            READ(*,*) a, b, c                 ! Read LOCAL Vars

            WRITE(*,*) "a = ", a
            WRITE(*,*) "b = ", b
            WRITE(*,*) "c = ", c                 ! Write local Vars
            WRITE(*,*)

            ! Compute boolean conditions to see whether inputs form a
            ! triangle
            Cond_1 = (a > 0.) .AND. (b > 0.) .AND. (c > 0.)
            Cond_2 = (a + b > c) .AND. (a + c > b) .AND. (b + c > a)
            ! If conditions are satisfied, compute Area. If not, throw
            ! error back at user
            IF (Cond_1 .AND. Cond_2) THEN
               s = (a + b + c) / 2.0
               Area = SQRT(s * (s - a) * (s - b) * (s - c))
               WRITE(*,*) "Triangle area =", Area
            ELSE
               WRITE(*,*) "ERROR: this is not a triangle!"
            END IF

            RightAng1 = (a**2 == b**2 + c**2) 
            RightAng2 = (b**2 == a**2 + c**2)
            RightAng3 = (c**2 == a**2 + b**2) 
            IF (RightAng1 .OR. RightAng2 .OR. RightAng3) THEN
               WRITE(*,*) "Triangle is Right-Angled"
            ELSE
               WRITE(*,*) "Triangle is NOT Right-Angled"
            END IF

      END PROGRAM HeronFormula





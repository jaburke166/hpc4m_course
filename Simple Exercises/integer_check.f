!---------------------------------------------------------
! Check if inputted integer is greater, less or equal to 0
!---------------------------------------------------------
      program integer_check
              implicit none

              integer   :: x    ! Inputted integer
              logical   :: cond1, cond2 ! logical conditions

              write(*,*) "Please enter an integer"
              read(*,*) x

              cond1 = x < 0
              cond2 = x > 0
              if (cond1) then
                      write(*,*) x, "is a negative integer"
              elseif (cond2) then
                      write(*,*) x, "is a positive integer"
              else
                      write(*,*) "x = 0"
              end if
      end program

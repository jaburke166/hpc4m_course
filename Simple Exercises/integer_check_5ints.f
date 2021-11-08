!---------------------------------------------------------
! Check if 5 inputted integers is greater, less or equal to 0
!---------------------------------------------------------
      program integer_check_5ints
              implicit none

              integer, dimension(5)   :: x ! Inputted integer list
              integer                 :: i ! dummy interator
              logical   :: cond1, cond2    ! logical conditions

              write(*,*) "Please enter a list of 5 integer"
              read(*,*) x

              do i=1, 5
                cond1 = x(i) < 0
                cond2 = x(i) > 0
                if (cond1) then
                      write(*,*) x(i), "is a negative integer"
                elseif (cond2) then
                      write(*,*) x(i), "is a positive integer"
                else
                      write(*,*) x(i), "is zero"
                end if
              end do
      end program

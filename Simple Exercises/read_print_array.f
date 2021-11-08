!---------------------------------------------------------------------
! Read in array of 10 elements and print out in same order and reverse
! order
!---------------------------------------------------------------------
      program read_print_array
          implicit none

          ! Set up INPUT parameter L and reversed list
          integer, dimension(10) :: L, L_reverse
          integer :: i ! Dummy variable

          write(*,*) "Input 10 numbers please"
          read(*,*) L
          
          do i=1, 10
             L_reverse(i) = L(10-i+1)
          end do

          write(*,*) "Input array:"
          write(*,*) L
          write(*,*) "Reversed array:"
          write(*,*) L_reverse

      end program

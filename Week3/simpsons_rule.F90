!--------------------------------------------------
! Compute Intergal of Function using Simpson's Rule
!--------------------------------------------------
        function func(x) result(y)
               real, intent(in) :: x ! Input
               real :: y ! Output

               y = sin(x)

        end function

        program simpsons_int
           implicit none

           real :: a, b, stepsize ! Lower, upper bounds of int & stepsize
           real :: fa, fb, integral ! variables used for computing integral
           real :: func ! definition of function
           real :: k ! evaluator for function during while loop
           integer :: i, N_subints ! dummy variable i, subinterval deltax

           ! Read in lower and upper boundary for integral
1          write(*,*) "Enter lower and upper boundary"
           read(*,*) a, b
           
           ! If a >= b then return error and prompt user for re-entry of a,b
           if (a >= b) then
              write(*,*) "ERROR. Enter lower and upper boundary"
              goto 1
           end if
        
           ! Prompt user to enter number of subintervals between a and b
           write(*,*) "Enter #subintervals between lower and upper boundary"
           read(*,*) N_subints

           ! Compute stepsize, function evaluations at boundaries and initial
           ! value for integral and iterator
           stepsize = (b - a) / N_subints
           fa = func(a)
           fb = func(b)
           integral = fa + fb
           i = 1
           
           ! Loop while i is less than the number of subintervals
           do while (i <= N_subints)
              k = a + i*stepsize

              if (mod(i,2) == 0) then
                 integral = integral + 2*func(k)
              else
                 integral = integral + 4*func(k)
              end if

              i = i+1
           end do

           integral = (stepsize/3)*integral

           write(*,'("Integrating sin(x) from ", F0.2, " to ", F0.2)') a, b
           write(*,'("Using ", I0, " sub-intervals yields: ", F0.2)') N_subints, integral
           

      end program

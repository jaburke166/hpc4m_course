!---------------------------------------------------------------------
! Given the 3 coefficients of a quadratic equation, compute real roots
! if possible
!---------------------------------------------------------------------
      program quadratic_roots
        implicit none

        real, dimension(3) :: coeffs ! Coefficients of quadratic
        real :: a1, a2, a3 ! Coefficients of quadratic
        real :: disc ! Discriminant
        real :: root1, root2 ! OUTPUT: Roots of quadratic equation
        complex :: comp_root1 ! OUTPUT: Complex roots
        real :: re, im ! Parts of Complex roots
        logical :: cond1, cond2 ! logical for discriminant

        write(*,*) "Enter coefficients of quadratic equation"
        read(*,*) coeffs
        a1 = coeffs(1)
        a2 = coeffs(2)
        a3 = coeffs(3)

        write(*,*) "Computing roots for equation:"

        ! This is for outputting integer format so no blank space
        write(*,'(F0.2, "x^2 + ", F0.2, "x + ", F0.2, " = 0")') a1,a2,a3


        ! Compute discriminant
        disc = a2**2 - 4*a1*a3
        cond1 = disc > 0.0
        cond2 = disc < 0.0

        ! Depending on discriminant value, compute roots
        write(*,*) "Roots of quadratic are:"
        if (cond1) then
          root1 = a2/(2.0*a1) + sqrt(disc)/(2.0*a1)
          root2 = a2/(2.0*a1) - sqrt(disc)/(2.0*a1)
          write(*,'("Root 1: ", F0.2)') root1
          write(*, '("Root 2: ", F0.2)') root2
        elseif (cond2) then
          comp_root1 = complex(-a2/(2.0*a1),-sqrt(-disc)/(2.0*a1))
          re = realpart(comp_root1)
          im = imagpart(comp_root1)
          if (im > 0) then
            write(*,'("Root 1: ", F0.2 , " + ", F0.2, "i")') re, im
            write(*,'("Root 2: ", F0.2 , " - ", F0.2, "i")') re, im
          else
            write(*,'("Root 1: ", F0.2, " - ", F0.2, "i")') re, -im
            write(*,'("Root 2: ", F0.2, " + ", F0.2, "i")') re, -im    
          end if
        else
          root1 = a2/(2.0*a1)
          root2 = -root1         
          write(*,'("Root 1: ", F0.2)') root1
          write(*, '("Root 2: ", F0.2)') root2
        end if

      end program

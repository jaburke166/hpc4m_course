!----------------------------------------------------------------
! Calculates the displacement caused by the vertical motion under
! gravity. Requires two INPUTS: time, t and initial velocity, u
!----------------------------------------------------------------
      program gravitationalDisp
          implicit none

          ! gravitational acceleration: named parameters
          real, parameter :: g = 9.81

          ! variable declaration
          real :: t, u ! time and initial velocity INPUTS
          real :: s ! displacement OUTPUT

          ! Read in values
          print *, "Please enter initial time and speed"
          read(*,*) t, u

          ! Output inputs
          write(*, '("Initial time: ", F0.2)') t
          write(*, '("Initial speed: ", F0.2)') u

          ! Compute displacement
          s = u * t - g * (t**2)/2

          ! output
          write(*, '("Displacement = ", F0.2)') s
      end program

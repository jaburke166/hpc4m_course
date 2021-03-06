!--------------------------------------------------------------------
! Program takes input of seconds and converts into hours, minutes and
! seconds
!--------------------------------------------------------------------
        program time_converter
                implicit none

                integer    :: s_input, iostat,ios ! seconds to input
                integer    :: m       ! minutes to compute
                integer    :: h       ! hours to compute
                integer    :: s       ! seconds to compute
                logical    :: cond1, cond2

15              write(*,*) "Please input the number of seconds"
                read(*,*, iostat=ios) s_input
                if (ios.ne.0) then
                  write(*,*) "You've entered an incorrect value."
                  goto 15
                end if

                write(*,'("Input Seconds = ", I0)') s_input

                cond1 = (s_input < 60)
                cond2 = (s_input >= 60) .and. (s_input < 3600) 
                if (cond1) then
                        h = 0
                        m = 0
                        s = s_input
                elseif (cond2) then
                        h = 0
                        m = int(floor(s_input / 60.0))
                        s = s_input - 60.0*m
                else
                        h = int(floor(s_input / 3600.0))
                        m = int(floor(floor(s_input / 60.0) - h*60.0))
                        s = int(s_input - 60.0*m - 3600.0*h)
                end if

                write(*,'("Hours = ", I0)') h
                write(*,'("Minutes = ", I0)') m
                write(*,'("Seconds = ", I0)') s

      end program

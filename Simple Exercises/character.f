        program charac
                ! You must specify length and must be at least more
                ! than what you intend on inputting into that string.
                character(len=50) :: string
                string = 'Hi, my name is Jamie Burke'
                print *, string
                write(*,*) string
        end program

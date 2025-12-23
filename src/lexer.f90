module lexer_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use iso_c_binding, only: C_NULL_CHAR
    implicit none

    private
    public :: lexer_t
    public :: is_digit, is_nl, is_space, is_alphanum, is_letter
    
    type lexer_t
        character(len=:), allocatable :: buff
        integer :: pos, size, non_extra
       
        contains

        procedure :: load_file, current, peek, skip_extra, is_eof
        procedure :: accept, skip_line 
        procedure :: accept_name, accept_skip_extra
    end type

    contains
        
    subroutine load_file(self, path)
        class(lexer_t) :: self
        character(*) :: path
            
        character(1) :: dummy
        integer :: unit, io_stat
        integer(int64) :: size

        open(newunit = unit, file=path, status='old', access="stream")

        inquire(unit = unit, size=size, iostat=io_stat)

        if (allocated(self%buff)) deallocate(self%buff)
        allocate( character(len=size) :: self%buff )

        read(unit) self%buff

        close(unit)
    
        self%pos = 1
        self%non_extra = 0
        self%size = size
    end subroutine

    character(1) function current(self)
        class(lexer_t) :: self

        if (self%pos < self%size) then
            current = self%buff(self%pos:self%pos)
        else
            current = C_NULL_CHAR
        end if
    end function

    logical function is_eof(self)
        class(lexer_t) :: self
        is_eof = self%pos >= self%size
    end function

    character(1) function peek(self)
        class(lexer_t) :: self

        if (self%pos + 1 <= self%size) then
            peek = self%buff(self%pos + 1:self%pos + 1)
        else
            peek = C_NULL_CHAR
        end if
    end function

    subroutine skip_extra(self)
        class(lexer_t) :: self
        character(1) :: p

        do
            p = self%current()
            if (p == C_NULL_CHAR) return

            ! Skip space
            if (is_space(p)) then
                self%pos = self%pos + 1
                cycle
            end if

            ! Skip comment
            if (p == '!') then
                self%pos = self%pos + 1
                do while(.not. is_nl(self%current())) 
                    self%pos = self%pos + 1
                end do

                cycle
            end if

            ! skip modern continuation
            if (p == "&") then
                self%pos = self%pos + 1
                p = self%current()

                do while(is_space(p) .or. is_nl(p))
                    ! TODO: skip commentary lines
                    self%pos = self%pos + 1
                    p = self%current()
                end do

                cycle
            end if

            exit
        end do
        
        self%non_extra = self%non_extra + 1
    end subroutine

    logical function accept(self, content)
        class(lexer_t) :: self
        character(*) :: content
        integer :: before, size

        accept = .false.
        size = len(content)
        
        if (self%pos + size > self%size) return

        accept = self%buff(self%pos : self%pos + size - 1) == content
        if (accept) then
            self%pos = self%pos + size
            self%non_extra = self%non_extra + size
            call self%skip_extra()
        end if
    end function

    logical function accept_skip_extra(self, content)
        class(lexer_t) :: self
        character(*) :: content
        accept_skip_extra = accept(self, content)
        if (accept_skip_extra) then
            self%pos = self%pos - 1
            call self%skip_extra()
        end if
    end function

    logical function accept_name(self, name)
        class(lexer_t) :: self
        character(len=:), allocatable, intent(out) :: name
        integer :: start
        
        accept_name = .false.
        if (is_letter(self%current())) then
            start = self%pos

            self%pos = self%pos + 1

            do while(is_alphanum(self%current()) .or. self%current() == "_")
                self%pos = self%pos + 1
            end do

            name = self%buff(start:self%pos - 1)
            accept_name = .true.

            ! advance extra tokens
            call self%skip_extra()
        end if
    end function

    subroutine skip_line(self)
        class(lexer_t) :: self

        do while(.not. (is_nl(self%current()) .or. self%is_eof() ) )
            self%pos = self%pos + 1
            call self%skip_extra()
        end do

        self%pos = self%pos + 1

        call self%skip_extra()
    end subroutine

    subroutine debug(self)
        class(lexer_t) :: self
    end subroutine

    logical elemental function is_space(char)
        character(1), intent(in) :: char
        is_space = char == ' ' .or. iachar(char) == 9
    end function

    logical elemental function is_nl(char)
        character(1), intent(in) :: char
        is_nl = iachar(char) == 10
    end function

    logical elemental function is_alphanum(char)
        character(1), intent(in) :: char
        is_alphanum = ((char >= 'A' .and. char <= 'Z') .or. &
                       (char >= 'a' .and. char <= 'z') .or. &
                       (char >= '0' .and. char <= '9'))
    end function

    logical elemental function is_digit(char)
        character(1), intent(in) :: char
        is_digit = char >= '0' .and. char <= '9'
    end function

    logical elemental function is_letter(char)
        character(1), intent(in) :: char
        is_letter = (char >= 'A' .and. char <= 'Z') .or. &
                    (char >= 'a' .and. char <= 'z')
    end function
end module


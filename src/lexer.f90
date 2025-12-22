module lexer_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use iso_c_binding, only: C_NULL_CHAR
    implicit none

    private
    public :: lexer_t
    
    type lexer_t
        character(len=:), allocatable :: buff
        integer :: start, cur, size, non_extra
       
        contains

        procedure :: load_file, current, peek, next, is_eof
        procedure :: accept, forget, skip_line, extract
        procedure :: accept_name, accept_next
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
    
        self%cur = 1
        self%start = 1
        self%non_extra = 0
        self%size = size
    end subroutine

    character(1) function current(self)
        class(lexer_t) :: self

        if (self%cur < self%size) then
            current = self%buff(self%cur:self%cur)
        else
            current = C_NULL_CHAR
        end if
    end function

    logical function is_eof(self)
        class(lexer_t) :: self
        is_eof = self%cur >= self%size
    end function

    character(1) function peek(self)
        class(lexer_t) :: self

        if (self%cur + 1 <= self%size) then
            peek = self%buff(self%cur + 1:self%cur + 1)
        else
            peek = C_NULL_CHAR
        end if
    end function

    subroutine next(self)
        class(lexer_t) :: self
        character(1) :: p

        p = self%peek()
        if (p == C_NULL_CHAR) return
    
        do
            p = self%peek()
            self%cur = self%cur + 1

            if (is_space(p)) then
                cycle
            end if

            ! skip modern continuation
            if (p == "&") then
                do 
                    p = self%peek()
                    if ((.not. is_space(p)) .and. (.not. is_nl(p))) exit
                    ! TODO: skip commentary lines
                    self%cur = self%cur + 1
                end do

                cycle
            end if

            exit
        end do
        
        self%non_extra = self%non_extra + 1
    end subroutine

    subroutine extract(self, content)
        class(lexer_t) :: self
        character(len=:), allocatable, intent(out) :: content

        integer :: i

        allocate(character(len = self%non_extra) :: content)

        self%cur = self%start
        do i = 1, self%non_extra
            content(i:i) = self%current()
            call self%next()
        end do

        call self%forget()
    end subroutine

    subroutine forget(self)
        class(lexer_t) :: self
        self%start = self%cur
        self%non_extra = 0
    end subroutine

    logical function accept(self, content)
        class(lexer_t) :: self
        character(*) :: content
        integer :: before, size

        accept = .false.
        size = len(content)
        
        if (self%cur + size > self%size) return

        accept = self%buff(self%cur : self%cur + size - 1) == content
        if (accept) then
            self%cur = self%cur + size
            self%non_extra = self%non_extra + size
        end if
    end function

    logical function accept_next(self, content)
        class(lexer_t) :: self
        character(*) :: content
        accept_next = accept(self, content)
        if (accept_next) call self%next()
    end function

    logical function accept_name(self)
        class(lexer_t) :: self
        
        accept_name = .false.
        if (is_letter(self%current())) then
            self%cur = self%cur + 1
            self%non_extra = self%non_extra + 1

            do while(is_alphanum(self%current()))
                self%cur = self%cur + 1
                self%non_extra = self%non_extra + 1
            end do
            accept_name = .true.
        end if

    end function

    subroutine skip_line(self)
        class(lexer_t) :: self

        do while(.not. (is_nl(self%current()) .or. self%is_eof() ) )
            call self%next()
        end do
        call self%next()

        do while(is_space(self%current()))
            call self%next()
        end do

        call self%forget()
    end subroutine

    subroutine debug(self)
        class(lexer_t) :: self
    end subroutine

    logical function is_space(char)
        character(1) :: char
        is_space = char == ' ' .or. iachar(char) == 9
    end function

    logical function is_nl(char)
        character(1) :: char
        is_nl = iachar(char) == 10
    end function

    logical function is_alphanum(char)
        character(1) :: char
        is_alphanum = ((char >= 'A' .and. char <= 'Z') .or. &
                       (char >= 'a' .and. char <= 'z') .or. &
                       (char >= '0' .and. char <= '9'))
    end function

    logical function is_letter(char)
        character(1) :: char
        is_letter = (char >= 'A' .and. char <= 'Z') .or. &
                    (char >= 'a' .and. char <= 'z')
    end function
    
end module


module lexer_mod
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use string_builder_mod
    
    implicit none

    enum, bind(C)
        enumerator :: LEX_EOF = 0
        enumerator :: LEX_ERROR
        enumerator :: LEX_IDENTIFIER
        enumerator :: LEX_EQUAL
        enumerator :: LEX_POINTER_ASSIGN
        enumerator :: LEX_INTEGER
        enumerator :: LEX_NEWLINE
        enumerator :: LEX_OTHER
    end enum

    type :: token_t
        integer :: type
        integer :: line
        integer :: start
        integer :: end

        contains
        
        procedure :: type_name, write_token
        generic :: write(formatted) => write_token
    end type token_t

    type :: lexer_t
        character(len=:), allocatable :: buff
        character(len=:), allocatable :: preprocessor_cmd
        logical :: preprocess = .false.
        integer(c_int) :: line = 1
        integer(c_int) :: pos = 1
        integer :: size = 0
        contains
        procedure :: load_file, preprocess_file
        procedure :: next_token
        procedure :: get_token_text
        final :: lexer_destroy
    end type lexer_t

    ! C lexer interface
    interface
        subroutine c_lexer_init() bind(C, name="lexer_init")
        end subroutine

        type(c_ptr) function c_lexer_create(buffer, length, line) bind(C, name="lexer_create")
            import :: c_ptr, c_char, c_int
            character(c_char), dimension(*) :: buffer
            integer(c_int), value :: length
            integer(c_int) :: line
        end function

        subroutine c_lexer_destroy(lexer) bind(C, name="lexer_destroy")
            import :: c_ptr
            type(c_ptr), value :: lexer
        end subroutine

        subroutine c_lexer_next(lexer, token_type, start, end) bind(C, name="lexer_next")
            import :: c_ptr, c_int
            type(c_ptr), value :: lexer
            integer(c_int) :: token_type
            integer(c_int) :: start
            integer(c_int) :: end
        end subroutine

        ! File I/O functions (keep your existing interface)
        function popen(command, mode) bind(C, name="popen")
            import :: c_char, c_ptr
            character(c_char), dimension(*) :: command
            character(c_char), dimension(*) :: mode
            type(c_ptr) :: popen
        end function

        function pclose(stream) bind(C, name="pclose")
            import :: c_ptr, c_int
            type(c_ptr), value :: stream
            integer(c_int) :: pclose
        end function

        function fgets(buf, size, stream) bind(C, name="fgets")
            import :: c_char, c_int, c_ptr
            character(c_char), dimension(*) :: buf
            integer(c_int), value :: size
            type(c_ptr), value :: stream
            type(c_ptr) :: fgets
        end function
    end interface

contains

    subroutine lexer_destroy(this)
        type(lexer_t) :: this
        if (allocated(this%buff)) deallocate(this%buff)
        if (allocated(this%preprocessor_cmd)) deallocate(this%preprocessor_cmd)
    end subroutine

    subroutine preprocess_file(self, filepath)
        class(lexer_t) :: self
        character(*), intent(in) :: filepath

        type(string_builder_t) :: builder
        integer, parameter :: read_size = 10 * 1024
        character(kind = c_char, len=:), allocatable :: cmd
        character(kind = c_char, len=read_size) :: line_buffer
        type(c_ptr) :: fp
        integer :: eol
        integer(c_int) :: ok

        call builder%initialize()
        cmd = self%preprocessor_cmd // " " // filepath // C_NULL_CHAR
        fp = popen(cmd, 'r'// C_NULL_CHAR)

        do while (c_associated(fgets(line_buffer, read_size, fp)))
            eol = index(line_buffer, C_NULL_CHAR)
            call builder%append(line_buffer(:eol - 1))
        end do

       ok = pclose(fp) 

       if (allocated(self%buff)) deallocate(self%buff)
       allocate(character(len=builder%size) :: self%buff)
       self%buff = builder%buffer(:builder%size)
    end subroutine
        
    subroutine load_file(self, path)
        class(lexer_t) :: self
        character(*) :: path
            
        character(1) :: dummy
        integer :: unit, io_stat
        integer(int64) :: size

        if (self%preprocess) then
            call self%preprocess_file(path)
        else
            open(newunit = unit, file=path, status='old', access="stream")

            inquire(unit = unit, size=size, iostat=io_stat)

            if (allocated(self%buff)) deallocate(self%buff)
            allocate( character(len=size) :: self%buff )

            read(unit) self%buff

            close(unit)
        end if

        call to_lowercase(self%buff)
    
        self%pos = 1
        self%size = len(self%buff)
    end subroutine

    subroutine next_token(self, token)
        class(lexer_t), intent(inout) :: self
        type(token_t), intent(out) :: token
        
        integer(c_int) :: c_type, c_start, c_end
        type(c_ptr) :: remaining
        
        interface
            subroutine next_token_c(buffer, length, line, type, start, end) &
                bind(C, name="next_token_c")
                import :: c_char, c_int
                character(c_char), dimension(*) :: buffer
                integer(c_int), value :: length
                integer(c_int) :: line
                integer(c_int) :: type, start, end
            end subroutine
        end interface

        if (self%size == 0 .or. self%pos > len(self%buff)) then
            token%type = LEX_EOF
            token%line = self%line
            token%start = self%pos
            token%end = self%pos
            return
        end if
        
        ! Call C lexer
        call next_token_c(self%buff(self%pos:), &
                          len(self%buff) - self%pos + 1, &
                          self%line, &
                          c_type, c_start, c_end)

        ! Create Fortran token
        token%type = c_type
        token%line = self%line
        token%start = self%pos + c_start
        token%end = self%pos + c_end - 1

        ! Update position
        self%pos = token%end + 1
    end subroutine

    function get_token_text(self, token) result(text)
        class(lexer_t) :: self
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: text
        
        if (token%start >= 0 .and. token%end >= token%start) then
            allocate(character(len=token%end - token%start + 1) :: text)
            text = self%buff(token%start:token%end)
        else
            text = ""
        end if
    end function

    subroutine to_lowercase(string)
        character(*), intent(inout) :: string
        integer :: i
        
        do i = 1, len(string)
            select case (string(i:i))
            case ('A':'Z')
                string(i:i) = char(ichar(string(i:i)) + 32)
            end select
        end do
    end subroutine

    function type_name(self) result(name)
        class(token_t), intent(in) :: self
        character(:), allocatable :: name
        
        select case (self%type)
        case (LEX_EOF)
            name = "EOF"
        case (LEX_ERROR)
            name = "ERROR"
        case (LEX_IDENTIFIER)
            name = "IDENTIFIER"
        case (LEX_EQUAL)
            name = "EQUAL"
        case (LEX_POINTER_ASSIGN)
            name = "POINTER_ASSIGN"
        case (LEX_INTEGER)
            name = "INTEGER"
        case (LEX_NEWLINE)
            name = "NEWLINE"
        case (LEX_OTHER)
            name = "OTHER"
        case default
            name = "UNKNOWN"
        end select
    end function

    subroutine write_token(self, unit, iotype, v_list, iostat, iomsg)
        ! Arguments for user-defined I/O procedure (standard interface)
        class(token_t), intent(in) :: self
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer(kind=int32), intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        ! Custom output format
        write(unit, "(2A, I0, A, I0, A)") self%type_name(), '(', self%start, ":", self%end ,')'
        iostat = 0 ! Indicate success
    end subroutine 
    
end module lexer_mod

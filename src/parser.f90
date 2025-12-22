module minimal_parser_mod 
    use lexer_mod    
    implicit none

    private
    public :: minimal_parser_t

    type :: minimal_parser_t
        type(lexer_t) :: lexer

        contains 

        procedure :: process_file
        procedure :: on_file => empty_on_file
        procedure :: on_file_end => empty_on_file_end
        procedure :: on_module => empty_on_module
        procedure :: on_module_end => empty_on_module_end
        procedure :: on_program => empty_on_program
        procedure :: on_use => empty_on_use
    end type

    interface 
    end interface


    contains

    subroutine process_file(self, filepath)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath 

        character(len = :), allocatable :: name
        logical :: first, ok

        associate (lexer => self%lexer)
        
        call lexer%load_file(filepath)
        if (self%on_file(filepath)) return
        first = .true.

        do while(.not. lexer%is_eof())
            if (first) then
                first = .false.
            else
                call lexer%skip_line()
                call lexer%forget()
            end if

            ! Parse module declaration
            if (lexer%accept_next("module")) then
                call lexer%forget()

                name = ""
                if (lexer%accept_next("procedure")) then
                    call lexer%forget()
                    if (.not. lexer%accept_name()) name = "procedure"
                else if (lexer%accept_name()) then
                    call lexer%extract(name)
                end if

                ! Dispatch event
                if (len_trim(name) > 0) then
                    if (self%on_module(filepath, name)) exit
                end if

                cycle
            end if

            if (lexer%accept("endmodule")) then
                ! Dispatch event
                if (self%on_module_end(filepath, name)) exit
                cycle
            end if

            if (lexer%accept_next("end")) then
                if (lexer%accept_next("module")) then
                    ! Dispatch event
                    if (self%on_module_end(filepath, name)) exit
                end if

                cycle
            end if

            ! Parse program declaration
            if (lexer%accept_next("program")) then
                call lexer%forget()

                if (lexer%accept_name()) then
                    call lexer%extract(name)
                    if (self%on_program(filepath, name)) exit
                end if
                cycle
            end if

            ! Parse use statement
            if (lexer%accept_next("use")) then
                if (lexer%accept_next(",")) then
                    if (lexer%accept_name()) call lexer%next()
                end if

                ok = lexer%accept_next("::")
                
                call lexer%forget()
                if (lexer%accept_name()) then
                    call lexer%extract(name)
                    ! Dispatch event
                    if (self%on_use(filepath, name)) exit
                end if

                cycle
            end if
        end do

        end associate
    end subroutine

    logical function empty_on_file(self, filepath) result(abort)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath
        abort = .false.
    end function

    logical function empty_on_file_end(self, filepath) result(abort)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath
        abort = .false.
    end function

    logical function empty_on_module(self, filepath, name) result(abort) 
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath, name
        abort = .false.
    end function

    logical function empty_on_module_end(self, filepath, name) result(abort)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath, name
        abort = .false.
    end function

    logical function empty_on_program(self, filepath, name) result(abort) 
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath, name
        abort = .false.
    end function

    logical function empty_on_use(self, filepath, name) result(abort)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath, name
        abort = .false.
    end function
end module

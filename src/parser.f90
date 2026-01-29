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
        procedure :: on_module => empty_on_module
        procedure :: on_submodule => empty_on_submodule
        procedure :: on_program => empty_on_program
        procedure :: on_use => empty_on_use
    end type

    contains

    subroutine process_file(self, filepath)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath 

        character(len = :), allocatable :: name, ancestor, parent
        logical :: first, ok
        type(token_t) :: token

        associate (lexer => self%lexer)
        
        call lexer%load_file(filepath)
        if (self%on_file(filepath)) return
        
        first = .true.
        call lexer%next_token(token)

        do while(token%type /= LEX_EOF)  ! Use LEX_EOF constant

            ! Skip comments and newlines
            if (token%type == LEX_NEWLINE) then
                call lexer%next_token(token)
                cycle
            end if
            
            ! Check for keywords
            if (token%type == LEX_IDENTIFIER) then
                select case (lexer%get_token_text(token))
                
                case ("module")
                    ! Parse module declaration
                    call lexer%next_token(token)

                    if (token%type /= LEX_IDENTIFIER) then
                        ! Skip to next line or error
                        call skip_to_eol(lexer, token)
                        cycle
                    end if
                    
                    name = lexer%get_token_text(token)
                    
                    ! Check for end of statement
                    call lexer%next_token(token)
                    if (token%type /= LEX_NEWLINE) then
                        ! Not valid module declaration
                        call skip_to_eol(lexer, token)
                        cycle
                    end if
                    
                    ! Dispatch event
                    if (self%on_module(filepath, name)) return
                    call skip_to_eol(lexer, token)
                
                case ("program")
                    ! Parse program declaration
                    call lexer%next_token(token)
                    if (token%type == LEX_IDENTIFIER) then
                        name = lexer%get_token_text(token)
                    else
                        ! Anonymous program
                        name = "MAIN$" // filepath
                    end if
                    
                    if (self%on_program(filepath, name)) return
                    call skip_to_eol(lexer, token)
                
                case ("use")
                    ! Parse use statement
                    call lexer%next_token(token)
                    
                    ! Optional comma
                    if (token%type == LEX_OTHER .and. lexer%get_token_text(token) == ",") then
                        call lexer%next_token(token)
                    end if
                    
                    ! Optional ::
                    if (token%type == LEX_EQUAL .and. lexer%get_token_text(token) == "=") then
                        call lexer%next_token(token)
                        if (token%type == LEX_EQUAL .and. lexer%get_token_text(token) == "=") then
                            call lexer%next_token(token)
                        else
                            ! Invalid
                            call skip_to_eol(lexer, token)
                            cycle
                        end if
                    end if
                    
                    if (token%type == LEX_IDENTIFIER) then
                        name = lexer%get_token_text(token)
                        if (self%on_use(filepath, name)) return
                    end if
                    
                    call skip_to_eol(lexer, token)
                
                case ("submodule")
                    ! Parse submodule
                    call lexer%next_token(token)
                    if (token%type /= LEX_OTHER .or. lexer%get_token_text(token) /= "(") then
                        call skip_to_eol(lexer, token)
                        cycle
                    end if
                    
                    call lexer%next_token(token)
                    if (token%type /= LEX_IDENTIFIER) then
                        call skip_to_eol(lexer, token)
                        cycle
                    end if
                    ancestor = lexer%get_token_text(token)
                    
                    call lexer%next_token(token)
                    parent = ""
                    if (token%type == LEX_OTHER .and. lexer%get_token_text(token) == ":") then
                        call lexer%next_token(token)
                        if (token%type == LEX_OTHER .and. lexer%get_token_text(token) == ":") then
                            call lexer%next_token(token)
                            if (token%type == LEX_IDENTIFIER) then
                                parent = lexer%get_token_text(token)
                            else
                                call skip_to_eol(lexer, token)
                                cycle
                            end if
                        else
                            call skip_to_eol(lexer, token)
                            cycle
                        end if
                        call lexer%next_token(token)
                    end if
                    
                    if (token%type /= LEX_OTHER .or. lexer%get_token_text(token) /= ")") then
                        call skip_to_eol(lexer, token)
                        cycle
                    end if
                    
                    call lexer%next_token(token)
                    if (token%type /= LEX_IDENTIFIER) then
                        call skip_to_eol(lexer, token)
                        cycle
                    end if
                    name = lexer%get_token_text(token)
                    
                    if (self%on_submodule(filepath, ancestor, parent, name)) return
                    call skip_to_eol(lexer, token)
                
                case default
                    ! Not a keyword we care about, skip line
                    call skip_to_eol(lexer, token)
                end select
            else
                ! Not an identifier, skip line
                call skip_to_eol(lexer, token)
            end if
            
            call lexer%next_token(token)
        end do

        end associate
    contains

        subroutine skip_to_eol(lexer, token)
            type(lexer_t) :: lexer
            type(token_t) :: token
            do while(token%type /= LEX_NEWLINE .and. token%type /= LEX_EOF)
                call lexer%next_token(token)
            end do
        end subroutine

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

    logical function empty_on_module_end(self, filepath) result(abort)
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath
        abort = .false.
    end function

    logical function empty_on_submodule(self, filepath, ancestor, parent, name) result(abort) 
        class(minimal_parser_t) :: self
        character(*), intent(in) :: filepath, ancestor, parent, name
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

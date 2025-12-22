program fbs
    use makefile_deps_mod 

    type(makefile_deps_t) :: makefile_deps 

    character(len=:), allocatable :: filepath
    integer :: n_args, i
    logical :: ok

    n_args = command_argument_count()

    call makefile_deps%initialize()

    do i = 1, n_args
        call get_arg(i, filepath)
        call makefile_deps%process_file(filepath)
    end do

    call makefile_deps%export()

    contains

    subroutine get_arg(i, arg)
        integer, intent(in) :: i
        character(len=:), allocatable, intent(out) :: arg

        integer :: len
        call get_command_argument(i, length = len)
        allocate(character(len=len) :: arg)
        call get_command_argument(i, arg)
    end subroutine

end program


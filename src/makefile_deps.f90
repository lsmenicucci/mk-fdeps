module makefile_deps_mod
    use, intrinsic :: iso_fortran_env, only: int32, int8
    use minimal_parser_mod
    use hash_table_mod
    use string_arena_mod
    implicit none

    private
    public :: makefile_deps_t

    type, extends(minimal_parser_t) :: makefile_deps_t
        type(string_arena_t) :: file_names, module_names
        type(hash_table_t) :: files, modules, dependencies, providers
        integer(int32) :: current_file, current_module

        contains

        procedure :: initialize 
        procedure :: on_file, on_file_end, on_module, on_module_end, on_use
        procedure :: add_file, add_module 
        procedure :: export
    end type

    contains

    subroutine initialize(self)
        class(makefile_deps_t) :: self

        call self%files%initialize()
        call self%modules%initialize()
        call self%dependencies%initialize()
        call self%providers%initialize()

        call self%file_names%initialize()
        call self%module_names%initialize()
    end subroutine

    subroutine export(self)
        use ISO_FORTRAN_ENV, only: output_unit, error_unit
        class(makefile_deps_t) :: self
        
        integer(int32), allocatable :: module_provider(:), dependencies(:, :)
        character(len=:), allocatable :: module, file, other_file
        integer :: i, j, module_id, file_id, other_id
        logical :: first

        allocate(module_provider(self%modules%count))
        module_provider = 0

        ! Find providers
        do i = 1, self%providers%cap
            associate (p_entry => self%providers%entries(i)) 
                if (.not. p_entry%occupied) cycle
                call decode_dependency_key(p_entry%key, file_id, module_id)
            end associate
            
            if (module_provider(module_id) == 0) then
                module_provider(module_id) = file_id
            else
                module = self%module_names%get(module_id)
                file = self%file_names%get(module_provider(module_id))
                other_file = self%file_names%get(module_provider(file_id))

                101 FORMAT('Module ', A, ' is provided by: ', A, ' but is also declared in: ', A)
                write(error_unit, 101) module, file, other_file
            end if
        end do

        ! Find dependencies
        allocate(dependencies(2, self%dependencies%count))
        dependencies = 0

        j = 1
        do i = 1, self%dependencies%cap
            associate (d_entry => self%dependencies%entries(i)) 
                if (.not. d_entry%occupied) cycle
                call decode_dependency_key(d_entry%key, file_id, module_id)
            end associate

            dependencies(1, j) = file_id
            dependencies(2, j) = module_id
            j = j + 1
        end do

        do file_id = 1, self%files%count
            first = .true.

            do i = 1, size(dependencies, 2)
                if (dependencies(1, i) /= file_id) cycle
                module_id = dependencies(2, i)

                other_id = module_provider(module_id)
                if (other_id == 0) cycle
                if (other_id == file_id) cycle

                file = self%file_names%get(file_id)
                other_file = self%file_names%get(other_id)

                if (len_trim(file) == 0) cycle
                if (len_trim(other_file) == 0) cycle

                if (first) then
                    write(output_unit, "(A, ':', 1x, A)", advance = "no") file, other_file
                    first = .false.
                else 
                    write(output_unit, "(1x, A)", advance = "no") other_file
                end if
            end do

            if (.not. first) write(output_unit, *)
        end do
    end subroutine

    logical function on_file(self, filepath) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath
        abort = .false.

        self%current_file = self%add_file(filepath)
    end function

    logical function on_file_end(self, filepath) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath
        abort = .false.

        self%current_file = 0
    end function

    logical function on_module(self, filepath, name) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath, name

        integer(int32) :: dep_id
        integer(int8) :: key(8)
        abort = .false.

        self%current_module = self%add_module(name)
        call encode_dependency_key(self%current_file, self%current_module, key)
        dep_id = self%providers%insert(key)
    end function

    logical function on_module_end(self, filepath, name) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath, name
        integer :: other_file_id
        character(len=:), allocatable :: other_file
        logical :: existed
        abort = .false.

        self%current_module = self%add_module(name)
    end function

    logical function on_use(self, filepath, name) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath, name
        
        integer :: module_id, dep_id
        integer(int8) :: key(8)
        abort = .false.
        
        module_id = self%add_module(name)
        call encode_dependency_key(self%current_file, module_id, key)
        dep_id = self%dependencies%insert(key)
    end function

    integer function add_file(self, name) result(id)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: name

        logical :: existed

        id = self%files%insert(name, existed = existed)
        if (.not. existed) call self%file_names%insert(name)
    end function

    integer function add_module(self, name) result(id)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: name

        logical :: existed

        id = self%modules%insert(name, existed = existed)
        if (.not. existed) call self%module_names%insert(name)
    end function

    subroutine encode_dependency_key(from, to, key)
        integer(int32), intent(in) :: from, to     
        integer(int8), intent(out) :: key(8)
        
        call int32_to_iarr(from, key(1:4))
        call int32_to_iarr(to, key(5:8))
    end subroutine

    subroutine decode_dependency_key(key, from, to)
        integer(int8), intent(in) :: key(8)
        integer(int32), intent(out) :: from, to     
        
        call iarr_to_int32(key(1:4), from)
        call iarr_to_int32(key(5:8), to)
    end subroutine

end module

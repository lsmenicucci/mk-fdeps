module makefile_deps_mod
    use, intrinsic :: iso_fortran_env, only: int32, int8
    use minimal_parser_mod
    use hash_table_mod
    use string_arena_mod
    implicit none

    private
    public :: makefile_deps_t, clean_path, join_path

    type, extends(minimal_parser_t) :: makefile_deps_t
        type(string_arena_t) :: file_names, module_names
        type(hash_table_t) :: files, modules, dependencies, providers
        integer(int32) :: current_file, current_module

        character(len=:), allocatable :: with_parent, with_ext
        character(len=:), allocatable :: with_suffix, with_prefix
        integer :: strip_parents = 1
        

        contains

        procedure :: initialize 
        procedure :: on_file, on_file_end, on_module, on_module_end, on_use
        procedure :: add_file, add_module 
        procedure :: export, format_path
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

        self%with_ext = ".o"
        self%with_parent = "build/"
    end subroutine

    function format_path(self, path) result(output_path)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: path
        character(len=:), allocatable :: output_path

        integer :: i, parent_start, parent_end, stem_end
        
        output_path = ""
        if (len_trim(self%with_parent) > 0) then
            output_path = output_path // self%with_parent
        end if

        i = 0 
        parent_start = 1
        do while(i < self%strip_parents)
            parent_start = parent_start + index(path(parent_start:), "/")
            i = i + 1
        end do
        
        parent_end = index(path, "/", back=.true.)

        stem_end = parent_end + index(path(parent_end:), '.') - 1
        if (stem_end == parent_end) stem_end = len(path)

        output_path = output_path &
                        // path(parent_start:parent_end) &
                        // self%with_prefix &
                        // path(parent_end + 1:stem_end - 1) &
                        // self%with_suffix

        if (len_trim(self%with_ext) > 0) then
            output_path = output_path // self%with_ext
        else 
            output_path = output_path // path(stem_end + 1:)
        end if

    end function

    function clean_path(path) result(output_path)
        character(*), intent(in) :: path
        character(len=:), allocatable :: output_path

        integer :: istart, iend, pos, slice_len
        
        allocate(character(len = len(path)) :: output_path)

        istart = 1
        pos = 1

        do while (istart < len(path))
            do while (path(istart + 1:istart + 1) == '/' .and. istart + 1 < len(path))
                istart = istart + 1
            end do

            iend = istart + index(path(istart + 1:), "/")
            if (iend <= istart) iend = len(path)

            slice_len = iend - istart
            output_path(pos:pos + slice_len) = path(istart:iend)
            pos = pos + slice_len

            istart = iend
        end do

        output_path = adjustl(trim(output_path))

        if (output_path(pos:pos) == '/') then
            output_path = output_path(:pos - 1)
        end if
    end function

    function join_path(a, b) result(output_path)
        character(*), intent(in) :: a, b
        character(len=:), allocatable :: output_path

        integer :: istart, iend

        iend = len(a)
        do while(iend > 1 .and. a(iend:iend) == '/')
            iend = iend - 1
        end do

        istart = len(b)
        do while(istart + 1 < len(b) .and. b(istart:istart) == '/')
            istart = istart + 1
        end do

        output_path = a(:iend) // "/" // b(istart:)
    end function

    subroutine export(self, output)
        use iso_fortran_env, only: output_unit, error_unit
        class(makefile_deps_t) :: self
        character(*), intent(in) :: output
        
        integer(int32), allocatable :: module_provider(:), dependencies(:, :)
        character(len=:), allocatable :: module, file, other_file
        integer :: i, j, module_id, file_id, other_id
        logical :: first
        integer :: unit

        allocate(module_provider(self%modules%count))
        module_provider = 0

        unit = output_unit
        if (len_trim(output) > 0) then
            open(newunit = unit, file = output, status="replace")
        end if

        ! Find providers
        i = 0
        do while(self%providers%next(i))
            associate (p_entry => self%providers%entries(i)) 
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
        i = 0
        do while(self%dependencies%next(i))
            associate (d_entry => self%dependencies%entries(i)) 
                call decode_dependency_key(d_entry%key, file_id, module_id)
            end associate

            dependencies(1, j) = file_id
            dependencies(2, j) = module_id
            j = j + 1
        end do

        ! sorting 'dependencies(:, i)' on 'i' would improve performance

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

                file = self%format_path(file)
                other_file = self%format_path(other_file)

                if (first) then
                    write(unit, "(A, ':', 1x, A)", advance = "no") file, other_file
                    first = .false.
                else 
                    write(unit, "(1x, A)", advance = "no") other_file
                end if
            end do

            if (.not. first) write(unit, *)
        end do

        if (unit /= output_unit) then
            close(unit)
        end if
    end subroutine

    logical function on_file(self, filepath) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath
        abort = .false.

        self%current_file = self%add_file(clean_path(filepath), existed = abort)
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

    integer function add_file(self, name, existed) result(id)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: name
        logical, intent(out) :: existed

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

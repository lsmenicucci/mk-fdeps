module makefile_deps_mod
    use, intrinsic :: iso_fortran_env, only: int32, int8
    use minimal_parser_mod
    use hash_table_mod
    use string_arena_mod
    use int_arena_mod
    implicit none

    private
    public :: makefile_deps_t, clean_path, join_path

    type, extends(minimal_parser_t) :: makefile_deps_t
        type(string_arena_t) :: file_names, punit_names
        type(hash_table_t) :: files, puntis, dependencies, providers, targets, submodules
        integer(int32) :: current_file, current_module

        character(len=:), allocatable :: with_parent, with_ext
        character(len=:), allocatable :: with_suffix, with_prefix
        integer :: strip_parents = 1
        logical :: include_targets = .false., replace_ext = .false.

        contains

        procedure :: initialize 
        procedure :: on_file, on_file_end, on_module, on_module_end, on_use, on_program, on_submodule
        procedure :: add_file, add_punit 
        procedure :: export, format_path
    end type

    contains

    subroutine initialize(self)
        class(makefile_deps_t) :: self

        call self%files%initialize()
        call self%puntis%initialize()
        call self%dependencies%initialize()
        call self%submodules%initialize()
        call self%providers%initialize()
        call self%targets%initialize()

        call self%file_names%initialize()
        call self%punit_names%initialize()

        self%with_ext = ".o"
        self%replace_ext = .true.
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
        parent_end = max(parent_end, 1)

        stem_end = parent_end + index(path(parent_end:), '.') - 1
        if (stem_end == parent_end) stem_end = len(path)

        output_path = output_path &
                        // path(parent_start:parent_end) &
                        // self%with_prefix &
                        // path(parent_end + 1:stem_end - 1) &
                        // self%with_suffix

        if (self%replace_ext) then
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
        
        integer(int32), allocatable :: provider(:)
        logical, allocatable :: has_target(:)
        type(int_arena_t), allocatable :: module_sub_deps(:), file_deps(:)
        character(len=:), allocatable :: file, ext
        character(len=:), allocatable :: module, other_file
        integer :: i, j, mod_id, submod_id, file_id, n_deps, n_sub_deps
        logical :: first
        integer :: unit

        allocate(provider(self%puntis%count))
        provider = 0

        ! Find providers
        i = 0
        do while(self%providers%next(i))
            associate (p_entry => self%providers%entries(i)) 
                call decode_dependency_key(p_entry%key, file_id, mod_id)
            end associate
            
            if (provider(mod_id) == 0) then
                provider(mod_id) = file_id
            else
                module = self%punit_names%get(mod_id)
                file = self%file_names%get(provider(mod_id))
                other_file = self%file_names%get(provider(file_id))

                101 FORMAT('Module ', A, ' is provided by: ', A, ' but is also declared in: ', A)
                write(error_unit, 101) module, file, other_file
            end if
        end do

        ! Save file dependencies
        allocate(file_deps(self%file_names%size))
        do i = 1, size(file_deps)
            call file_deps(i)%initialize()
        end do

        i = 0
        j = 1
        do while(self%dependencies%next(i))
            associate (d_entry => self%dependencies%entries(i)) 
                call decode_dependency_key(d_entry%key, file_id, mod_id)
            end associate
            call file_deps(file_id)%insert([mod_id])
        end do

        ! Save submodule dependencies
        allocate(module_sub_deps(self%punit_names%size))
        do i = 1, size(module_sub_deps)
            call module_sub_deps(i)%initialize()
        end do

        i = 0
        do while(self%submodules%next(i))
            associate (e => self%submodules%entries(i)) 
                call decode_dependency_key(e%key, submod_id, mod_id)
            end associate
            call module_sub_deps(mod_id)%insert([submod_id])
        end do

        ! Add the implicit submodule dependencies to each module descendent
        do i = 1, size(file_deps)
            n_deps = file_deps(i)%size
            do j = 1, n_deps
                mod_id = file_deps(i)%buffer(j)
                n_sub_deps = module_sub_deps(mod_id)%size

                associate (sub_deps => module_sub_deps(mod_id)%buffer(:n_sub_deps) )
                    call file_deps(i)%insert(sub_deps)
                end associate
            end do
        end do

        ! Export
        unit = output_unit
        if (len_trim(output) > 0) then
            open(newunit = unit, file = output, status="replace")
        end if

        do i = 1, size(file_deps)
            n_deps = file_deps(i)%size
            associate ( deps => file_deps(i)%buffer(:n_deps) )
                ! Convert file_deps values to file_ids
                deps = provider(deps)
                deps = merge(0, deps, deps == i)

                ! All imports are external
                if (all(deps == 0)) cycle

                file = self%file_names%get(i)
                file = self%format_path(file)
                write(unit, "(A, ': ')", advance = 'no') file

                first = .true.
                do j = 1, n_deps 
                    if (deps(j) == 0) cycle
                    file = self%file_names%get(deps(j))
                    file = self%format_path(file)

                    if (first) then
                        write(unit, "(A)", advance = 'no') file
                        first = .false.
                    else 
                        write(unit, "(1x, A)", advance = 'no') file
                    end if
                end do
            end associate

            write(unit, *)
        end do

        ! Export targets
        if (self%include_targets) then
            allocate(has_target(self%file_names%size), source = .false.)

            i = 0
            j = 1
            do while(self%targets%next(i))
                associate (d_entry => self%targets%entries(i)) 
                    call decode_dependency_key(d_entry%key, file_id, mod_id)
                end associate
                has_target(file_id) = .true.
            end do

            if (.not. any(has_target)) return
            write(unit, *)

            do i = 1, self%file_names%size
                if (.not. has_target(i)) cycle

                n_deps = file_deps(i)%size
                associate ( deps => file_deps(i)%buffer(:n_deps) )
                    file = self%file_names%get(i)

                    ! format executable name
                    ext = self%with_ext ; self%with_ext = ""
                    file = self%format_path(file)
                    self%with_ext = ext

                    other_file = self%file_names%get(i)
                    other_file = self%format_path(other_file)

                    write(unit, "(A, ': ', A)", advance = 'no') file, other_file

                    do j = 1, n_deps 
                        if (deps(j) == 0) cycle
                        file = self%file_names%get(deps(j))
                        file = self%format_path(file)

                        write(unit, "(1x, A)", advance = 'no') file
                    end do
                end associate

                write(unit, *)
            end do

        end if

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

        self%current_module = self%add_punit(name)
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

        self%current_module = self%add_punit(name)
    end function

    logical function on_program(self, filepath, name) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath, name

        integer(int32) :: dep_id
        integer(int8) :: key(8)
        abort = .false.

        call encode_dependency_key(self%current_file, 0, key)
        dep_id = self%targets%insert(key)
    end function

    logical function on_submodule(self, filepath, ancestor, parent, name) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath, ancestor, parent, name
        
        integer :: punit_id, submod_id, dep_id
        integer(int8) :: key(8)
        abort = .false.

        submod_id = self%add_punit(name)
        punit_id  = self%add_punit(ancestor)

        call encode_dependency_key(self%current_file, submod_id, key)
        dep_id = self%providers%insert(key)

        ! Add submodule host dependency
        call encode_dependency_key(submod_id, punit_id, key)
        dep_id = self%submodules%insert(key)

        call encode_dependency_key(self%current_file, punit_id, key)
        dep_id = self%dependencies%insert(key)

        ! Add submodule ancestor dependency
        if (len_trim(parent) > 0) then
            punit_id = self%add_punit(parent)
            call encode_dependency_key(self%current_file, punit_id, key)
            dep_id = self%dependencies%insert(key)
        end if
         
    end function

    logical function on_use(self, filepath, name) result(abort)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: filepath, name
        
        integer :: punit_id, dep_id
        integer(int8) :: key(8)
        abort = .false.
        
        punit_id = self%add_punit(name)
        call encode_dependency_key(self%current_file, punit_id, key)
        dep_id = self%dependencies%insert(key)
    end function

    integer function add_file(self, name, existed) result(id)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: name
        logical, intent(out) :: existed

        id = self%files%insert(name, existed = existed)
        if (.not. existed) call self%file_names%insert(name)
    end function

    integer function add_punit(self, name) result(id)
        class(makefile_deps_t) :: self
        character(*), intent(in) :: name

        logical :: existed

        id = self%puntis%insert(name, existed = existed)
        if (.not. existed) call self%punit_names%insert(name)
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

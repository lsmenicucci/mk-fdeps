module int_arena_mod 
    use, intrinsic :: iso_fortran_env, only: int32, int8

    private
    public :: int_arena_t
    
    integer, parameter :: init_size = 1024
    integer, parameter :: growth_factor = 2

    type :: int_arena_t
        integer(int32), allocatable :: buffer(:)
        integer(int32) :: cap, size

        contains

        procedure :: initialize, grow
        procedure :: insert, reset
    end type

    contains

    subroutine initialize(self)
        class(int_arena_t) :: self
        
        allocate(self%buffer(init_size))
        self%cap = init_size
        self%size = 0
    end subroutine

    subroutine grow(self, target_cap)
        class(int_arena_t) :: self
        integer, intent(in) :: target_cap
        
        integer :: new_cap
        integer(int32), allocatable :: tmp(:)

        new_cap = max(self%cap * growth_factor, target_cap)
        allocate(tmp(new_cap))
        tmp(:self%cap) = self%buffer
        call move_alloc(from=tmp, to=self%buffer)

        self%cap = new_cap
    end subroutine

    subroutine insert(self, vals)
        class(int_arena_t) :: self
        integer(int32) :: vals(:)

        integer(int32) :: required_cap

        required_cap = self%size + size(vals)
        if (required_cap > self%cap) then
            call self%grow(required_cap)
        end if
        
        ! Insert 
        self%buffer(self%size + 1 : self%size + size(vals)) = vals
        self%size = self%size + size(vals)
    end subroutine

    subroutine reset(self)
        class(int_arena_t) :: self
        self%size = 0
    end subroutine
end module

module llc
    use, intrinsic :: iso_c_binding
    !$use omp_lib

    implicit none
    private
    public :: llc_fd, llc_fe, llc_f, llc_fs, llc_fl, llc_fls

contains

    elemental function llce(x, l, a) result(y)
    
        real(kind = c_double), intent(in)               :: x, l, a
        real(kind = c_double)                           :: y
        
        y = max(0.0_c_double, min(x - a, l))

    end function llce

    subroutine llc_fd(x, n, l, a, llc) bind(C, name = "llc_fd_")

        real(kind = c_double), intent(in)               :: l, a
        integer(kind = c_int), intent(in), value        :: n
        real(kind = c_double), intent(in), dimension(n) :: x
        real(kind = c_double), intent(out)              :: llc
        
        llc = sum(max(0.0_c_double, min(x - a, l)))

    end subroutine llc_fd
    
    subroutine llc_fe(x, n, l, a, llc) bind(C, name = "llc_fe_")

        real(kind = c_double), intent(in)               :: l, a
        integer(kind = c_int), intent(in), value        :: n
        real(kind = c_double), intent(in), dimension(n) :: x
        real(kind = c_double), intent(out)              :: llc

        llc = sum(llce(x, l, a))

    end subroutine llc_fe
    
    subroutine llc_f(x, n, l, a, llc) bind(C, name = "llc_f_")

        real(kind = c_double), intent(in)               :: l, a
        integer(kind = c_int), intent(in), value        :: n
        real(kind = c_double), intent(in), dimension(n) :: x
        real(kind = c_double), intent(out)              :: llc
        integer                                         :: i
        
        llc = 0.0_c_double
        do i = 1, n
            llc = llc + max(0.0_c_double, min(x(i) - a, l))
        end do

    end subroutine llc_f
    
    subroutine llc_fs(x, n, l, a, llc) bind(C, name = "llc_fs_")

        real(kind = c_double), intent(in)               :: l, a
        integer(kind = c_int), intent(in), value        :: n
        real(kind = c_double), intent(in), dimension(n) :: x
        real(kind = c_double), intent(out)              :: llc
        integer                                         :: i
        
        llc = 0.0_c_double
        !$omp do simd reduction(+:llc)
        do i = 1, n
            llc = llc + max(0.0_c_double, min(x(i) - a, l))
        end do
        !$omp end do simd
        
    end subroutine llc_fs
    
    subroutine llc_fl(x, n, l, a, llc) bind(C, name = "llc_fl_")

        real(kind = c_double), intent(in)               :: l, a
        integer(kind = c_int), intent(in), value        :: n
        real(kind = c_double), intent(in), dimension(n) :: x
        real(kind = c_double), intent(out)              :: llc
        integer                                         :: i
        
        llc = 0.0_c_double
        !$omp parallel do reduction(+:llc), schedule(static), private(i)
        do i = 1, n
            llc = llc + max(0.0_c_double, min(x(i) - a, l))
        end do
        !$omp end parallel do

    end subroutine llc_fl
    
    subroutine llc_fls(x, n, l, a, llc) bind(C, name = "llc_fls_")

        real(kind = c_double), intent(in)               :: l, a
        integer(kind = c_int), intent(in), value        :: n
        real(kind = c_double), intent(in), dimension(n) :: x
        real(kind = c_double), intent(out)              :: llc
        integer                                         :: i
        
        llc = 0.0_c_double
        !$omp parallel do simd reduction(+:llc), schedule(static)
        do i = 1, n
            llc = llc + max(0.0_c_double, min(x(i) - a, l))
        end do
        !$omp end parallel do simd

    end subroutine llc_fls
    
end module llc
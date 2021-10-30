! Author: Ian Orzel, iorzel2019@my.fit.edu
! Course: CSE 4250, Summer 2021
! Project: Proj1, Spread of Epidemics

! This program compiles with gfortan version :
! GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0


! This module is used in order to run calculations of the lambert-W function
module lambert_calc
    implicit none

    real*16 :: precision = 0.0000000001
    integer :: runs = 100000
    integer :: i

contains
    ! This function runs the calculation for the lambert-W function using Newton's Method
    ! INPUT :  n = The input value of the lambert-W
    ! OUTPUT :  old = The return value of the function
    function lambert(n) result(old)
        real*16, intent(in) :: n
        real*16 :: old

        real*16 :: new     ! Used for calculating the new value at each step
        real*16 :: diff    ! The difference between the new and old value
        real*16 :: e_old   ! The value of e^old

        old = 1
        new = old
        diff = precision * 2

        ! Loops until the difference is within the precision
        do while (diff >= precision)
!        do i = 0, runs
            e_old = exp(old)
            new = old - (old*e_old - n) / (e_old + old*e_old)
            diff = abs(new - old)

            old = new
        end do

    end function lambert
end module lambert_calc


program spread
    use lambert_calc
    implicit none

    integer :: n
    real*16 :: alpha
    real*16 :: gamma
    integer :: answer
    integer :: percent
    integer :: flag
    integer :: counter

    counter = 1
    flag = 0

    ! This will continue to loop through input until EOF is hit
    do while (flag == 0)
        read(*, *, IOSTAT=flag) n, alpha

        ! This will trigger once EOF is reached
        if (flag /= 0) then
            exit
        end if

        ! This computes the value of gamma
        gamma = 1 + lambert(-1 * alpha * exp(-1 * alpha)) / alpha

        ! This computes the values needing to be printed
        answer = NINT(n * gamma)
        percent = NINT(REAL(answer) / n * 100)

        print "(a, i5, a, i13, i4, a)", "Case ", counter, ": ", answer, percent, "%"

        counter = counter + 1
    end do
end program spread

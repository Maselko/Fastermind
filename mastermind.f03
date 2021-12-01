program mastermind
implicit none
    real, dimension(4) :: r
    real :: rtmp
    integer, dimension(4) :: ans, try, col
    integer, dimension(10,8) :: history
    integer :: i, j, k, itmp, ios, colok, posok
    character(len=:), allocatable :: printer, answer
    character(len=4) :: clr = achar(27)//'[0m'
    character(len=7) :: red = achar(27)//'[0;31m'
    character(len=7) :: grn = achar(27)//'[0;32m'
    character(len=7) :: yel = achar(27)//'[0;33m'
    character(len=7) :: blu = achar(27)//'[0;34m'
    character(len=7) :: pnk = achar(27)//'[0;35m'
    character(len=7) :: gry = achar(27)//'[0;37m'
    character :: opt

! generate answer
    print *, blu//'MASTERMIND'//clr
10  history = history * 0
    call random_number(r)
    init: do i = 1, 4
        ans(i) = floor(r(i)*6) + 1
        if (ans(i) .eq. 7) then
            ans(i) = 6
        end if
    end do init

    main: do i = 1, 10
        print *, ''
        print "(A6,1X,I2)", 'ROUND:', i
        board: do j = 1, 10
            printer = clr
            lcol: do k = 1, 4
                select case (history(11-j,k))
                case (1)
                    printer = printer//blu//"* "//clr
                case (2)
                    printer = printer//grn//"* "//clr
                case (3)
                    printer = printer//yel//"* "//clr
                case (4)
                    printer = printer//red//"* "//clr
                case (5)
                    printer = printer//pnk//"* "//clr
                case (6)
                    printer = printer//gry//"* "//clr
                case default
                    printer = printer//"- "
                end select
            end do lcol
            printer = printer//" | "
            rcol: do k = 5, 8
                select case (history(11-j,k))
                case(1)
                    printer = printer//'*'
                case(2)
                    printer = printer//red//'*'//clr
                case default
                    printer = printer//'-'
                end select
            end do rcol
            select case(j)
            case(6)
                print *, "     "//printer//"     1:"//blu//"*"//clr//&
                    &"  2:"//grn//"*"//clr
            case(7)
                print *, "     "//printer//"     3:"//yel//"*"//clr//&
                    &"  4:"//red//"*"//clr
            case(8)
                print *, "     "//printer//"     5:"//pnk//"*"//clr//&
                    &"  6:"//gry//"*"//clr
            case(9)
                print *, "     "//printer//"     Ctrl+C: QUIT"
            case default
                print *, "     "//printer
            end select
        end do board
        print *, ''
        ios = 1
        readin: do while (ios .ne. 0)
            write(*,"(A1,1X)",advance="no") '>'
            read(*,"(I6)",iostat=ios) itmp
            if (ios .eq. 0) then
                rangecheck: do j = 1, 4
                    rtmp = itmp / 10**(4-j)
                    if (floor(rtmp) .gt. 6 .or. floor(rtmp) .eq. 0) then
                        ios = 1
                        exit rangecheck
                    else
                        try(j) = floor(rtmp)
                        history(i, j) = try(j)
                        itmp = itmp - (try(j) * 10**(4-j))
                    end if
                end do rangecheck
            end if
        end do readin

        col = ans
        posok = 0
        colok = 0
        postest: do j = 1, 4
            if (try(j) .eq. col(j)) then
                posok = posok + 1
            end if
        end do postest
        coltestout: do j = 1, 4
            coltestin: do k = 1, 4
                if (try(j) .eq. col(k) .and. col(k) .gt. 0) then
                    colok = colok + 1
                    col(k) = 0
                    try(j) = 7
                end if
            end do coltestin
        end do coltestout
        colok = colok - posok

        if (posok .ne. 4) then
            record: do j = 5, 8
                if (posok .gt. 0) then
                    history(i,j) = 2
                    posok = posok - 1
                else if (colok .gt. 0) then
                    history(i,j) = 1
                    colok = colok - 1
                end if
            end do record
        else ! success!!
            print *, 'You solved it'//red//'!'//clr
            exit main
        end if
    end do main
    if (i .eq. 10) then
        print *, 'You lost.'
    end if
    answer = ''
    ansmake: do j = 1, 4
        select case (ans(j))
        case (1)
            answer = answer//blu//"* "//clr
        case (2)
            answer = answer//grn//"* "//clr
        case (3)
            answer = answer//yel//"* "//clr
        case (4)
            answer = answer//red//"* "//clr
        case (5)
            answer = answer//pnk//"* "//clr
        case (6)
            answer = answer//gry//"* "//clr
        end select
    end do ansmake
    print *, 'answer was: '//answer
    print *, ''
    print *, 'Do you want to play again?[Y/[N]]'
    write(*,"(A1,1X)",advance="no") '>'
    read(*,"(A1)",iostat=ios) opt
    itmp = iachar(opt)
    if (itmp .eq. 89 .or. itmp .eq. 121) then
        goto 10
    end if
end program mastermind

!program stats_1
!    integer :: n=0
!    real :: std_dev=0.
!    real :: sum_x=0.
!    real :: sum_x2=0.
!    real :: x=0.
!    real :: x_bar
!    
!    do
!        write (*,*) 'Enter number: '
!        read (*,*) x
!        write (*,*) 'The number is ' , x
!        if (x<0) exit
!        n=n+1
!        sum_x=sum_x+x
!        sum_x2=sum_x2+x**2
!    end do
!    if (n<2) then
!        write(*,*) ' '
!    else
!        x_bar=sum_x/real(n)
!        std_dev=sqrt((real(n)*sum_x2-sum_x**2)/(real(n)*real(n-1)))
!        write (*,*) x_bar
!        write (*,*) std_dev
!    end if
!end program stats_1
    
!program jiechen
!    implicit none
!    integer n_factorial,n,i
!    n_factorial=1
!    read (*,*) n
!    do i=1,n
!        n_factorial=n_factorial*i
!    end do
!    write (*,*) n_factorial
!end program jiechen

!program doy
!    integer :: year,month,day,day_of_year,n,i
!    read (*,*) year,month,day
!    if ( MOD(year,400)==0 ) then
!        n=29
!    else
!        if ( MOD(year,100)==0 ) then
!            n=28
!        else
!            if ( MOD(year,4)==0 ) then
!                n=29
!            else
!                n=28
!            end if
!        end if
!    end if
!    day_of_year=day
!    do i=1,month-1
!        select case (i)
!        case (1,3,5,7,8,10,12)
!            day_of_year=day_of_year+31
!        case (4,6,9,11)
!            day_of_year=day_of_year+30
!        case (2)
!            day_of_year=day_of_year+n
!        case default
!            write (*,*) '111'
!        end select
!    end do
!    write (*,*) 'day of year is : ',day_of_year
!end program doy
    
!program test_cycle
!    integer :: i
!    do i=1,5
!        if (i==3) cycle
!        write (*,*) i
!    end do
!end program test_cycle

!program test_exit
!    integer :: i
!    do i=1,5
!        if (i==3) exit
!        write (*,*) i
!    end do
!end program test_exit
    
!program nested_loope
!    integer :: i,j,product
!    do i=1,3
!        do j=1,3
!            do k=i,j
!                product=i*j
!                write (*,*) i,'*',j,'=',product
!                write (*,*) IACHAR('a')
!                write (*,*) len('annnn')
!            end do
!        end do
!    end do
!end program nested_loope
    
!program ball
!    implicit none
!    real,parameter :: degrees_2_rad=0.01745329
!    real,parameter :: gravity=-9.81
!    
!    integer :: max_degrees
!    real :: max_range
!    real :: range
!    real :: radian
!    integer :: theta
!    real :: v0
!    
!    max_range=0
!    max_degrees=0
!    v0=20
!    
!    do theta=1,90
!        radian=real(theta)*degrees_2_rad
!        range=-2*v0**2/gravity*cos(radian)*sin(radian)
!        write (*,*) range
!        if (range>max_range) then
!        max_range=range
!        max_degrees=theta
!        end if
!    end do
!    write (*,*) max_range
!    write (*,*) max_degrees
!end program ball

!program writ
!    implicit none
!    integer :: i = 100
!    real :: result = 2.0
!    write (*,100) i , result 
!    !100 format (' The result for iteration ',I3,' is ',F7.3) 
!    100 format ( 10X , I3 , F7.3 )
!end program writ

!program prin
!    write (*,100)
!    100 format (1X,' This heading is at the top of a new page . ')
!    write (*,110)
!    110 format ('0',' Control character    Action')    
!    write (*,120)
!    120 format (' ',' ============     =====')
!end program prin
    
program inpu
    integer :: ierror
    open( unit=8 , file=' example.dat ' , status='OLD' , action = 'READ' , iostat = ierror )
    WRITE (*,*) ierror
end program inpu
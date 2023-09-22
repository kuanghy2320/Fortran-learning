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
    
!program inpu
!    integer :: ierror
!    open( unit=8 , file=' example.dat ' , status='OLD' , action = 'READ' , iostat = ierror )
!    WRITE (*,*) ierror
!end program inpu
    
!program outpu
!    implicit none
!    integer :: unit,ierror
!    character (len=6) :: filename
!    unit=25
!    filename='outdat'
!    open(UNIT=unit,FILE=filename,STATUS='NEW',ACTION='WRITE',IOSTAT=ierror)
!end program outpu
    
!program arra
!    implicit none
!    real,dimension(16) :: voltage
!    character (len=20),dimension(50) :: last_name
!    real,dimension(10) :: array1
!    integer,dimension(10) :: number,square
!    integer,dimension(5) :: array2=(/1,2,3,4,5/)
!    integer,dimension(1000) :: array3=(/ (i,i=1,1000) /)
!    integer,dimension(25) :: array4=(/ ((0,i=1,4),5*j,j=1,5) /)
!    real,dimension(100) :: array5=1.0
!    real,dimension(-2:2) :: b1
!    
!    array1=0.!初始化数组中的所有元素为同一个值
!    integer :: i
!    !(1)
!    do i = 1,10
!        voltage(i)=real(i)
!        write (*,*) voltage(i)
!        number(i)=i
!        square=number(i)**2
!    end do
!    !(2)
!    
!    array1 = (/1.,2.,3.,4.,5.,6.,7.,8.,9.,10./)
!    write (*,*) array1
!    write (*,*) array1(1:10:2)
!    array(:)
!    
!    integer , dimension(5) :: vec=(/1,6,4,1,9/)
!    real , dimension(10) :: a=(/1.,2.,3.,4.,5.,6.,7.,8.,9./)
!    a(vec)
!    
!    write (*,100) (i,2*i,3*i,i=1,3)
!100 format (1X,9I6)    
!    write (*,101) ((i,j,j=1,3),i=1,2)
!101 format (1X,I5,1X,I5)
!end program arra
    
!Selection sort
!program selec
!    real , dimension(5) :: a=(/10.,6.,8.,7.,1./)
!    real :: temp
!    integer :: index
!    
!    do j=1,4
!        temp=a(j)
!        index=j
!        do i=j,5
!           if (temp>a(i)) then
!           temp=a(i)
!           index=i
!           end if
!        end do
!        a(index)=a(j)
!        a(j)=temp
!    end do
!    write (*,*) a
!end program selec
    
!program test_hypotenuse
!    implicit none
!    real :: s1
!    real :: s2
!    real :: hypot
!    read (*,*) s1
!    read (*,*) s2
!    
!    call calc_hypotenuse (s1,s2,hypot)
!    write (*,1000) hypot
!1000 format (1X,F10.4)    
!end program test_hypotenuse
!
!subroutine calc_hypotenuse (side_1,side_2,hypotenuse)
!    implicit none
!    real,intent(in) :: side_1
!    real,intent(in) :: side_2
!    real,intent(out) :: hypotenuse
!    integer :: n
!    real,dimension(n),intent(inout) :: arr
!    real :: temp
!    
!    temp=side_1**2+side_2**2
!    hypotenuse=sqrt(temp)
!end subroutine calc_hypotenuse
!
!subroutine process (data1,data2,n,nvals)
!    integer,intent(in) :: n,nvals
!    real,intent(in),dimension(n) :: data1
!    real,intent(out),dimension(n) :: data2
!    
!    do i=1,nvals
!        data2(i)=3.*data1(i)
!    end do
!    
!    data2(1:nvals)=3.*data1(1:nvals)
!    
!end subroutine process
    
!program array2
!    implicit none
!    integer :: i
!    real,dimension(5) :: a=0.
!    
!    call sub1 (a,5,5)
!    do i=1,5
!        write (*,*) i,a(i)
!    end do
!end program array2
!
!subroutine sub1 (a,ndim,n)
!    implicit none
!    integer,intent(in) :: ndim
!    real,intent(out),dimension(ndim) :: a
!    integer,intent(in) :: n
!    integer :: i
!    do i=1,n
!        a(i)=i
!    end do
!end subroutine sub1
!
!subroutine sample (string)
!    character(len=*),intent(in) :: string
!end subroutine sample
!
!subroutine process (a,b,result1,error)
!    implicit none
!    real,intent(in) :: a,b
!    real,intent(out) :: result1
!    integer,intent(out) :: error
!    real :: temp
!    temp=a-b
!    if (temp>=0.) then
!        result=sqrt(temp)
!        error=0
!    else
!        result=0
!        error=1
!    end if
!end subroutine process
    
!program 
!
!end program 
!
!subroutine rmax ()
!
!end subroutine rmax
!
!subroutine rmin ()
!
!end subroutine rmin
!
!subroutine ave_sd ()
!
!end subroutine ave_sd
!
!subroutine median ()
!
!end subroutine median
    

!module shared_data
!    implicit none
!    save
!    integer,parameter :: num_vals=5
!    real,dimension(num_vals) :: values
!end module shared_data
!
!program test_module
!    use shared_data
!    implicit none
!    real,parameter :: PI=3.141592
!    values=PI*(/1.,2.,3.,4.,5./)
!    call sub1
!    write (*,*) values
!end program test_module
!
!subroutine sub1
!    use shared_data
!    implicit none
!    write (*,*) values
!end subroutine sub1
    
!module ran001
!    implicit none
!    save
!    integer :: n=9876
!end module ran001
!
!subroutine random0 (ran)
!    use ran001
!    implicit none
!    real,intent(out) :: ran
!    n=mod(8121*n+28411,134456)
!    ran=real(n)/134456.
!end subroutine random0
!
!subroutine seed (iseed)
!    use ran001
!    implicit none
!    integer,intent(in) :: iseed
!    n=abs(iseed)
!end subroutine seed
!
!program test_random0
!    use ran001
!    implicit none
!    real :: ave
!    integer :: iseed
!    integer :: iseq
!    real :: ran
!    real :: sum
!    integer :: i
!    
!    read (*,*) iseed
!    call seed (iseed)
!    do i=1,10
!        call random0 (ran)
!        write (*,*) ran
!    end do
!end program test_random0

!module my_subs
!    implicit none
!    contains
!        subroutine sub1 (a,b,c,x,error)
!        implicit none
!        real,dimension(3),intent(in) :: a
!        real,intent(in) :: b,c
!        real,intent(out) :: x
!        logical,intent(out) :: error
!        end subroutine sub1
!end module my_subs
!
!program main_prog
!    use my_subs
!    implicit none  
!    call sub1 (a,b,c,x,error)
!end program main_prog
!program yt
!implicit none
!real :: a
!real :: b
!real :: discrinant
!
!end program yt

!real function quadf (x,a,b,c)
!    implicit none
!    real,intent(in) :: x,a,b,c
!    quadf=a*x**2+b*x+c
!end function quadf
!
!program test_quadf
!    implicit none
!    real :: quadf
!    real :: a,b,c,x
!    read (*,*) a,b,c,x
!    write (*,*) quadf(x,a,b,c)
!!100 format (A,F10.4,A,F12.4)    
!end program test_quadf
    
!function sinc(x)
!    implicit none
!    real,intent(in) :: x
!    real :: sinc
!    real,parameter :: epsilon=1.0E-30
!    
!    if (abs(x) > epsilon) then
!        sinc=sin(x)/x
!    else
!        sinc=1.
!    end if
!end function sinc
!program test_sinc
!    implicit none
!    real :: sinc
!    real :: x
!    read (*,*) x
!    write (*,*) sinc(x)
!end program test_sinc
    
!program test
!    real,external :: fun_1,fun_2
!    real :: x,y,output
!
!    call evalute (fun_1,x,y,output)
!    call evalute (fun_2,x,y,output)
!
!end program test
!
!subroutine evalute (fun,x,y,output)
!    real,external :: fun
!    real,intent(in) :: x,y
!    real,intent(out) :: output
!    output=y*fun(x)
!end subroutine evalute
    
!real function ave_value (func,first_name,last_name,n)
!    implicit none
!    real,external :: func
!    real,intent(in) :: first_name
!    real,intent(in) :: last_name
!    integer,intent(in) :: n
!    real :: delta
!    integer :: i
!    real :: sum
!    
!    delta=(last_name-first_name)/(real(n-1))
!    sum=0.
!    do i=1,n
!        sum=sum+func(real(i-1)*delta)
!    end do
!    ave_value=sum/real(n)
!end function ave_value
!    
!program test_ave_value
!    implicit none
!    real :: ave_value
!    real,external :: my_function
!    real :: ave
!    ave=ave_value(my_function,0.,1.,101)
!    write (*,*) ave
!end program 
!
!real function my_function (x)
!    implicit none
!    real,intent(in) :: x
!    my_function=3.*x
!end function my_function
    
subroutine subs_as_arguments (x,y,sub,result1)
    implicit none
    external :: sub
    real,intent(in) :: x
    real,intent(in) :: y
    real,intent(out) :: result1
    call sub(x,y,result1)
end subroutine subs_as_arguments

program test_subs_as_arguments
    implicit none
    external :: sum,prod
    real :: x
    real :: y
    real :: result1
    read (*,*) x
    read (*,*) y
    call subs_as_arguments (x,y,prod,result1)
    write (*,*) result1
    call subs_as_arguments (x,y,sum,result1)
    write (*,*) result1
end program test_subs_as_arguments

subroutine prod (x,y,result1)
    implicit none
    real,intent(in) :: x
    real,intent(in) :: y
    real,intent(out) :: result1
    result1=x*y
end subroutine prod

subroutine sum (x,y,result1)
    implicit none
    real,intent(in) :: x
    real,intent(in) :: y
    real,intent(out) :: result1
    result1=x+y
end subroutine sum
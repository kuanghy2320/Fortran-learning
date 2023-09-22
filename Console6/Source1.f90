program funxy
    implicit none
    real :: x,y,fxy
    read (*,*) x,y
    
    if ((x>=0.) .AND. (y>=0.)) then
        fxy=x+y
    else if ((x>=0.) .AND. (y<0.)) then
        fxy=x+y**2
    else if ((x<0.) .AND. (y>=0.)) then
        fxy=x**2+y
    else
        fxy=x**2+y**2
    end if
    write (*,*) fxy
    stop
end program funxy
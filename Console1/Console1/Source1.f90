program roots
    ! Purpose:
    !  This program solves for the roots of a quadratic equation of the form
    !  A*X**2+B*X+C=0
    
    implicit none
    ! Declare the variables used in this program
    real :: a
    real :: b
    real :: c
    real :: discriminant
    real :: imag_part
    real :: real_part
    real :: x1
    real :: x2
    
    !prompt the user for the coefficients of the equation
    write (*,*) 'This program solves for the roots of a quadratic'
    write (*,*) 'equation of the form A*X**2+B*X+C=0'
    write (*,*) 'enter the coefficients A  B and C:'
    read (*,*) a,b,c
    
    !Echo back coefficients
    write (*,*) 'the coefficients A B and C are :',a,b,c
    
    !Calculate discriminant
    discriminant = b**2-4*a*c
    
    !solve for the roots ,depending upon the value of the discriminant
    if (discriminant>0) then !there are two real roots,so ...
        x1=(-b+sqrt(discriminant))/(2.*a)
        x2=(-b-sqrt(discriminant))/(2.*a)
        write (*,*) 'this equation has two real roots:'
        write (*,*) 'X1=',X1
        write (*,*) 'X2=',X2
    else if (discriminant==0) then
        x1=(-b)/(2.*a)
        write (*,*) 'this equation has two identical real roots:'
        write (*,*) 'X1=X2=',X1
    else
        real_part=(-b)/(2.*a)
        imag_part=sqrt(abs(discriminant))/(2.*a)
        write (*,*) 'this program has complex roots:'
        write (*,*) 'X1=',real_part,'+i',imag_part
        write (*,*) 'X2=',real_part,'-i',imag_part
    end if
    end program roots
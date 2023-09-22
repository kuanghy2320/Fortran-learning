program temperature_conversion
    implicit none
    real :: a,b
    write (*,*) ' Enter the temperature in degrees fahrenheit: '
    read (*,*) a
    
    !convert to kelvins
    b=(5./9.*(a-32.))+273.5
    write (*,*) a,'degrees fahrenheit = ',b,'kelvins'
end program temperature_conversion
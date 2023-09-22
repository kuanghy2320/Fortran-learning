!program mixup
!    if1:if ( grade > 95.0 ) then
!        write (*,*) 'The grade is A'
!    else
!        if2: if (grade > 86.0) then
!            write (*,*) 'The grade is B'
!        else
!            if3: if (grade > 80) then
!                write (*,*) 'The grade is C'
!            else
!            end if if3
!        end if if2
!    end if if1
!end program mixup
!
program mixup2
    integer :: temp_c
    
    temp : select case (temp_c)
    case (:-1)
        write (*,*) 
    case (0) 
        write (*,*) 
    case (1:20)
        write (*,*) 
    case (21:)
        write (*,*) 
    case default
        write (*,*)
    end select temp
end program mixup2
!program test
!    real,dimension(3,6) :: sum
!    integer,dimension(0:100,0:20) :: hist
!    character(len=6),dimension(-3:3,10) :: counts
!    integer,dimension(3,4) :: istat,istat1
!    integer,dimension(4,3) :: istat2(4,3)=reshape((/1,1,1,1,2,2,2,2,3,3,3,3/),(/4,3/))
!    integer,dimension(4,3) :: istat3
!    open (7,file='initial,dat',status='old',action='read')
!    read (7,*) istat3
!    
!    do i=1,3
!        do j=1,4
!            istat(i,j)=j
!        end do
!    end do
!    istat1=reshape((/1,1,1,1,2,2,2,2,3,3,3,3/),(/3,4/))
!    write (*,*) istat,istat1
!end program test
    
program check_array
    implicit none
    real,dimension(-5:5,0:3) :: a=0.
    real,dimension(-5:5,0:3) :: logval
    write (*,*) shape(a)
    write (*,*) size(a)
    write (*,*) lbound(a)
    write (*,*) ubound(a)
    where(a>0.)
        logval=log(a)
    elsewhere
        logval=-99999.
    end where
    write (*,*) logval
end program check_array
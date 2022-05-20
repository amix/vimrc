module prettify_selftest
    implicit none
   private
     public :: dp, test_routine, &
                 test_function,  test_type,    str_function
   integer,   parameter :: dp = selected_real_kind ( 15 , 307)
   type  test_type
      real  (kind =dp ) :: r = 1.0d-3
      integer :: i
   end type test_type

contains


   subroutine test_routine( &
   r, i, j, k, l)
        integer, intent(in)                                :: r, i, j, k
       integer,   intent (out)                               :: l

    l  = test_function(r,i,j,k)
 end &
subroutine

 pure function test_function(r, i, j, &
                                        k) &
   result(l)
   integer, intent(in)                                :: r, i, j, k
   integer                                            :: l

      l=r + i +j  +k
   end function
   function &
        str_function(a)      result(l)
      character(len=*)                                   :: a
      integer                                            :: l

     if(len(a)<5)then
       l=0
      else
       l=1
       endif
end function

end module

program example_prog
 use example, only: dp, test_routine, test_function, test_type,str_function

   implicit  none
  integer :: r,i,j,k,l,my_integer,m
     integer, dimension(5) :: arr
   integer, dimension(20) :: big_arr
integer :: endif
   type(test_type) :: t
real(kind=dp) :: r1,   r2,  r3, r4, r5,  r6
  integer, pointer :: point

  point=>  null( )

! 1) white space formatting !
!***************************!
! example 1.1
   r=1;i=-2;j=3;k=4;l=5
   r2  =  0.0_dp; r3= 1.0_dp;   r4 =2.0_dp; r5=3.0_dp;  r6 = 4.0_dp
   r1=-(r2**i*(r3+r5*(-r4)-r6))-2.e+2
   if( r.eq.2.and.r<=5) i=3
   write(*, *)(merge(3, 1, i<=2))
   write(*, *)test_function(r,i,j , k)
   t % r  = 4.0_dp
   t%i  = str_function( "t  % i   =  " )

! example 1.2
   my_integer=2
   i=3
   j=5

   big_arr = [1, 2, 3, 4, 5, &
              6, 7, 8, 9, 10, &
              11, 12, 13, 14, 15, &
              16, 17, 18, 19, 20]

! example 1.3: disabling auto-formatter:
   my_integer = 2 !&
   i          = 3 !&
   j          = 5 !&

!&<
   my_integer = 2
   i          = 3
   j          = 5
!&>

   big_arr = [ 1,  2,  3,  4,  5, & !&
               6,  7,  8,  9, 10, & !&
              11, 12, 13, 14, 15, & !&
              16, 17, 18, 19, 20] !&

! example 1.4:

   big_arr = [1, 2, 3, 4, 5,&
           &  6, 7, 8, 9, 10, &
           & 11, 12, 13, 14, 15,&
            &16, 17, 18, 19, 20]

! 2) auto indentation for loops !
!*******************************!

! example 2.1
   l = 0
   do r=   1 , 10
    select case (r)
      case(1)
    do_label: do i  =  1,100
        if  (i<=2)   then
          m =0
      do while(m <4)
              m =m+1
             do k=1,3
      if (k==1)  l =l +1
    end do
      enddo
     endif
    enddo do_label
    case ( 2 )
      l=i + j + k
      end select
   enddo

! example 2.2
   do m = 1, 2
     do r = 1, 3
        write (*, *) r
         do k = 1, 4
         do l = 1, 3
         do i = 4, 5
          do my_integer = 1, 1
          do j = 1, 2
          write (*, *) test_function(m, r, k, l) + i
            enddo
            enddo
         enddo
        enddo
          enddo
      enddo
   enddo

! 3) auto alignment for linebreaks   !
!************************************!

! example 3.1
   l = test_function(1, 2, test_function(1, 2, 3, 4), 4) + 3 *(2+1)

   l = test_function (1, 2, test_function(1,2, 3, 4),4)  +&
              3*(2+ 1 )

   l =  test_function(1, 2,   &
               test_function(1, 2,  3, 4),  4)+ &
                                    3 * (2+1)

   l = test_function(1, 2, &
   test_function(1, 2, 3, &
   4), 4) + &
   3*(2 + 1)

! example 3.2
   arr = [1, (/3,4, 5/),   6] + [  1, 2,3, 4,5 ]

   arr = [1,(/ 3, 4, 5 /) , &
   6]  +[1,2, 3, 4, 5 ]

   arr = [1,(/3,4,5/),  &
   6]+ &
             [1, 2, 3, 4, 5]

   arr = [1, (/3, 4, &
         5/), &
         6] + &
         [1, 2,3, 4, 5 ]

! example 3.3
   l = test_function(1, 2, &
         3, 4)

   l = test_function( &
                  1, 2, 3, 4)

   arr = [1, 2, &
        3, 4, 5]
   arr = [ &
            1, 2, 3, 4, 5]

! 4) more complex formatting and tricky test cases !
!**************************************************!

! example 4.1
   l = 0
   do r = 1, 10
         select case ( r )
   case( 1)
           do i=1,100;if (i<=2) then! comment
          do j = 1,5
                    do   k= 1, 3
                 l = l +  1
! unindented comment
         ! indented comment
              end do; enddo
                elseif ( .not. j ==4 ) then
                  my_integer  =   4
               else
            write (*,*)   " hello"
             endif
           enddo
            case(2 )
           l = i+ j + k
              end select
               enddo

! example 4.2
   if ( &
    l == &
       111) &
         then
       do k = 1, 2
   if (k == 1) &
               l = test_function(1, &
                          test_function(r=4, i=5, &
                      j=6, k=test_function(1,2*(3*(1 +1)), str_function ( ")a!(b['(;=dfe"), &
                                                                 9) + &
                            test_function(1, 2, 3, 4)), 9, 10) &
                     ! test_function(1,2,3,4)),9,10) &
            ! +13*str_function('') + str_function('"')
                    + 13*str_function('') + str_function('"')
                    end & ! comment
          ! comment
      do
       endif

! example 4.3
   arr = [1,( /3,4, &
      5 /),&
          6 ]+ &
    [1,2, 3, 4,5] ; arr = [1, 2,&
       3, 4, 5]

! example 4.4
   endif = 3
   if(endif==2)then
   endif=5
   else if(endif==3)then
   write(*,*)endif
   endif

! example 4.5
   do i=1,2;if(.true.)then
   write(*, *)"hello"
   endif; enddo

   end program

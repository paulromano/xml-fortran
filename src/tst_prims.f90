! tst_prims.f90 - Test program for reading the primitive data
!
! $Id: tst_prims.f90,v 1.2 2006/03/26 19:05:48 arjenmarkus Exp $
!
! Arjen Markus
!
! General information:
! This program tests the data reading routines in
! read_xml_prims.f90
! To avoid editing the private/public clauses in the
! module I have copied the code in here.
!
program tst_prims

   character(len=20)              :: buffer
   integer, dimension(:), pointer :: var
   integer                        :: ierror
   integer                        :: nofail

   !
   ! Tests: correct numbers returned?
   ! 1. Empty string, size(var) = 0
   ! 2. One value, size(var) = 1
   ! 3. Four values, size(var) = 4
   ! 4. Five values, size(var) = 5
   ! 5. Invalid data, ierror /= 0
   !
   nofail = 0
   var    => null()

   buffer = ' '
   call read_from_buffer_integers( buffer, var, ierror )
   if ( size(var) /= 0 .or. ierror /= 0 ) then
      write(*,*) 'Test 1 failed - expected:'
      write(*,*) 'Size: ', 0, ' - got:', size(var)
      write(*,*) 'Error:', 0, ' - got:', ierror
      nofail = nofail+1
   endif

   buffer = '   1 '
   call read_from_buffer_integers( buffer, var, ierror )
   if ( size(var) /= 1 .or. ierror /= 0 ) then
      write(*,*) 'Test 2 failed - expected:'
      write(*,*) 'Size: ',  2, ' - got:', size(var)
      write(*,*) 'Error:',  0, ' - got:', ierror
      write(*,*) 'Values:', var
      nofail = nofail+1
   endif

   buffer = '   1 2 3 4 '
   call read_from_buffer_integers( buffer, var, ierror )
   if ( size(var) /= 4 .or. ierror /= 0 ) then
      write(*,*) 'Test 3 failed - expected:'
      write(*,*) 'Size: ',  4, ' - got:', size(var)
      write(*,*) 'Error:',  0, ' - got:', ierror
      write(*,*) 'Values:', var
      nofail = nofail+1
   endif

   buffer = '   1 2 3 4 5 '
   call read_from_buffer_integers( buffer, var, ierror )
   if ( size(var) /= 5 .or. ierror /= 0 ) then
      write(*,*) 'Test 4 failed - expected:'
      write(*,*) 'Size: ',  5, ' - got:', size(var)
      write(*,*) 'Error:',  0, ' - got:', ierror
      write(*,*) 'Values:', var
      nofail = nofail+1
   endif

   buffer = '   1 A 3 4 5 '
   call read_from_buffer_integers( buffer, var, ierror )
   if ( size(var) /= 1 .or. ierror == 0 ) then
      write(*,*) 'Test 5 failed - expected:'
      write(*,*) 'Size: ',  1, ' - got:', size(var)
      write(*,*) 'Error:', '/= 0', ' - got:', ierror
      write(*,*) 'Values:', var
      nofail = nofail+1
   endif

   if ( nofail == 0 ) then
      write(*,*) 'All tests succeeded'
   endif
contains

! read_from_buffer_integers --
!    Routine to read all integers from a long string
!
! Arguments:
!    buffer      String containing the data
!    var         Variable to be filled
!    ierror      Error flag
!
subroutine read_from_buffer_integers( buffer, var, ierror )
   character(len=*), intent(in)                  :: buffer
   integer, dimension(:), pointer                :: var
   integer, intent(inout)                        :: ierror

   integer, dimension(:), pointer                :: work
   integer                                       :: n
   integer                                       :: i
   integer                                       :: step
   integer                                       :: ierr
   !
   ! First allocate an array that is surely large enough
   ! Note:
   ! This is not completely failsafe: with list-directed
   ! input you can also use repeat counts (10000*1.0 for
   ! instance).
   !
   allocate( work(len(buffer)/2+1) )

   !
   ! NOTE:
   ! This is not portable!!
   !
   ! read( buffer, *, iostat = ierror ) (work(n), n=1,size(work))
   !
   ! So, use a different strategy: a binary search
   ! First: establish that we have at least one item to read
   ! Second: do the binary search
   !
   n = 1
   do while ( n <= size(work) )
      n = 2 * n
   enddo
   n    = n / 2
   step = n / 2

   do while ( step > 0 )
      read( buffer, *, iostat = ierr ) (work(i), i = 1,n)
      if ( ierr /= 0 ) then
         ierror = ierr       ! Store the error code for later use
         n = n - step
      else
         n = n + step
      endif
      step = step / 2
   enddo

   !
   ! Then allocate an array of the actual size needed
   ! and copy the data
   !
   !
   if ( associated( var ) ) then
      deallocate( var )
   endif
   !
   ! One complication: we may have one too many
   ! (consequence of the binary search)
   !
   read( buffer, *, iostat = ierr ) (work(i), i = 1,n)
   if ( ierr < 0 ) then
      n = n - 1
   endif

   allocate( var(n) )
   var(1:n) = work(1:n)
   deallocate( work )

   if ( ierror .lt. 0 ) then
      ierror = 0
   endif
end subroutine read_from_buffer_integers

end program

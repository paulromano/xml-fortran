! test_read_prims.f90 - Test program for reading primitive data
!
! $Id: test_read_prims.f90,v 1.3 2006/03/26 19:05:48 arjenmarkus Exp $
!
! Arjen Markus
!
! General information:
! This is a test program for the module read_xml_primitives,
! part of the XML-Fortran library.
!
program test_read_prims
   use read_xml_primitives

   implicit none

   type(XML_PARSE)                               :: info
   character(len=40)                             :: tag
   character(len=40), dimension(2,10)            :: attribs
   integer                                       :: noattribs
   character(len=80), dimension(100)             :: data
   integer                                       :: nodata
   integer                                       :: var
   integer, dimension(:), pointer                :: array
   logical                                       :: has_var

   !
   ! Test cases regarding reading integers
   !
   attribs(1,1) = 'value'
   attribs(2,1) = '    1'
   noattribs    = 1
   data(1)      = '    2'
   nodata       = 1
   var          = -1
   has_var      = .false.

   tag          = 'integer-test'

   var       = -1
   noattribs = 1
   nodata    = 1
   call read_xml_integer( info, tag, attribs, noattribs, data, nodata, &
                          var, has_var )
   if ( var .ne. 1 ) then
      write(*,*) 'Error 1: var should be 1, is', var
   endif
   if ( .not. has_var ) then
      write(*,*) 'Error 1a: has_var should be true'
   endif

   var       = -1
   noattribs = 0
   nodata    = 1
   call read_xml_integer( info, tag, attribs, noattribs, data, nodata, &
                          var, has_var )
   if ( var .ne. 2 ) then
      write(*,*) 'Error 2: var should be 2, is', var
   endif
   if ( .not. has_var ) then
      write(*,*) 'Error 2a: has_var should be true'
   endif

   var       = -1
   noattribs = 0
   nodata    = 0
   call read_xml_integer( info, tag, attribs, noattribs, data, nodata, &
                          var, has_var )
   if ( var .ne. -1 ) then
      write(*,*) 'Error 3: var should be -1, is', var
   endif
   if ( has_var ) then
      write(*,*) 'Error 3a: has_var should be FALSE'
   endif
   if ( has_var ) then
      write(*,*) 'Error 3a: has_var should be FALSE'
   endif

   var       = -1
   noattribs = 0
   nodata    = 3
   data(1)   = '  '
   data(2)   = '  '
   data(3)   = ' 3'
   call read_xml_integer( info, tag, attribs, noattribs, data, nodata, &
                          var, has_var )
   if ( var .ne. 3 ) then
      write(*,*) 'Error 4: var should be 3, is', var
   endif
   if ( .not. has_var ) then
      write(*,*) 'Error 4a: has_var should be true'
   endif

   var       = -1
   noattribs = 0
   nodata    = 3
   data(1)   = '  '
   data(2)   = '  '
   data(3)   = 'xxx'
   call read_xml_integer( info, tag, attribs, noattribs, data, nodata, &
                          var, has_var )
   if ( var .ne. -1 ) then
      write(*,*) 'Error 5: var should be -1, is', var
   endif
   if ( has_var ) then
      write(*,*) 'Error 5a: has_var should be FALSE'
   endif

   nullify( array )
   noattribs = 1
   attribs(1,1) = 'values'
   attribs(2,1) = '1 2 3'
   nodata    = 3
   data(1)   = '  '
   data(2)   = '  '
   data(3)   = 'xxx'
   call read_xml_integer_array( info, tag, attribs, noattribs, data, &
                                nodata, array, has_var )
   if ( size(array) .ne. 3 ) then
      write(*,*) 'Error 6: array should be size 3, is', size(array)
   endif
   if ( any(array .ne. (/1,2,3/)) ) then
      write(*,*) 'Error 6: array should be 1,2,3, is', array
   endif
   if ( .not. has_var ) then
      write(*,*) 'Error 6a: has_var should be TRUE'
   endif

   noattribs = 1
   attribs(1,1) = 'xxxxxx'
   attribs(2,1) = '3 4 5'
   nodata    = 3
   data(1)   = '1  '
   data(2)   = '2  '
   data(3)   = '3  '
   call read_xml_integer_array( info, tag, attribs, noattribs, data, &
                                nodata, array, has_var )
   if ( size(array) .ne. 3 ) then
      write(*,*) 'Error 7: array should be size 3, is', size(array)
   endif
   if ( any(array .ne. (/1,2,3/)) ) then
      write(*,*) 'Error 7: array should be 1,2,3, is', array
   endif
   if ( .not. has_var ) then
      write(*,*) 'Error 7a: has_var should be TRUE'
   endif

   noattribs = 0
   attribs(1,1) = 'values'
   attribs(2,1) = '1 2 3'
   nodata    = 3
   data(1)   = '1 2'
   data(2)   = '   '
   data(3)   = '3  '
   call read_xml_integer_array( info, tag, attribs, noattribs, data, &
                                nodata, array, has_var )
   if ( size(array) .ne. 3 ) then
      write(*,*) 'Error 8: array should be size 3, is', size(array)
   endif
   if ( any(array .ne. (/1,2,3/)) ) then
      write(*,*) 'Error 8: array should be 1,2,3, is', array
   endif
   if ( .not. has_var ) then
      write(*,*) 'Error 8a: has_var should be TRUE'
   endif

   noattribs = 1
   attribs(1,1) = 'values'
   attribs(2,1) = 'xxx'
   nodata    = 3
   data(1)   = '1 2'
   data(2)   = '   '
   data(3)   = '3  '
   call read_xml_integer_array( info, tag, attribs, noattribs, data, &
                                nodata, array, has_var )
   if ( has_var ) then
      write(*,*) 'Error 9a: has_var should be FALSE'
   endif

end program

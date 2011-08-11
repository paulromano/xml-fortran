! test_write_prims.f90 - Test program for writing primitive data
!
! $Id: test_write_prims.f90,v 1.1 2007/06/10 10:08:38 arjenmarkus Exp $
!
! Arjen Markus
!
! General information:
! This is a test program for the module write_xml_primitives,
! part of the XML-Fortran library.
!
program test_write_prims
   use write_xml_primitives

   implicit none

   type(XML_PARSE)                               :: info
   character(len=40)                             :: tag
   integer                                       :: var
   integer, dimension(:), pointer                :: array
   logical                                       :: has_var

   info%lun = 10
   open( info%lun, file='test_write.xml' )

   call write_to_xml_integer( info, 'integer', 4, -123456789 )
   call write_to_xml_real(    info, 'real', 3, -1.23456789 )
   call write_to_xml_double(  info, 'double', 3, -1.23456789d0 )
   call write_to_xml_logical( info, 'logical', 3, .true. )
   call write_to_xml_logical( info, 'logical', 4, .false. )
   call write_to_xml_string( info, 'string', 4, 'Hm, some string' )

   call write_to_xml_integer_array( info, 'integers', 4, &
       (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 /) )
   call write_to_xml_real_array( info, 'reals', 4, &
       (/ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 /) )
   call write_to_xml_double_array( info, 'doubles', 4, &
       (/ 1.0d0, 2.0d0, 3.0d0,  4.0d0,  5.0d0,  6.0d0, &
          7.0d0, 8.0d0, 9.0d0, 10.0d0, 11.0d0, 12.0d0 /) )
   call write_to_xml_logical_array( info, 'logicals', 4, &
       (/ .true., .true., .true., .false., .false., .false., &
          .true., .true., .true., .false., .false., .false.  /) )

   close( info%lun )

end program

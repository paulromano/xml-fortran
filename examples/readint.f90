module xml_data_readint
   use READ_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_
   integer                                         :: x
   integer                                         :: y
   integer                                         :: z
   integer, dimension(:), pointer                  :: w
contains
subroutine init_xml_file_readint
   x = 1
   y = 2
   z = 2
end subroutine

subroutine read_xml_file_readint(fname, lurep, errout)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep
   logical, intent(out), optional         :: errout

   type(XML_PARSE)                        :: info
   logical                                :: error
   character(len=80)                      :: tag
   logical                                :: endtag
   character(len=80), dimension(1:2,1:20) :: attribs
   integer                                :: noattribs
   character(len=200), dimension(1:100)   :: data
   integer                                :: nodata
   logical                                         :: has_x
   logical                                         :: has_y
   logical                                         :: has_z
   logical                                         :: has_w
   has_x                                = .false.
   has_y                                = .false.
   has_z                                = .false.
   has_w                                = .false.

   call init_xml_file_readint
   call xml_open( info, fname, .true. )
   call xml_options( info, report_errors=.true. )
   lurep_ = 0
   if ( present(lurep) ) then
      lurep_ = lurep
      call xml_options( info, report_lun=lurep )
   endif
   strict_ = .false.
   error = .false.
   do
      call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
      if ( xml_error(info) ) then
         write(*,*) 'Error reading input file!'
         stop
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('x')
         call read_xml_integer( &
            info, tag, attribs, noattribs, data, nodata, &
            x, has_x )
      case('y')
         call read_xml_integer( &
            info, tag, attribs, noattribs, data, nodata, &
            y, has_y )
      case('z')
         call read_xml_integer( &
            info, tag, attribs, noattribs, data, nodata, &
            z, has_z )
      case('w')
         call read_xml_integer_array( &
            info, tag, attribs, noattribs, data, nodata, &
            w, has_w )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            if ( lurep_ .gt. 0 ) then
               write( lurep_, * ) 'Unknown or wrongly placed tag: ',  trim(tag)
            else
               write( *, * ) 'Unknown or wrongly placed tag: ',  trim(tag)
            endif
         endif
      end select
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_w ) then
      error = .true.
      if ( lurep_ .gt. 0 ) then
         write(lurep_,*) 'Missing data on w'
      else
         write(*,*) 'Missing data on w'
      endif
   endif
   if ( present(errout) ) errout = error
end subroutine

end module

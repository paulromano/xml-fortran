module xml_data_GRID
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_
   integer                                         :: A
   integer                                         :: B
   integer                                         :: C

type grid_t
   integer                                         :: X
   integer                                         :: Y
end type grid_t
   type(grid_t)                                    :: grid
   type(grid_t), dimension(:), pointer             :: grid_array => null()
contains
subroutine read_xml_place_general( info, starttag, attribs, noattribs, data, nodata )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata

   logical                                      :: error
   logical                                      :: endtag
   character(len=len(starttag))                 :: tag
   logical                                         :: has_A
   logical                                         :: has_B
   logical                                         :: has_C
   has_A                                = .false.
   has_B                                = .false.
   has_C                                = .false.
   error = .false.
   do
      call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
      if ( xml_error(info) ) then
         write(lurep_,*) 'Error reading input file!'
         error = .true.
         return
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('A')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            A, has_A )
      case('B')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            B, has_B )
      case('C')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            C, has_C )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_B ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on B')
   endif
   if ( .not. has_C ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on C')
   endif
end subroutine read_xml_place_general
subroutine write_xml_place_general( &
      info, indent )
   type(XML_PARSE)                          :: info
   integer                                  :: indent
   character(len=100)                       :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<general>'
   call write_to_xml_integer( info, 'A', indent+3, A)
   call write_to_xml_integer( info, 'B', indent+3, B)
   call write_to_xml_integer( info, 'C', indent+3, C)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</general>'
end subroutine write_xml_place_general

subroutine read_xml_type_grid_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(grid_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(grid_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_grid_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_grid_t_array

subroutine read_xml_type_grid_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(grid_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_X
   logical                                         :: has_Y
   has_X                                = .false.
   has_Y                                = .false.
   call init_xml_type_grid_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('X')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%X, has_X )
      case('Y')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Y, has_Y )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_Y ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on Y')
   endif
end subroutine read_xml_type_grid_t
subroutine init_xml_type_grid_t_array( dvar )
   type(grid_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_grid_t_array
subroutine init_xml_type_grid_t(dvar)
   type(grid_t) :: dvar
   dvar%X = 20
end subroutine init_xml_type_grid_t
subroutine write_xml_type_grid_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(grid_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_grid_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_grid_t_array

subroutine write_xml_type_grid_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(grid_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_integer( info, 'X', indent+3, dvar%X)
   call write_to_xml_integer( info, 'Y', indent+3, dvar%Y)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_grid_t

subroutine read_xml_file_GRID(fname, lurep, errout)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep
   logical, intent(out), optional         :: errout

   type(XML_PARSE)                        :: info
   logical                                :: error
   character(len=80)                      :: tag
   character(len=80)                      :: starttag
   logical                                :: endtag
   character(len=80), dimension(1:2,1:20) :: attribs
   integer                                :: noattribs
   character(len=200), dimension(1:100)   :: data
   integer                                :: nodata
   logical                                         :: has_grid
   logical                                         :: has_grid_array
   has_grid                             = .false.
   has_grid_array                       = .false.
   allocate(grid_array(0))

   call init_xml_file_GRID
   call xml_open( info, fname, .true. )
   call xml_options( info, report_errors=.true., ignore_whitespace=.true.)
   lurep_ = 0
   if ( present(lurep) ) then
      lurep_ = lurep
      call xml_options( info, report_lun=lurep )
   endif
   do
      call xml_get( info, starttag, endtag, attribs, noattribs, &
         data, nodata)
      if ( starttag .ne. '!--' ) exit
   enddo
   if ( starttag .ne. "grid_example" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "grid_example"')
      error = .true.
      call xml_close(info)
      return
   endif
   strict_ = .true.
   error = .false.
   do
      call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
      if ( xml_error(info) ) then
         write(lurep_,*) 'Error reading input file!'
         error = .true.
         return
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('general')
         call read_xml_place_general( info, &
            tag, attribs, noattribs, data, nodata )
      case('grid')
         call read_xml_type_grid_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            grid, has_grid )
      case('grid_array')
         call read_xml_type_grid_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            grid_array, has_grid_array )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_grid ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on grid')
   endif
   if ( .not. has_grid_array ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on grid_array')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_GRID(fname, lurep)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep

   type(XML_PARSE)                        :: info
   integer                                :: indent = 0

   call xml_open( info, fname, .false. )
   call xml_options( info, report_errors=.true.)
   if ( present(lurep) ) then
       call xml_options( info, report_errors=.true.)
   endif
   write(info%lun,'(a)') &
      '<grid_example>'
   call write_xml_place_general( info, indent+3 )
   call write_xml_type_grid_t( info, 'grid', indent+3, grid)
   call write_xml_type_grid_t_array( info, 'grid_array', indent+3, grid_array)
   write(info%lun,'(a)') '</grid_example>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_GRID
   A = 12

end subroutine

end module

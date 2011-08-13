module xml_data_menu
   use READ_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_

type submenu_t
   character(len=10)                                :: item
end type submenu_t

type item_t
   character(len=20)                                :: type
   character(len=20)                                :: name
   type(submenu_t)                                 :: submenu
   character(len=1)                                :: data
end type item_t

type menu_t
   character(len=20)                                :: name
   type(item_t), dimension(:), pointer             :: item => null()
end type menu_t

type menubar_t
   character(len=20)                                :: name
   type(menu_t), dimension(:), pointer             :: menu => null()
   character(len=10), dimension(2,3)                :: number
end type menubar_t
   type(menubar_t)                                 :: menubar
   integer                                         :: just_a_value
   character(len=10), dimension(5,7)                :: string_array
   integer, dimension(7)                           :: int_array
contains
subroutine read_xml_type_submenu_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(submenu_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(submenu_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_submenu_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_submenu_t_array

subroutine read_xml_type_submenu_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(submenu_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_item
   has_item                             = .false.
   call init_xml_type_submenu_t(dvar)
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
      case('item')
         call read_xml_line( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%item, has_item )
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
   if ( .not. has_item ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on item')
   endif
end subroutine read_xml_type_submenu_t
subroutine init_xml_type_submenu_t_array( dvar )
   type(submenu_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_submenu_t_array
subroutine init_xml_type_submenu_t(dvar)
   type(submenu_t) :: dvar
end subroutine init_xml_type_submenu_t
subroutine read_xml_type_item_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(item_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(item_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_item_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_item_t_array

subroutine read_xml_type_item_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(item_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_type
   logical                                         :: has_name
   logical                                         :: has_submenu
   logical                                         :: has_data
   has_type                             = .false.
   has_name                             = .false.
   has_submenu                          = .false.
   has_data                             = .false.
   call init_xml_type_item_t(dvar)
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
      case('type')
         call read_xml_line( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%type, has_type )
      case('name')
         call read_xml_line( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%name, has_name )
      case('submenu')
         call read_xml_type_submenu_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%submenu, has_submenu )
      case('data')
         call read_xml_line( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%data, has_data )
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
   if ( .not. has_type ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on type')
   endif
   if ( .not. has_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on name')
   endif
end subroutine read_xml_type_item_t
subroutine init_xml_type_item_t_array( dvar )
   type(item_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_item_t_array
subroutine init_xml_type_item_t(dvar)
   type(item_t) :: dvar
   dvar%submenu = submenu_t('')
   dvar%data = '?'
end subroutine init_xml_type_item_t
subroutine read_xml_type_menu_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(menu_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(menu_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_menu_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_menu_t_array

subroutine read_xml_type_menu_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(menu_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_name
   logical                                         :: has_item
   has_name                             = .false.
   has_item                             = .false.
   allocate(dvar%item(0))
   call init_xml_type_menu_t(dvar)
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
      case('name')
         call read_xml_line( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%name, has_name )
      case('item')
         call read_xml_type_item_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%item, has_item )
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
   if ( .not. has_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on name')
   endif
   if ( .not. has_item ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on item')
   endif
end subroutine read_xml_type_menu_t
subroutine init_xml_type_menu_t_array( dvar )
   type(menu_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_menu_t_array
subroutine init_xml_type_menu_t(dvar)
   type(menu_t) :: dvar
end subroutine init_xml_type_menu_t
subroutine read_xml_type_menubar_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(menubar_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(menubar_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_menubar_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_menubar_t_array

subroutine read_xml_type_menubar_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(menubar_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_name
   logical                                         :: has_menu
   character(len=10), dimension(:), pointer         :: p_number
   logical                                         :: has_number
   has_name                             = .false.
   has_menu                             = .false.
   allocate(dvar%menu(0))
   has_number                           = .false.
   call init_xml_type_menubar_t(dvar)
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
      case('name')
         call read_xml_line( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%name, has_name )
      case('menu')
         call read_xml_type_menu_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%menu, has_menu )
      case('number')
         call read_xml_word_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            p_number, has_number )
         if ( has_number) then
            if ( size(p_number) .ge. size(dvar%number) ) then
               dvar%number = reshape(p_number, shape(dvar%number))
            else
               has_number = .false.
               call xml_report_errors(info, 'Incorrect number of values for number')
            endif
            deallocate( p_number )
         endif
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
   if ( .not. has_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on name')
   endif
   if ( .not. has_menu ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on menu')
   endif
   if ( .not. has_number ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on number')
   endif
end subroutine read_xml_type_menubar_t
subroutine init_xml_type_menubar_t_array( dvar )
   type(menubar_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_menubar_t_array
subroutine init_xml_type_menubar_t(dvar)
   type(menubar_t) :: dvar
end subroutine init_xml_type_menubar_t
subroutine read_xml_file_menu(fname, lurep, errout)
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
   logical                                         :: has_menubar
   logical                                         :: has_just_a_value
   character(len=10), dimension(:), pointer         :: p_string_array
   logical                                         :: has_string_array
   integer, dimension(:), pointer                  :: p_int_array
   logical                                         :: has_int_array
   has_menubar                          = .false.
   has_just_a_value                     = .false.
   has_string_array                     = .false.
   has_int_array                        = .false.

   call init_xml_file_menu
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
   if ( starttag .ne. "menus" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "menus"')
      error = .true.
      call xml_close(info)
      return
   endif
   strict_ = .false.
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
      case('menubar')
         call read_xml_type_menubar_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            menubar, has_menubar )
      case('just_a_value')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            just_a_value, has_just_a_value )
      case('string_array')
         call read_xml_word_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            p_string_array, has_string_array )
         if ( has_string_array) then
            if ( size(p_string_array) .ge. size(string_array) ) then
               string_array = reshape(p_string_array, shape(string_array))
            else
               has_string_array = .false.
               call xml_report_errors(info, 'Incorrect number of values for string_array')
            endif
            deallocate( p_string_array )
         endif
      case('int_array')
         call read_xml_integer_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            p_int_array, has_int_array )
         if ( has_int_array) then
            if ( size(int_array) <= size(p_int_array) ) then
               int_array = p_int_array(1:size(int_array))
            else
               int_array(1:size(p_int_array)) = p_int_array
            endif
            deallocate( p_int_array )
         endif
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
   if ( .not. has_menubar ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on menubar')
   endif
   if ( .not. has_string_array ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on string_array')
   endif
   if ( .not. has_int_array ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on int_array')
   endif
   if ( present(errout) ) errout = error
end subroutine
subroutine init_xml_file_menu
   just_a_value = 23

end subroutine

end module

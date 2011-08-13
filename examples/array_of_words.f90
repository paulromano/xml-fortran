module xml_data_array_of_words
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_
   character(len=10), dimension(:), pointer         :: word => null()
   real, dimension(:), pointer                     :: real_value => null()
contains
subroutine read_xml_file_array_of_words(fname, lurep, errout)
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
   logical                                         :: has_word
   logical                                         :: has_real_value
   has_word                             = .false.
   allocate(word(0))
   has_real_value                       = .false.
   allocate(real_value(0))

   call init_xml_file_array_of_words
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
   if ( starttag .ne. "words" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "words"')
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
      case('word')
         call read_xml_word_1dim( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            word, has_word )
      case('real')
         call read_xml_real_1dim( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            real_value, has_real_value )
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
   if ( .not. has_word ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on word')
   endif
   if ( .not. has_real_value ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on real_value')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_array_of_words(fname, lurep)
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
      '<?xml version="1.0"?>', &
      '<words>'
   call write_to_xml_word_1dim( info, 'word', indent+3, word)
   call write_to_xml_real_1dim( info, 'real', indent+3, real_value)
   write(info%lun,'(a)') '</words>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_array_of_words

end subroutine

end module

! Test program for xml_process
!
! The program reads a small file in the docbook format
! (extracted from the PLplot project - http://plplot.sf.net)
! and writes it out as an HTML file
!
! Note:
! In this example we do not check the structure of the
! XML file.
! Of course the conversion is very, very simple.
!
module convert_html
   character(len=20)                :: section_type
   character(len=20), dimension(10) :: end_tag
   logical                          :: title
   integer                          :: level = 0
contains

subroutine startfunc( tag, attribs, error )
   character(len=*)                 :: tag
   character(len=*), dimension(:,:) :: attribs
   logical                          :: error

   level = level + 1
   select case( tag )
   case( 'chapter' )
      section_type = '<h1>'
   case( 'sect1' )
      section_type = '<h2>'
   case( 'title' )
      title = .true.
      end_tag(level) = section_type(1:1) // '/' // section_type(2:)
   case( 'para')
      write( 20, * ) '<p>'
      end_tag(level) = '</p>'
   case( 'itemizedlist' )
      write( 20, * ) '<ul>'
      end_tag(level) = '</ul>'
   case( 'orderedlist' )
      write( 20, * ) '<ol>'
      end_tag(level) = '</ol>'
   case( 'listitem' )
      write( 20, * ) '<li>'
      end_tag(level) = '</li>'
   end select
end subroutine

subroutine datafunc( tag, data, error )
   character(len=*)               :: tag
   character(len=*), dimension(:) :: data
   logical                        :: error

   integer                        :: i

   ! Nothing much to do ...

   if ( .not. title ) then
      if ( any( data .ne. ' ' ) ) then
         write(20,'(a)') ( data(i), i=1,size(data) )
      endif
   else
      write(20,'(a)') section_type, ( data(i), i=1,size(data) ), end_tag(level)
      level = level - 1
   endif

end subroutine

subroutine endfunc( tag, error )
   character(len=*)               :: tag
   logical                        :: error

   integer                        :: i

   ! Nothing much to do ...
   if ( title ) then
      title = .false.
   else
      write(20,*) trim(end_tag(level))
      level = level - 1
   endif

end subroutine

end module

program tst_process
   use xmlparse
   use convert_html

   character(len=40), dimension(2,10) :: attribs
   character(len=80), dimension(100)  :: data
   logical                            :: error

   open( 20, file = 'simple.html' )
   write( 20 , * ) '<html><head><title>Example of converting XML to HTML</title><head>'
   write( 20 , * ) '<body>'

   call xml_process( 'simple.xml', attribs, data, startfunc, datafunc, endfunc, 0, error )

   write( 20 , * ) '</body></html>'

end program

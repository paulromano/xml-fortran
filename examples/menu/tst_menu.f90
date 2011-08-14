! Test program for generated code:
! A menubar with menus and submenus
!
program tst_menu
   use xml_data_menu
   implicit none
   integer :: i, j, k

   call read_xml_file_menu( 'menuitems.xml' )
   write(*,*) 'Just a value: ', just_a_value
   write(*,*) 'Menubar: ', menubar%name
   write(*,*) '   Number:', menubar%number
   write(*,*) '   (Shape:', shape(menubar%number), ')'
   write(*,*) '   Menus: '
   do i = 1,size(menubar%menu)
      write(*,*) '   Menu: ', menubar%menu(i)%name
      write(*,*) '      Items: '
      do j = 1,size(menubar%menu(i)%item)
         write(*,*) '      Item: ', menubar%menu(i)%item(j)%name,' - ', &
            menubar%menu(i)%item(j)%type
         write(*,*) '            Data:', menubar%menu(i)%item(j)%data
         if ( menubar%menu(i)%item(j)%type == 'submenu' ) then
            write(*,*) '         Submenu: '
            !
            ! Should be an array ... but there is a problem with defaults
            !
            write(*,*) '            Item: ', menubar%menu(i)%item(j)%submenu%item

            ! do k = 1,size(item)
            !    write(*,*) '            Item: ', menubar%menu(i)%item(j)%submenu%item(k)%name &
            !       '- ', menubar%menu(i)%item(j)%submenu%item(k)%(name,type)
            ! enddo
         endif
      enddo
   enddo
end program

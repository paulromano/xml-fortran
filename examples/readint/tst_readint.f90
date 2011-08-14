! Test program for generated code
!
program tst_readint
   use xml_data_readint
   call read_xml_file_readint( 'readint_example.xml' )
   write(*,*) 'x,y,z:', x, y, z
   write(*,*) 'w:', w
end program

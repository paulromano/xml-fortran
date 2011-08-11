! Test program for generated code
!
program tst_grid
   use xml_data_grid
   call read_xml_file_grid( 'grid_example.xml' )
   write(*,*) 'A,B,C:', a, b, c
   write(*,*) 'grid:', grid
   write(*,*) 'grid_array:', grid_array
   call write_xml_file_grid( 'out_grid.xml', 20 )
end program

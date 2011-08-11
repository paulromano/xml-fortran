! Test program for generated code:
! Test the option of one word or real per element
!
program tst_array_of_words
   use xml_data_array_of_words
   implicit none

   call read_xml_file_array_of_words( 'example_words.xml' )

   write(*,*) 'Words:'
   write(*,'(a)') word
   write(*,*) 'Reals:'
   write(*,'(f10.4)') real_value

   call write_xml_file_array_of_words( 'output_words.xml' )
end program

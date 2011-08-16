program test_tallies

  use xml_data_tallies_t

  integer :: i

  call read_xml_file_tallies_t('tallies.xml')

  do i = 1, size(tally)
     print *, "Tally ", tally(i) % id
     print *, "Regions: ", trim(tally(i) % filters % region)
     print *, "Energy: ", trim(tally(i) % filters % energy)
  end do

end program test_tallies

program test_tallies

  use xml_data_tallies_t

  integer :: i

  call read_xml_file_tallies_t('tallies.xml')

  do i = 1, size(tally_)
     print *, "Tally ", tally_(i) % id
     print *, "Cell: ", trim(tally_(i) % filters % cell)
     print *, "Energy: ", trim(tally_(i) % filters % energy)
  end do

end program test_tallies

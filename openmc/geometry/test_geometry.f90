program test_geometry

  use xml_data_geometry_t

  integer :: i, n

  call read_xml_file_geometry_t('geometry.xml')

  print *, associated(cell(1) % surface)

  do i = 1, size(cell)
     print *, 'uid =', cell(i) % uid
     print *, 'universe = ', cell(i) % universe
     print *, 'material = ', cell(i) % material
     print *, 'surfaces = ', cell(i) % surface
     print *
  end do

end program test_geometry

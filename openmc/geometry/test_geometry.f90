program test_geometry

  use xml_data_geometry_t

  integer :: i

  call read_xml_file_geometry_t('geometry.xml')

  do i = 1, size(cell_)
     print *, 'uid      =', cell_(i) % uid
     print *, 'universe = ', cell_(i) % universe
     print *, 'material = ', cell_(i) % material
     print *, 'fill     = ', cell_(i) % fill
     print *, 'surfaces = ', cell_(i) % surfaces
     print *
  end do

  do i = 1, size(surface_)
     print *, 'uid      = ', surface_(i) % uid
     print *, 'type     = ', trim(surface_(i) % type)
     print *, 'coeffs   = ', surface_(i) % coeffs
     print *, 'boundary = ', trim(surface_(i) % boundary)
     print *
  end do

end program test_geometry

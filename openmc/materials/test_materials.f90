program test_material

  use xml_data_materials_t

  integer :: i, j, n, m

  call read_xml_file_materials_t('material.xml')

  n = size(material_)
  do i = 1, n
     print *, 'uid =', material_(i) % uid
     print *, 'density =', material_(i) % density % value
     print *, 'units =', material_(i) % density % units
     m = size(material_(i) % nuclides)
     print *, 'number of nuclides =', m
     do j = 1, m
        print *, 'nuclide name =', material_(i) % nuclides(j) % name
        print *, 'nuclide xs =', material_(i) % nuclides(j) % xs
        print *, 'nuclide ao =', material_(i) % nuclides(j) % ao
        print *, 'nuclide wo =', material_(i) % nuclides(j) % wo
     end do
     print *
  end do

end program test_material

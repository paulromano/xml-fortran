program test_material

  use xml_data_material_t

  integer :: i, j, n, m

  call read_xml_file_material_t('material.xml')

  n = size(materials)
  do i = 1, n
     print *, 'id =', materials(i) % id
     print *, 'density =', materials(i) % density % value
     m = size(materials(i) % nuclides)
     print *, 'number of nuclides =', m
     do j = 1, m
        print *, 'nuclide name =', materials(i) % nuclides(j) % id
        print *, 'nuclide xs =', materials(i) % nuclides(j) % xs
        print *, 'nuclide ao =', materials(i) % nuclides(j) % ao
        print *, 'nuclide wo =', materials(i) % nuclides(j) % wo
     end do
     print *
  end do

end program test_material

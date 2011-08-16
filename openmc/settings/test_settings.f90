program test_settings

  use xml_data_settings_t

  call read_xml_file_settings_t('settings.xml')

  print *, "Xslibrary = ", trim(xslibrary % path)
  print *, "Cycles    = ", criticality % cycles
  print *, "Inactive  = ", criticality % inactive
  print *, "Particles = ", criticality % particles
  print *, "Verbosity = ", verbosity

end program test_settings

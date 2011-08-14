! writexml.f90 --
!     Example of writing a simple XML file
!
!     Note:
!     This example uses the fairly low-level routine xml_put.
!     In the near future the xmlreader program should be able
!     to produce a writer routine automatically in much the
!     same way as it can produce a reader routine from the
!     definition.
!
program writexml
    use xmlparse

    implicit none
    type(xml_parse)                    :: info
    character(len=20)                  :: tag
    character(len=20), dimension(2,10) :: attribs
    character(len=50), dimension(10)   :: data
    integer                            :: no_attribs
    integer                            :: no_data
    character(len=20)                  :: type

!
! Open the file for writing (.false.)
!
    call xml_open( info, "example.xml", .false. )

!
! Write the root element (only the open tag)
!
    no_attribs = 0
    no_data    = 0
    call xml_put( info, "examples", attribs, no_attribs, data, no_data, "open" )

!
! Write a few elements:
! Each element is given with all information that is required, so we
! can write the opening and closing tags at once.
!
    no_attribs   = 1
    attribs(1,1) = "name"
    attribs(2,1) = "tst_menu.f90"
    no_data      = 2
    data(1)      = "Define a menubar with nested menus"
    data(2)      = "Shows how to deal with arrays"
    call xml_put( info, "example", attribs, no_attribs, data, no_data, "elem")
!
    no_attribs   = 3
    attribs(1,1) = "name"
    attribs(2,1) = "tst_grid.f90"
    attribs(1,2) = "definition"
    attribs(2,2) = "grid.xml"
    attribs(1,3) = "input"
    attribs(2,3) = "grid_example.xml"
    no_data      = 2
    data(1)      = "Conglomerate of options, nothing really structured"
    data(2)      = "- despite the name"
    call xml_put( info, "example", attribs, no_attribs, data, no_data, "elem")
!
! Close the root tag
!
    no_attribs  = 0
    no_data     = 0
    call xml_put( info, "examples", attribs, no_attribs, data, no_data, "close")
!
! Close the file
!
    call xml_close( info )

end program

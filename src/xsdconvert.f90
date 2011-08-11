! xsdconvert.f90 --
!     Auxiliary program to convert an XSD file to a schema file
!     as used by xmlreader
!
module handle_elems
    use xmlparse

    implicit none

    integer :: lunout

    logical, dimension(0:10), save   :: intype = .false.
    logical, save                    :: rootelement = .false.
    logical, dimension(0:10), save   :: optional = .false.
    integer, save                    :: type_level = 0
    integer, save                    :: level = 0
    integer, save                    :: type_count = 0
    character(len=40), save          :: rootname

    type complextype_definition
        character(len=40)                :: type_name
        character(len=40), dimension(20) :: attrib_name
        character(len=40), dimension(20) :: attrib_type
        logical, dimension(20)           :: attrib_use
        character(len=40), dimension(20) :: attrib_default
        logical, dimension(20)           :: has_default
        integer                          :: number_attribs
        logical                          :: is_array
    end type complextype_definition

    type(complextype_definition), dimension(10), save :: type_def
    logical, dimension(10), save :: type_write

contains

! start_elem --
!     Handle the start of the elements in the XSD file
!
! Arguments:
!     tag              Tag that was found
!     attribs          List of attributes and their values
!     error            Error parameter (output)
!
! Note:
!     Required because CVF 6.6 gave a strange error message when
!     trying to use start_elem directly.
!
recursive subroutine start_elemx( tag, attribs, error )

    character(len=*)                 :: tag
    character(len=*), dimension(:,:) :: attribs
    logical                          :: error

    call start_elem( tag, attribs, error )
end subroutine

recursive subroutine start_elem( tag, attribs, error )

    character(len=*)                 :: tag
    character(len=*), dimension(:,:) :: attribs
    logical                          :: error

    character(len=100), dimension(100) :: new_data
    character(len=100), dimension(2,100) :: new_attribs
    character(len=300)               :: filename
    character(len=40)                :: name
    character(len=40)                :: type
    integer                          :: idx

!!    write( lunout, * ) '>>Tag: ', tag, ' - level:', type_level
    select case( tag )
        case( 'schema', 'annotation', 'documentation', '!--' )
            ! Ignore
        case( 'xs:schema', 'xs:annotation', 'xs:documentation' )
            ! Ignore
        case( 'xs:include', 'include' )
            idx = xml_find_attrib( attribs, size(attribs,2), 'schemaLocation', filename )

            ! We now use the fact that all routines are recursive to
            ! implement the inclusion of other XSD schemas
            ! (For the "module variables" this makes no difference -
            ! they should be globally updated anyway

            call xml_process( filename, new_attribs, new_data, start_elemx, data_elem, &
                    end_elem, 20, error )

        case( 'xs:element', 'element' )

            !
            ! If this is the first time we encounter "element", it
            ! defines the rootname
            ! In all other cases, examine the rest of the information
            !
            if ( .not. rootelement ) then
                rootelement = .true.
                idx = xml_find_attrib( attribs, size(attribs,2), 'name', name )
                write( lunout, * ) '<options rootname="',trim(name),'">'
            else
                type_level = type_level + 1
                type_write(type_level) = .true.
                call convert_elem( attribs, .false., optional(level) )
            endif

        case( 'xs:attribute', 'attribute' )
            call convert_attrib( attribs, optional(level) )

        case( 'xs:sequence', 'sequence' )
            level    = level + 1
            optional(level) = .false.

        case( 'xs:choice', 'choice' )
            level    = level + 1
            optional(level) = .true.

        case( 'xs:complexType', 'complexType' )
            if ( size(attribs,2) > 0 ) then
                type_level = type_level + 1
                type_write(type_level) = .true.
                call convert_elem( attribs, .true., optional(level) )
            endif

        case( 'xs:simpleType', 'simpleType' )
            idx = xml_find_attrib( attribs, size(attribs,2), 'name', name )
            write( lunout, * ) '<typedef name="'//trim(name)//'">'

        case( 'xs:restriction', 'restriction' )
            idx = xml_find_attrib( attribs, size(attribs,2), 'base', name )
            write( lunout, * ) '   <component name="value" type="',trim(name),'"/>'

        case( 'xs:enumeration', 'enumeration' )
            ! Ignore
        case default
            write( *, * ) 'Ignoring: ', tag
    end select

end subroutine

! data_elem --
!     Handle the data belonging to the elements in the XSD file
!
! Arguments:
!     tag              Tag that was found
!     data             Array of data strings
!     error            Error parameter (output)
!
recursive subroutine data_elem( tag, data, error )

    character(len=*)                 :: tag
    character(len=*), dimension(:)   :: data
    logical                          :: error

    ! Dummy in this case

    return

end subroutine data_elem

! end_elem --
!     Handle the end of the elements in the XSD file
!
! Arguments:
!     tag              Tag that was found
!     error            Error parameter (output)
!
! Note:
!     It is easier to write all the relevant information in this
!     routine. The others simply gather the information.
!
recursive subroutine end_elem( tag, error )

    character(len=*)                 :: tag
    logical                          :: error

!!    write( lunout, * ) '>>Endtag: ', trim(tag), ' - level: ',type_level
    select case( tag )
        case( 'xs:complexType', 'complexType' )
            if ( type_level == 1 ) then
                if ( type_write(type_level) ) then
                    call write_elem
                endif
                type_write(type_level) = .false.
                type_level = type_level - 1
            endif

        case( 'xs:simpleType', 'simpleType' )
            write( lunout, * ) '</typedef>'

        case( 'xs:sequence', 'sequence', 'xs:choice', 'choice' )
            level = level - 1

        case( 'xs:element', 'element' )
            if ( type_level > 0 ) then
                if ( type_write(type_level) ) then
                    call write_elem
                endif
                type_write(type_level) = .false.
                type_level = type_level - 1
            endif

    end select

    return

end subroutine end_elem

! convert_elem --
!     Convert the definition of an element
!
! Arguments:
!     attrib           Attributes making up the definition
!     complextype      Is the item a complexType or not
!     optional         Is the item optional or not?
!
subroutine convert_elem( attribs, complextype, optional )

    character(len=*), dimension(:,:) :: attribs
    logical                          :: complextype
    logical                          :: optional

    character(len=40)                :: name
    character(len=40)                :: type
    character(len=40)                :: minoccurs
    character(len=40)                :: maxoccurs
    integer                          :: idx
    integer                          :: att
    integer                          :: off

    type_def(type_level)%number_attribs = 0

    !
    ! If the type is anonymous, we must construct a type name
    ! and there will be an extra element complexType inbetween
    !
    idx = xml_find_attrib( attribs, size(attribs,2), 'name', name )

    idx = xml_find_attrib( attribs, size(attribs,2), 'type', type )

    off = 1
    if ( idx <= 0 .and. .not. complextype ) then
        type_count = type_count + 1
        write( type_def(type_level)%type_name, '(a,i0)' ) 'type', type_count
    else
        if ( complextype ) then
            type_def(type_level)%type_name = name
        else
            type_write(type_level) = .false.
        endif
    endif

    if ( .not. complextype ) then
        att = type_def(type_level-off)%number_attribs + 1
        type_def(type_level-off)%number_attribs = att
        type_def(type_level-off)%attrib_name(att) = name
        type_def(type_level-off)%attrib_type(att) = type_def(type_level)%type_name
    endif

    minoccurs = '1'
    maxoccurs = '1'
    idx = xml_find_attrib( attribs, size(attribs,2), 'minOccurs', minoccurs )
    idx = xml_find_attrib( attribs, size(attribs,2), 'maxOccurs', maxoccurs )

    !
    ! TODO: fixed shape
    !
    type_def(type_level)%is_array = minoccurs /= maxoccurs

    ! TODO:
    ! - optional parameters
    ! - array-type or not
    ! - map the type to the xmlreader/Fortran type

    return

end subroutine convert_elem

! convert_attrib --
!     Convert the definition of an attribute
!
! Arguments:
!     attrib           Attributes making up the definition
!     optional         Is the item optional or not?
!
subroutine convert_attrib( attribs, optional )

    character(len=*), dimension(:,:) :: attribs
    logical                          :: optional

    character(len=40)                :: name
    character(len=40)                :: type
    character(len=40)                :: use
    character(len=40)                :: defvalue
    integer                          :: idx
    integer                          :: att

    idx = xml_find_attrib( attribs, size(attribs,2), 'name', name )
    idx = xml_find_attrib( attribs, size(attribs,2), 'type', type )

    if ( idx <= 0 ) then
        type = 'line'
    endif

    att = type_def(type_level)%number_attribs + 1
    type_def(type_level)%number_attribs = att

    type_def(type_level)%attrib_name(att) = name
    type_def(type_level)%attrib_type(att) = type

    use = 'required'
    defvalue  = ' '

    idx = xml_find_attrib( attribs, size(attribs,2), 'use'     , use       )
    type_def(type_level)%attrib_use(att)  = use == 'required'

    idx = xml_find_attrib( attribs, size(attribs,2), 'default' , defvalue  )
    type_def(type_level)%attrib_default(att) = defvalue
    type_def(type_level)%has_default(att)    = idx > 0

    return

end subroutine convert_attrib

! write_elem --
!     Write the definition of an element to the file
!
! Arguments:
!     None
!
subroutine write_elem

    character(len=40)                :: type
    character(len=40)                :: name
    integer                          :: i

    type = type_def(type_level)%type_name

    !
    ! Write the element definition
    !
    write( lunout, '(a)' ) '<typedef name="'//trim(type)//'">'

    if ( type_def(type_level)%number_attribs > 0 ) then
        do i = 1,type_def(type_level)%number_attribs
            name = type_def(type_level)%attrib_name(i)
            type = type_def(type_level)%attrib_type(i)

            write( lunout, '(a)', advance = 'no' ) '    <component name="'//trim(name)//'"'
            write( lunout, '(a)', advance = 'no' ) ' type="'//trim(type)//'"'
            if ( .not. type_def(type_level)%attrib_use(i) ) then
                write( lunout, '(a)', advance = 'no' ) ' optional="yes"'
            endif
            if ( type == 'line' ) then
                write( lunout, '(a)', advance = 'no' ) ' length="80"'
                if ( type_def(type_level)%has_default(i) ) then
                    write( lunout, '(a)', advance = 'no' ) ' default="'''//&
                    trim(type_def(type_level)%attrib_default(i))//'''"'
                else
                    write( lunout, '(a)', advance = 'no' ) ' default="'//&
                    trim(type_def(type_level)%attrib_default(i))//'"'
                endif
            endif
            write( lunout, '(a)' ) '/>'
        enddo
    else
        !
        ! No attributes given, so make it a string by default
        !
        write( lunout, '(a)' ) '    <component name="string" type="line" length="80">'
    endif

    write( lunout, '(a)' ) '</typedef>'

end subroutine write_elem

end module handle_elems

program xsdconvert

    use xmlparse
    use handle_elems

    character(len=80) :: filename
    logical           :: error

    character(len=100), dimension(2,100) :: attribs
    character(len=100), dimension(100) :: data

    open( 10, file = 'xsdconvert.inp' )
    read( 10, '(a)' ) filename
    open( 12, file = trim(filename) // '.xml' )

    lunout = 20
    open( lunout, file = 'xsdconvert.out' )
    write( lunout, * ) '<?xml version="1.0" encoding="UTF-8"?>'

    filename = trim(filename) // '.xsd'

    call xml_process( filename, attribs, data, start_elem, data_elem, &
          end_elem, 20, error )

end program

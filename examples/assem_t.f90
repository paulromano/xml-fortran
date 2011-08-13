module xml_data_assem_t
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_

type files_type
   character(len=20)                                :: stator
   character(len=20)                                :: rotor
   character(len=20)                                :: bh_file
end type files_type

type stator_type
   real(kind=kind(1.0d0))                          :: da
   real(kind=kind(1.0d0))                          :: di
   integer                                         :: p
   integer                                         :: N1
   integer                                         :: st_rq
   real(kind=kind(1.0d0))                          :: magn
   real(kind=kind(1.0d0))                          :: lfe
   real(kind=kind(1.0d0))                          :: f1
   real(kind=kind(1.0d0))                          :: hN
   real(kind=kind(1.0d0))                          :: tN
   real(kind=kind(1.0d0))                          :: bN
   real(kind=kind(1.0d0))                          :: bZ
end type stator_type

type winding_type
   character(len=3)                                :: type
   integer                                         :: np_cir
   integer                                         :: nc_turns
   integer                                         :: coil_sides_per_phase
   integer                                         :: coil_span
end type winding_type

type skew_type
   integer                                         :: ks
   real(kind=kind(1.0d0))                          :: gamma
end type skew_type

type rotor_type
   real(kind=kind(1.0d0))                          :: da
   real(kind=kind(1.0d0))                          :: di
   integer                                         :: N2
   integer                                         :: rt_rq
   real(kind=kind(1.0d0))                          :: q_axis
   character(len=10)                                :: magnet
   real(kind=kind(1.0d0))                          :: hM
   real(kind=kind(1.0d0))                          :: delta_T
   integer                                         :: mpp
end type rotor_type

type mech_rot_type
   real(kind=kind(1.0d0))                          :: theta_1
   real(kind=kind(1.0d0))                          :: theta_2
   real(kind=kind(1.0d0))                          :: dtheta
end type mech_rot_type

type id_type
   real(kind=kind(1.0d0))                          :: min
   real(kind=kind(1.0d0))                          :: max
   real(kind=kind(1.0d0))                          :: step
end type id_type

type iq_type
   real(kind=kind(1.0d0))                          :: min
   real(kind=kind(1.0d0))                          :: max
   real(kind=kind(1.0d0))                          :: step
end type iq_type

type assem_type
   character(len=25)                                :: project
   real(kind=kind(1.0d0))                          :: version
   character(len=5)                                :: band
   type(files_type)                                :: files
   type(stator_type)                               :: stator
   type(winding_type)                              :: winding
   type(skew_type)                                 :: skew
   type(rotor_type)                                :: rotor
   type(mech_rot_type)                             :: mech_rot
   type(id_type)                                   :: id
   type(iq_type)                                   :: iq
end type assem_type
   type(assem_type), dimension(:), pointer         :: assem => null()
contains
subroutine read_xml_type_files_type_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(files_type), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(files_type), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_files_type( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_files_type_array

subroutine read_xml_type_files_type( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(files_type), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_stator
   logical                                         :: has_rotor
   logical                                         :: has_bh_file
   has_stator                           = .false.
   has_rotor                            = .false.
   has_bh_file                          = .false.
   call init_xml_type_files_type(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('stator')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%stator, has_stator )
      case('rotor')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%rotor, has_rotor )
      case('bh_file')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%bh_file, has_bh_file )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_stator ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on stator')
   endif
   if ( .not. has_rotor ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on rotor')
   endif
   if ( .not. has_bh_file ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on bh_file')
   endif
end subroutine read_xml_type_files_type
subroutine init_xml_type_files_type_array( dvar )
   type(files_type), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_files_type_array
subroutine init_xml_type_files_type(dvar)
   type(files_type) :: dvar
end subroutine init_xml_type_files_type
subroutine write_xml_type_files_type_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(files_type), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_files_type( info, tag, indent+3, dvar(i) )
   enddo
end subroutine write_xml_type_files_type_array

subroutine write_xml_type_files_type( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(files_type)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'stator', indent+3, dvar%stator)
   call write_to_xml_word( info, 'rotor', indent+3, dvar%rotor)
   call write_to_xml_word( info, 'bh_file', indent+3, dvar%bh_file)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_files_type

subroutine read_xml_type_stator_type_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(stator_type), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(stator_type), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_stator_type( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_stator_type_array

subroutine read_xml_type_stator_type( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(stator_type), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_da
   logical                                         :: has_di
   logical                                         :: has_p
   logical                                         :: has_N1
   logical                                         :: has_st_rq
   logical                                         :: has_magn
   logical                                         :: has_lfe
   logical                                         :: has_f1
   logical                                         :: has_hN
   logical                                         :: has_tN
   logical                                         :: has_bN
   logical                                         :: has_bZ
   has_da                               = .false.
   has_di                               = .false.
   has_p                                = .false.
   has_N1                               = .false.
   has_st_rq                            = .false.
   has_magn                             = .false.
   has_lfe                              = .false.
   has_f1                               = .false.
   has_hN                               = .false.
   has_tN                               = .false.
   has_bN                               = .false.
   has_bZ                               = .false.
   call init_xml_type_stator_type(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('da')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%da, has_da )
      case('di')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%di, has_di )
      case('p')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%p, has_p )
      case('N1')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%N1, has_N1 )
      case('st_rq')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%st_rq, has_st_rq )
      case('magn')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%magn, has_magn )
      case('lfe')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%lfe, has_lfe )
      case('f1')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%f1, has_f1 )
      case('hN')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%hN, has_hN )
      case('tN')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%tN, has_tN )
      case('bN')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%bN, has_bN )
      case('bZ')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%bZ, has_bZ )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_da ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on da')
   endif
   if ( .not. has_di ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on di')
   endif
   if ( .not. has_p ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on p')
   endif
   if ( .not. has_N1 ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on N1')
   endif
   if ( .not. has_st_rq ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on st_rq')
   endif
   if ( .not. has_magn ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on magn')
   endif
   if ( .not. has_lfe ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on lfe')
   endif
   if ( .not. has_f1 ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on f1')
   endif
   if ( .not. has_hN ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on hN')
   endif
   if ( .not. has_tN ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on tN')
   endif
   if ( .not. has_bN ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on bN')
   endif
   if ( .not. has_bZ ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on bZ')
   endif
end subroutine read_xml_type_stator_type
subroutine init_xml_type_stator_type_array( dvar )
   type(stator_type), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_stator_type_array
subroutine init_xml_type_stator_type(dvar)
   type(stator_type) :: dvar
end subroutine init_xml_type_stator_type
subroutine write_xml_type_stator_type_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(stator_type), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_stator_type( info, tag, indent+3, dvar(i) )
   enddo
end subroutine write_xml_type_stator_type_array

subroutine write_xml_type_stator_type( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(stator_type)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_double( info, 'da', indent+3, dvar%da)
   call write_to_xml_double( info, 'di', indent+3, dvar%di)
   call write_to_xml_integer( info, 'p', indent+3, dvar%p)
   call write_to_xml_integer( info, 'N1', indent+3, dvar%N1)
   call write_to_xml_integer( info, 'st_rq', indent+3, dvar%st_rq)
   call write_to_xml_double( info, 'magn', indent+3, dvar%magn)
   call write_to_xml_double( info, 'lfe', indent+3, dvar%lfe)
   call write_to_xml_double( info, 'f1', indent+3, dvar%f1)
   call write_to_xml_double( info, 'hN', indent+3, dvar%hN)
   call write_to_xml_double( info, 'tN', indent+3, dvar%tN)
   call write_to_xml_double( info, 'bN', indent+3, dvar%bN)
   call write_to_xml_double( info, 'bZ', indent+3, dvar%bZ)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_stator_type

subroutine read_xml_type_winding_type_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(winding_type), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(winding_type), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_winding_type( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_winding_type_array

subroutine read_xml_type_winding_type( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(winding_type), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_type
   logical                                         :: has_np_cir
   logical                                         :: has_nc_turns
   logical                                         :: has_coil_sides_per_phase
   logical                                         :: has_coil_span
   has_type                             = .false.
   has_np_cir                           = .false.
   has_nc_turns                         = .false.
   has_coil_sides_per_phase             = .false.
   has_coil_span                        = .false.
   call init_xml_type_winding_type(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('type')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%type, has_type )
      case('np_cir')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%np_cir, has_np_cir )
      case('nc_turns')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%nc_turns, has_nc_turns )
      case('coil_sides_per_phase')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%coil_sides_per_phase, has_coil_sides_per_phase )
      case('coil_span')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%coil_span, has_coil_span )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_type ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on type')
   endif
   if ( .not. has_np_cir ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on np_cir')
   endif
   if ( .not. has_nc_turns ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on nc_turns')
   endif
   if ( .not. has_coil_sides_per_phase ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on coil_sides_per_phase')
   endif
   if ( .not. has_coil_span ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on coil_span')
   endif
end subroutine read_xml_type_winding_type
subroutine init_xml_type_winding_type_array( dvar )
   type(winding_type), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_winding_type_array
subroutine init_xml_type_winding_type(dvar)
   type(winding_type) :: dvar
end subroutine init_xml_type_winding_type
subroutine write_xml_type_winding_type_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(winding_type), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_winding_type( info, tag, indent+3, dvar(i) )
   enddo
end subroutine write_xml_type_winding_type_array

subroutine write_xml_type_winding_type( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(winding_type)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'type', indent+3, dvar%type)
   call write_to_xml_integer( info, 'np_cir', indent+3, dvar%np_cir)
   call write_to_xml_integer( info, 'nc_turns', indent+3, dvar%nc_turns)
   call write_to_xml_integer( info, 'coil_sides_per_phase', indent+3, dvar%coil_sides_per_phase)
   call write_to_xml_integer( info, 'coil_span', indent+3, dvar%coil_span)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_winding_type

subroutine read_xml_type_skew_type_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(skew_type), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(skew_type), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_skew_type( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_skew_type_array

subroutine read_xml_type_skew_type( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(skew_type), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_ks
   logical                                         :: has_gamma
   has_ks                               = .false.
   has_gamma                            = .false.
   call init_xml_type_skew_type(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('ks')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%ks, has_ks )
      case('gamma')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%gamma, has_gamma )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_ks ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on ks')
   endif
   if ( .not. has_gamma ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on gamma')
   endif
end subroutine read_xml_type_skew_type
subroutine init_xml_type_skew_type_array( dvar )
   type(skew_type), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_skew_type_array
subroutine init_xml_type_skew_type(dvar)
   type(skew_type) :: dvar
end subroutine init_xml_type_skew_type
subroutine write_xml_type_skew_type_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(skew_type), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_skew_type( info, tag, indent+3, dvar(i) )
   enddo
end subroutine write_xml_type_skew_type_array

subroutine write_xml_type_skew_type( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(skew_type)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_integer( info, 'ks', indent+3, dvar%ks)
   call write_to_xml_double( info, 'gamma', indent+3, dvar%gamma)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_skew_type

subroutine read_xml_type_rotor_type_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(rotor_type), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(rotor_type), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_rotor_type( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_rotor_type_array

subroutine read_xml_type_rotor_type( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(rotor_type), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_da
   logical                                         :: has_di
   logical                                         :: has_N2
   logical                                         :: has_rt_rq
   logical                                         :: has_q_axis
   logical                                         :: has_magnet
   logical                                         :: has_hM
   logical                                         :: has_delta_T
   logical                                         :: has_mpp
   has_da                               = .false.
   has_di                               = .false.
   has_N2                               = .false.
   has_rt_rq                            = .false.
   has_q_axis                           = .false.
   has_magnet                           = .false.
   has_hM                               = .false.
   has_delta_T                          = .false.
   has_mpp                              = .false.
   call init_xml_type_rotor_type(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('da')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%da, has_da )
      case('di')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%di, has_di )
      case('N2')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%N2, has_N2 )
      case('rt_rq')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%rt_rq, has_rt_rq )
      case('q_axis')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%q_axis, has_q_axis )
      case('magnet')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%magnet, has_magnet )
      case('hM')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%hM, has_hM )
      case('delta_T')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%delta_T, has_delta_T )
      case('mpp')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%mpp, has_mpp )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_da ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on da')
   endif
   if ( .not. has_di ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on di')
   endif
   if ( .not. has_N2 ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on N2')
   endif
   if ( .not. has_rt_rq ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on rt_rq')
   endif
   if ( .not. has_q_axis ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on q_axis')
   endif
   if ( .not. has_magnet ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on magnet')
   endif
   if ( .not. has_hM ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on hM')
   endif
   if ( .not. has_delta_T ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on delta_T')
   endif
   if ( .not. has_mpp ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on mpp')
   endif
end subroutine read_xml_type_rotor_type
subroutine init_xml_type_rotor_type_array( dvar )
   type(rotor_type), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_rotor_type_array
subroutine init_xml_type_rotor_type(dvar)
   type(rotor_type) :: dvar
end subroutine init_xml_type_rotor_type
subroutine write_xml_type_rotor_type_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(rotor_type), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_rotor_type( info, tag, indent+3, dvar(i) )
   enddo
end subroutine write_xml_type_rotor_type_array

subroutine write_xml_type_rotor_type( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(rotor_type)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_double( info, 'da', indent+3, dvar%da)
   call write_to_xml_double( info, 'di', indent+3, dvar%di)
   call write_to_xml_integer( info, 'N2', indent+3, dvar%N2)
   call write_to_xml_integer( info, 'rt_rq', indent+3, dvar%rt_rq)
   call write_to_xml_double( info, 'q_axis', indent+3, dvar%q_axis)
   call write_to_xml_word( info, 'magnet', indent+3, dvar%magnet)
   call write_to_xml_double( info, 'hM', indent+3, dvar%hM)
   call write_to_xml_double( info, 'delta_T', indent+3, dvar%delta_T)
   call write_to_xml_integer( info, 'mpp', indent+3, dvar%mpp)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_rotor_type

subroutine read_xml_type_mech_rot_type_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(mech_rot_type), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(mech_rot_type), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_mech_rot_type( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_mech_rot_type_array

subroutine read_xml_type_mech_rot_type( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(mech_rot_type), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_theta_1
   logical                                         :: has_theta_2
   logical                                         :: has_dtheta
   has_theta_1                          = .false.
   has_theta_2                          = .false.
   has_dtheta                           = .false.
   call init_xml_type_mech_rot_type(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('theta_1')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%theta_1, has_theta_1 )
      case('theta_2')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%theta_2, has_theta_2 )
      case('dtheta')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%dtheta, has_dtheta )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_theta_1 ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on theta_1')
   endif
   if ( .not. has_theta_2 ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on theta_2')
   endif
   if ( .not. has_dtheta ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on dtheta')
   endif
end subroutine read_xml_type_mech_rot_type
subroutine init_xml_type_mech_rot_type_array( dvar )
   type(mech_rot_type), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_mech_rot_type_array
subroutine init_xml_type_mech_rot_type(dvar)
   type(mech_rot_type) :: dvar
end subroutine init_xml_type_mech_rot_type
subroutine write_xml_type_mech_rot_type_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(mech_rot_type), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_mech_rot_type( info, tag, indent+3, dvar(i) )
   enddo
end subroutine write_xml_type_mech_rot_type_array

subroutine write_xml_type_mech_rot_type( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(mech_rot_type)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_double( info, 'theta_1', indent+3, dvar%theta_1)
   call write_to_xml_double( info, 'theta_2', indent+3, dvar%theta_2)
   call write_to_xml_double( info, 'dtheta', indent+3, dvar%dtheta)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_mech_rot_type

subroutine read_xml_type_id_type_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(id_type), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(id_type), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_id_type( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_id_type_array

subroutine read_xml_type_id_type( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(id_type), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_min
   logical                                         :: has_max
   logical                                         :: has_step
   has_min                              = .false.
   has_max                              = .false.
   has_step                             = .false.
   call init_xml_type_id_type(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('min')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%min, has_min )
      case('max')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%max, has_max )
      case('step')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%step, has_step )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_min ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on min')
   endif
   if ( .not. has_max ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on max')
   endif
   if ( .not. has_step ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on step')
   endif
end subroutine read_xml_type_id_type
subroutine init_xml_type_id_type_array( dvar )
   type(id_type), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_id_type_array
subroutine init_xml_type_id_type(dvar)
   type(id_type) :: dvar
end subroutine init_xml_type_id_type
subroutine write_xml_type_id_type_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(id_type), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_id_type( info, tag, indent+3, dvar(i) )
   enddo
end subroutine write_xml_type_id_type_array

subroutine write_xml_type_id_type( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(id_type)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_double( info, 'min', indent+3, dvar%min)
   call write_to_xml_double( info, 'max', indent+3, dvar%max)
   call write_to_xml_double( info, 'step', indent+3, dvar%step)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_id_type

subroutine read_xml_type_iq_type_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(iq_type), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(iq_type), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_iq_type( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_iq_type_array

subroutine read_xml_type_iq_type( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(iq_type), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_min
   logical                                         :: has_max
   logical                                         :: has_step
   has_min                              = .false.
   has_max                              = .false.
   has_step                             = .false.
   call init_xml_type_iq_type(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('min')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%min, has_min )
      case('max')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%max, has_max )
      case('step')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%step, has_step )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_min ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on min')
   endif
   if ( .not. has_max ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on max')
   endif
   if ( .not. has_step ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on step')
   endif
end subroutine read_xml_type_iq_type
subroutine init_xml_type_iq_type_array( dvar )
   type(iq_type), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_iq_type_array
subroutine init_xml_type_iq_type(dvar)
   type(iq_type) :: dvar
end subroutine init_xml_type_iq_type
subroutine write_xml_type_iq_type_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(iq_type), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_iq_type( info, tag, indent+3, dvar(i) )
   enddo
end subroutine write_xml_type_iq_type_array

subroutine write_xml_type_iq_type( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(iq_type)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_double( info, 'min', indent+3, dvar%min)
   call write_to_xml_double( info, 'max', indent+3, dvar%max)
   call write_to_xml_double( info, 'step', indent+3, dvar%step)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_iq_type

subroutine read_xml_type_assem_type_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(assem_type), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(assem_type), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_assem_type( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_assem_type_array

subroutine read_xml_type_assem_type( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(assem_type), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_project
   logical                                         :: has_version
   logical                                         :: has_band
   logical                                         :: has_files
   logical                                         :: has_stator
   logical                                         :: has_winding
   logical                                         :: has_skew
   logical                                         :: has_rotor
   logical                                         :: has_mech_rot
   logical                                         :: has_id
   logical                                         :: has_iq
   has_project                          = .false.
   has_version                          = .false.
   has_band                             = .false.
   has_files                            = .false.
   has_stator                           = .false.
   has_winding                          = .false.
   has_skew                             = .false.
   has_rotor                            = .false.
   has_mech_rot                         = .false.
   has_id                               = .false.
   has_iq                               = .false.
   call init_xml_type_assem_type(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('project')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%project, has_project )
      case('version')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%version, has_version )
      case('band')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%band, has_band )
      case('files')
         call read_xml_type_files_type( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%files, has_files )
      case('stator')
         call read_xml_type_stator_type( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%stator, has_stator )
      case('winding')
         call read_xml_type_winding_type( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%winding, has_winding )
      case('skew')
         call read_xml_type_skew_type( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%skew, has_skew )
      case('rotor')
         call read_xml_type_rotor_type( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%rotor, has_rotor )
      case('mech_rot')
         call read_xml_type_mech_rot_type( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%mech_rot, has_mech_rot )
      case('id')
         call read_xml_type_id_type( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%id, has_id )
      case('iq')
         call read_xml_type_iq_type( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%iq, has_iq )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_project ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on project')
   endif
   if ( .not. has_version ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on version')
   endif
   if ( .not. has_band ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on band')
   endif
   if ( .not. has_files ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on files')
   endif
   if ( .not. has_stator ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on stator')
   endif
   if ( .not. has_winding ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on winding')
   endif
   if ( .not. has_skew ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on skew')
   endif
   if ( .not. has_rotor ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on rotor')
   endif
   if ( .not. has_mech_rot ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on mech_rot')
   endif
   if ( .not. has_id ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on id')
   endif
   if ( .not. has_iq ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on iq')
   endif
end subroutine read_xml_type_assem_type
subroutine init_xml_type_assem_type_array( dvar )
   type(assem_type), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_assem_type_array
subroutine init_xml_type_assem_type(dvar)
   type(assem_type) :: dvar
end subroutine init_xml_type_assem_type
subroutine write_xml_type_assem_type_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(assem_type), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_assem_type( info, tag, indent+3, dvar(i) )
   enddo
end subroutine write_xml_type_assem_type_array

subroutine write_xml_type_assem_type( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(assem_type)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'project', indent+3, dvar%project)
   call write_to_xml_double( info, 'version', indent+3, dvar%version)
   call write_to_xml_word( info, 'band', indent+3, dvar%band)
   call write_xml_type_files_type( info, 'files', indent+3, dvar%files)
   call write_xml_type_stator_type( info, 'stator', indent+3, dvar%stator)
   call write_xml_type_winding_type( info, 'winding', indent+3, dvar%winding)
   call write_xml_type_skew_type( info, 'skew', indent+3, dvar%skew)
   call write_xml_type_rotor_type( info, 'rotor', indent+3, dvar%rotor)
   call write_xml_type_mech_rot_type( info, 'mech_rot', indent+3, dvar%mech_rot)
   call write_xml_type_id_type( info, 'id', indent+3, dvar%id)
   call write_xml_type_iq_type( info, 'iq', indent+3, dvar%iq)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_assem_type

subroutine read_xml_file_assem_t(fname, lurep, errout)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep
   logical, intent(out), optional         :: errout

   type(XML_PARSE)                        :: info
   logical                                :: error
   character(len=80)                      :: tag
   character(len=80)                      :: starttag
   logical                                :: endtag
   character(len=80), dimension(1:2,1:20) :: attribs
   integer                                :: noattribs
   character(len=200), dimension(1:100)   :: data
   integer                                :: nodata
   logical                                         :: has_assem
   has_assem                            = .false.
   allocate(assem(0))

   call init_xml_file_assem_t
   call xml_open( info, fname, .true. )
   call xml_options( info, report_errors=.true., ignore_whitespace=.true.)
   lurep_ = 0
   if ( present(lurep) ) then
      lurep_ = lurep
      call xml_options( info, report_lun=lurep )
   endif
   do
      call xml_get( info, starttag, endtag, attribs, noattribs, &
         data, nodata)
      if ( starttag .ne. '!--' ) exit
   enddo
   if ( starttag .ne. "database" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "database"')
      error = .true.
      call xml_close(info)
      return
   endif
   strict_ = .true.
   error = .false.
   do
      call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
      if ( xml_error(info) ) then
         write(lurep_,*) 'Error reading input file!'
         error = .true.
         return
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('assem')
         call read_xml_type_assem_type_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            assem, has_assem )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_assem ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on assem')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_assem_t(fname, lurep)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep

   type(XML_PARSE)                        :: info
   integer                                :: indent = 0

   call xml_open( info, fname, .false. )
   call xml_options( info, report_errors=.true.)
   if ( present(lurep) ) then
       call xml_options( info, report_errors=.true.)
   endif
   write(info%lun,'(a)') &
      '<database>'
   call write_xml_type_assem_type_array( info, 'assem', indent+3, assem)
   write(info%lun,'(a)') '</database>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_assem_t

end subroutine

end module

! tree_struct.f90 --
!    Module that implements a general tree structure in Fortran 90
!
! General information:
!    The tree is stored via a derived type TREE_DATA. A tree can
!    have an arbitrary number of nodes, each of which can again
!    have an arbitrary number of subnodes and so on.
!    The data type for a node is the same as for a tree - there
!    is no difference, except that the routine tree_create()
!    returns the initialised root of a new tree.
!    A node (and the root as well) can have the following
!    properties:
!    - A name (at most 80 characters)
!    - Arbitrary data
!    - A string indicating the type of data
!    - Zero, one or more subnodes
!    The storage is fairly efficient (via the TRANSFER() function
!    all data and strings are converted to arrays of default
!    integers). A node does not "know" its parent though and
!    there is no check on circularity.
!
!    $Id: tree_struct.f90,v 1.2 2006/03/26 19:05:48 arjenmarkus Exp $
!
module TREE_DATA_TYPES
   type TREE_DATA
      character(len=1), dimension(:), pointer         :: node_name
      character(len=1), dimension(:), pointer         :: node_data
      character(len=1), dimension(:), pointer         :: node_data_type
      type(TREE_DATA_PTR), dimension(:), pointer :: child_nodes
   end type

   type TREE_DATA_PTR
      type(TREE_DATA), pointer               :: node_ptr
   end type
end module

module TREE_STRUCTURES
   use TREE_DATA_TYPES
   implicit none

   private
   !
   ! A variable that indicates the type of all data
   !
   character(len=1), dimension(1:1), public     :: node_value

   !
   ! Auxiliary variable
   !
   integer, private                             :: traverse_level = 0

   !
   ! Public routines, types and parameters
   !
   public  :: TREE_DATA, TREE_DATA_PTR
   public  :: tree_create, tree_create_node, tree_get_node_by_name, &
              tree_get_data_ptr, tree_put_data, tree_get_name, &
              tree_get_datatype, tree_get_data_string,         &
              tree_find, tree_find_descendants,                &
              tree_traverse, tree_traverse_level

   !
   ! For testing purposes - should actually be private
   !
   public :: tree_next_segment, tree_append_handles

contains

! tree_create --
!    Create a new tree
!
! Arguments:
!    name         Name of the new tree
!    tree         Pointer to the new tree
! Result:
!    The argument tree points to a new, empty tree structure or is
!    not associated
!
subroutine tree_create( name, tree )
   character(len=*), intent(in)    :: name
   type(TREE_DATA), pointer        :: tree

   integer                         :: error
   integer                         :: newsize

   allocate( tree, stat = error )

   if ( error .ne. 0 ) then
      nullify( tree )
   else
      newsize = size( transfer( name, node_value ) )
      allocate( tree%node_name(1:newsize), stat = error )
      if ( error .ne. 0 ) then
         deallocate( tree )
         return
      else
         tree%node_name(1:newsize) = transfer( name, node_value )
         nullify( tree%node_data )
         nullify( tree%node_data_type )
         nullify( tree%child_nodes )
      endif
   endif
end subroutine tree_create

! tree_create_node --
!    Create a new node to the given tree or node
!
! Arguments:
!    tree         The tree or node to which to append the new node
!    name         Name of the new node
!    node         Pointer to the new node
! Result:
!    The argument node points to a new, empty node or is
!    not associated
!
subroutine tree_create_node( tree, name, node )
   character(len=*), intent(in)    :: name
   type(TREE_DATA), pointer        :: tree
   type(TREE_DATA), pointer        :: node

   type(TREE_DATA_PTR), dimension(:), pointer :: children

   integer                         :: error
   integer                         :: newsize

   !
   ! Check for uniqueness -- no: do not do that!
   !
   !call tree_get_node_by_name( tree, name, node )
   !if ( associated( node ) ) then
   !   return
   !endif

   !
   ! Create a new node, store it in the array of child nodes
   ! for this (sub)tree
   !
   call tree_create( name, node )

   if ( associated( node ) ) then
      newsize = 1
      if ( associated( tree%child_nodes ) ) then
         newsize = 1 + size( tree%child_nodes )
      endif

      allocate( children(1:newsize), stat = error )
      if ( error .ne. 0 ) then
         deallocate( node )
         return
      else
         if ( newsize .gt. 1 ) then
            children(1:newsize-1) = tree%child_nodes
            deallocate( tree%child_nodes )
         endif

         tree%child_nodes => children
         tree%child_nodes(newsize)%node_ptr => node
      endif
   endif
end subroutine tree_create_node

! tree_get_name --
!    Return the name of the tree or node
!
! Arguments:
!    tree         The tree or node
!
function tree_get_name( tree ) result( node_name )
   type(TREE_DATA), pointer        :: tree
   character(len=80)               :: node_name

   integer                         :: length

   length    = size( tree%node_name )
   node_name = ' '
   node_name(1:length) = transfer( tree%node_name, node_name )
end function tree_get_name

! tree_get_datatype --
!    Return the data type for the data stored in the tree or node
!
! Arguments:
!    tree         The tree or node
!
function tree_get_datatype( tree ) result( data_type )
   type(TREE_DATA), pointer        :: tree
   character(len=40)               :: data_type

   integer                         :: length

   data_type = '?'
   if ( associated( tree%node_data_type ) ) then
      length    = size( tree%node_data_type )
      data_type(1:length) = transfer( tree%node_data_type, data_type )
   endif
end function tree_get_datatype

! tree_get_node_by_name --
!    Return the child node by name
!
! Arguments:
!    tree         The tree or node to which to append the new node
!    name         Name of the node to find
!    node         Pointer to the node or "null"
! Result:
!    The argument node points to a new, empty node or is
!    not associated
!
subroutine tree_get_node_by_name( tree, name, node )
   character(len=*), intent(in)    :: name
   type(TREE_DATA), pointer        :: tree
   type(TREE_DATA), pointer        :: node

   character(len=80)               :: node_name

   integer                         :: i

   nullify( node )

   if ( associated(tree%child_nodes) ) then
      do i = 1,size(tree%child_nodes)
         node_name = tree_get_name( tree%child_nodes(i)%node_ptr )

         if ( node_name .eq. name ) then
            node => tree%child_nodes(i)%node_ptr
            exit
         endif
      enddo
   endif

end subroutine tree_get_node_by_name

! tree_get_data_ptr --
!    Return a pointer to the tree/node's data
!
! Arguments:
!    tree        The tree or node from which to get the data
!    data_ptr    Pointer to the node/tree data
!    data_type   String indicating the type
! Result:
!    The argument data_ptr points to the stored data or is
!    not associated
!
subroutine tree_get_data_ptr( tree, data_ptr, data_type )
    type(TREE_DATA), pointer        :: tree
    character(len=1), dimension(:), pointer  :: data_ptr
    character(len=*)                         :: data_type

    nullify( data_ptr )

    data_type = '?'
    if ( associated( tree%node_data) ) then
       data_ptr  => tree%node_data
       data_type =  tree_get_datatype( tree )
    endif

end subroutine tree_get_data_ptr

! tree_put_data --
!    Put (a copy of) the data in the tree/node
!
! Arguments:
!    tree        The tree or node with which to attach the data
!    data        Array of integers
!    data_type   Optional string indicating the type
!    success     True if all went well, false otherwise
! Result:
!    The tree structure points to a copy of the data
! Note:
!    A direct call to this routine will look something like:
!
!       call tree_put_data( tree, transfer( some_data, node_value ) )
!
!    where node_value acts as the mold for transferring the data
!
subroutine tree_put_data( tree, data, data_type, success )
    type(TREE_DATA), pointer        :: tree
    character(len=1), dimension(:)  :: data
    character(len=*), optional      :: data_type
    logical, intent(out), optional  :: success

    integer                         :: error

    if ( associated(tree%node_data) ) then
       deallocate( tree%node_data )
    endif

    if ( associated(tree%node_data_type) ) then
       deallocate( tree%node_data_type )
    endif

    allocate( tree%node_data(1:size(data)), stat = error )
    if ( error .eq. 0 ) then
       tree%node_data = data
       allocate( tree%node_data_type(1:len_trim(data_type)), &
          stat = error )
       if ( error .eq. 0 ) then
          tree%node_data_type = transfer( data_type, tree%node_data_type )
       endif
    endif

    if ( present( success ) ) then
       success = error .eq. 0
    endif

end subroutine tree_put_data

! tree_traverse_level --
!    Convenience function: level of the node during traversal
!
! Arguments:
!    None
!
! Note:
!    Functions without arguments can be optimised away!
!    I should turn this into a subroutine instead
!
integer function tree_traverse_level( )
   tree_traverse_level = traverse_level
end function tree_traverse_level

! tree_traverse --
!    Traverse a tree and handle the nodes by a depth-first method
!
! Arguments:
!    tree        The tree or node to traverse
!    handler     Routine to handle each node
!    data        Arbitrary data to be passed to the handler
!    stop        Whether to continue or stop (if set true)
! Result:
!    Each tree node is visited (unless the traversal is
!    prematurely ended by setting "stop" to true)
!
recursive subroutine tree_traverse( tree, handler, data, stop )
    type(TREE_DATA), pointer        :: tree
    character(len=1), dimension(:)  :: data
    logical, intent(out)            :: stop

    interface
       subroutine handler( node, data, stop )
          use TREE_DATA_TYPES
          type(TREE_DATA), pointer        :: node
          character(len=1), dimension(:)  :: data
          logical, intent(inout)          :: stop
       end subroutine handler
    end interface

    integer                         :: i

    stop = .false.
    if ( .not. associated( tree ) ) then
       return
    endif

    !
    ! First call the handler for the current node/tree
    !
    call handler( tree, data, stop )
    if ( stop ) then
       return
    endif

    !
    ! Then recurse through the child nodes (if any)
    !
    if ( associated( tree%child_nodes) ) then
       do i = 1,size(tree%child_nodes)
          traverse_level = traverse_level + 1
          call tree_traverse( tree%child_nodes(i)%node_ptr, &
                              handler, data, stop )
          traverse_level = traverse_level - 1
          if ( stop ) then
             exit
          endif
       enddo
    endif

end subroutine tree_traverse

! tree_get_data_string --
!    Return data as a simple string
!
! Arguments:
!    tree        The tree or node from which to get the data
!    string      String to be filled
!    success     Whether successful or not
! Result:
!    The string is filled with the data stored in the node
!    not associated. The routine is successful if:
!    - there is data associated with the node/tree
!    - the data type is "STRING" or "ATTRIBUTE"
!    If the routine is not successful, the string is
!    not changed.
!
subroutine tree_get_data_string( tree, string, success )
   type(TREE_DATA), pointer                 :: tree
   character(len=*), intent(inout)          :: string
   logical, intent(out)                     :: success

   character(len=1), dimension(:), pointer  :: data_ptr
   character(len=40)                        :: data_type
   integer                                  :: length

   success = .false.
   if ( associated(tree) ) then
      call tree_get_data_ptr( tree, data_ptr, data_type )

      if ( .not. associated(data_ptr) ) then
         return
      endif
      if ( data_type .ne. 'STRING' .and. data_type .ne. 'ATTRIBUTE' ) then
         return
      endif

      success          = .true.
      length           = size(data_ptr)
      string           = ' '
      string(1:length) = transfer(data_ptr,string)
   endif

end subroutine tree_get_data_string

include 'tree_find.f90'

end module TREE_STRUCTURES


! ===================================================================
! Put it to the test
!
program test_tree
  use TREE_STRUCTURES

  implicit none
  type(TREE_DATA), pointer   :: tree
  type(TREE_DATA), pointer   :: node1, node2, node3

  character(len=1), dimension(1)          :: dummy
  character(len=1), dimension(:), pointer :: data_ptr
  character(len=40) :: node_name, node_type, stored_data, type
  character(len=40) :: string, name

  logical                                 :: stop
  logical                                 :: success

  character(len=80) :: path
  character(len=80) :: segment
  integer           :: i, j
  type(TREE_DATA_PTR), dimension(:), pointer :: all_handles
  type(TREE_DATA_PTR), dimension(:), pointer :: handles
  integer                                    :: status

  interface
     subroutine handler( tree, data, stop )
        use TREE_STRUCTURES
        type(TREE_DATA), pointer               :: tree
        character(len=1), dimension(:)         :: data
        logical, intent(inout)                 :: stop
     end subroutine handler
  end interface

  !
  ! TREE:
  !    NODE A (data: STORED A)
  !       NODE C (data: AC)
  !       NODE D (data: AD)
  !    NODE B (data: --)
  !       NODE C (data: BC1)
  !          NODE D (data: BCD1)
  !       NODE C (data: BC2)
  !       NODE C (data: BC3)
  !    NODE D (data: --)
  !

  call tree_create( "TREE", tree )

  call tree_create_node( tree, "NODE A", node1 )
  call tree_put_data( node1, transfer("STORED A",node_value), "STRING" )

  call tree_create_node( node1, "NODE C", node2 )
  call tree_put_data( node2, transfer("AC",node_value), "STRING" )
  call tree_create_node( node1, "NODE D", node2 )
  call tree_put_data( node2, transfer("AD",node_value), "STRING" )

  call tree_create_node( tree, "NODE B", node1 )

  call tree_create_node( node1, "NODE C", node3 )
  call tree_put_data( node3, transfer("BC1",node_value), "STRING" )
  call tree_create_node( node1, "NODE C", node2 )
  call tree_put_data( node2, transfer("BC2",node_value), "STRING" )
  call tree_create_node( node1, "NODE C", node2 )
  call tree_put_data( node2, transfer("BC3",node_value), "STRING" )

  call tree_create_node( node3, "NODE D", node2 )
  call tree_put_data( node2, transfer("BCD1",node_value), "STRING" )

  call tree_create_node( tree, "NODE D", node1 )

  call tree_get_node_by_name( tree, "NODE A", node1 )
  call tree_get_data_ptr( node1, data_ptr, type )
  stored_data = transfer( data_ptr, stored_data )
  write(*,*) 'NODE A:', stored_data

  call tree_get_node_by_name( tree, "NODE B", node1 )
  call tree_get_data_ptr( node1, data_ptr, type )
  if ( associated( data_ptr ) ) then
     stored_data = transfer( data_ptr, stored_data )
     write(*,*) 'NODE B:', stored_data
  else
     write(*,*) 'NODE B - no data'
  endif

  call tree_get_node_by_name( node1, "NODE C", node2 )
  call tree_get_data_ptr( node2, data_ptr, type )
  stored_data = transfer( data_ptr, stored_data )
  write(*,*) 'NODE C:', stored_data

  write(*,*) ' '
  write(*,*) 'Traverse the tree:'

  call tree_traverse( tree, handler, dummy, stop )


  path = '//A/B/C///D'
  do while ( path /= ' ' )
     call tree_next_segment( path, segment )
     write(*,*) 'path:    >',trim(path),'<'
     write(*,*) 'segment: >',trim(segment),'<'
  enddo

!
! allocate( all_handles(1:0), stat=status ) ! A zero-length array!
!
! write(*,*) 'Status: ', status
! allocate( handles(1:10) )
! handles(1:10) = (/ (i, i=1,10) /)
! call tree_append_handles( all_handles, handles )
! write(*,*) 'First step: ', all_handles
! call tree_append_handles( all_handles, handles )
! write(*,*) 'Second step: ', all_handles

!
! 1. Find all nodes "A"   - just one
! 2. Find all nodes "C"   - four
! 3. Find all nodes "A/C" - just one
! 4. Find all nodes "B/C" - three
!
  do i = 1,4
     select case (i)
     case (1)
        path = "NODE A"
     case (2)
        path = "NODE C"
     case (3)
        path = "NODE A/NODE C"
     case (4)
        path = "NODE B/NODE C"
     end select
     call tree_find( tree, path, handles )
     write(*,*) 'Path: ',trim(path)
     do j = 1,size(handles)
        name = tree_get_name( handles(j)%node_ptr )
        call tree_get_data_string( handles(j)%node_ptr, string, success )
        write(*,*) 'Name: ', name, ' - ',trim(string)
     enddo
  enddo

end program

subroutine handler( tree, data, stop )
  use TREE_STRUCTURES
  type(TREE_DATA), pointer               :: tree
  character(len=1), dimension(:)         :: data
  logical, intent(inout)                 :: stop

  character(len=1), dimension(:),pointer :: data_ptr
  character(len=60)                      :: string
  character(len=20)                      :: type_string

  integer       :: level
  integer       :: i
  logical       :: success

  level = tree_traverse_level()

  write(*,*) ('   ', i=1,level), 'Node: ', trim(tree_get_name(tree))
  call tree_get_data_ptr( tree, data_ptr, type_string )

  string = '(no data)'

  call tree_get_data_string( tree, string, success )
  write(*,*) ('   ', i=1,level+1), trim(string), ' -- ', &
     trim(type_string), ' -- ', success

end subroutine handler

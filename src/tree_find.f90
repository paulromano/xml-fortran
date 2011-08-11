! tree_find.f90 --
!    Include file for tree_struct.f90: "advanced" search routines
!
!    $Id: tree_find.f90,v 1.2 2006/03/26 19:05:48 arjenmarkus Exp $
!
! General information:
!    This file contains the following routines:
!    - tree_next_segment:     strip a segment from the search path
!    - tree_append_handles:   append to an array of nodes
!    - tree_find_descendants: find all nodes with a given name and type
!    - tree_find:             find all nodes given by a path
!

! tree_next_segment --
!    Strip a segment from the search path
!
! Arguments:
!    path         Full path (or partially stripped)
!    segment      First segment in the path or nothing
! Result:
!    The first segment (that is a non-empty string between /'s) is
!    returned in the argument "segment", the path is updated
!    For instance:
!    //A/B/C ==> segment=A, new path=B/C
!
subroutine tree_next_segment( path, segment )
   character(len=*), intent(inout) :: path
   character(len=*), intent(out)   :: segment

   integer :: idx
   !
   ! Strip off any leading slashes first
   !
   do while ( path(1:1) == '/' )
      path(1:) = path(2:)
   enddo

   !
   ! Find the next segment
   !
   idx = index( path, '/' )
   if ( idx > 0 ) then
      segment  = path(1:idx-1)
      path(1:) = path(idx+1:)
   else
      if ( path == ' ' ) then
         segment = ' '
      else
         segment = path
         path    = ' '
      endif
   endif
end subroutine tree_next_segment

! tree_append_handles --
!    Append to an array of handles
!
! Arguments:
!    all_handles  Pointer to array holding all handles
!    handles      New array to be appended
! Result:
!    A new array with all handles included
!
subroutine tree_append_handles( all_handles, handles )
   type(TREE_DATA_PTR), dimension(:), pointer :: all_handles
   type(TREE_DATA_PTR), dimension(:)          :: handles

   type(TREE_DATA_PTR), dimension(:), pointer :: new_array

   integer :: total_size

   total_size = size(all_handles) + size(handles)
   allocate( new_array(1:total_size) )

   new_array(1:size(all_handles))            = all_handles
   new_array(size(all_handles)+1:total_size) = handles

   deallocate( all_handles )
   all_handles => new_array

end subroutine tree_append_handles

! tree_find_descendants --
!    Find all nodes with a given name and type
!
! Arguments:
!    tree         The node in the tree to start the search
!    name         The name of the node (empty for all)
!    type         The type of the node (empty for all)
!    all          Descend the tree (.true.) or only look at children
!    handles      A returned array of handles to the nodes that
!                 were found
! Result:
!    An array of node handles - each node is found on the specified
!    path. The user should deallocate the array when done
!
recursive subroutine tree_find_descendants_priv( &
                        tree, name, type, all, handles )
   type(TREE_DATA), pointer                     :: tree
   type(TREE_DATA_PTR), dimension(:), pointer   :: handles
   character(len=*)                             :: name
   character(len=*)                             :: type
   logical                                      :: all

   type(TREE_DATA_PTR), dimension(1:1)          :: new_handle
   character(len=80)                            :: node_name
   character(len=80)                            :: node_type
   integer                                      :: i

   if ( associated(tree%child_nodes) ) then
      do i = 1,size(tree%child_nodes)
         node_name = tree_get_name( tree%child_nodes(i)%node_ptr )
         node_type = tree_get_datatype( tree%child_nodes(i)%node_ptr )

         if ( (node_name .eq. name .or. name .eq. ' ') .and. &
              (node_type .eq. type .or. type .eq. ' ') ) then
            new_handle(1)%node_ptr => tree%child_nodes(i)%node_ptr
            call tree_append_handles( handles, new_handle )
         else
            if ( all ) then
               call tree_find_descendants_priv( &
                       tree%child_nodes(i)%node_ptr, &
                       name, type, all, handles )
            endif
         endif
      enddo
   endif

end subroutine tree_find_descendants_priv

! tree_find_descendants --
!    Find all nodes with a given name and type
!
! Arguments:
!    tree         The node in the tree to start the search
!    name         The name of the node (empty for all)
!    type         The type of the node (empty for all)
!    all          Descend the tree (.true.) or only look at children
!    handles      A returned array of handles to the nodes that
!                 were found
! Result:
!    An array of node handles - each node is found on the specified
!    path. The user should deallocate the array when done
!
subroutine tree_find_descendants( tree, name, type, all, handles )
   type(TREE_DATA), pointer                     :: tree
   type(TREE_DATA_PTR), dimension(:), pointer   :: handles
   character(len=*)                             :: name
   character(len=*)                             :: type
   logical                                      :: all

   type(TREE_DATA_PTR), dimension(:), pointer   :: new_handle
   character(len=80)                            :: node_name
   character(len=80)                            :: node_type

   allocate( handles(1:0) )
   call tree_find_descendants_priv( tree, name, type, all, handles )

end subroutine tree_find_descendants

! tree_find --
!    Find all nodes given by a path
!
! Arguments:
!    tree         The node in the tree to start the search
!    path         The path (a string of names separated by /)
!    handles      A returned array of handles to the nodes that
!                 were found
! Result:
!    An array of node handles - each node is found on the specified
!    path. The user should deallocate the array when done
!
subroutine tree_find( tree, path, handles )
   type(TREE_DATA), pointer                   :: tree
   type(TREE_DATA_PTR), dimension(:), pointer :: handles
   character(len=*)                           :: path

   !
   ! Local variables: use the string length of path!
   !
   character(len=len(path))                   :: path_stripped
   character(len=len(path))                   :: segment

   type(TREE_DATA_PTR), dimension(:), pointer :: result_handles
   type(TREE_DATA_PTR), dimension(:), pointer :: new_handles

   logical                                    :: all
   integer                                    :: i

   !
   ! For the first segment we will search the whole tree,
   ! after that we are only interested in direct children
   !
   all           = .true.
   path_stripped = path

   allocate( handles(1:1) )
   handles(1)%node_ptr => tree

   do
      call tree_next_segment( path_stripped, segment )

      if ( segment /= ' ' ) then
         allocate( result_handles(1:0) )
         do i = 1,size(handles)
            call tree_find_descendants( handles(i)%node_ptr, segment, &
                    "", all, new_handles )
            call tree_append_handles( result_handles, new_handles )
            deallocate( new_handles )
         enddo
         all = .false.
      else
         exit
      endif
      deallocate( handles )
      handles => result_handles
   enddo

end subroutine tree_find

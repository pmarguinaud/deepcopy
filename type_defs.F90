module type_defs
  implicit none

  type level1
     integer i
     real r
     real, pointer,dimension(:) :: field1
     type (level2),pointer :: lvl2_ptr
  end type level1

  type level2
     logical flag
     real, allocatable,dimension(:) :: field2
  end type level2
end module type_defs

program example
  use type_defs
  use openacc_transfers
  use work
  implicit none

  type(level1) :: var_lvl1
  type(level2),target :: var_lvl2
  type(level2):: another_lvl2

  integer,parameter:: nlen=20
  integer i,iter

! allocate data structures with some predefined size
  allocate(var_lvl1%field1(nlen))
  allocate(var_lvl2%field2(nlen))
  allocate(another_lvl2%field2(nlen))
! let the pointer inside var_lvl1 point to var_lvl2
  var_lvl1%lvl2_ptr => var_lvl2

! initialize with some data
  var_lvl1%field1(:)=    1.0
  var_lvl2%field2(:)=    2.0
  another_lvl2%field2(:)=0.0

! begin manual deepcopy data region, using our own routines
  call acc_manual_copyin(var_lvl1)
  call acc_manual_copyin(another_lvl2)

!$acc parallel loop present(var_lvl1,another_lvl2) default(none)
  do i=1,nlen
     another_lvl2%field2(i) = var_lvl1%field1(i) + var_lvl1%lvl2_ptr%field2(i) 
  enddo
!$acc end parallel

! end manual deepcopy data region, using our own routines
  call acc_manual_delete(var_lvl1)
  call acc_manual_copyout(another_lvl2)

! check if the field2 member is not zero anymore 
  print*,'Result after deepcopy:'
  print*,another_lvl2%field2(:)
  print*,'Expected value:',var_lvl1%field1(1) + var_lvl1%lvl2_ptr%field2(1) 

  print*,'---------------------------------------------------'
! This part of the code is faulty: there is a missing update. The routine called, 'computation',
! assumes there is no outer data region. When it calls a copyin for var_lvl1, the outer data 
! region here around the callsite leads to no data being transferred in the routine and it
! works with stale data, getting incorrect results.
! Setting acc_debug_on helps finding the issue, as it enforces transfers in the manual deepcopy
! routines. For demonstration, the 1st iteration of the below loop runs normal, then on the 2nd
! iteration, acc_debug_on is set and the results are correct.

  do iter=1,10
     if(iter==2) acc_debug_on=.true.

     print*,'Iteration:',iter,'acc_debug_on:',acc_debug_on
     call acc_manual_copyin(var_lvl1)
     call computation(var_lvl1)
     call acc_manual_delete(var_lvl1)

     if(iter==2) acc_debug_on=.false.
  enddo

  deallocate(var_lvl1%field1)
  deallocate(var_lvl2%field2)
  deallocate(another_lvl2%field2)

end program example



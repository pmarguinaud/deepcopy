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
!============================================
!============================================
module openacc_transfers
! This module implements the routines for a simple manual deepcopy. 
  use type_defs
  use openacc

  implicit none
! debugging flag, when on it enforces data transfers in acc_manual_copyin / acc_manual_copyout, 
! even if the data is already / still present, respectively
  logical,save:: acc_debug_on=.false.

! these generic interfaces allow using the same name in calls in the main code, no matter 
! which type is passed

! deep copyin of the derived type variable
  interface acc_manual_copyin
     module procedure copyin_lvl1
     module procedure copyin_lvl2
  end interface acc_manual_copyin

! deep delete of the derived type variable
  interface acc_manual_delete
     module procedure delete_lvl1
     module procedure delete_lvl2
  end interface acc_manual_delete

! deep copyout of the derived type variable
  interface acc_manual_copyout
     module procedure copyout_lvl1
     module procedure copyout_lvl2
  end interface acc_manual_copyout

! deep update host of the derived type variable
  interface acc_manual_upd_host
     module procedure upd_host_lvl1
     module procedure upd_host_lvl2
  end interface acc_manual_upd_host

! deep update device of the derived type variable
  interface acc_manual_upd_dev
     module procedure upd_dev_lvl1
     module procedure upd_dev_lvl2
  end interface acc_manual_upd_dev

! shallow update host of the derived type variable. Only the immediate members are transferred,
! but members which are themselves of derived type are left untouched
  interface acc_manual_upd_host_shallow
     module procedure upd_host_lvl1_shallow
     module procedure upd_host_lvl2_shallow
  end interface acc_manual_upd_host_shallow

! shallow update device of the derived type variable. Only the immediate members are transferred,
! but members which are themselves of derived type are left untouched
  interface acc_manual_upd_dev_shallow
     module procedure upd_dev_lvl1_shallow
     module procedure upd_dev_lvl2_shallow
  end interface acc_manual_upd_dev_shallow

contains

!============================================
! copyin routines
  subroutine copyin_lvl1(lvl1_var)
    type(level1):: lvl1_var

! If debugging flag is set, enforce a data transfer even if data is already present.
! Note hat we're using the 'shallow' variant of the update routines here, because we only want to
! update the members on the current level of the type. As this copyin routine calls back into the
! generic interface for the lower level derived type members, these will call corresponding 
! shallow updates on their respective levels as well. This avoids doing a full, deep update here,
! and then subsequently issuing transfers of the lower level data later, which would only 
! duplicate those transfers of the lower level derived type members.
    if(acc_debug_on)then
       call acc_manual_upd_dev_shallow(lvl1_var)
    endif

!$acc enter data copyin(lvl1_var)
!$acc enter data copyin(lvl1_var%field1)

    call acc_manual_copyin(lvl1_var%lvl2_ptr)
    call acc_attach(lvl1_var%lvl2_ptr)  ! this is necessary because inside the above call, no information is present anymore that the pointer argument is itself member of a parent type !
  end subroutine copyin_lvl1


  subroutine copyin_lvl2(lvl2_var)
    type(level2):: lvl2_var

! if debugging flag is set, enforce a data transfer even if data is already present 
    if(acc_debug_on)then
       call acc_manual_upd_dev_shallow(lvl2_var)
    endif

!$acc enter data copyin(lvl2_var)
!$acc enter data copyin(lvl2_var%field2)
  end subroutine copyin_lvl2
!============================================
!delete routines
  subroutine delete_lvl1(lvl1_var)
    type(level1):: lvl1_var

!$acc exit data delete(lvl1_var%field1)

    call acc_manual_delete(lvl1_var%lvl2_ptr)
    call acc_detach(lvl1_var%lvl2_ptr) ! this is necessary because inside the above call, no information is present anymore that the pointer argument is itself member of a parent type !
!$acc exit data delete(lvl1_var)
  end subroutine delete_lvl1


  subroutine delete_lvl2(lvl2_var)
    type(level2):: lvl2_var

!$acc exit data delete(lvl2_var%field2)
!$acc exit data delete(lvl2_var)
  end subroutine delete_lvl2
!============================================
!copyout routines
  subroutine copyout_lvl1(lvl1_var)
    type(level1):: lvl1_var

! if debugging flag is set, enforce a data transfer even if data is already present 
! Note hat we're using the 'shallow' variant of the update routines here, because we only want to
! update the members on the current level of the type. As this copyin routine calls back into the
! generic interface for the lower level derived type members, these will call corresponding 
! shallow updates on their respective levels as well. This avoids doing a full, deep update here,
! and then subsequently issuing transfers of the lower level data later, which would only 
! duplicate those transfers of the lower level derived type members.
    if(acc_debug_on)then
       call acc_manual_upd_hostv_shallow(lvl1_var)
    endif
!$acc exit data copyout(lvl1_var%field1)

    call acc_manual_copyout(lvl1_var%lvl2_ptr)
    call acc_detach(lvl1_var%lvl2_ptr) ! this is necessary because inside the above call, no information is present anymore that the pointer argument is itself member of a parent type !
!$acc exit data copyout(lvl1_var)
  end subroutine copyout_lvl1


  subroutine copyout_lvl2(lvl2_var)
    type(level2):: lvl2_var

! if debugging flag is set, enforce a data transfer even if data is already present 
    if(acc_debug_on)then
       call acc_manual_upd_hostv_shallow(lvl2_var)
    endif

!$acc exit data copyout(lvl2_var%field2)
!$acc exit data copyout(lvl2_var)
  end subroutine copyout_lvl2
!============================================
!update host routines. These are the 'deep' variants, which traverse the full type tree and
! transfer lower level derived type data as well.
  subroutine upd_host_lvl1(lvl1_var)
   type(level1):: lvl1_var

! update all the scalar members
!$acc update host(lvl1_var%i,lvl1_var%r)

!update the content of arrays, leaving the pointers intact
!$acc update host(lvl1_var%field1)
   call acc_manual_upd_host(lvl1_var%lvl2_ptr)

  end subroutine upd_host_lvl1


  subroutine upd_host_lvl2(lvl2_var)
   type(level2):: lvl2_var
! update all the scalar members
!$acc update host(lvl2_var%flag)

!update the content of arrays, leaving the pointers intact
!$acc update host(lvl2_var%field2)

 end subroutine upd_host_lvl2
!============================================
!update host routines. These are the 'shallow' variants, which only transfer data of intrinsic
! types at the current level, but do not process lower level derived type data.
  subroutine upd_host_lvl1_shallow(lvl1_var)
   type(level1):: lvl1_var

! update all the scalar members
!$acc update host(lvl1_var%i,lvl1_var%r)

!update the content of arrays, leaving the pointers intact
!$acc update host(lvl1_var%field1)

 end subroutine upd_host_lvl1_shallow


  subroutine upd_host_lvl2_shallow(lvl2_var)
   type(level2):: lvl2_var
! update all the scalar members
!$acc update host(lvl2_var%flag)

!update the content of arrays, leaving the pointers intact
!$acc update host(lvl2_var%field2)

 end subroutine upd_host_lvl2_shallow
!============================================
!update device routines. These are the 'deep' variants, which traverse the full type tree and
! transfer lower level derived type data as well.
  subroutine upd_dev_lvl1(lvl1_var)
   type(level1):: lvl1_var

! update all the scalar members
!$acc update device(lvl1_var%i,lvl1_var%r) if_present

!update the content of arrays, leaving the pointers intact
!$acc update device(lvl1_var%field1) if_present
   call acc_manual_upd_dev(lvl1_var%lvl2_ptr)

  end subroutine upd_dev_lvl1


  subroutine upd_dev_lvl2(lvl2_var)
   type(level2):: lvl2_var
! update all the scalar members
!$acc update device(lvl2_var%flag) if_present

!update the content of arrays, leaving the pointers intact
!$acc update device(lvl2_var%field2) if_present
 end subroutine upd_dev_lvl2
!============================================
!update device routines. These are the 'shallow' variants, which only transfer data of intrinsic
! types at the current level, but do not process lower level derived type data.
  subroutine upd_dev_lvl1_shallow(lvl1_var)
   type(level1):: lvl1_var

! update all the scalar members
!$acc update device(lvl1_var%i,lvl1_var%r) if_present

!update the content of arrays, leaving the pointers intact
!$acc update device(lvl1_var%field1) if_present

 end subroutine upd_dev_lvl1_shallow


  subroutine upd_dev_lvl2_shallow(lvl2_var)
   type(level2):: lvl2_var
! update all the scalar members
!$acc update device(lvl2_var%flag) if_present

!update the content of arrays, leaving the pointers intact
!$acc update device(lvl2_var%field2) if_present
 end subroutine upd_dev_lvl2_shallow

end module openacc_transfers
!============================================
!============================================
module work
  use type_defs
  use openacc_transfers
  implicit none
contains
  subroutine computation(var_lvl1)
    type(level1) :: var_lvl1
    real init_val
    integer lower,upper,i

    init_val=10.0
    var_lvl1%field1(:)=init_val
! IF there is an outer data region, the copyin below will not transfer any data. In that case,
! the compute loop below will work with stale data on the device, because the (host side)
! initialziation with init_val right above here will have been lost.
    call acc_manual_copyin(var_lvl1)

    lower=lbound(var_lvl1%field1,1)
    upper=ubound(var_lvl1%field1,1)
!$acc parallel loop present(var_lvl1)
    do i=lower,upper
       var_lvl1%field1(i) = var_lvl1%field1(i) * 2.0
    enddo

!$acc update host(var_lvl1%field1)
    if(var_lvl1%field1(1) == init_val * 2.0)then
       print*,'Correct result:',init_val * 2.0
    else
       print*,'ERROR! Result:',var_lvl1%field1(1),' but expected:',init_val * 2.0
    endif

    call acc_manual_delete(var_lvl1)

  end subroutine computation
end module work
!============================================
!============================================
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

  do iter=1,2
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



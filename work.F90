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

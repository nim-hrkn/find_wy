!Copyright 2016,  Hiori Kino
!
!Licensed under the Apache License, Version 2.0 (the "License");
!you may not use this file except in compliance with the License.
!You may obtain a copy of the License at
!
!    http://www.apache.org/licenses/LICENSE-2.0
!
!Unless required by applicable law or agreed to in writing, software
!distributed under the License is distributed on an "AS IS" BASIS,
!WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!See the License for the specific language governing permissions and
!limitations under the License.


program est
  implicit none

  call main

contains 



  subroutine main
    implicit none

    integer:: nwycoff,nkind
    integer,allocatable:: dup(:),num_atoms(:)
    logical, allocatable:: special(:)

    integer:: idata=3

    call util_rand_set(0,auto=.true.)

    ! input:
    ! spacegroup(id,choice) で与えられる (これは別program) 
    ! wychoff position 数(nwycoff) とその重複度(dup)

    select case(idata)
    case(1)
       special= [ .true., .true., .false. ,.false.,.false.]
       dup= [ 1,2,3,4,5 ]
    case(2)
       dup=    [  2 , 2 , 4 , 4 , 8 ,12, 24, 24, 48 ]
       special=[  .true. , .true. , .true. , .true. , .false. , .false.,  .false.,  .false.,  .false. ]
    case(3)     
       dup= [  1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  4 ]
       special= [  .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true.,&
            .false.,  .false.,  .false.,  .false.,  .false.,  .false.,  .false.,  .false.,&
            .false.,  .false.,  .false.,  .false.,  .false. ]
    end select

    if (size(dup)/=size(special)) then
       write(*,*)'size mismatch',size(dup),size(special)
       stop 30000
    endif
    nwycoff=size(dup)


    write(6,*) 'duplication and specialpositions of wycoff positions'
    write(6,'(a)',advance='no') '['
    write(6,'(100I3)',advance='no') dup
    write(6,'(a)') ']'
    write(6,'(a)',advance='no') '['
    write(6,'(100L3)',advance='no') special
    write(6,'(a)') ']'

    ! input:
    ! 原子種数(nkind) と原子種毎の原子数。(num_atoms[])

    num_atoms = [ 3,4, 10]
    !num_atoms = [ 6,8,9]
    !num_atoms = [ 12, 16, 20 ]
    nkind=size(num_atoms)

    write(6,'(a)',advance='no') '['
    write(6,'(100I3)',advance='no') num_atoms
    write(6,'(a)')']'

    call make_combinations4(nwycoff,dup,special,nkind,num_atoms)

    stop 0

  end subroutine main


  recursive function iqsort(ix) result(ires)
    integer, allocatable :: ires(:)
    integer, intent(in)  :: ix(:)
    integer :: k
    allocate(ires(size(ix)))
    if (size(ix) <= 1) then 
       ires = ix
    else  
       k = ix(size(ix) / 2)
       ires = [iqsort(pack(ix, ix < k)), pack(ix, ix == k), iqsort(pack(ix, ix > k))] 
    end if
  end function iqsort


  subroutine find_chocice_for_preidx2(dup,natom, is,ie)
    implicit none
    integer:: dup(:)
    integer:: natom
    integer,intent(out):: is,ie

    integer:: nwycoff
    integer:: im1,im2,is0,ie0
    integer,allocatable:: dup_sorted(:)

#if 1
    is=natom*0.7
    ie=natom*0.8
    is=min(is,natom-4)
    ie=max(is,natom-3)
#else
    nwycoff=size(dup)
    im1=min(nwycoff,2)
    im2=min(nwycoff,3)
    allocate( dup_sorted( nwycoff ) )
    dup_sorted= iqsort(dup) 

    is0=sum(dup_sorted(nwycoff-im1+1:nwycoff))
    ie0=sum(dup_sorted(nwycoff-im2+1:nwycoff))

    ie=natom-is0
    is=natom-ie0
#endif

  end subroutine find_chocice_for_preidx2

  subroutine make_combinations2(nwycoff,dup,special,nkind,num_atoms)
    !  random + all combination
    use m_combination
    implicit none
    integer,intent(in):: nwycoff,nkind
    integer:: dup(nwycoff)
    logical:: special(nwycoff)
    integer num_atoms(nkind)

    integer:: ikind
    integer:: alreadyused(nkind)

    integer:: num_atom_s, num_atom_e
    character(19):: thisfunc='make_combinations: '
    integer:: ifound
    logical:: found
    type(t_combination):: combi

    integer:: min_dup, max_choice
    integer:: num_atoms_s, num_atoms_e

    integer:: threshold,level,max_level
    integer,allocatable:: info(:), newinfo(:), loopmax(:), occup(:), alreadyoccup(:)
    integer,allocatable:: dup_tmp(:)

    allocate( alreadyoccup(nwycoff) ,occup(nwycoff)) 
    alreadyoccup=0

    do ikind=1,nkind
       write(6,*)
       write(6,*)'ikind=',ikind


       ! すでに専有されているspecial positionを除いてdupをつくる。

       ! num_atoms_s, num_atoms_eの計算
       call  find_chocice_for_preidx2(dup,num_atoms(ikind),num_atoms_s, num_atoms_e )


       write(6,*)thisfunc,ikind,'search  from',num_atoms_s,'to', num_atoms_e 

       min_dup=minval(dup)
       max_level=num_atoms(ikind)/min_dup + 1
       max_level=min(max_level,num_atoms(ikind))
       write(6,*)thisfunc,"kind=",ikind,"maxlevel=",max_level

       do threshold=num_atoms_e, num_atoms_s, -1

          min_dup=minval(dup)
          max_choice=num_atoms(ikind)/min_dup + 1
          max_choice=min(max_choice,threshold)

          write(6,*)thisfunc,'i_num_atom,max_choice=',threshold, max_choice

          call combi%init( max_level ) 
          call combi%set_num(num_atoms(ikind))
          call combi%set_dup(dup)
          call combi%set_special(special)

          if (allocated(info)) deallocate( info, loopmax )
          allocate( info(max_choice), loopmax(max_choice) )
          info=0; loopmax=nwycoff

          ifound=combi%pre_makeidx2(max_choice, info, loopmax, level, nwycoff,threshold,alreadyoccup )
          if (ifound/=0)then
             write(6,*)'found,level=',level
             write(6,'(a,100i3)')'info=',info(1:level)
             write(6,*)'sum=',combi%sum_num(level, info) 

             occup=0
             call combination_calc_occup(level,info,occup)
             write(6,'(a,100i3)')'alreadyoccup=',occup

             alreadyoccup=occup ! temporary setup
             ! start_level=level
             combi%infolevel=21
             if (allocated(newinfo)) deallocate(newinfo) 
             allocate( newinfo(max_level) )
             newinfo=0
             newinfo(1:level)=info(1:level)
             level=level+1
             found= combi%makeidx2(max_level,newinfo,loopmax,level,nwycoff)!,alreadyoccup)

             call  combi%show_list()

          endif

       enddo

    enddo

  end subroutine make_combinations2

  subroutine make_combinations3(nwycoff0,dup0,special0,nkind,num_atoms)
    !  random search
    use m_combination
    implicit none
    integer,intent(in):: nwycoff0,nkind
    integer:: dup0(nwycoff0)
    logical:: special0(nwycoff0)
    integer num_atoms(nkind)

    integer:: ikind,i

    character(20):: thisfunc='make_combinations3: '
    integer:: ifound, nwycoff
    logical:: found,havedup


    integer:: level,max_level, tot,min_dup,irand,nrand,jkind, ilevel
    integer,allocatable :: info(:),occup(:),dup(:),duporder(:),iv(:)
    logical,allocatable:: special(:)

    type(t_vector_i2),allocatable:: infolist(:)
    type(t_combination):: combi

    nrand=100

    allocate(infolist(nkind))

    do ikind=1,nkind
       write(6,*)
       write(6,*)'ikind=',ikind


       if (ikind==1) then

          if (allocated(dup))deallocate(dup)
          allocate(dup(size(dup0)))
          dup=dup0
          if (allocated(duporder)) deallocate(duporder)
          allocate(duporder(size(dup))) 
          do i=1,size(dup)
             duporder(i)=i
          enddo
          allocate(special(size(special0)))
          special=special0

          write(6,'(a,100i3)')'dup=    ',dup
          write(6,'(a,100l3)')'special=',special

       else
          if (allocated(occup)) deallocate(occup)
          allocate(occup(size(dup0)))
          occup=0
          do jkind=1,ikind-1
             max_level= infolist(jkind)%size1(1)
             call combination_calc_occup(max_level,infolist(jkind)%return_list_dim1(1),occup )
          enddo
          call make_nonduplicated_dup( dup0,  occup, special0, dup,special,duporder) 

       endif

       min_dup=minval(dup)
       max_level=num_atoms(ikind)/min_dup + 1
       max_level=min(max_level,num_atoms(ikind))
       if (allocated(info)) deallocate(info)
       allocate(info(max_level))

       nwycoff=size(dup)

       found=.false.
       do irand=1,nrand

          info=0
          level=0
          do level=1,max_level
             info(level) = util_rand_get_i(nwycoff) 

             tot = sum( dup(info(:level)) )
             if (tot==num_atoms(ikind) ) then
                call combi%init(0)
                call combi%set_num(num_atoms(ikind))
                call combi%set_dup(dup)
                call combi%set_special(special)                   
                if ( combi%havedup(info,level)) then
                   exit
                else
                   write(6,'(a,100i3)',advance='no')'found, info=',info(:level) 
                   write(6,*)'sum=',tot
                   found=.true.
                   call infolist(ikind)%init(level)
                   call infolist(ikind)%append(duporder(info(1:level))) 
                endif
             else if (tot >num_atoms(ikind) ) then
                exit
             endif
          enddo

          if (found) exit

       enddo

       if (.not. found) then
          write(6,*) thisfunc,'failed to find a candidate'
          write(6,*) 'for ikind=',ikind,'num=',num_atoms(ikind)
          write(6,'(a,100i3)') 'dup=',dup
          stop 500000
       endif

    enddo


    ! check duplication again
    do ikind=1,nkind
       iv= infolist(ikind)%return_list_dim1(1)
       write(6,'(a,i3,a)',advance='no') 'ikind=',ikind,', '
       do i=1,size(iv)
          write(6,'(i3,a,l1,a)',advance='no') iv(i),'(',special0(iv(i)),') '
       enddo
       write(6,*)
    enddo

    if (allocated(occup)) deallocate(occup) 
    allocate(occup(size(dup0)))
    occup=0
    do jkind=1,ikind-1
       max_level= infolist(jkind)%size1(1)
       call combination_calc_occup(max_level,infolist(jkind)%return_list_dim1(1),occup )
    enddo

    havedup=.false.
    do ilevel=1,nwycoff
       if (special0(ilevel))then
          if (occup(ilevel)>1)then
             havedup=.true.
             exit
          endif
       endif
    enddo

    if (havedup) then
       write(6,*)'internal error, havedup'
       stop 30000
    endif


    write(6,'(a,100i3)') 'occup   ',occup
    write(6,'(a,100l3)') 'special ',special0


  end subroutine make_combinations3


  subroutine make_nonduplicated_dup( dup0,  occup0,special0, dup,special,duporder)
    implicit none
    integer,intent(in):: dup0(:), occup0(:)
    logical,intent(in):: special0(:)
    integer,intent(out),allocatable:: dup(:),duporder(:)
    logical,intent(out),allocatable:: special(:)

    ! dup0(nwycoff)
    ! occup(nwycoff)
    ! special(nwycoff)

    ! tot = nwycoff - (site numbers occupied at special positions)
    ! dup(tot)
    ! duporder(tot)

    integer :: nwycoff, i,tot

    nwycoff= size(dup0) 

    write(6,'(a,100i3)')'occup  ',occup0
    write(6,'(a,100l3)')'special',special0
    tot=0
    do i=1, nwycoff
       if ( occup0(i)>0 .and.  special0(i) ) then 
!          write(6,*)'skip ',i,'due to dup'
          cycle
       endif
       tot=tot+1
    enddo

    write(6,*)'tot=',tot
    allocate( dup(tot), duporder(tot),special(tot) )

    tot=0
    do i=1, nwycoff
       if ( occup0(i)>0 .and.  special0(i) ) cycle
       tot=tot+1
!       write(*,*)'itot=',tot,'for',i
       dup(tot)= dup0(i) 
       duporder(tot)= i 
       special(tot)= special0(i) 
    enddo
    write(6,'(a,100i3)')'new dup     ',dup
    write(6,'(a,100i3)')'new duporder',duporder
    write(6,'(a,100l3)')'new special ',special

  end subroutine make_nonduplicated_dup


  subroutine util_rand_set(rnd,auto)
    implicit none
    integer:: rnd
    logical,optional:: auto
    integer:: seedsize
    integer,allocatable:: seed(:)
    logical:: lauto
    integer:: i

    if (present(auto)) then 
       if (auto) then
          lauto=.True.
       else 
          lauto=.False.
       endif
    endif

    write(*,*)'set random seed=',rnd
    call random_seed(size=seedsize)
    allocate(seed(seedsize))
    if (lauto) then
       ! we don't use rnd , but set rnd from system_clock 
       do i=1,seedsize
          call system_clock(count=seed(i))
       enddo
    else
       seed(:)=rnd
    endif
    write(6,*)'seed=',seed
    call random_seed(put=seed)
  end subroutine util_rand_set


  function search_condition( natom, dup )  result(cond)
   implicit none
   integer:: natom,dup(:)
   integer :: cond
   integer:: min_dup, max_level 

   
       min_dup=minval(dup)
       max_level=natom/min_dup
     if (max_level<=4) then 
        cond=1
     else
        cond=2
     endif
  end function search_condition

  subroutine make_combinations4(nwycoff0,dup0,special0,nkind,num_atoms)
    !  all combination for small number of atoms
    !  random search
    use m_combination
    implicit none
    integer,intent(in):: nwycoff0,nkind
    integer:: dup0(nwycoff0)
    logical:: special0(nwycoff0)
    integer num_atoms(nkind)

    integer:: ikind,i

    character(20):: thisfunc='make_combinations4: '
    integer:: ifound, nwycoff
    logical:: found,havedup


    integer:: level,max_level, tot,min_dup,irand,nrand,jkind, ilevel,iselect
    integer,allocatable :: info(:),occup(:),dup(:),duporder(:),iv(:),loopmax(:)
    logical,allocatable:: special(:)

    type(t_vector_i2),allocatable:: infolist(:)
    type(t_combination):: combi

    nrand=100

    allocate(infolist(nkind))

    do ikind=1,nkind
       write(6,*)
       write(6,*)'ikind=',ikind

       if (ikind==1) then

          if (allocated(dup))deallocate(dup)
          allocate(dup(size(dup0)))
          dup=dup0
          if (allocated(duporder)) deallocate(duporder)
          allocate(duporder(size(dup))) 
          do i=1,size(dup)
             duporder(i)=i
          enddo
          allocate(special(size(special0)))
          special=special0

          write(6,'(a,100i3)')'dup=    ',dup
          write(6,'(a,100l3)')'special=',special

       else
          if (allocated(occup)) deallocate(occup)
          allocate(occup(size(dup0)))
          occup=0
          do jkind=1,ikind-1
             max_level= infolist(jkind)%size1(1)
             call combination_calc_occup(max_level,infolist(jkind)%return_list_dim1(1),occup )
          enddo
          call make_nonduplicated_dup( dup0,  occup, special0, dup,special,duporder) 

       endif

!
!    dup, special is only for this ikind
!

       min_dup=minval(dup)
       max_level=num_atoms(ikind)/min_dup + 1
       max_level=min(max_level,num_atoms(ikind))
       if (allocated(info)) deallocate(info)
       allocate(info(max_level))

       nwycoff=size(dup)


       select case ( search_condition( num_atoms(ikind), dup ) ) 
       case (1) 
!---- all combinations
         write(6,*)'search all the combinations for n=',num_atoms(ikind)
         if ( allocated( loopmax ) ) deallocate( loopmax )
         allocate( loopmax( max_level ) ); loopmax= nwycoff
                call combi%init(max_level) 
                call combi%set_num(num_atoms(ikind))
                call combi%set_dup(dup)
                call combi%set_special(special)

         call combi%makeidx(max_level,info,loopmax, 1 )
         if (combi%list%last2==0) then 
            found=.false.
         else 
         iselect = util_rand_get_i( combi%list%last2 )
         info = combi%list%return_list_dim1(iselect) 
         level=size(info)
         call infolist(ikind)%init(level)
         call infolist(ikind)%append(duporder(info(1:level))) 
         found=.true.
         endif 

       case (2)

       write(6,*)'search randomly for n=',num_atoms(ikind)
!--- random serach
       found=.false.
       do irand=1,nrand

          info=0
          level=0
          do level=1,max_level
             info(level) = util_rand_get_i(nwycoff) 

             tot = sum( dup(info(:level)) )
             if (tot==num_atoms(ikind) ) then
                call combi%init(0)
                call combi%set_num(num_atoms(ikind))
                call combi%set_dup(dup)
                call combi%set_special(special)                   
                if ( combi%havedup(info,level)) then
                   exit
                else
                   write(6,'(a,100i3)',advance='no')'found, info=',info(:level) 
                   write(6,*)'sum=',tot
                   found=.true.
                   call infolist(ikind)%init(level)
                   call infolist(ikind)%append(duporder(info(1:level))) 
                endif
             else if (tot >num_atoms(ikind) ) then
                exit
             endif
          enddo

          if (found) exit

       enddo
!--- end random search
       end select 

       if (.not. found) then
          write(6,*) thisfunc,'failed to find a candidate'
          write(6,*) 'for ikind=',ikind,'num=',num_atoms(ikind)
          write(6,'(a,100i3)') 'dup=',dup
          stop 500000
       endif

    enddo


    ! check duplication again
    do ikind=1,nkind
       iv= infolist(ikind)%return_list_dim1(1)
       write(6,'(a,i3,a)',advance='no') 'ikind=',ikind,', '
       do i=1,size(iv)
          write(6,'(i3,a,l1,a)',advance='no') iv(i),'(',special0(iv(i)),') '
       enddo
       write(6,*)
    enddo

    if (allocated(occup)) deallocate(occup) 
    allocate(occup(size(dup0)))
    occup=0
    do jkind=1,ikind-1
       max_level= infolist(jkind)%size1(1)
       call combination_calc_occup(max_level,infolist(jkind)%return_list_dim1(1),occup )
    enddo

    havedup=.false.
    do ilevel=1,nwycoff
       if (special0(ilevel))then
          if (occup(ilevel)>1)then
             havedup=.true.
             exit
          endif
       endif
    enddo

    if (havedup) then
       write(6,*)'internal error, havedup'
       stop 30000
    endif


    write(6,'(a,100i3)') 'occup   ',occup
    write(6,'(a,100l3)') 'special ',special0


  end subroutine make_combinations4



end program est




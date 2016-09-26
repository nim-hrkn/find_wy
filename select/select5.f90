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


#define  RETURN_ARRAY 1
module m_select
  use m_combisummary
  use m_combination
  implicit none

  type t_select
     integer:: infolevel=10
     type(t_combisummary):: combisum
     integer,allocatable:: info(:)
   contains
     procedure:: make_combinations=> select_make_combinations
     procedure:: make_combinations4=> select_make_combinations4
  end type t_select

contains

  ! 原子種毎に行う。
  ! 原子数とspacegroupの重複度(dup),specialpositionかどうか(special)が与えられている
  ! 
  subroutine make_combination(combi_eachspecie,nsite,dup,special,num_atom)
    implicit none
    type(t_combination):: combi_eachspecie
    integer,intent(in):: nsite
    integer,intent(in):: dup(nsite)
    logical,intent(in):: special(nsite)
    integer,intent(in):: num_atom

    type(t_combination):: combi
    integer:: min_dup,max_choice,choice,i,level
    integer,allocatable:: sizes(:),info(:)
    character(19)::thisfunc='make_combination: '

    min_dup=minval(dup)
    max_choice=num_atom/min_dup + 1

    max_choice=min(max_choice,num_atom)

    call combi_eachspecie%init(max_choice)

    ! nsiteからchoiceサイト選択
    ! nsite**choiceのloop indexをつくる
    do choice=1,max_choice
       if (combi_eachspecie%infolevel>100) then
          write(6,*) thisfunc,'choice=',choice
       endif
#if 0
       write(6,'(a,1x,a)',advance='no')thisfunc,'dup=    ['
       write(6,'(100i3)',advance='no') dup; write(6,*) ']'
       write(6,'(a)',advance='no')'special=['
       write(6,'(100L3)',advance='no') special; write(6,*) ']'
#endif

       if (allocated(sizes)) deallocate(sizes)
       allocate(sizes(choice)); sizes=nsite
       if (allocated(info)) deallocate(info)
       allocate(info(choice)); info=0

       level=choice
       ! parameterの設定
       combi%infolevel=combi_eachspecie%infolevel
       call combi%init(choice)
       call combi%set_num(num_atom)
       call combi%set_dup(dup)
       call combi%set_special(special)
       ! 組み合わせを計算
       call combi%makeidx(choice,info,sizes,1)  ! recursiveにloop を回す。

       ! combi_kinallにcombiを追加
       call combi_eachspecie%append_combi(combi)

    enddo

  end subroutine make_combination

  subroutine select_make_combinations(self,run,nsite,dup,special,nkind,num_atoms,chooseid,dryrun)
    use m_combisummary
    implicit none
    class(t_select):: self
    character(*),intent(in):: run
    integer,intent(in):: nsite
    integer,intent(in):: dup(nsite)
    logical,intent(in):: special(nsite)
    integer,intent(in):: nkind
    integer,intent(in):: num_atoms(nkind)
    integer,intent(in):: chooseid
    logical,intent(in),optional:: dryrun
    !    type(t_combisummary):: combisum

    integer:: ikind,level
    integer::sizes(nkind),info(nkind)
    logical:: doit
    character(27)::thisfunc='select_make_combinations: '


    write(6,*)thisfunc,' run=',run

    if (run=="all") then

       write(6,*)thisfunc,' infolevel=',self%infolevel


       if (present(dryrun)) then 
          call self%combisum%init(dryrun=dryrun)
       else
          call self%combisum%init()
       endif

       if (allocated( self%combisum%combi_eachspecie) ) deallocate ( self%combisum%combi_eachspecie )
       allocate(self%combisum%combi_eachspecie(nkind)) 

       ! atomic species毎に数、dup,specialから組み合わせを求める。
       do ikind=1,nkind
          self%combisum%infolevel=self%infolevel
          if (self%infolevel>100)then
             write(6,*)thisfunc
             write(6,*)thisfunc,'num_atom=',num_atoms(ikind)
          endif
          call make_combination(self%combisum%combi_eachspecie(ikind),nsite,dup,special,num_atoms(ikind))

          if(self%infolevel>100)then
             write(6,*)thisfunc,'kind=',ikind,' num_atom=',num_atoms(ikind)
             call self%combisum%combi_eachspecie(ikind)%show_list()
          endif

       enddo

    endif

    !
    !  さらにkind毎のcombiをすべて組み合わせる。
    !  条件はspecial position は一度だけ。
    !  

    do ikind=1,nkind
       sizes(ikind)= self%combisum%combi_eachspecie(ikind)%list%last2
    enddo
    if (self%infolevel>9) then
       write(6,*)thisfunc,'combination size of each atom'
       write(6,'(a,1x,a)',advance='no')thisfunc,'[' 
       write(6,'(100(i10,1x))',advance='no') sizes
       write(6,*)']' 
       do ikind=1,nkind
          write(6,'(a,a,i3)')thisfunc,' combination of specie=',ikind
          call self%combisum%combi_eachspecie(ikind)%show_list
       enddo
    endif

    level=nkind
    !条件の設定
    call self%combisum%set_dup(dup)
    call self%combisum%set_special(special)
    call self%combisum%set_nkind(nkind)
    ! 全種類を含めた全体の組み合わせを求める。
    write(6,*)thisfunc,'combination of the crystal'
    doit=.true.
    call self%combisum%makeidx(nkind,info,sizes,level,chooseid,doit) ! recursive call
    if (chooseid>0) then
       !write(6,*)'choose info=',info 
       allocate(self%info(nkind))
       self%info=info 
       !write(6,*) 'info=', info 
    endif

  end subroutine select_make_combinations



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
    character(24)::thisfunc='make_nonduplicated_dup: '

    nwycoff= size(dup0) 

    write(6,'(a,100i3)')thisfunc//'occup  ',occup0
    write(6,'(a,100l3)')thisfunc//'special',special0
    tot=0
    do i=1, nwycoff
       if ( occup0(i)>0 .and.  special0(i) ) then 
!          write(6,*)'skip ',i,'due to dup'
          cycle
       endif
       tot=tot+1
    enddo

    write(6,*)thisfunc//'tot=',tot
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
    write(6,'(a,100i3)')thisfunc//'new dup     ',dup
    write(6,'(a,100i3)')thisfunc//'new duporder',duporder
    write(6,'(a,100l3)')thisfunc//'new special ',special

  end subroutine make_nonduplicated_dup




  subroutine select_make_combinations4(self,nwycoff0,dup0,special0,nkind,num_atoms,nrand)
    !  all combination for small number of atoms
    !  random search
    use m_combination
    implicit none
    class(t_select):: self
    integer,intent(in):: nwycoff0,nkind
    integer:: dup0(nwycoff0)
    logical:: special0(nwycoff0)
    integer num_atoms(nkind)
    integer,intent(in):: nrand

    integer:: ikind,i

    character(27):: thisfunc='select_make_combinations4: '
    integer:: ifound, nwycoff
    logical:: found,havedup


    integer:: level,max_level, tot,min_dup,irand,jkind, ilevel,iselect
    integer,allocatable :: info(:),occup(:),dup(:),duporder(:),iv(:),loopmax(:)
    logical,allocatable:: special(:)

    type(t_vector_i2),allocatable:: infolist(:)
    type(t_combination):: combi

    allocate(infolist(nkind))

    do ikind=1,nkind
       write(6,*)thisfunc
       write(6,*)thisfunc,'ikind=',ikind

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

          write(6,'(a,100i3)')thisfunc//'dup=    ',dup
          write(6,'(a,100l3)')thisfunc//'special=',special

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
         write(6,*)thisfunc//'search all the combinations for n=',num_atoms(ikind)
#if 1
         call make_combination(combi,nwycoff,dup,special, num_atoms(ikind) )
         call combi%show_list()
#else
         if ( allocated( loopmax ) ) deallocate( loopmax )
         allocate( loopmax( max_level ) ); loopmax= nwycoff
                call combi%init(max_level) 
                call combi%set_num(num_atoms(ikind))
                call combi%set_dup(dup)
                call combi%set_special(special)

         call combi%makeidx(max_level,info,loopmax, 1 )
         call combi%show_list()
#endif
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

       write(6,*)thisfunc//'search randomly for n=',num_atoms(ikind)
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
                   write(6,'(a,100i3)')thisfunc//'found, info=',info(:level) 
                   write(6,*)thisfunc//'sum=',tot
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

! summary
    write(6,*)thisfunc
    write(6,*)thisfunc
    do ikind=1,nkind
       if (allocated(iv)) deallocate(iv)
       allocate(iv(infolist(ikind)%size1(1)))
       iv= infolist(ikind)%return_list_dim1(1)
       write(6,'(a,i3,a)',advance='no') thisfunc//'ikind=',ikind,', '
       do i=1,size(iv)
          write(6,'(i3,a,l1,a)',advance='no') iv(i),'(',special0(iv(i)),') '
       enddo
       write(6,*)
    enddo

    ! check duplication again
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
       write(6,*)thisfunc//'internal error, havedup'
       stop 30000
    endif


    write(6,'(a,100i3)') thisfunc//'occup   ',occup
    write(6,'(a,100l3)') thisfunc//'special ',special0
    write(6,*)thisfunc
    write(6,*)thisfunc


    call self%combisum%init()
    call self%combisum%set_dup(dup0)
    call self%combisum%set_special(special0)
    call self%combisum%set_nkind(nkind)
    allocate( self%info(nkind) ); self%info=1
    do ikind=1,nkind
       iv =infolist(ikind)%return_list_dim1(1)
       call combi%init(size(iv))
       call combi%set_num(num_atoms(ikind))
       call combi%set_dup(dup0)
       call combi%set_special(special0)
       call combi%list%append(iv) 
       call self%combisum%combi_eachspecie(ikind)%copy(combi)
    enddo


  end subroutine select_make_combinations4


end module m_select


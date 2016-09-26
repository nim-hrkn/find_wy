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


#define RETURN_ARRAY 0
module m_combination
  use m_vector_i2
  implicit none


  type t_combination
     integer::infolevel=10
     integer:: num =0 ! number of the specific atom 
     integer,allocatable::dup(:) ! duplication of wycoff position
     logical,allocatable::special(:) ! special position or not
     !  最大数が分からないのでvector型を定義
     type(t_vector_i2):: list  ! list of combination 

   contains
     procedure:: init=> combination_init
     procedure:: makeidx=> combination_makeidx
     procedure:: pre_makeidx2 => combination_pre_makeidx2
     procedure:: makeidx2=> combination_makeidx2
     procedure:: set_num=> combination_set_num
     procedure:: set_dup=> combination_set_dup
     procedure:: set_special=> combination_set_special
     procedure:: show_list=> combination_show_list
     procedure:: append_combi=> combination_append_combi
     procedure:: sum_num => combination_sum_num
     procedure:: havedup => combination_havedup
     procedure:: copy => combination_copy
  end type t_combination


  !  type t_combination_sum
  !
  !  end type t_combination_sum

contains

   subroutine combination_copy(self, src)
   implicit none
     class(t_combination),intent(out):: self
     type(t_combination),intent(in):: src
     self%infolevel=src%num
     self%num=src%num
     if (.not.allocated(self%dup)) allocate(self%dup(size(src%dup)))
     self%dup=src%dup
     if (.not.allocated(self%special)) allocate(self%special(size(src%special)))
     self%special=src%special
     call self%list%copy(src%list)
   end subroutine combination_copy

   subroutine combination_calc_occup( maxlevel,info,positionsum)
implicit none
   integer:: maxlevel,info(:)
    integer,intent(inout):: positionsum(:)

   integer:: ilevel,i
       do ilevel=1,maxlevel
          i=info(ilevel)
          if (i>0) then 
            positionsum(i)=positionsum(i)+1
          else
            write(6,*)'combination_calc_occup: serious error'
            write(6,*)'combination_calc_occup info=',info
            write(6,*)'run with check_boundary options'
          endif
       enddo

   end subroutine combination_calc_occup

   function combination_havedup ( self, info, maxlevel, occup)  result(havedup)
     implicit none
     class(t_combination):: self
       integer:: info(maxlevel)
       integer:: maxlevel
       integer,optional:: occup(:) ! size(occup)==nwycoff
       logical :: havedup

       integer:: nwycoff
       integer:: ilevel,i 
       integer,allocatable::positionsum(:)
       character(21)::thisfunc='combination_havedup: '

       nwycoff=size(self%dup)
       allocate(positionsum(nwycoff))
       positionsum=0
#if 0
       if (present(occup)) then 
          if (size(occup)/=nwycoff) then
             write(6,*)thisfunc,'size(occup)/=nwycoff',size(occup),nwycoff
             stop 40001
          endif
          positionsum=occup
       endif
#endif

#if 1
       call combination_calc_occup(maxlevel,info,positionsum)
#else
       do ilevel=1,maxlevel
          i=info(ilevel)
          positionsum(i)=positionsum(i)+1
       enddo
#endif
!       write(6,'(a,a,100I3)')thisfunc,'occup=',positionsum
       havedup=.false.
       do ilevel=1,nwycoff
          if (self%special(ilevel))then
             if (positionsum(ilevel)>1)then
                havedup=.true.
                return
             endif
          endif
       enddo
   end function combination_havedup 


  ! info(1:maxlevl)にdupのindexが入っているので、dup( info ) で全部の原子数になる。
  integer function combination_sum_num(self,maxlevel,info)
    implicit none
    class(t_combination):: self
    integer,intent(in):: maxlevel,info(maxlevel)
    integer:: s,ilevel,j
    s=0;
    do ilevel=1,maxlevel
       j=info(ilevel)
       if (j>0) then 
          s=s+self%dup(j)
       endif
    enddo
    combination_sum_num=s 
  end function combination_sum_num

  ! add -> selfへ追加
  subroutine combination_append_combi(self, add)
    implicit none
    class(t_combination):: self
    class(t_combination),intent(in):: add
    if (self%num==0) then 
       self%num=add%num ! 原子数はコピー元の値に 
    else
       if (self%num/=add%num) then 
          !          もともと値を持っていたら同じ数であることをかくにん
          write(6,*)'combination_append_combi: internal error, self%num/=add%num'
          write(6,*)'combination_append_combi:','self%num=',self%num,'add%num=',add%num 
          stop  
       endif

    endif
    call self%list%append_list(add%list)
    !write(*,*)'combi copy, size2=',size(self%list%v,dim=2)
  end subroutine combination_append_combi


  subroutine combination_show_list(self)
    implicit none
    class(t_combination):: self
    integer:: i,k
    character(23):: thisfunc='combination_show_list: '
#if RETURN_ARRAY
    integer,allocatable :: iv(:)
    allocate(iv(self%list%max1))
#endif
    write(6,*)thisfunc,"show_list"
    do i=1,self%list%last2
        write(6,'(a,a)',advance='no') thisfunc, '['
#if RETURN_ARRAY
       iv=0
       iv = self%list%return_list_dim1(i)
       write(6,'(100i3)',advance='no') iv 
#else
       k= self%list%size1(i)
       write(6,'(100i3)',advance='no') self%list%v(1:k,i)
#endif
       write(6,'(a)') ' ]'
    enddo
  end subroutine combination_show_list


  subroutine combination_init(self,n1)
    implicit none
    class(t_combination):: self
    integer,intent(in):: n1
    call self%list%init(n1,title="combination")
  end subroutine combination_init


  subroutine combination_set_num(self,n)
    implicit none
    class(t_combination):: self
    integer,intent(in):: n
    self%num=n 
  end subroutine combination_set_num

  subroutine combination_set_dup(self,dup)
    implicit none
    class(t_combination):: self
    integer,intent(in):: dup(:)
    integer:: n
    n=size(dup)
    if (allocated(self%dup)) deallocate(self%dup)
    allocate(self%dup(n))
    self%dup=dup 
  end subroutine combination_set_dup

  subroutine combination_set_special(self,special)
    implicit none
    class(t_combination):: self
    logical,intent(in):: special(:)
    integer:: n
    n=size(special)
    if (allocated(self%special)) deallocate(self%special)
    allocate(self%special(n))
    self%special=special
!!!write(6,'(a,100L1)')'combination_set_special: special=',special
  end subroutine combination_set_special


  ! levelは順方向に増える。
  recursive subroutine combination_makeidx(self,maxlevel,info,loopmax,level)
    implicit none
    class(t_combination):: self
    integer,intent(in):: maxlevel
    integer,intent(in):: level
    integer,intent(in)::loopmax(maxlevel)
    integer,intent(inout)::info(maxlevel)
    integer::idx,ilevel
    integer:: s,i,nwycoff,imin
    !  logical:: flag
    integer,allocatable::positionsum(:)
    character(21)::thisfunc='combination_makeidx: '

    nwycoff=size(self%dup)

    if (level==maxlevel+1) then
       !---------------------------
!!! do something 

!!! info(1)<=info(2)<=info(3) ...

       ! 全原子数がnumと等しい という条件
       !s=0; 
       !do ilevel=1,maxlevel
       !   s=s+self%dup(info(ilevel))
       !enddo
       s=self%sum_num(maxlevel,info)
       if (s/=self%num) then 
          return;
       endif

       ! info(i)<=info(i+1) という条件
       do ilevel=1,maxlevel-1
          if (info(ilevel)>info(ilevel+1)) then; 
             return
          endif
       enddo

       ! special positionがひとつだけという条件
       if (self%infolevel>100) then
          write(6,'(a,100L1)') 'specialpos=',self%special
       endif
#if 1
       if (self%havedup(info,maxlevel)) return 
#else
       allocate(positionsum(nwycoff))
       positionsum=0
       do ilevel=1,maxlevel
          i=info(ilevel)
          positionsum(i)=positionsum(i)+1
       enddo
       do ilevel=1,nwycoff
          if (self%special(ilevel))then
             if (positionsum(ilevel)>1)then
                return
             endif
          endif
       enddo
#endif

       ! listに加える。

       if (self%infolevel>20) then
          write(6,'(a)',advance='no') '                         add '
          write(6,'(100i5)') info
       endif
       call self%list%append(info) 

       !---------------------------
       return
    endif



    !      do i=1,n
    !       do j=i,n
    !         do k=j,n
    ! 相当のことを行う。
    !
    ! recursive calls
    !
    imin=1
    if (level>1) imin=info(level-1)
    do idx=imin,loopmax(level)
       info(level)=idx

       s=self%sum_num(level,info)
       !write(6,*)'total_e', s, self%num
       if (s>self%num) then
          if (self%infolevel>20)then
             write(6,'(a,3i4)',advance='no')'skip this branch, idx,level,sum=',idx,level,s
             write(6,'(a,10i4)')'info=',info(:level)
          endif
          return 
       endif

#if 0
       write(6,'(a)',advance='no') 'a '
       write(6,'(100i3)',advance='no') info
       write(6,'(a)',advance='no') ' ,'
       write(6,'(100i3)',advance='no') self%dup
       write(6,'(a)',advance='no') ' ,'
       write(6,*)  self%sum_num(maxlevel,info)
#endif

       ! if (self%sum_num(maxlevel,info) > self%num) then 
       !    cycle 
       ! endif
       ! 動作確認の表示
       if (level==1) then 
          !         if (self%infolevel>100) then
          write(6,'(a,i4,a,100i4)',advance='no')thisfunc, maxlevel,'/',loopmax
          write(6,'(a,i4,a,i4)') ' idx=',idx,' / ', loopmax(maxlevel)
          !         endif
       endif
       call combination_makeidx(self,maxlevel,info,loopmax,level+1)
    enddo

  end subroutine combination_makeidx


  function combination_pre_makeidx2(self,maxlevel,info, loopmax,level,nwycoff,threshold,occup) result (found)
!
!    sum(info(1:level-1))>thresholdをrandomにつくる。
!
    implicit none
    class(t_combination):: self
    integer,intent(in):: maxlevel
    integer,intent(out):: level
    integer,intent(in)::loopmax(maxlevel)
    integer,intent(inout)::info(maxlevel)
    integer,intent(in):: nwycoff
    integer,intent(in)::threshold
    integer,intent(in),optional:: occup(nwycoff) ! already used positionsum(iwycoff)>=1,  not used positionsum(iwycoff)=0
    integer:: found

    logical:: havedup
    integer:: s,l_occup(nwycoff)

    integer:: irand, nrand, i, j
    character(26)::thisfunc='combination_pre_makeidx2: '

!     write(6,*)'>start, threshold,num=',threshold,self%num
    found=0
    nrand=100
    do irand=1,nrand
!       write(6,*)'irand=',irand,'/',nrand
       info=0
       do level=1,maxlevel
          info(level)=util_rand_get_i(nwycoff)
          s=self%sum_num(level,info)
!          write(6,'(a,20i3)')'level,info=',level,info(1:level),s,threshold
          if (s<=self%num .and. s>=threshold ) then 

             ! special positionがひとつだけという条件
             if (self%infolevel>100) then
                write(6,'(a,100L1)') 'specialpos=',self%special
             endif

             ! special positionの重複check
             l_occup=0
             havedup=self%havedup(info,level,l_occup)
!             write(6,*)'havedup',havedup

             if ( havedup) then
                found=0
             else
                if (s==self%num) then 
                   found=2
                else
                   found=1
                endif
                return 
             endif

          endif

         if (s>threshold) exit

       enddo
    enddo

  end function combination_pre_makeidx2



  ! levelは順方向に増える。
  recursive function combination_makeidx2(self,maxlevel,info,loopmax,level,nwycoff) result(found)
!
!  info(1:level-1)を予めsetしておく。
!  loopにinfo(i)<=info(i+1) という条件がない。
!
    implicit none
    class(t_combination):: self
    integer,intent(in):: maxlevel
    integer,intent(in):: level
    integer,intent(in)::loopmax(maxlevel)
    integer,intent(inout)::info(maxlevel)
    integer,intent(in):: nwycoff
  !integer,intent(in):: alreadyoccup(nwycoff) ! already used positionsum(iwycoff)>=1,  not used positionsum(iwycoff)=0
    logical:: found
    integer::idx
    integer:: s,i,imin
    !  logical:: flag
    integer,allocatable::positionsum(:),occup(:)
    character(22)::thisfunc='combination_makeidx2: '


    write(6,*)thisfunc,'level=',level
    
    found=.false.

    if (nwycoff/=size(self%dup)) then
       write(6,*)thisfunc,'nwycoff, inconsistent',nwycoff,size(self%dup)
       stop 40000
    endif

    allocate(occup(nwycoff)) 

    if (level==maxlevel+1) then
       ! みつからなかった。
       return 
    endif
    !---------------------------


    !      do i=1,n
    !       do j=i,n
    !         do k=j,n
    !           do ...
    ! 相当のことを行う。
    !
    ! recursive calls
    !
    do idx=1,loopmax(level)
       info(level)=idx

       s=self%sum_num(level,info)
       write(6,'(a,i3,a,100i3)',advance='no')thisfunc//'add',info(level), ' info=',info(:level)
       write(6,'(a,i3,a,100i3)')' sum=',s 
       if (s>self%num) then
          if (self%infolevel>20)then
             write(6,'(a,3i4)',advance='no')thisfunc//'skip this branch, idx,level,sum=',idx,level,s
             write(6,'(a,10i3)')'info=',info(:level)
          endif
          return 
       endif

       ! 全原子数がnumと等しい という条件
       if (s==self%num) then 

          ! special positionがひとつだけという条件
             occup=0
             call combination_calc_occup(level,info,occup)
             found=self%havedup(info,level,occup)
             write(6,'(a,100i3)') thisfunc//"info=",info(:level)
             write(6,'(a,100i3)') thisfunc//"occup=",occup
          if (found) then
             write(6,*)thisfunc//'skip because of dup'
             cycle
          endif

          if (self%infolevel>20) then
             write(6,'(a)',advance='no') thisfunc//' found'
             write(6,'(100i3)') info(:level)
          endif

          found=.true.
          call self%list%append(info(:level))
          cycle

       endif

       if (s<self%num) then
          ! まだ原子を加えられるならば
          write(6,'(a,a,100i3)',advance="no")thisfunc,'add further, now info=',info(1:level)
          write(6,'(a,100i3)') '/', s , self%num
          found= self%makeidx2(maxlevel,info,loopmax,level+1,nwycoff)
       endif
    enddo

  end function combination_makeidx2

  function util_rand_get_i(m) result(i)
    implicit none
    integer:: m
    real(8):: r
    integer:: i

    call random_number(r)

    r=r*m
    i=int(r)
    if (i==m) i=m-1

    i=i+1
  end function util_rand_get_i

  function util_rand_get_iv(m,nsize) result(i)
    implicit none
    integer:: m,nsize

    real(8):: r(nsize)
    integer:: i(nsize)
    integer:: j
   
    call random_number(r)

    r=r*m
    i=int(r)
    do j=1,nsize
       if (i(j)==m) i(j)=m-1
    enddo

    i=i+1
  end function util_rand_get_iv



end module m_combination


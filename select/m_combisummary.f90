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


1 module m_combisummary
    use m_combination
    implicit none

    type t_combisummary
       integer::infolevel=10
       integer,allocatable::dup(:)  ! duplication of wycoff position
       logical,allocatable::special(:)  ! special position or not
       integer:: nkind =0   ! number of species
       type(t_combination),allocatable:: combi_eachspecie(:) ! list of combination

       type(t_vector_i2):: infolist

       integer,allocatable:: freq(:)  ! frequency of 
       logical:: dryrun=.false.
     contains
       procedure:: init=> combisummary_init
       procedure:: set_dup=>combisummary_set_dup
       procedure:: set_special=>combisummary_set_special
       procedure:: set_nkind=>combisummary_set_nkind
       procedure:: makeidx=>combisummary_makeidx
       procedure:: info_calc_freq=>combisummary_info_calc_freq
       procedure:: info_accept=> combination_info_accept
    end type t_combisummary

  contains

    subroutine combisummary_init(self,dryrun)
      implicit none
      class(t_combisummary):: self
      logical,optional:: dryrun
      if (present(dryrun)) self%dryrun=dryrun
    end subroutine combisummary_init

    logical function combination_info_accept(self)
      implicit none
      class(t_combisummary):: self
      integer:: nwycoff,i
      logical:: flag
      nwycoff=size(self%dup)

      flag=.true.
      do i=1,nwycoff
         ! specialpositionは頻度(frequency)が一度のみ
         if (self%special(i)) then 
            if ( self%freq(i)>1) then ; flag=.false.; exit; endif
            endif
         enddo
         combination_info_accept=flag
       end function combination_info_accept

       ! 頻度計算
       subroutine combisummary_info_calc_freq(self,info) 
         implicit none
         class(t_combisummary):: self
         integer::info(:)
         integer:: nwycoff,ikind,i,j,n,k
         integer,allocatable :: iv(:)
         nwycoff=size(self%dup)
         !  freqは頻度
         if (allocated(self%freq)) deallocate(self%freq)
         allocate(self%freq(nwycoff)); self%freq=0
         do ikind=1,self%nkind
            i=info(ikind)
            n=size( self%combi_eachspecie(ikind)%list%v(:,i) )
            do j=1,n 
               k=self%combi_eachspecie(ikind)%list%v(j,i) 
               if (k/=0) then 
                  ! 各wycoff座標が何回現れたか
                  self%freq(k)=self%freq(k)+1
               endif
            enddo
         enddo
       end subroutine combisummary_info_calc_freq

       subroutine combisummary_set_nkind(self,nkind)
         implicit none
         class(t_combisummary):: self
         integer:: nkind
         integer:: ikind
         self%nkind=nkind
         if (.not.allocated( self%combi_eachspecie ) ) then
           allocate( self%combi_eachspecie(nkind) )
           do ikind=1,nkind
              call self%combi_eachspecie(ikind)%init(1) ! dummy
           enddo
         endif
         call self%infolist%init(nkind,title="combisummary",dryrun=self%dryrun)
       end subroutine combisummary_set_nkind


       subroutine combisummary_set_dup(self,dup)
         implicit none
         class(t_combisummary):: self
         integer,intent(in)::dup(:)
         integer:: n
         n=size(dup)
         if (allocated(self%dup)) deallocate(self%dup)
         allocate(self%dup(n))
         self%dup=dup
       end subroutine combisummary_set_dup


       subroutine combisummary_set_special(self,special)
         implicit none
         class(t_combisummary):: self
         logical,intent(in)::special(:)
         integer:: n
         character(27)::thisfunc='combisummary_set_special: '
         n=size(special)
         if (allocated(self%special)) deallocate(self%special)
         allocate(self%special(n))
         self%special=special
         if (self%infolevel>100) then
            write(*,*)thisfunc,'combisummary_set_special'
            write(*,*)thisfunc, special
            write(*,*)thisfunc, self%special
         endif
       end subroutine combisummary_set_special


       ! levelは減る方向に動かしてloop
       recursive subroutine combisummary_makeidx(self,maxlevel,info,loopmax,level,chooseid,doit)
         implicit none
         class(t_combisummary):: self
         integer,intent(in):: maxlevel
         integer:: level
         integer,intent(in)::loopmax(maxlevel)
         integer,intent(in):: chooseid
         logical,intent(inout):: doit
         integer:: info(maxlevel)
         integer::idx,ilevel
         integer:: s,i,nwycoff,j,k,ikind,n
         logical:: flag
         character(23)::thisfunc='combisummary_makeidx: '

         if (.not. self%infolist%initialized() ) then 
            write(6,*)thisfunc,' initialized self%infolist first'
            stop 
         endif

         nwycoff=size(self%dup)

         if (level==0) then
            !---------------------------
!!! do something

            !頻度を数える
            call self%info_calc_freq(info)

            ! 条件に合致するか？
            flag=self%info_accept()

#if 0
            if (flag) then 
               write(*,'(a)',advance='no') 'accept '
            else
               write(*,'(a)',advance='no') 'reject '
            endif
#endif

            if (flag) then 
               ! infolistに追加
               call self%infolist%append(info)
               if (chooseid>0 .and. chooseid==self%infolist%last2) then
                  !write(*,*) 'choose', chooseid,'info',info
                  doit=.false.
                  return 
               endif
               if (self%infolevel>9) then
                  !  ここでは表示するだけ。
                  do ikind=1,self%nkind
                     i=info(ikind)
                     !k=0
                     !do j=1,size(self%combi_eachspecie(ikind)%list%v(:,i))
                     !   if ( self%combi_eachspecie(ikind)%list%v(j,i)==0) then
                     !      k=j-1; exit;
                     !   endif
                     !enddo
                     k=self%combi_eachspecie(ikind)%list%size1(i)
                     if (self%infolevel>10) then
                        write(6,'(a,a)',advance='no') thisfunc,'['
                        write(6,'(100i3)',advance='no') self%combi_eachspecie(ikind)%list%v(1:k,i)
                        write(6,'(a)',advance='no') ' ] '
                     endif
                  enddo
                  if (self%infolevel>10) write(6,*) thisfunc
               endif
            endif
            !---------------------------
            return
         endif

         !
         ! recursive calls
         !
         do idx=1,loopmax(level)
            info(level)=idx
            ! 逆方向にlevelを進める。
            call combisummary_makeidx(self,maxlevel,info,loopmax,level-1,chooseid,doit)
            if (.not.doit) return
         enddo

       end  subroutine combisummary_makeidx

     end module m_combisummary


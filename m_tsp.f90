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


#define ERROR_CODE 100
module m_tsp
  use m_wycoff
  implicit none
  character(4) ,parameter :: xyzch(-5:5)= &
       (/' -2x','-x+y',' -z ',' -y ',' -x ','  0 ',  '  x ','  y ','  z ',' x-y',' 2x '/)
  integer,parameter:: max_sitemultiplicity=30
  character(20),private,parameter:: filename_generator='GENERTOR', filename_wycoff='wycoff2015'
  character(20),private,parameter:: filename_out='spglist.dat'

  type t_tsp
     real(8):: eps=1.0d-5

#ifdef USE_GEN
     integer::file_out=0
#else
     integer::file_generator=3, file_wycoff=4,file_out=0
#endif
     logical:: done_openfiles=.false.

     integer::id=0
     logical:: done_setid=.false.

     integer::iorigin=0
     logical:: done_setorigin=.false.

     integer::norigin=0
     integer::iz=0 ! z factor
     logical:: done_readwycoff=.false.

     ! generator
     integer:: ngenerator=0
     character:: gn(2,3,48)*4
     integer:: ka(2,3,48)
     integer:: gen_rotmatrix(3,3,48)
     real(8):: gen_shift(3,48)

     character*5 schnam
     character*10 hmname

     integer:: nwycoff=0
#if 0
     integer:: shift(2,3,max_sitemultiplicity)
     integer:: xyz(3,max_sitemultiplicity) ! wycoff sites
     integer::sitemultiplicity(max_sitemultiplicity) , primitivesitemultiplicity(max_sitemultiplicity)
     logical,private:: specialpos(max_sitemultiplicity)
     character*1 :: sitecharacter(max_sitemultiplicity)
#endif

     type(t_wycoff):: wy(max_sitemultiplicity) 

!!!    integer xyzwm(3,48),jam(2,3,48)  ! wycoff equivalent sites

     real(8):: a_len,b_len,c_len,cosa,cosb,cosc
     integer:: ta_len,tb_len,tc_len,tcosa,tcosb,tcosc
     character(20):: latticetype=''

     real(8):: primitivevector(3,3)=0.0d0
     real(8):: conventionalvector(3,3)=0.0d0

   contains
     procedure:: init=> tsp_init
     procedure:: set_id=> tsp_set_id
     procedure:: set_origin=> tsp_set_origin
     procedure:: read_wycoff=> tsp_read_wycoff
     procedure:: read_wycoff_equiv => tsp_read_wycoff_equiv
     procedure:: write_wycoff=> tsp_write_wycoff
     procedure:: write_wycoff_equiv=> tsp_write_wycoff_equiv
     procedure:: finalize=> tsp_finalize
     procedure:: open_files=> tsp_openfiles
     procedure:: close_files=> tsp_closefiles
     procedure:: find_rot_samepos => tsp_find_rot_samepos
     procedure:: set_lattice => tsp_set_lattice
     procedure:: set_primitivevector => tsp_set_primitivevector
     procedure:: set_conventionalvector => tsp_set_conventionalvector
     procedure:: write_json => tsp_write_json
     procedure:: gettable => tsp_gettable

  end type t_tsp

!!! private subroutine
  private::  zfactor

contains

  subroutine tsp_gettable(tsp,id,og)
    implicit none
    class(t_tsp):: tsp
    integer:: id,og
    call tsp%init
    call tsp%set_id(id)
    call tsp%set_origin(og)
    call tsp%read_wycoff()
    call tsp%read_wycoff_equiv ()

  end subroutine tsp_gettable

  subroutine tsp_init(self)
    implicit none
    class(t_tsp):: self
    if (self%done_openfiles) call self%close_files()
    self%done_setid=.false.
    self%done_setorigin=.false.
  end subroutine tsp_init

  !-------------------------------------------------------
  subroutine tsp_openfiles(self)
    implicit  none
    class(t_tsp):: self
    integer:: iso
    character(14):: thisfunc='tsp_openfiles:'
#if USE_GEN
    call gen_init
    call genwy_init
#else
    open(self%file_generator,file=filename_generator,iostat=iso,action='read')
    if(iso.ne.0) then; write(*,*)'failed to open', trim(filename_generator); stop;
    endif
    open(self%file_wycoff,file=filename_wycoff,iostat=iso,status='old',action="read")
    if(iso.ne.0) then; write(*,*)'failed to open', trim(filename_wycoff); stop;
    endif
#endif
    if (self%file_out>0) then
       open(self%file_out,file=filename_out,iostat=iso,status='unknown',action="write")
       if(iso.ne.0) then; write(*,*)'failed to open', trim(filename_wycoff); stop;
       endif
    endif

    self%done_openfiles=.true.

  end subroutine tsp_openfiles

  !-------------------------------------------------------
  subroutine tsp_finalize(self)
    implicit  none
    class(t_tsp):: self
    if (self%done_openfiles) call self%close_files
  end subroutine tsp_finalize
  !-------------------------------------------------------
  subroutine tsp_closefiles(self)
    implicit  none
    class(t_tsp):: self
    integer:: iso
#if USE_GEN
#else
    close(self%file_generator)
    close(self%file_wycoff)
#endif
    if (self%file_out>0) then
       close(self%file_out)
    endif
    self%done_openfiles=.false.
  end subroutine tsp_closefiles
  !------------------------------------------------------

  subroutine tsp_set_id(self,nn)
    implicit none
    class(t_tsp):: self
    integer:: nn

    if (.not. self%done_openfiles) then 
       call self%open_files
    endif

    self%id=nn
    call tsntnm(nn,self%norigin,self%schnam,self%hmname)
    if (self%file_out>0) &
         write(self%file_out,'(a,1x,i4,i2,a,1x,a)') 'id',nn,self%norigin,self%schnam,self%hmname
    call zfactor(self%hmname,self%iz,self%latticetype)
    write(self%file_out,'(I3,1x,a)') self%iz,self%latticetype

    self%done_setid=.true.
    self%done_setorigin=.false.
    self%done_readwycoff=.false.

  end subroutine tsp_set_id
  !------------------------------------------------------

  subroutine tsp_set_origin(self,iorigin)
    implicit none
    integer:: iorigin
    class(t_tsp):: self
    if (.not.self%done_openfiles) then
       call self%open_files
    endif
    if (.not.self%done_setid) then 
       write(6,*)'tsp_set_origin: call self%set_id first'; stop
    endif

    self%iorigin=iorigin
    if (self%file_out>0) &
         write(self%file_out,'(a,1x,I3)')'origin',self%iorigin

    self%done_setorigin=.true.
    self%done_readwycoff=.false.
  end subroutine tsp_set_origin
  !------------------------------------------------------

  subroutine tsp_read_wycoff(self)
    use m_gn_ka, only:  gn_ka_nmax
    implicit none
    class(t_tsp):: self
#if 0
    real(8):: A,B,C,CA,CB,CC,A1,B1,C1
    COMMON/LAT/A,B,C,CA,CB,CC,A1,B1,C1
#endif
    integer:: shift(2,3,max_sitemultiplicity)
    integer:: xyz(3,max_sitemultiplicity) ! wycoff sites
    integer::sitemultiplicity(max_sitemultiplicity) , primitivesitemultiplicity(max_sitemultiplicity)
    logical:: specialpos(max_sitemultiplicity)
    character*1 :: sitecharacter(max_sitemultiplicity)

    character :: gn(2,3)*4
    integer:: ka(2,3)
    integer:: i,j,iwp
    character(len('tsp_read_wycoff')):: thisfunc='tsp_read_wycoff'

    if (.not. self%done_openfiles) call self%open_files
    if (.not. self%done_setid) then
       write(6,*)thisfunc,'internal error, call self%set_id first'; stop
    endif
    if (.not.self%done_setorigin) then 
       write(6,*)thisfunc,'internal error, call self%set_origin first'; stop
    endif

    if (self%iorigin>self%norigin .or. self%iorigin<1) then
       write(6,*)thisfunc,'origin out of range'
       write(6,*)thisfunc,'origin=',self%iorigin
       stop ERROR_CODE
    endif

    call gn_ka_init()

    !  gn_kaの代入がtspngeでなされる。

    call tspnge(self%id,self%iorigin)

    !    write(6,*)
    ! generator 
    self%ngenerator=gn_ka_nmax()
    !    write(6,*)'generator=',self%ngenerator
    do i=1,self%ngenerator
       call gn_ka_get(i,gn,ka)  ! gn kaはtspngeで代入された。
       self%gn(:,:,i)=gn
       self%ka(:,:,i)=ka
       self%gen_rotmatrix(:,:,i)=transpose(generator_make_rotmatrix(gn)) ! matmulで計算するためにtranspose  
       do j=1,3
          self%gen_shift(j,i)=real(self%ka(1,j,i))/real(self%ka(2,j,i))
       enddo
       ! x, x':  fractional coordinates
       ! transformation
       !     x'=  rotamtrix x + shift 

       !       write(6,*)'id=',i
       !       write(6,*)'rotation'
       !       write(6,'(1H[,1x,3I4,1x,1H])') self%gen_rotmatrix(:,:,i)
       !       write(6,*)'translation'
       !       write(6,'(1H[,1x,3F10.5,1x,1H])') self%gen_shift(:,i)

    enddo
    !    write(6,*)


    ! wycoff positionをファイルから読み込む
    call tswyrd(self%id,self%iorigin,self%nwycoff,&
         sitemultiplicity,sitecharacter,xyz,shift)
    write(6,*)thisfunc,': tswyrd done'
    primitivesitemultiplicity=sitemultiplicity/self%iz

    do iwp=1,self%nwycoff
       call self%wy(iwp)%set_param( sitemultiplicity(iwp), primitivesitemultiplicity(iwp),&
            sitecharacter(iwp), xyz(:,iwp),shift(:,:,iwp))
       call self%wy(iwp)%exam_property()
    enddo

    ! wycoff positionの属性
!!!    call exam_wycoff(self%nwycoff,self%wy)

    ! multiplicityはconventional cellでの値なので, primitive cellの値に治す。

    self%done_readwycoff=.true.

  end subroutine tsp_read_wycoff

  subroutine tsp_read_wycoff_equiv(self)
    implicit none
    class(t_tsp):: self
    integer::  iwp, nsite_in_iwp
    integer::sitemultiplicity(max_sitemultiplicity) , primitivesitemultiplicity(max_sitemultiplicity)
    character*1 :: sitecharacter(max_sitemultiplicity)
    integer:: xyz(3,max_sitemultiplicity) ! wycoff sites
    integer:: shift(2,3,max_sitemultiplicity)
    integer:: xyzwm(3,48),jam(2,3,48)


    sitemultiplicity=self%wy(:)%sitemultiplicity
    sitecharacter=self%wy(:)%sitecharacter
    do iwp=1,self%nwycoff
       xyz(:,iwp)=self%wy(iwp)%xyz(:)
       shift(:,:,iwp)=self%wy(iwp)%shift(:,:)
    enddo
!!!    write(6,*)'tsp_read_wycoff_equiv: nwycoff=',self%nwycoff
    do iwp=1,self%nwycoff
       call tswycf(self%wy(iwp)%sitecharacter, &
            self%nwycoff,sitemultiplicity,sitecharacter,&
            xyz,shift , &
            nsite_in_iwp,xyzwm,jam)
       self%wy(iwp)%nequivsite=nsite_in_iwp
       self%wy(iwp)%xyzwm=xyzwm
       self%wy(iwp)%jam = jam
#if 0
       write(6,*)'tsp_read_wycoff_equiv:',iwp
       write(6,*) nsite_in_iwp
       write(6,*) self%xyzwm
       write(6,*) self%jam
#endif 
    enddo
  end subroutine tsp_read_wycoff_equiv

#if 0
  subroutine exam_wycoff(nwycoff,wy)
    implicit none
    integer,intent(in):: nwycoff
    type(t_wycoff):: wy(nwycoff)

    integer:: iwy

    do iwy=1,nwycoff
       call wy(iwy)%exam_property()
    enddo

  end subroutine exam_wycoff
#endif

  !------------------------------------------------------
  subroutine tsp_set_lattice(self,work)
    implicit none
    class(t_tsp):: self 
    integer:: work
    real(8):: A,B,C,CA,CB,CC,A1,B1,C1
    COMMON/LAT/A,B,C,CA,CB,CC,A1,B1,C1

    character(16):: thisfunc='tsp_set_lattice:'

    if ( .not. self%done_readwycoff ) then 
       write(6,*)thisfunc,'call self%read_wycoff first'; stop
    endif

    if (work==0) then 

       self%a_len=4.1; self%b_len=4.2; self%c_len=4.3
       self%cosa=.41; self%cosb=.42; self%cosc=.43;
       ! tspaceを呼ぶ
       call tslatc(self%a_len,self%b_len,self%c_len, self%cosa,self%cosb,self%cosc)
       ! uniqなlatticeがどれか調べる。 output: self%t* 
       call exam_uniq_lattice(&
            self%a_len,self%b_len,self%c_len, self%cosa,self%cosb,self%cosc,&
            A,B,C,CA,CB,CC,&
            self%ta_len,self%tb_len,self%tc_len,self%tcosa,self%tcosb,self%tcosc)

       write(self%file_out,'(a,9F10.5)') 'lat0',self%a_len,self%b_len,self%c_len, self%cosa,self%cosb,self%cosc
       write(self%file_out,'(a,9F10.5)') 'lat', A,B,C,CA,CB,CC
       self%a_len=a; self%b_len=b; self%c_len=c; self%cosa=ca; self%cosb=cb; self%cosc=cc 
       write(self%file_out,'(a,9I3)') 'tlat', self%ta_len,self%tb_len,self%tc_len, self%tcosa,self%tcosb,self%tcosc
       write(6,'(a,9I3)') thisfunc//'set tlat', self%ta_len,self%tb_len,self%tc_len, self%tcosa,self%tcosb,self%tcosc
       write(6,'(a,9F10.5)') thisfunc//'new lat',self%a_len,self%b_len,self%c_len, self%cosa,self%cosb,self%cosc


       call self%write_json()

    endif

    if (work==1) then 

       call tslatc(self%a_len,self%b_len,self%c_len, self%cosa,self%cosb,self%cosc)
       write(6,*)thisfunc,'after tslatc', self%a_len,self%b_len,self%c_len, self%cosa,self%cosb,self%cosc
       call exam_uniq_lattice(&
            self%a_len,self%b_len,self%c_len, self%cosa,self%cosb,self%cosc,&
            A,B,C,CA,CB,CC,&
            self%ta_len,self%tb_len,self%tc_len,self%tcosa,self%tcosb,self%tcosc)
       self%a_len=a; self%b_len=b; self%c_len=c; self%cosa=ca; self%cosb=cb; self%cosc=cc
       write(6,'(a,9I3)')thisfunc// 'set tlat', self%ta_len,self%tb_len,self%tc_len, self%tcosa,self%tcosb,self%tcosc
       write(6,'(a,9F10.5)') thisfunc//'new lat',self%a_len,self%b_len,self%c_len, self%cosa,self%cosb,self%cosc

       ! conventional vectorの計算
       call self%set_conventionalvector
       ! primitive vectorの計算
       call self%set_primitivevector

       write(6,*)thisfunc,'conventional vector'
       write(6,'(a)',advance='no') thisfunc
       write(6,'(1H[,3F10.5,1x,1H])') self%conventionalvector
       write(6,*)thisfunc,'primitive vector'
       write(6,'(a)',advance='no') thisfunc
       write(6,'(1H[,3F10.5,1x,1H])') self%primitivevector

    endif

  end subroutine tsp_set_lattice

  !------------------------------------------------------
  ! self%convenionalvectorからangleの計算
  subroutine tsp_calc_angle(self,ca,cb,cc)
    implicit none
    type(t_tsp):: self
    real(8),intent(out):: ca,cb,cc

    real(8):: v1,v2,v3
    integer:: i1,i2
    i1=1; i2=2
    v1= sqrt(sum(self%conventionalvector(:,i1)**2))
    v2= sqrt(sum(self%conventionalvector(:,i2)**2))
    v3= sum(self%conventionalvector(:,i1)*self%conventionalvector(:,i2))
    cc=v3/(v1*v2)

    i1=2; i2=3
    v1= sqrt(sum(self%conventionalvector(:,i1)**2))
    v2= sqrt(sum(self%conventionalvector(:,i2)**2))
    v3= sum(self%conventionalvector(:,i1)*self%conventionalvector(:,i2))
    ca=v3/(v1*v2)

    i1=3; i2=1
    v1= sqrt(sum(self%conventionalvector(:,i1)**2))
    v2= sqrt(sum(self%conventionalvector(:,i2)**2))
    v3= sum(self%conventionalvector(:,i1)*self%conventionalvector(:,i2))
    cb=v3/(v1*v2)

  end subroutine tsp_calc_angle
  !------------------------------------------------------


  ! conventional vectorの計算
  subroutine tsp_set_conventionalvector(self)
    implicit none
    class(t_tsp):: self
    real(8):: sinc
    sinc=sqrt(1.0d0-self%cosc**2)
    ! conventional vector
    self%conventionalvector(:,1)=self%a_len* [ 1.0d0, 0.0d0, 0.0d0]
    self%conventionalvector(:,2)=self%b_len* [ self%cosc, sinc, 0.0d0]
    self%conventionalvector(:,3)=self%c_len* [ self%cosb, (self%cosa-self%cosb*self%cosc)/ sinc ,&
         dsqrt(1.d0-self%cosa*self%cosa-self%cosb*self%cosb-self%cosc*self%cosc+2.d0*self%cosa*self%cosb*self%cosc)/sinc ]


  end subroutine tsp_set_conventionalvector

  !------------------------------------------------------
  ! conventional vectorが計算されているとしてprimitive vectorの計算。
  subroutine tsp_set_primitivevector(self)
    implicit none
    class(t_tsp):: self
    real(8):: tt , ot
    integer:: i,j
    real(8):: a,eps=1.0d-8

    ! conventional vectorがあまりに小さい時はconventional vectorがsetされていないと考えて中止。
    j=0
    do i=1,3
       a = sqrt( sum(self%conventionalvector(:,i)**2 ) )
       if (a<eps) j=j+1 
    enddo
    if (j>1) then 
       write(6,*)'tsp_set_primitivevector: set conventionalveoctor first.'; stop 
    endif

    tt=2.0d0/3.0d0
    ot=1.0d0/3.0d0

    ! primitive vector
    select case(self%latticetype) 
    case('simple');   self%primitivevector= self%conventionalvector
    case('face_centered')
       self%primitivevector(:,1)= 0.5d0*self%conventionalvector(:,1)+0.5d0*self%conventionalvector(:,2)
       self%primitivevector(:,2)= 0.5d0*self%conventionalvector(:,1)+0.5d0*self%conventionalvector(:,3)
       self%primitivevector(:,3)= 0.5d0*self%conventionalvector(:,2)+0.5d0*self%conventionalvector(:,3)
    case('body_centered')
       self%primitivevector(:,1)=&
            -0.5d0*self%conventionalvector(:,1)+0.5d0*self%conventionalvector(:,2)+0.5d0*self%conventionalvector(:,3)
       self%primitivevector(:,2)=&
            0.5d0*self%conventionalvector(:,1)-0.5d0*self%conventionalvector(:,2)+0.5d0*self%conventionalvector(:,3)
       self%primitivevector(:,3)=&
            0.5d0*self%conventionalvector(:,1)+0.5d0*self%conventionalvector(:,2)-0.5d0*self%conventionalvector(:,3)
    case('c-centered')
       self%primitivevector(:,1)=-0.5d0*self%conventionalvector(:,1)+0.5d0*self%conventionalvector(:,2)
       self%primitivevector(:,2)= 0.5d0*self%conventionalvector(:,1)-0.5d0*self%conventionalvector(:,3)
       self%primitivevector(:,3)= self%conventionalvector(:,3)
    case('trigonal')
       self%primitivevector(:,1)= tt*self%conventionalvector(:,1)+ot*self%conventionalvector(:,2)+ot*self%conventionalvector(:,3)
       self%primitivevector(:,2)=-ot*self%conventionalvector(:,1)+ot*self%conventionalvector(:,2)+ot*self%conventionalvector(:,3)
       self%primitivevector(:,3)=-ot*self%conventionalvector(:,1)-tt*self%conventionalvector(:,2)+ot*self%conventionalvector(:,3)
    case default; write(6,*)'tsp_set_latticevectors: error, unknown latticetype',self%latticetype; stop
    end select

  end subroutine tsp_set_primitivevector


  subroutine tsp_write_json(self)
   implicit none
   class(t_tsp):: self
   integer::fid 
   character:: bc_def(0:1)=['0','a']
   character(2):: ang_def(0:11)

   character(10)::filename='LAT.json'

   fid=100
   open(fid,file=filename,status='unknown')
   ang_def= '0 '
   ang_def(10)='90'
   ang_def(11)='60'
   write(fid,*)'{'
   write(fid,*)'"spacegroupid": ',self%id ,','
   write(fid,*)'"Z":',self%iz,","
   write(fid,*)'"originchoide":',self%iorigin,","
   write(fid,*)'"a":"0",'
   write(fid,*)'"b":"', bc_def(self%tb_len),'",'
   write(fid,*)'"c":"', bc_def(self%tc_len),'",'
   write(fid,*)'"alpha": ',ang_def(self%tcosa),","
   write(fid,*)'"beta" : ',ang_def(self%tcosb),","
   write(fid,*)'"gamma": ',ang_def(self%tcosc)
   write(fid,*)'}'
   close(fid)
  end subroutine tsp_write_json

  !------------------------------------------------------
  ! setした値、A0,B0,...
  ! とtspaceの返り値、 A,B,...を比較して uniqな格子パラメタを求める。
  ! tA= .true. uniq, =false , not uniq
  subroutine exam_uniq_lattice( &
       A0,B0,C0,CA0,CB0,CC0,&
       A,B,C,CA,CB,CC,&
       tA,tB,tC,tCA,tCB,tCC) 
    implicit none
    real(8),intent(in)::  A0,B0,C0,CA0,CB0,CC0,&
         A,B,C,CA,CB,CC
    integer,intent(out):: tA,tB,tC,tCA,tCB,tCC

    real(8),parameter::zero=0.0d0,half=-0.5d0
    character(len('exam_uniq_lattice')):: thisfunc='exam_uniq_lattice'
    logical:: flag

    flag=.false.
    tA=0

    if ( B0==B ) then 
       tB=0
    else
       if (A==B) then
          tB=1
       else
          write(*,*)thisfunc,' error in tB'
          flag=.true.
       endif
    endif

    if (C0==C) then
       tC=0
    else
       if (A==C) then
          tC=1
       else
          write(*,*)thisfunc,' error in tC'
          flag=.true.
       endif
    endif

    if (CA==zero) then 
       tCA=10
    else if (CA==half) then 
       tCA=11
    else
       if (CA==CA0) then 
          tCA=0
       else
          write(*,*)thisfunc,' error in tca'
          flag=.true.
       endif
    endif

    if (CB==zero) then 
       tCB=10
    else if (CB==half) then 
       tCB=11
    else
       if (CB==CB0) then 
          tCB=0
       else
          write(*,*)thisfunc,' error in tcb'
          flag=.true.
       endif
    endif

    if (CC==zero) then 
       tCC=10
    else if (CC==half) then 
       tCC=11
    else
       if (CC==CC0) then 
          tCC=0
       else
          write(*,*)thisfunc,' error in tcc',CC==half
          flag=.true.
       endif
    endif

    if (flag) stop 

  end subroutine exam_uniq_lattice

  !------------------------------------------------------

  ! wychoff 座標を書きだず。
  ! special positionかどうかも書き出す。
  subroutine tsp_write_wycoff(self)
    implicit none
    class(t_tsp)::self
    integer:: iwp,j,k
    character(18)::thisfunc='tsp_write_wycoff:'
    if (self%file_out>0) &
         write(self%file_out,'(a,1x,I3)') 'nwycoff',self%nwycoff
    write(6,*)thisfunc,'wycoff representative positions'
    do  iwp=1,self%nwycoff
       if (self%file_out>0) &
            write(self%file_out,'(I3,L2,I4,I3,A2)') iwp,self%wy(iwp)%specialpos,&
            self%wy(iwp)%primitivesitemultiplicity, &
            self%wy(iwp)%sitemultiplicity,self%wy(iwp)%sitecharacter

       write(6,600) thisfunc//'W',iwp,self%wy(iwp)%specialpos,&
            self%wy(iwp)%primitivesitemultiplicity, &
            self%wy(iwp)%sitemultiplicity,self%wy(iwp)%sitecharacter &
            ,(xyzch(self%wy(iwp)%xyz(k)),(self%wy(iwp)%shift(j,k),j=1,2),k=1,3)

    enddo
600 format(a,i5,L2,2i5,a1,1x,3(a4,'+',i2,'/',i1))

#if 0
    write(6,*)thisfunc,'sitemultiplicity and special positions'
    write(6,'(a)',advance='no') '['
    write(6,'(100i3)',advance='no') (self%primitivesitemultiplicity(iwp),iwp=1,self%nwycoff)
    write(6,'(a)') ' ]'
    write(6,'(a)',advance='no')thisfunc//'['
    write(6,'(100L3)',advance='no') (self%specialpos(iwp),iwp=1,self%nwycoff)
    write(6,'(a)',advance='no') ' ]'
    write(6,*)
#endif
  end subroutine tsp_write_wycoff

  !------------------------------------------------------

  ! 各wychoff positionで、等価な位置を書き出す。
  subroutine tsp_write_wycoff_equiv(self,iwp)
    implicit none
    class(t_tsp)::self
    integer:: iwp
    type(t_wycoff)::wy
    real(8):: wy_realpos(3)

    integer:: nsite_in_iwp,isit,j,k,rotlist(48),nrot
    integer xyzwm(3,48),jam(2,3,48)
    character(23)::thisfunc='tsp_write_wycoff_equiv:'

    if (self%wy(iwp)%specialpos) return

    if (iwp<1 .or. iwp>self%nwycoff) then 
       write(6,*)thisfunc,'iwp range error'
       stop ERROR_CODE
    endif

#if 1
    nsite_in_iwp = self%wy(iwp)%nequivsite
    wy=self%wy(iwp)
    xyzwm = wy%xyzwm
    jam = wy%jam 
!!! write(6,*)'nsite_in_iwp=', self%wy(:)%nequivsite
#else
!!! calculate equivalent sites
    call tswycf(self%sitecharacter(iwp), &
         self%nwycoff,self%sitemultiplicity,self%sitecharacter,&
         self%xyz,self%shift , &
         nsite_in_iwp,self%xyzwm,self%jam)
#endif


    write(6,*)thisfunc,'wycoff equivalent positions iwp=',iwp
    do isit=1,nsite_in_iwp
       if (self%file_out>0) &
            write(self%file_out,'(99I4)') isit,(xyzwm(k,isit), (jam(j,k,isit),j=1,2),k=1,3)
    enddo
601 format(a,1x,i3,1x,3(a4,'+',i2,'/',i2,3x))
    do isit=1,nsite_in_iwp
       write(6,601) thisfunc//'Q',isit,(xyzch(xyzwm(k,isit)), (jam(j,k,isit),j=1,2),k=1,3)
    enddo
    if (nsite_in_iwp*self%iz/=self%wy(iwp)%sitemultiplicity) then
       write(6,*)thisfunc,'internal error, nsite,iz/=sitemultiplicity'
       write(6,*) thisfunc,'iz=',self%iz
       write(6,*)thisfunc,'nsite=',nsite_in_iwp
       write(6,*)thisfunc,'sitemulltiplicity=',self%wy(iwp)%sitemultiplicity
       stop ERROR_CODE
    endif

    isit=1
    write(6,'(a,i3,1x,a,3I3,1x,a,3L3,1x,a,3L3)')thisfunc//'iwp=',iwp,'pos=',xyzwm(:,isit),'uniq?=',wy%uniq,'fix?=',wy%fix
    if (wy%uniq(1)) call wy%set_x(0.21d0)
    if (wy%uniq(2)) call wy%set_y(0.32d0)
    if (wy%uniq(3)) call wy%set_z(0.33d0)
    do isit=1,nsite_in_iwp
       wy_realpos = wy%calc_xyz(self%wy(iwp)%xyzwm(:,isit),self%wy(iwp)%jam(:,:,isit))
       call self%find_rot_samepos(wy_realpos,nrot,rotlist)
       write(*,'(a,1x,i3,1x,a,3F10.5,1x,a,1x,100I3)')thisfunc//'for', isit,'site xyz=',wy_realpos,'rot=',rotlist(1:nrot)
    enddo

  end subroutine tsp_write_wycoff_equiv
  !------------------------------------------------------
  ! [0:1)の範囲にする。
  function pos_fract(p0)
    implicit none
    real(8),intent(in):: p0(3)
    real(8):: pos_fract(3)
    real(8):: p
    integer:: i,j
    do i=1,3
       pos_fract(i) = mod(p0(i)-int(p0(i))+1.0d0, 1.0d0)
    enddo
  end function pos_fract
  !------------------------------------------------------
  ! 同じ位置になる変換を求める。
  subroutine tsp_find_rot_samepos(self,pos0,nrot,rotlist)
    implicit none
    class(t_tsp)::self
    real(8),intent(in):: pos0(3)
    integer,intent(out)::nrot
    integer,intent(out):: rotlist(48)

    integer:: ig
    real(8)::  rotpos(3),diff(3),pos(3)
    nrot=0
    pos=pos_fract(pos0)
    !    write(6,*)'# of generators=',self%ngenerator
    do ig=1,self%ngenerator
       rotpos= matmul(self%gen_rotmatrix(:,:,ig),pos)+self%gen_shift(:,ig)
       rotpos=pos_fract(rotpos)
       call frac_diff_min(pos,rotpos,diff)
       !      write(6,'(i3,3F10.5,1x,a,1x,3F10.5,F10.5)') ig,pos,'->',rotpos, sum(diff**2)
       if ( sum(diff**2) < self%eps**2 ) then 
          nrot=nrot+1
          rotlist(nrot)=ig
       endif
    enddo

  end subroutine tsp_find_rot_samepos

  ! 最も差が小さいfractional coordinateにしてdiffを返す。
  ! output: p1, diff
  subroutine frac_diff_min(p,p1,diff)
    implicit none
    real(8),intent(in):: p(3)
    real(8),intent(out)::p1(3)
    integer:: i1,i2,i3,ilist(3)
    real(8):: diffmin(3),diff(3),difflen

    diff=p-p1
    difflen =  sum(diff**2)
    ilist=[0,0,0]
    do i3=-1,1
       do i2=-1,1
          do i1=-1,1
             diff = p-p1-[ i1,i2,i3] 
             if (difflen > sum(diff**2) ) then 
                difflen= sum(diff**2) 
                ilist= [ i1,i2,i3]
             endif
          enddo
       enddo
    enddo

    i1=ilist(1); i2=ilist(2); i3=ilist(3)
    p1= p1+ [ real(i1,kind=8),real(i2,kind=8),real(i3,kind=8) ]
    diff= p-p1

  end subroutine frac_diff_min
  !------------------------------------------------------

#if 0
  subroutine zfactor(hmname,iz,latticetype)
    implicit none
    character(*):: hmname
    integer,intent(out)::iz
    character(*),intent(out):: latticetype

    integer:: il,NG,IG,JV
    COMMON/SPG2/IL,NG,IG(48),JV(2,3,48)                                  

    write(6,*)'IL=',il
    select case(il)
    case(-1); latticetype= 'TRIGONAL LATTICE'; iz=3
    case(0) ; latticetype= 'HEXAGONAL LATTICE'; iz=1
    case(1) ; latticetype= 'SIMPLE LATTICE'; iz=1
    case(2) ; latticetype= 'FACE CENTERED LATTICE'; iz=4
    case(3) ; latticetype= 'BODY CENTERED LATTICE'; iz=2
    case(4) ; latticetype= 'C- CENTERED LATTICE'; iz=2
    case default; write(6,*)'zfactor: unknown structure',hmname,' il=',il; stop "abort"
    end select

  end subroutine zfactor
#else
  subroutine zfactor(hmname,iz,latticetype)
    character(*):: hmname
    integer,intent(out)::iz
    character(*):: latticetype
    character(8)::thisfunc='zfactor:'
    select case (hmname(1:1))
    case ('P'); iz=1;write(6,*)thisfunc,'simple Z=', iz; latticetype="simple"
    case ('F'); iz=4;write(6,*)thisfunc,'face centered Z=',iz; latticetype="face_centered"
    case ('I'); iz=2;write(6,*)thisfunc,'body centered Z=',iz; latticetype="body_centered"
    case ('C'); iz=2;write(6,*)thisfunc,'C- centered Z=',iz; latticetype="c-centered"
    case ('A'); iz=2;write(6,*)thisfunc,'C- centered Z=',iz; latticetype="c-centered"
    case ('R'); iz=3;write(6,*)thisfunc,'trigonal Z=',iz; latticetype="trigonal"
    case default;    write(6,*)thisfunc,'zfactor: unknown structure',hmname; stop "abort"
    end select
  end subroutine zfactor

#endif
  !------------------------------------------------------

#if 0
  ! special positionかどうかの判定
  subroutine exam_specialpos(nwycoff, xyz,specialpos)
    implicit none
    integer,intent(in):: nwycoff
    integer,intent(in):: xyz(3,nwycoff)
    logical,intent(out):: specialpos(nwycoff)
    integer:: i
    specialpos=.false.
    do i=1,nwycoff
       if (xyz(1,i)==0 .and. xyz(2,i)==0  .and. xyz(3,i)==0) then
          specialpos(i)=.true.
       endif
    enddo
  end subroutine exam_specialpos
#endif 
  !------------------------------------------------------
  ! for interface definition
  function generator_make_rotmatrix(gn)
    implicit none
    character,intent(in) :: gn(2,3)*4
    integer:: generator_make_rotmatrix(3,3) 
    integer:: ig,i,j
    integer:: matrix(3,3)=0
    character(25)::thisfunc='generator_make_rotmatrix:'
    do i=1,3
       select case(gn(2,i))
       case('X'); matrix(:,i)= [1,0,0] ![ 1.0d0, 0.0d0, 0.0d0 ]
       case('Y'); matrix(:,i)= [0,1,0] ![ 0.0d0, 1.0d0, 0.0d0 ]
       case('Z'); matrix(:,i)= [0,0,1] ![ 0.0d0, 0.0d0, 1.0d0 ]
       case('W'); matrix(:,i)= [1,-1,0] ![ 1.0d0, -1.0d0, 0.0d0 ]
       case default; write(6,*)thisfunc,'generator_makematrix, error, gn2=',gn(2,i),' i=',i;stop
       end select
       if (gn(1,i)=='-') then
          matrix(:,i)= -matrix(:,i)
       else if (len_trim(gn(1,i))==0) then
          continue
       else
          write(6,*)thisfunc,'generator_makematrix, error, gn1=',gn(1,i),' i=',i
          stop ERROR_CODE
       endif
    enddo
    generator_make_rotmatrix=matrix
    !do i=1,3
    !   write(6,'(a,i2,1x,3I4)')'M',i, matrix(:,i)
    !enddo
  end function generator_make_rotmatrix

end module m_tsp





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



program test
  use iso_fortran_env, only : error_unit
  use m_fixedparam 
  implicit none
  real(8):: pi=2.0d0*asin(1.0d0)
  type t_param
     integer:: nspecies
     integer,allocatable:: num_atoms(:)
     character(len_specie_name),allocatable:: specie_atoms(:)
     integer:: spgid, spgog
     real(8):: a,b,c,ca,cb,cc
     logical:: selectone
     integer:: randomseed,rand_iter_max
     logical:: write_detail
     logical:: latinfo_only
  end type t_param
  call main
  stop 0

contains
  subroutine input_get(p)
    use m_util
    use m_keyvalue

    implicit none
    type(t_param):: p
    integer:: success,i,ios

    character(10):: str
    character(50):: filename='input'
    integer:: narg, COMMAND_ARGUMENT_COUNT
    character(11)::thisfunc='input_get: '

    narg= COMMAND_ARGUMENT_COUNT()
    if (narg>0) then
       call GET_COMMAND_ARGUMENT(1,filename)
    endif
!!! defaultで"input"を読み込む。

    call keyvalue%set_filename(filename)
    call keyvalue%get('nspecies',p%nspecies,success)
    allocate( p%num_atoms(p%nspecies), p%specie_atoms(p%nspecies) )
    call keyvalue%get( 'species_name', p%specie_atoms , status=success)
    call keyvalue%get( 'species_num', p%num_atoms , status=success)

    call keyvalue%get('spacegroup',p%spgid,success)
    call keyvalue%get_i('originchoice',p%spgog,default=1,status=success)
    call keyvalue%get('latinfo_only',p%latinfo_only,default=.false.)

    call keyvalue%get("a",p%a,default=0.0d0)
    call keyvalue%get("b",p%b,default=0.0d0)
    call keyvalue%get("c",p%c,default=0.0d0)
    call keyvalue%get("cosa",p%ca,default=0.0d0)
    call keyvalue%get("cosb",p%cb,default=0.0d0)
    call keyvalue%get("cosc",p%cc,default=0.0d0)

    call keyvalue%get("selectone",p%selectone,default=.true.)
    if (p%latinfo_only) p%selectone=.false.
    call keyvalue%get("rand_iter_max",p%rand_iter_max,default=1000)
    call keyvalue%get("randomseed",str)
    if (str=="auto") then 
       call util_rand_set(0,auto=.True.)
    else
       call keyvalue%get("randomseed",p%randomseed,default=123456789)
    endif
    call keyvalue%get("write_detail",p%write_detail,default=.true.)

    write(6,*)thisfunc,'nspecies=',p%nspecies
    write(6,*)thisfunc,'species_name=',p%specie_atoms
    write(6,*)thisfunc,'species_num=',p%num_atoms
    write(6,*)thisfunc,'spacegroup=',p%spgid
    write(6,*)thisfunc,'originhoice=',p%spgog

    if (p%a/=0.0d0 .and. p%b/=0.0d0 .and. p%c/=0.0d0) then
       write(6,*)thisfunc,'a,b,c=',p%a,p%b,p%c
       write(6,*)thisfunc,'ca,cb,cc=',p%ca,p%cb,p%cc
    endif

    write(*,*)thisfunc,'selectone',p%selectone,p%randomseed
    write(*,*)thisfunc,'write_detail',p%write_detail


    if (p%selectone) then 
       if (p%a==0.0d0 .or. p%b==0.0d0 .or. p%c==0.0d0) then 
          write(*,*)thisfunc,'Error in the input file'
          write(*,*)thisfunc, 'supply a,b,c when selectone=True'
          stop 100 
       endif
    endif

  end subroutine input_get


  subroutine main
    use m_util
    use m_tsp
    use m_select
    use m_vector_c2
    use m_vector_r2
    use m_vector_t_xyz1
    use m_xyz1
    use m_json_write
    implicit none
    type(t_param):: prm
    type(t_tsp):: tsp

    !    integer:: spgid,iorigin,iequiv
    !    integer:: natom
    integer:: iwp

    integer:: nspecies
    !    integer:: nspecies,num_uniqvar,uniqvartotal
    integer,allocatable:: num_atoms(:)
    character(len_specie_name),allocatable:: specie_atoms(:)
    !    character:: wychar

    logical:: specialpos(max_sitemultiplicity)
    integer:: primitivesitemultiplicity(max_sitemultiplicity)


    type(t_select):: select

    !    integer:: ilist,ispecie,i,j,k,isit,l,nsite_in_iwp,ip
    !    integer xyzwm(3,48),jam(2,3,48)

    !    integer:: choicenumber
    !    real(8) ::xyz(3),frac1(3),frac2(3)

    !    type(t_vector_c2):: atoms_name
    !    type(t_vector_r2):: atoms_frac
    !    type(t_xyz1),allocatable:: atoms(:)

    !    type(t_vector_t_xyz1):: atoms_w_wy
    !    type(t_xyz1):: aatom

    integer::fid_in,fid_out
    integer:: choicenumber
    character(10):: doit

    character(6)::thisfunc='main: '


    call input_get(prm)


    !    call util_rand_set(prm%randomseed)

    nspecies=prm%nspecies
    allocate( num_atoms(nspecies),specie_atoms(nspecies) )
    num_atoms=prm%num_atoms
    specie_atoms=prm%specie_atoms

    call tsp%gettable(prm%spgid, prm%spgog )
    ! read wycoff position from the file
    ! call tsp%read_wycoff()
    ! call tsp%read_wycoff_equiv()
    call tsp%set_lattice(0)

    if (prm%a/=0.0d0)then
       tsp%a_len=prm%a
       tsp%b_len=prm%b
       tsp%c_len=prm%c
       tsp%cosa=prm%ca
       tsp%cosb=prm%cb
       tsp%cosc=prm%cc
       call tsp%set_lattice(work=1)
    endif

    ! write wycoff position
    call tsp%write_wycoff

    write(6,'(a,a)',advance='no')thisfunc,'sitemultiplicity '
    write(6,'(a)',advance='no') '['
    write(6,'(100i3)',advance='no') (tsp%wy(iwp)%primitivesitemultiplicity,iwp=1,tsp%nwycoff)
    write(6,'(a)') ' ]'
    write(6,'(a,a)',advance='no')thisfunc,'special positions'
    write(6,'(a)',advance='no') '['
    write(6,'(100L3)',advance='no') (tsp%wy(iwp)%specialpos,iwp=1,tsp%nwycoff)
    write(6,'(a)') ' ]'
    write(6,*) thisfunc

    call tsp%finalize()

    if (prm%latinfo_only) stop 0 

    specialpos(:) = tsp%wy(:)%specialpos
    primitivesitemultiplicity(:)= tsp%wy(:)%primitivesitemultiplicity

    if (.not. prm%write_detail) select%infolevel=0

#if 1

    call select%make_combinations4(tsp%nwycoff,primitivesitemultiplicity,&
            specialpos,nspecies,num_atoms,prm%rand_iter_max)
    call write_a_position(tsp,select,specie_atoms,nspecies,&
            select%info, write_wy_skel=10, write_wy=20 , write_wy_json=30, write_wy_skel_json=40, &
           write_wy_skel_all_json=50, &
           write_detail=prm%write_detail)

#else
    doit="all"
    if (prm%selectone) then 

       call select%make_combinations(doit,tsp%nwycoff,primitivesitemultiplicity,&
            specialpos,nspecies,num_atoms,0,dryrun=.true.)

    else

       call select%make_combinations(doit,tsp%nwycoff,primitivesitemultiplicity,&
            specialpos,nspecies,num_atoms,0)

    endif
    write(6,*)thisfunc,'numoflist =', select%combisum%infolist%last2


    doit="combinations"
    if (prm%selectone) then
       choicenumber=util_rand_get_i(select%combisum%infolist%last2)
       write(6,*)thisfunc,'choose', choicenumber
       call select%make_combinations(doit,tsp%nwycoff,primitivesitemultiplicity,&
            specialpos,nspecies,num_atoms,choicenumber)
       write(6,*) thisfunc,'main,info=' , select%info 
       call write_a_position(tsp,select,specie_atoms,nspecies,&
            select%info, write_wy_skel=10, write_wy=20 , write_detail=prm%write_detail)


    else
       choicenumber=0 

       call write_possible_position(tsp,select,specie_atoms,nspecies,& 
            choicenumber=choicenumber, write_wy_skel=10, write_wy=20 , write_detail=prm%write_detail)
    endif

#endif

#if 0
    fid_in=100 ! unused_fid()
    fid_out=110 ! unused_fid()
    open(fid_in,file="POS_WY",status="old",action="read")
    open(fid_out,file="POS_WY.json",status="unknown",action="write")
    call json_write_wy(tsp%id, tsp%a_len,tsp%b_len,tsp%c_len, &
         acos(tsp%cosa)/PI*180.0d0 , acos(tsp%cosb)/PI*180.0d0 , acos(tsp%cosc)/PI*180.0d0 ,  &
         fid_in,fid_out) 
    close(fid_in)
    close(fid_out)
    write(6,*)thisfunc,' POS_WY.json is made.'
#endif

  end subroutine main

  !------------------------

  subroutine write_possible_position(tsp,select,specie_atoms,nspecies, choicenumber, write_wy_skel, write_wy, write_detail )
    use m_tsp
    use m_select
    use m_xyz1
    use m_vector_t_xyz1
    use m_util

    implicit none
    type(t_tsp):: tsp
    type(t_select):: select
    character(len_specie_name):: specie_atoms(:)
    integer:: nspecies
    integer,optional:: choicenumber
    integer,optional ::  write_wy_skel, write_wy
    logical,optional:: write_detail

    integer :: l_write_wy_skel, l_write_wy
    integer:: l_choicenumber
    logical:: l_write_detail


    integer:: ilist,i,j,k,l,ispecie,ip
    integer:: num_uniqvar,uniqvartotal, isit,nsite_in_iwp
    integer:: xyzwm(3,48),jam(2,3,48)

    type(t_vector_t_xyz1):: atoms_w_wy
    type(t_xyz1):: aatom
    type(t_xyz1),allocatable:: atoms(:)

    integer:: fid
    real(8):: xyz(3), frac1(3),frac2(3),frac(3)
    character:: wychar

    character(20):: file_pos_wy='POS_WY'
    character(20):: file_pos_wy_skel='POS_WY_SKEL'
    character(26)::thisfunc='write_possible_position: '

    l_write_wy_skel=0
    if (present(write_wy_skel)) l_write_wy_skel=write_wy_skel
    l_write_wy=0
    if (present(write_wy)) l_write_wy=write_wy
    l_choicenumber=0
    if (present(choicenumber)) l_choicenumber=choicenumber
    l_write_detail=.True.
    if (present(write_detail)) l_write_detail=write_detail

    if (l_write_wy>0) then 
       open(l_write_wy,file=file_pos_wy,status="unknown",action="write")
    endif
    if (l_write_wy_skel>0) then 
       open(l_write_wy_skel,file=file_pos_wy_skel,status="unknown",action="write")
    endif


    write(6,*)thisfunc,'l_choicenumber,l_write_wy_skel,l_write_wy,l_write_detail=',&
         l_choicenumber, l_write_wy_skel,l_write_wy,l_write_detail


    if (l_write_detail) write(6,*)thisfunc,'list'

    ilistloop: do ilist=1,select%combisum%infolist%last2

       if (l_choicenumber>0) then
          if ( ilist==l_choicenumber) then
             write(6,*)thisfunc,'----choose this------',l_choicenumber
             call atoms_w_wy%init(1)
          else  
             cycle
          endif
       endif

       if (l_write_detail) &
            write(6,*)thisfunc,'---------------------',ilist,"/",select%combisum%infolist%last2
       uniqvartotal=0
       do ispecie=1,nspecies
          i=select%combisum%infolist%v(ispecie,ilist)
          k=select%combisum%combi_eachspecie(ispecie)%list%size1(i)
          do l=1,k
             num_uniqvar=tsp%wy(select%combisum%combi_eachspecie(ispecie)%list%v(l,i))%num_uniqvar
             uniqvartotal=uniqvartotal+num_uniqvar
          enddo
       enddo
       if (l_write_detail) &
            write(6,*) thisfunc,'number of unique variables=',uniqvartotal
       do ispecie=1,nspecies
          i=select%combisum%infolist%v(ispecie,ilist)
          k=select%combisum%combi_eachspecie(ispecie)%list%size1(i)

          if (l_write_detail) then
             write(6,'(a,a)',advance='no')thisfunc,'wycoff list '
             write(6,'(a)',advance='no') '['
             write(6,'(100a3)',advance='no') tsp%wy( select%combisum%combi_eachspecie(ispecie)%list%v(1:k,i) )%sitecharacter
             write(6,'(a)') ' ] '
             write(6,'(a,a)',advance='no')thisfunc,'multiplicity'
             write(6,'(a)',advance='no') '['
             write(6,'(100i3)',advance='no') &
                  tsp%wy(select%combisum%combi_eachspecie(ispecie)%list%v(1:k,i))% primitivesitemultiplicity
             write(6,'(a)') ' ] '
             write(6,'(a,a)',advance='no')thisfunc,'variables   '
             write(6,'(a)',advance='no') '['
             write(6,'(100i3)',advance='no') tsp%wy(select%combisum%combi_eachspecie(ispecie)%list%v(1:k,i))%num_uniqvar
             write(6,'(a)') ' ] '
          endif

!!!
!!! make wycoff positions
!!!
          do l=1,k
             ip= select%combisum%combi_eachspecie(ispecie)%list%v(l,i)
             wychar=tsp%wy(ip)%sitecharacter
             num_uniqvar=tsp%wy(ip)%num_uniqvar

             nsite_in_iwp=tsp%wy(ip)%nequivsite
             xyzwm= tsp%wy(ip)%xyzwm
             jam = tsp%wy(ip)%jam


601          format(a,a,1x,i3,a,1x,i3,1x,i3,1x,3(a4,' + ',i2,' / ',i2,3x),i3)
             if (l_choicenumber>0 .and. ilist==l_choicenumber) then 
                call random_number(xyz)
                write(6,*)thisfunc,'random_number=',xyz
                if (l_write_wy>0) then 
                   write( l_write_wy, '(a,a,3(F20.10,1x))' )thisfunc, "#rand=",xyz
                endif
             endif
             do isit=1,nsite_in_iwp
                write(6,601)thisfunc//'atom> ',specie_atoms(ispecie),tsp%wy(ip)%sitemultiplicity,wychar,&
                     l,isit,(xyzch(xyzwm(k,isit)), (jam(j,k,isit),j=1,2),k=1,3),&
                     num_uniqvar
                if (choicenumber>0 .and. ilist==choicenumber) then
                   frac1=apply_xyz(xyzch(xyzwm(:,isit)),xyz)
                   frac2=jam2xyz(jam(:,:,isit))
                   write(6,*) thisfunc,"R> ",specie_atoms(ispecie), frac1+frac2
                   frac=frac1+frac2
                   call aatom%set(specie_atoms(ispecie), frac=frac )
                   call atoms_w_wy%append( [aatom] )

                   if (isit==1 .and. l_write_wy_skel>0) then 
                      write(l_write_wy_skel , 601)'',specie_atoms(ispecie),tsp%wy(ip)%sitemultiplicity,wychar,&
                           l,isit,(xyzch(xyzwm(k,isit)), (jam(j,k,isit),j=1,2),k=1,3),&
                           num_uniqvar
                   endif
                   if (isit==1 .and. l_write_wy>0) then
610                   format(a,1x,i3,a,1x,i3,1x,i3,1x,3(F20.10,1x),i3)
                      write(l_write_wy , 610)specie_atoms(ispecie),tsp%wy(ip)%sitemultiplicity,wychar,&
                           l,isit, frac , num_uniqvar
                   endif

                endif
             enddo

          enddo

       enddo
       write(6,*) thisfunc


       if (l_write_wy_skel>0) close(l_write_wy_skel)
       if (l_write_wy>0) close(l_write_wy)

       if (l_choicenumber>0 .and. ilist==l_choicenumber) then
          !allocate( atoms(atoms_name%last2) )
          allocate( atoms(atoms_w_wy%last2) )
          do i=1, atoms_w_wy%last2
             atoms(i)%name=atoms_w_wy%v(1,i)%name
             atoms(i)%frac=atoms_w_wy%v(1,i)%frac
          enddo
          fid=unusedfid()
          open(fid,file="POSCAR",status="unknown",action="write")
          call xyz2poscar('spacegroup.tspaceB',tsp%primitivevector,tsp%conventionalvector,atoms,fid)
          close(fid)
          write(6,*)thisfunc,'POSCAR is mde.'
          return 
       endif

    enddo ilistloop

  end subroutine write_possible_position

  !-----------------------------------
  subroutine write_a_position(tsp,select,specie_atoms,nspecies,info,&
       write_wy_skel, write_wy, write_detail , write_wy_skel_json,write_wy_json,&
       write_wy_skel_all_json)
    use m_tsp
    use m_select
    use m_xyz1
    use m_vector_t_xyz1
    use m_util

    implicit none
    type(t_tsp):: tsp
    type(t_select):: select
    character(len_specie_name):: specie_atoms(:)
    integer:: nspecies
    integer:: info(nspecies)
    integer,optional ::  write_wy_skel, write_wy, write_wy_skel_json,write_wy_json, &
       write_wy_skel_all_json
    logical,optional:: write_detail

    integer :: l_write_wy_skel, l_write_wy, l_write_wy_skel_json, l_write_wy_json, &
      l_write_wy_skel_all_json
    !    integer:: l_choicenumber
    logical:: l_write_detail


    integer:: ilist,i,j,k,l,ispecie,ip
    integer:: num_uniqvar,uniqvartotal, isit,nsite_in_iwp
    integer:: xyzwm(3,48),jam(2,3,48)

    type(t_vector_t_xyz1):: atoms_w_wy
    type(t_xyz1):: aatom
    type(t_xyz1),allocatable:: atoms(:)

    integer:: fid,lmax
    real(8):: xyz(3), frac1(3),frac2(3),frac(3),alpha,beta,gamma,jam_real(3)
    character:: wychar
    character(10):: str(3)
    character(20):: file_pos_wy='POS_WY'
    character(20):: file_pos_wy_skel='POS_WY_SKEL'
    character(20):: file_pos_wy_json='POS_WY.json'
    character(20):: file_pos_wy_skel_json='POS_WY_SKEL.json'
    character(20):: file_pos_wy_skel_all_json='POS_WY_SKEL_ALL.json'

    character(18)::thisfunc='write_a_position: '

    l_write_wy_skel=0
    if (present(write_wy_skel)) l_write_wy_skel=write_wy_skel
    l_write_wy=0
    if (present(write_wy)) l_write_wy=write_wy
    l_write_wy_skel_json=0
    if (present(write_wy_skel_json)) l_write_wy_skel_json=write_wy_skel_json
    l_write_wy_json=0
    if (present(write_wy_json)) l_write_wy_json=write_wy_json
    l_write_wy_skel_all_json=0
    if (present(write_wy_skel_all_json)) l_write_wy_skel_all_json = write_wy_skel_all_json

    l_write_detail=.True.
    if (present(write_detail)) l_write_detail=write_detail

    if (l_write_wy>0) then 
       open(l_write_wy,file=file_pos_wy,status="unknown",action="write")
    endif
    if (l_write_wy_skel>0) then 
       open(l_write_wy_skel,file=file_pos_wy_skel,status="unknown",action="write")
    endif
    if (l_write_wy_json>0) then 
       open(l_write_wy_json,file=file_pos_wy_json,status="unknown",action="write")
    endif
    if (l_write_wy_skel_json>0) then 
       open(l_write_wy_skel_json,file=file_pos_wy_skel_json,status="unknown",action="write")
    endif
    if (l_write_wy_skel_all_json>0) then
       open(l_write_wy_skel_all_json,file=file_pos_wy_skel_all_json,status="unknown",action="write")
    endif



    write(6,*)thisfunc,'l_write_wy_skel,l_write_wy,l_write_detail=', l_write_wy_skel,l_write_wy,l_write_detail
    write(6,*)thisfunc,'l_write_wy_skel_json,l_write_wy_json=', l_write_wy_skel_json,l_write_wy_json
    write(6,*)thisfunc,'l_write_wy_skel_all_json=', l_write_wy_skel_all_json

    call atoms_w_wy%init(1)
    if (l_write_detail) &
         uniqvartotal=0
    do ispecie=1,nspecies
       i=info(ispecie)
       k=select%combisum%combi_eachspecie(ispecie)%list%size1(i)
       do l=1,k
          num_uniqvar=tsp%wy(select%combisum%combi_eachspecie(ispecie)%list%v(l,i))%num_uniqvar
          uniqvartotal=uniqvartotal+num_uniqvar
       enddo
    enddo
    if (l_write_detail) &
         write(6,*) thisfunc,'number of unique variables=',uniqvartotal
  
    alpha=acos(tsp%cosa)/PI*180.0d0
    beta =acos(tsp%cosb)/PI*180.0d0
    gamma=acos(tsp%cosc)/PI*180.0d0
    write(l_write_wy_json,*)'{'
    write(l_write_wy_json,*)'"spacegroupid" : ',tsp%id,","
    write(l_write_wy_json,*)'"originchoice" : ',tsp%iorigin,","
    write(l_write_wy_json,*)'"lat" : [',tsp%a_len,',',tsp%b_len,',',tsp%c_len,',',alpha,',',beta,',',gamma ,'],'
    write(l_write_wy_json,*) '"atoms" : ['
    write(l_write_wy_skel_json,*)'{'
    write(l_write_wy_skel_json,*)'"spacegroupid" : ',tsp%id,","
    write(l_write_wy_skel_json,*)'"originchoice" : ',tsp%iorigin,","
    write(l_write_wy_skel_json,*)'"lat" : [',tsp%a_len,',',tsp%b_len,',',tsp%c_len,',',alpha,',',beta,',',gamma ,'],'
    write(l_write_wy_skel_json,*) '"atoms" : ['

    write(l_write_wy_skel_all_json,*) '{'
    write(l_write_wy_skel_all_json,*)'"spacegroupid" : ',tsp%id,","
    write(l_write_wy_skel_all_json,*)'"originchoice" : ',tsp%iorigin,","
    write(l_write_wy_skel_all_json,*)'"lat" : [',tsp%a_len,',',tsp%b_len,',',tsp%c_len,',',alpha,',',beta,',',gamma ,'],'
    write(l_write_wy_skel_all_json,*)'"primitivevector" : ['
    do i=1,3
        if (i==3) then
            write(l_write_wy_skel_all_json,'("[",F20.10,",",F20.10,",",F20.10,"]")') tsp%primitivevector(:,i)
        else
            write(l_write_wy_skel_all_json,'("[",F20.10,",",F20.10,",",F20.10,"],")') tsp%primitivevector(:,i)
        endif
    enddo
    write(l_write_wy_skel_all_json,*)'],'

    write(l_write_wy_skel_all_json,*)'"conventionalvector" : ['
    do i=1,3
        if (i==3) then
            write(l_write_wy_skel_all_json,'("[",F20.10,",",F20.10,",",F20.10,"]")') tsp%conventionalvector(:,i)
        else
            write(l_write_wy_skel_all_json,'("[",F20.10,",",F20.10,",",F20.10,"],")') tsp%conventionalvector(:,i)
        endif
    enddo
    write(l_write_wy_skel_all_json,*)'],'

    write(l_write_wy_skel_all_json,*) '"atoms" : ['

   do ispecie=1,nspecies
       i=info(ispecie)
       k=select%combisum%combi_eachspecie(ispecie)%list%size1(i)

       if (l_write_detail) then
          write(6,'(a,a)',advance='no')thisfunc,'wycoff list '
          write(6,'(a)',advance='no') '['
          write(6,'(100a3)',advance='no') tsp%wy( select%combisum%combi_eachspecie(ispecie)%list%v(1:k,i) )%sitecharacter
          write(6,'(a)') ' ] '
          write(6,'(a,a)',advance='no')thisfunc,'multiplicity'
          write(6,'(a)',advance='no') '['
          write(6,'(100i3)',advance='no') &
               tsp%wy(select%combisum%combi_eachspecie(ispecie)%list%v(1:k,i))% primitivesitemultiplicity
          write(6,'(a)') ' ] '
          write(6,'(a,a)',advance='no')thisfunc,'variables   '
          write(6,'(a)',advance='no') '['
          write(6,'(100i3)',advance='no') tsp%wy(select%combisum%combi_eachspecie(ispecie)%list%v(1:k,i))%num_uniqvar
          write(6,'(a)') ' ] '
       endif

      lmax=k
!!!
!!! make wycoff positions
!!!
          write(l_write_wy_skel_all_json,*) '['
       do l=1,lmax
          write(6,*)'l,lmax=',l,lmax
          ip= select%combisum%combi_eachspecie(ispecie)%list%v(l,i)
          wychar=tsp%wy(ip)%sitecharacter
          num_uniqvar=tsp%wy(ip)%num_uniqvar

          nsite_in_iwp=tsp%wy(ip)%nequivsite
          xyzwm= tsp%wy(ip)%xyzwm
          jam = tsp%wy(ip)%jam


601       format(a,a,1x,i3,a,1x,i3,1x,i3,1x,3(a4,' + ',i2,' / ',i2,3x),i3)
          !             if (l_choicenumber>0 .and. ilist==l_choicenumber) then 
          call random_number(xyz)
          write(6,*)thisfunc,'random_number=',xyz
          if (l_write_wy>0) then 
             write( l_write_wy, '(a,3(F20.10,1x))' ) "#rand=",xyz
          endif
          !             endif
          write(l_write_wy_skel_all_json,*) '['
          do isit=1,nsite_in_iwp
             write(6,601)thisfunc//'atom> ',specie_atoms(ispecie),tsp%wy(ip)%sitemultiplicity,wychar,&
                  l,isit,(xyzch(xyzwm(k,isit)), (jam(j,k,isit),j=1,2),k=1,3),&
                  num_uniqvar
             !                if (choicenumber>0 .and. ilist==choicenumber) then
             frac1=apply_xyz(xyzch(xyzwm(:,isit)),xyz)
             frac2=jam2xyz(jam(:,:,isit))
             write(*,*) thisfunc,"R> ",specie_atoms(ispecie), frac1+frac2
             frac=frac1+frac2
             call aatom%set(specie_atoms(ispecie), frac=frac )
             call atoms_w_wy%append( [aatom] )

             if (isit==1 .and. l_write_wy_skel>0) then 
                write(l_write_wy_skel , 601)'',specie_atoms(ispecie),tsp%wy(ip)%sitemultiplicity,wychar,&
                     l,isit,(xyzch(xyzwm(k,isit)), (jam(j,k,isit),j=1,2),k=1,3),&
                     num_uniqvar
             endif
             if (isit==1 .and. l_write_wy>0) then
610             format(a,1x,i3,a,1x,i3,1x,i3,1x,3(F20.10,1x),i3)
                write(l_write_wy , 610)specie_atoms(ispecie),tsp%wy(ip)%sitemultiplicity,wychar,&
                     l,isit, frac , num_uniqvar
             endif
             if (isit==1 .and. l_write_wy_json>0) then 
                write(l_write_wy_json,*) '{"name":"',trim(specie_atoms(ispecie)),'", "mul":',tsp%wy(ip)%sitemultiplicity,&
                   ',"wy":"',trim(wychar),'", "frac": [',frac(1),",",frac(2),",",frac(3),'], "num_uniqvar":',num_uniqvar
                if (ispecie == nspecies .and. l==lmax) then 
                   write(l_write_wy_json,*) '}'
                else
                   write(l_write_wy_json,*) '},'
                endif
             endif

             if (isit==1 .and. l_write_wy_json>0) then
                do k=1,3
                  jam_real(k)= real(jam(1,k,isit),kind=8)/real(jam(2,k,isit),kind=8)
                enddo
                write(l_write_wy_skel_json,*) '{"name":"',trim(specie_atoms(ispecie)),'", "mul":',tsp%wy(ip)%sitemultiplicity,&
                   ',"wy":"',trim(wychar),'",'
                do k=1,3
                   str(k)=adjustl(xyzch(xyzwm(k,isit)))
                enddo
                write(l_write_wy_skel_json,*) &
                   '"xyzch": ["',trim(str(1)),'","',trim(str(2)),'","',trim(str(3)),'"],'
                write(l_write_wy_skel_json,*) &
                   '"add": [',  jam_real(1),",",jam_real(2),",",jam_real(3),'], "num_uniqvar":',num_uniqvar
                if (ispecie == nspecies .and. l==lmax) then
                   write(l_write_wy_skel_json,*) '}'
                else
                   write(l_write_wy_skel_json,*) '},'
                endif
             endif

             if ( l_write_wy_skel_all_json>0 ) then
                do k=1,3
                  jam_real(k)= real(jam(1,k,isit),kind=8)/real(jam(2,k,isit),kind=8)
                enddo
                write(l_write_wy_skel_all_json,*) '{"name":"',trim(specie_atoms(ispecie)),'", "mul":',tsp%wy(ip)%sitemultiplicity,&
                   ',"wy":"',trim(wychar),'",'
                do k=1,3
                   str(k)=adjustl(xyzch(xyzwm(k,isit)))
                enddo
                write(l_write_wy_skel_all_json,*) &
                   '"xyzch": ["',trim(str(1)),'","',trim(str(2)),'","',trim(str(3)),'"],'
                write(l_write_wy_skel_all_json,*) &
                   '"add": [',  jam_real(1),",",jam_real(2),",",jam_real(3),'], "num_uniqvar":',num_uniqvar

             endif

              if (isit==nsite_in_iwp ) then
                 write(l_write_wy_skel_all_json,*) '}'
             else
                 write(l_write_wy_skel_all_json,*) '},'
             endif
 
             !                endif
         enddo

               if (l==lmax) then
                   write(l_write_wy_skel_all_json,*) ']'
                else
                   write(l_write_wy_skel_all_json,*) '],'
                endif


       enddo

          if (ispecie==nspecies) then
              write( l_write_wy_skel_all_json,*) ']'
          else
              write( l_write_wy_skel_all_json,*) '],'
          endif



    enddo


    write(6,*) thisfunc


    if ( l_write_wy_json>0) write(l_write_wy_json,*)']'
    if ( l_write_wy_json>0) write(l_write_wy_json,*)'}'
    if ( l_write_wy_skel_json>0) write(l_write_wy_skel_json,*)']'
    if ( l_write_wy_skel_json>0) write(l_write_wy_skel_json,*)'}'
    if ( l_write_wy_skel_all_json>0) write(l_write_wy_skel_all_json,*)'] }'


    if (l_write_wy_skel>0) then; close(l_write_wy_skel) ; write(6,*)thisfunc,file_pos_wy_skel,'is made.'; endif
       if (l_write_wy>0) then;close(l_write_wy); write(6,*)thisfunc,file_pos_wy,'is made.'; endif

          !       if (l_choicenumber>0 .and. ilist==l_choicenumber) then
          !allocate( atoms(atoms_name%last2) )
          allocate( atoms(atoms_w_wy%last2) )
          do i=1, atoms_w_wy%last2
             atoms(i)%name=atoms_w_wy%v(1,i)%name
             atoms(i)%frac=atoms_w_wy%v(1,i)%frac
          enddo
          fid=unusedfid()
          open(fid,file="POSCAR",status="unknown",action="write")
          call xyz2poscar('spacegroup.tspaceB',tsp%primitivevector,tsp%conventionalvector,atoms,fid)
          close(fid)
          write(6,*)thisfunc,'POSCAR is mde.'
          return 
          !       endif

          !    enddo ilistloop

        end subroutine write_a_position


        !------------------------

      end program test

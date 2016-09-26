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


module m_util
  use m_fixedparam
  use m_xyz1

#if 0
  type t_xyz1
     character(len_specie_name):: name
     real(8):: frac(3)
     real(8):: cart(3)
   contains
     procedure:: frac2cart  => t_xyz1_frac2cart
  end type t_xyz1
#endif

contains

#if 0
  function t_xyz1_frac2cart(self,clat)  result( cart )
    implicit none
    class(t_xyz1):: self
    real(8):: v(3)
    real(8):: clat(3,3)
    real(8):: cart(3)
    self%cart=matmul(clat,self%frac) 
    cart=self%cart
  end function  t_xyz1_frac2cart
#endif

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


  function get_uniqchar(str) result(xyz)
    implicit none
    character(*) :: str(:) 
    integer:: n,i
    logical:: xyz(3)

    character(14)::thisfunc='get_uniqchar: '

    n=len(str)

    xyz=.false.
    do i=1,n
       if ( index(str(i),'x')>0 )  xyz(1)=.true.
       if ( index(str(i),'y')>0 )  xyz(2)=.true.
       if ( index(str(i),'z')>0 )  xyz(3)=.true.
    enddo

  end function get_uniqchar

  function apply_xyz(str,xyz) result(res)
    implicit none
    character(*) :: str(3) 
    real(8):: xyz(3)
    real(8):: res(3)

    character(11)::thisfunc='apply_xyz: '

    integer:: n,i

    n= 3
    res=0.0d0
    do i=1,n

       select case(str(i))
       case(' -2x')
          res(i)= -2.0d0*xyz(1)
       case('-x+y')
          res(i)= -xyz(1)+xyz(2)
       case(' -z ')
          res(i)= -xyz(3)
       case(' -y ')
          res(i)= -xyz(2)
       case(' -x ')
          res(i)= -xyz(1)
       case('  0 ')
          res(i)= 0.0d0
       case('  x ')
          res(i)= xyz(1)
       case('  y ')
          res(i)= xyz(2)
       case('  z ')
          res(i)= xyz(3)
       case(' x-y')
          res(i)= xyz(1)-xyz(2)
       case(' 2x ')
          res(i)= 2.0d0* xyz(1)
       case default
          write(*,*)thisfunc,'internal error, unknown str=(',str(i),') at i=',i
          stop 120
       end select

    enddo

  end function apply_xyz

  function jam2xyz(jam) result(xyz)
    implicit none
    integer::jam(2,3)
    real(8)::xyz(3)
    integer:: i,j

    do i=1,3
       xyz(i)=real(jam(1,i),kind=8)/real(jam(2,i),kind=8)
    enddo

  end function jam2xyz


  integer function unusedfid()
    integer:: i
    logical ::L
    !find new unused file id

    do i=10,99
       inquire(i,opened=L)
       if (.not.L) then
          unusedfid=i
          return
       endif
    enddo

    unusedfid=-1

  end function unusedfid


  subroutine xyz1_reorder(atoms,atoms2,atoms_uniq_name,atoms_uniq_num)
    implicit none
    type(t_xyz1):: atoms(:)
    integer:: n_atoms_uniq,i,j,iall,n
    type(t_xyz1),allocatable,intent(out):: atoms2(:)
    character(len_specie_name),allocatable,intent(out):: atoms_uniq_name(:)
    integer,allocatable,intent(out) :: atoms_uniq_num(:)
    character(len_specie_name),allocatable::  uniq_name(:)
    logical::notfound
    character(14)::thisfunc='xyz1_reorder: '

    n_atoms_uniq=0

    n=size(atoms)
    allocate(uniq_name(n))
    do i=1,n
       notfound=.true.
       do j=1, n_atoms_uniq
          if (atoms(i)%name==uniq_name(j)) then
             notfound=.false.
             exit
          endif
       enddo
       if (notfound) then 
          n_atoms_uniq=n_atoms_uniq+1
          uniq_name(n_atoms_uniq)=atoms(i)%name
       endif
    enddo

    write(*,*)thisfunc,'n_atoms_uniq=',n_atoms_uniq

    allocate( atoms_uniq_name(n_atoms_uniq) , atoms_uniq_num(n_atoms_uniq) )
    atoms_uniq_name(1:n_atoms_uniq) = uniq_name(1:n_atoms_uniq)
    atoms_uniq_num=0
    deallocate(uniq_name)

    write(*,*)thisfunc,'atoms_uniq_name=',atoms_uniq_name

    allocate( atoms2(n) )
    iall=0
    do j=1,n_atoms_uniq
       do i=1,n
          if (atoms(i)%name==atoms_uniq_name(j)) then
             iall=iall+1
             atoms2(iall)=atoms(i)
             atoms_uniq_num(j)=atoms_uniq_num(j)+1
          endif
       enddo
    enddo

    if (iall /= n ) then 
       write(*,*)thisfunc,'internal error , iall /= n ',iall,n 
       stop 12000
    endif

    if ( sum(atoms_uniq_num) /= n ) then
       write(*,*) thisfunc,'internal error, sum(atoms_uniq_num) /= n ', atoms_uniq_num,n 
       stop 12001
    endif

  end subroutine xyz1_reorder


  subroutine xyz2poscar(comment,plat,clat,atoms,fid)
    implicit none

    character(*)::comment
    real(8):: plat(3,3),clat(3,3)
    type(t_xyz1) :: atoms(:)
    integer::fid
    integer::i,n

    type(t_xyz1),allocatable :: atoms2(:)
    character(len_specie_name),allocatable:: atoms_uniq_name(:)
    integer,allocatable :: atoms_uniq_num(:)

    call xyz1_reorder(atoms,atoms2,atoms_uniq_name,atoms_uniq_num)
    n=size(atoms2)

    !fid=unusedfid()

    !open(fid,file=filename,status='unknown',action='write')
    write(fid,*)comment
    write(fid,*)1.0
    write(fid,'(3F20.10)') (plat(:,i),i=1,3)
    write(fid,'(20(a,1x))')  (trim(atoms_uniq_name(i)),i=1,size(atoms_uniq_name))
    write(fid,'(20(I5,1x))') atoms_uniq_num
    write(fid,'(a)')'Cart'
    do i=1,n
       write(fid,'(3(F20.10,1x))') atoms2(i)%frac2cart(clat)
    enddo

    !close(fid)

  end subroutine xyz2poscar



end module m_util

#if 0
program test
  use util
  integer:: i,j 
  call util_rand_set(121)
  do j=1,10
     i=util_rand_get_i(10)
     write(*,*)j, i
  enddo
end program test
#endif


#if 0
program test
  use m_util
  implicit none
  integer,parameter:: n=3
  character(4):: str(N)
  real(8)::xyz(3),xyz1(N),xyz2(N)
  integer::jam(2,3)

  integer:: i

  str(1)='  x '
  str(2)='  y '
  str(3)=' x-y'

  write(*,*) get_uniqchar(str)

  xyz=[0.1d0,0.15d0,0.5d0]

  write(*,*)'xyz=',xyz

  jam(:,1)=[1,2]
  jam(:,2)=[0,1]
  jam(:,3)=[1,3]

  xyz1= apply_xyz(str,xyz)
  xyz2= jam2xyz(jam)
  do i=1,3
     write(*,'(a,2i3,3f10.5)') str(i),jam(:,i),xyz1(i),xyz2(i),xyz1(i)+xyz2(i)
  enddo

end program test

#endif



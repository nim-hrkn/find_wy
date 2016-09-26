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


#define ERROR_CODE
module m_gn_ka
  implicit none

  integer,parameter:: mmax=48
  integer::nmax=0
  character:: gn(2,3,mmax)*4=''
  integer:: ka(2,3,mmax)=0

contains

#if 0
! for interface definition
  function generator_make_rotmatrix(gn)
    implicit none
    character,intent(in) :: gn(2,3)*4
    integer:: generator_make_rotmatrix(3,3) 
    integer:: ig,i,j
    integer:: matrix(3,3)=0
    do i=1,3
       select case(gn(2,i))
       case('X'); matrix(:,i)= [1,0,0] ![ 1.0d0, 0.0d0, 0.0d0 ]
       case('Y'); matrix(:,i)= [0,1,0] ![ 0.0d0, 1.0d0, 0.0d0 ]
       case('Z'); matrix(:,i)= [0,0,1] ![ 0.0d0, 0.0d0, 1.0d0 ]
       case('W'); matrix(:,i)= [1,-1,0] ![ 1.0d0, -1.0d0, 0.0d0 ]
       case default; write(6,*)'generator_makematrix, error, gn2=',gn(2,i),' i=',i;stop
       end select
       if (gn(1,i)=='-') then
          matrix(:,i)= -matrix(:,i)
       else if (len_trim(gn(1,i))==0) then
          continue
       else
          write(6,*)'generator_makematrix, error, gn1=',gn(1,i),' i=',i
          stop ERROR_CODE
       endif
    enddo
    generator_make_rotmatrix=matrix
    !do i=1,3
    !   write(6,'(a,i2,1x,3I4)')'M',i, matrix(:,i)
    !enddo
  end function generator_make_rotmatrix
#endif

function gn_ka_nmax()
  implicit none
  integer:: gn_ka_nmax
  gn_ka_nmax=nmax
end function gn_ka_nmax

end module m_gn_ka

!--------------------------

subroutine gn_ka_init()
  use m_gn_ka
  implicit none
  gn=''
  ka=0
  nmax=0
  write(6,*)'gn_ka_init:'
end subroutine gn_ka_init

!--------------------------

subroutine gn_ka_get(id,gn0,ka0)
  use m_gn_ka
  implicit none
  integer,intent(in):: id
  character,intent(out):: gn0(2,3)*4
  integer,intent(out):: ka0(2,3)

  if (id>mmax)then
     write(6,*)'gn_ka_get: range error id>mmax, id=',id
     stop ERROR_CODE
  endif
  if (id>nmax)then
     write(6,*)'gn_ka_get: range error id>nmax, id=',id,' nmax=',nmax
     stop ERROR_CODE
  endif

  gn0=gn(:,:,id)
  ka0=ka(:,:,id)

end subroutine gn_ka_get

!--------------------------

subroutine gn_ka_set(id,gn0,ka0)
  use m_gn_ka
  implicit none
  integer,intent(in):: id
  character,intent(in):: gn0(2,3)*4
  integer,intent(in):: ka0(2,3)
  integer:: i
  if (id>mmax)then
     write(6,*)'gn_ka_set: range error id=',id
     stop ERROR_CODE
  endif
!  write(6,'(3(1x,a1,a1),9i3)')gn0,ka0
  gn(:,:,id)=gn0
  ka(:,:,id)=ka0
  nmax=max(id,nmax)
end  subroutine gn_ka_set




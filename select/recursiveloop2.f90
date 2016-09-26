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


module m_multiloopindex
use m_vector_i2
implicit none

type t_multiloopindex
integer:: maxlevel
integer,allocatable:: info(:)
type(t_vector_i2) :: idxlist

contains 
procedure:: init => multiloopindex_init
end type

private:: makeidx

contains

!-----------------------------------

subroutine multiloopindex_init(self,n,sizes)
implicit none
class(t_multiloopindex):: self
integer:: n,sizes(n),level
self%maxlevel=n
if (allocated(self%info)) deallocate(self%info)
allocate(self%info(n))
self%info=0

call self%idxlist%init(n)

level=n
call makeidx(self, sizes, level)

end subroutine multiloopindex_init

!--------------------------------------

recursive subroutine makeidx(self,loopmax,level)
class(t_multiloopindex):: self
integer::level,loopmax(:)
integer::idx

if (level==0) then 
  write(6,*) self%info 
  call self%idxlist%add(self%info)
  return 
endif

do idx=1,loopmax(level)
  self%info(level)=idx
  call makeidx(self,loopmax,level-1)
enddo

end subroutine makeidx

end module m_multiloopindex

!-----------------------------------
#ifdef HAVE_MAIN
program test
use m_multiloopindex
implicit none
integer,parameter:: nmax=3
integer:: loopsize(nmax)
integer:: level,info(nmax)
type(t_multiloopindex):: idx


level=3
loopsize= [2,3,4]

!call makeidx(nmax,info,loopsize,level)
call idx%init(level,loopsize)

!-----------------------------------
end program test
#endif

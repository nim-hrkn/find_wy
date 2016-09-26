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



#define TYPE type(t_xyz1)

module m_vector_t_xyz1
 use m_fixedparam
 use m_xyz1
implicit none

type t_vector_t_xyz1
  integer:: nappend=10
  integer:: max1=0
  integer:: max2=0
  integer:: last2=0
  TYPE,allocatable:: v(:,:)
  contains
   procedure:: init=> vector_t_xyz1_init
   procedure:: initialized=> vector_t_xyz1_initialized
   procedure:: append => vector_t_xyz1_append
   procedure:: append_list => vector_t_xyz1_append_list
   procedure:: copy => vector_t_xyz1_copy
   procedure:: show => vector_t_xyz1_show
   procedure:: size1=> vector_t_xyz1_size1
   procedure:: return_list_dim1 => vector_t_xyz1_return_list_dim1 
end type

contains


function vector_t_xyz1_return_list_dim1(self,i2)
implicit none
class(t_vector_t_xyz1),intent(in):: self
integer,intent(in):: i2
integer:: i1 
TYPE,allocatable :: vector_t_xyz1_return_list_dim1(:)

i1= self%size1(i2) 
allocate( vector_t_xyz1_return_list_dim1 ( i1 ) )
vector_t_xyz1_return_list_dim1(1:i1) = self%v(1:i1,i2) 
end function vector_t_xyz1_return_list_dim1


integer function vector_t_xyz1_size1(self,i2)
implicit none
class(t_vector_t_xyz1):: self
integer:: i2
integer::j
vector_t_xyz1_size1=size(self%v,dim=1)
do j=1, vector_t_xyz1_size1
   if (self%v(j,i2)%is_null()) then
      vector_t_xyz1_size1=j-1; return 
   endif
enddo

end function vector_t_xyz1_size1

subroutine vector_t_xyz1_show(self)
implicit none
class(t_vector_t_xyz1):: self
integer:: i
do i=1,self%last2
   write(*,'(100a)') self%v(:,i)
enddo
end subroutine vector_t_xyz1_show


subroutine vector_t_xyz1_copy(self,append)
implicit none
class(t_vector_t_xyz1):: self, append
self%max1=append%max1
self%max2=append%max2
self%last2=append%last2
if (allocated(self%v)) deallocate(self%v) 
allocate(self%v( size(append%v,dim=1), size(append%v,dim=2) ) )
self%v=append%v
!write(6,*)'vector_t_xyz1_copy, last2=',self%last2
end subroutine vector_t_xyz1_copy

subroutine vector_t_xyz1_init(self,n1)
implicit none
class(t_vector_t_xyz1):: self
integer::n1,i,j
self%max1=n1
self%max2=self%nappend
self%last2=0
if (allocated(self%v)) deallocate(self%v)
allocate(self%v(self%max1,self%max2))
do i=1,size(self%v,dim=2)
do j=1,size(self%v,dim=1)
call self%v(j,i)%init()
enddo
enddo
end subroutine vector_t_xyz1_init

logical function vector_t_xyz1_initialized(self)
implicit none
class(t_vector_t_xyz1):: self
if ( allocated(self%v) ) then 
vector_t_xyz1_initialized=.true. 
else 
vector_t_xyz1_initialized=.false.
endif
end function vector_t_xyz1_initialized

! 要素でなく、vector_t_xyz1をappendする。
subroutine vector_t_xyz1_append_list(self,add)
implicit none
class (t_vector_t_xyz1):: self,add

integer::i, n
n=add%last2
do i=1,n
!  write(6,*)'vector_t_xyz1_append_list source',add%v(:,i)
  call self%append(add%v(:,i))
enddo

end subroutine vector_t_xyz1_append_list

! 要素をappend
subroutine vector_t_xyz1_append(self,v)
implicit none
class (t_vector_t_xyz1):: self
TYPE::v(:)
TYPE,allocatable:: newv(:,:)
integer:: n,i,j 

! 配列のサイズを増やす
if (self%last2+1>self%max2) then

   allocate(newv(self%max1,self%max2+self%nappend))
   newv(:,1:self%max2)=self%v
   deallocate(self%v)
   call move_alloc(from=newv, to=self%v)
   self%max2=size(self%v,dim=2)
!   write(6,*)'resize',self%max2
endif

! append 
self%last2=self%last2+1

do j=1,size(self%v,dim=1)
call self%v(j,self%last2)%init()
enddo
n=size(v)
self%v(1:n,self%last2)=v(1:n) 
!write(6,*)'append ',v
!write(6,*)'last2=',self%last2

!write(6,*)'result'
!call self%show()
end subroutine vector_t_xyz1_append

end module m_vector_t_xyz1

#if 1
#ifdef HAVE_MAIN
program test
use m_vector_t_xyz1
implicit none

integer,parameter:: n1=1
TYPE:: v
type(t_vector_t_xyz1):: list
integer:: i,iv(3)

call list%init(n1)

do i=1,20
 write(v%name,'(a,i3)') 'test',i 
 v%frac=[ i,i+1,i+2]*0.01d0
 write(*,*) "append",trim(v%str())
call list%append([v])
enddo
write(*,*)'result',list%last2
write(*,'((a,1x))') (trim(list%v(1,i)%str()),i=1,list%last2)

end program test
#endif
#endif

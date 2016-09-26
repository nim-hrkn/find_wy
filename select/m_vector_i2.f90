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


module m_vector_i2
  implicit none

  type t_vector_i2
     character(20):: title
     integer:: nappend=100000
     integer:: max1=0
     integer:: max2=0
     integer:: last2=0
     integer,allocatable:: v(:,:)
     logical:: dryrun=.false.
     integer::printmod=1000000
   contains
     procedure:: init=> vector_i2_init
     procedure:: initialized=> vector_i2_initialized
     procedure:: append => vector_i2_append
     procedure:: append_list => vector_i2_append_list
     procedure:: copy => vector_i2_copy
     procedure:: show => vector_i2_show
     procedure:: size1=> vector_i2_size1
     procedure:: return_list_dim1 => vector_i2_return_list_dim1 
  end type t_vector_i2

contains


  function vector_i2_return_list_dim1(self,i2)
    implicit none
    class(t_vector_i2),intent(in):: self
    integer,intent(in):: i2
    integer:: i1 
    integer,allocatable :: vector_i2_return_list_dim1(:)

    if (self%dryrun) then
       write(6,*)'vector_i2_return_list_dim1: is not supported on dryrun=.true.'
       stop 20000
    else
       if (.not.allocated(self%v))  then
           write(6,*)'vector_i2_return_list_dim1: self%v isnot alloated'
           stop 8000
       endif
       i1= self%size1(i2) 
       allocate( vector_i2_return_list_dim1 ( i1 ) )
       vector_i2_return_list_dim1=0
       vector_i2_return_list_dim1(1:i1) = self%v(1:i1,i2) 
    endif
  end function vector_i2_return_list_dim1


  integer function vector_i2_size1(self,i2)
    implicit none
    class(t_vector_i2):: self
    integer:: i2
    integer::j
    if (self%dryrun) then
       vector_i2_size1=self%max1
    else
       !vector_i2_size1=size(self%v,dim=1)
       vector_i2_size1=self%max1
       do j=1, vector_i2_size1
          if (self%v(j,i2)==0) then
             vector_i2_size1=j-1; return 
          endif
       enddo
    endif
  end function vector_i2_size1

  subroutine vector_i2_show(self)
    implicit none
    class(t_vector_i2):: self
    integer:: i
    if (.not.self%dryrun) then
       do i=1,self%last2
!          write(*,'(a)',advance='no') "vector_i2_show: "
!          write(*,'(100i4)') self%v(:,i)
       enddo
    endif
  end subroutine vector_i2_show


  subroutine vector_i2_copy(self,append)
    implicit none
    class(t_vector_i2):: self, append
    self%max1=append%max1
    self%max2=append%max2
    self%last2=append%last2
    if (.not.self%dryrun) then
       if (allocated(self%v)) deallocate(self%v) 
       allocate(self%v( size(append%v,dim=1), size(append%v,dim=2) ) )
       self%v=append%v
    endif
    !write(6,*)'vector_i2_copy, last2=',self%last2
  end subroutine vector_i2_copy

  subroutine vector_i2_init(self,n1,title,dryrun)
    implicit none
    class(t_vector_i2):: self
    integer::n1
    character(*),optional:: title
    logical,optional:: dryrun
    if (present(dryrun)) self%dryrun=dryrun
    self%title=""
    if (present(title)) self%title=title
    !write(6,*)'vector_i2_init: with title=',trim(self%title)
    self%max1=n1
    self%max2=self%nappend
    self%last2=0
    if (.not.self%dryrun) then
       if (allocated(self%v)) deallocate(self%v)
       allocate(self%v(self%max1,self%max2))
       self%v=0
    endif
  end subroutine vector_i2_init

  logical function vector_i2_initialized(self)
    implicit none
    class(t_vector_i2):: self
    if (self%dryrun) then

       vector_i2_initialized=.true.

    else
       if ( allocated(self%v) ) then 
          vector_i2_initialized=.true. 
       else 
          vector_i2_initialized=.false.
       endif
    endif
  end function vector_i2_initialized

  ! 要素でなく、vector_i2をappendする。
  subroutine vector_i2_append_list(self,add)
    implicit none
    class (t_vector_i2):: self,add

    integer::i, n
    if (.not.self%dryrun) then
       n=add%last2
       do i=1,n
          !  write(6,*)'vector_i2_append_list source',add%v(:,i)
          call self%append(add%v(:,i))
       enddo
    endif
  end subroutine vector_i2_append_list

  ! 要素をappend
  subroutine vector_i2_append(self,v)
    implicit none
    class (t_vector_i2):: self
    integer::v(:)
    integer,allocatable:: newv(:,:)
    integer:: n

    if (.not.self%dryrun) then
       ! 配列のサイズを増やす
       if (self%last2+1>self%max2) then
          allocate(newv(self%max1,self%max2+self%nappend))
          newv(:,1:self%max2)=self%v
          deallocate(self%v)
          call move_alloc(from=newv, to=self%v)
          self%max2=size(self%v,dim=2)
          !write(6,*)'resize',self%max2

          write(6,*) 'vector_i2_append: size=',real(self%max1*(self%max2))* 4/ 1024/1024,'Mbyte'
       endif
    endif

    ! append 
    self%last2=self%last2+1
    if (self%printmod>0 .and. mod(self%last2,self%printmod)==0) then
       write(6,*)"vector_i2_append:",trim(self%title)//': adding',self%last2
    endif
    if (self%dryrun) then
       ! write(6,*)'dryrun add, # =', self%last2
    endif

    if (.not.self%dryrun) then
       self%v(:,self%last2)=0
       n=size(v)
       self%v(1:n,self%last2)=v(1:n) 
       !write(6,*)'vector_i2_append: add',v
    endif

!    write(6,*)'append ',v
!    write(6,*)'last2=',self%last2
!
!    write(6,*)'result'
!    call self%show()
  end subroutine vector_i2_append

end module m_vector_i2

#if 0
#ifdef HAVE_MAIN
program test
  use m_vector_i2
  implicit none

  integer,parameter:: n1=3
  integer:: v(n1)
  type(t_vector_i2):: list
  integer:: i

  call list%init(n1)

  do i=1,10
     v= [i,i,i]
     call list%append(v)
  enddo
  write(*,'(3i4)') list%v(:,1:list%last2)

end program test
#endif
#endif

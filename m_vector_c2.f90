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



#define TYPE character(len_specie_name)

module m_vector_c2
  use m_fixedparam
  implicit none

  type t_vector_c2
     integer:: nappend=100
     integer:: max1=0
     integer:: max2=0
     integer:: last2=0
     TYPE,allocatable:: v(:,:)
      contains
        procedure:: init=> vector_c2_init
        procedure:: initialized=> vector_c2_initialized
        procedure:: append => vector_c2_append
        procedure:: append_list => vector_c2_append_list
        procedure:: copy => vector_c2_copy
        procedure:: show => vector_c2_show
        procedure:: size1=> vector_c2_size1
        procedure:: return_list_dim1 => vector_c2_return_list_dim1 
     end type 

   contains


     function vector_c2_return_list_dim1(self,i2)
       implicit none
       class(t_vector_c2),intent(in):: self
       integer,intent(in):: i2
       integer:: i1 
       TYPE,allocatable :: vector_c2_return_list_dim1(:)

          i1= self%size1(i2) 
          allocate( vector_c2_return_list_dim1 ( i1 ) )
          vector_c2_return_list_dim1(1:i1) = self%v(1:i1,i2) 
        end function vector_c2_return_list_dim1


        integer function vector_c2_size1(self,i2)
          implicit none
          class(t_vector_c2):: self
          integer:: i2
          integer::j
          vector_c2_size1=size(self%v,dim=1)
          do j=1, vector_c2_size1
             if (self%v(j,i2)=='') then
                vector_c2_size1=j-1; return 
             endif
          enddo

        end function vector_c2_size1

        subroutine vector_c2_show(self)
          implicit none
          class(t_vector_c2):: self
          integer:: i
          do i=1,self%last2
             write(*,'(100a)') self%v(:,i)
          enddo
        end subroutine vector_c2_show


        subroutine vector_c2_copy(self,append)
          implicit none
          class(t_vector_c2):: self, append
          self%max1=append%max1
          self%max2=append%max2
          self%last2=append%last2
          if (allocated(self%v)) deallocate(self%v) 
          allocate(self%v( size(append%v,dim=1), size(append%v,dim=2) ) )
          self%v=append%v
          !write(6,*)'vector_c2_copy, last2=',self%last2
        end subroutine vector_c2_copy

        subroutine vector_c2_init(self,n1)
          implicit none
          class(t_vector_c2):: self
          integer::n1
          self%max1=n1
          self%max2=self%nappend
          self%last2=0
          if (allocated(self%v)) deallocate(self%v)
          allocate(self%v(self%max1,self%max2))
          self%v=''
        end subroutine vector_c2_init

        logical function vector_c2_initialized(self)
          implicit none
          class(t_vector_c2):: self
          if ( allocated(self%v) ) then 
             vector_c2_initialized=.true. 
          else 
             vector_c2_initialized=.false.
          endif
        end function vector_c2_initialized

        ! 要素でなく、vector_c2をappendする。
        subroutine vector_c2_append_list(self,add)
          implicit none
          class (t_vector_c2):: self,add

          integer::i, n
          n=add%last2
          do i=1,n
             !  write(6,*)'vector_c2_append_list source',add%v(:,i)
             call self%append(add%v(:,i))
          enddo

        end subroutine vector_c2_append_list

        ! 要素をappend
        subroutine vector_c2_append(self,v)
          implicit none
          class (t_vector_c2):: self
          TYPE::v(:)
             TYPE,allocatable:: newv(:,:)
                integer:: n

                ! 配列のサイズを増やす
                if (self%last2+1>self%max2) then
                   allocate(newv(self%max1,self%max2+self%nappend))
                   newv(:,1:self%max2)=self%v
                   deallocate(self%v)
                   call move_alloc(from=newv, to=self%v)
                   self%max2=size(self%v,dim=2)
                   !write(6,*)'resize',self%max2
                endif

                ! append 
                self%last2=self%last2+1

                self%v(:,self%last2)=''
                n=size(v)
                self%v(1:n,self%last2)=v(1:n) 
                !write(6,*)'append ',v
                !write(6,*)'last2=',self%last2

                !write(6,*)'result'
                !call self%show()
              end subroutine vector_c2_append

            end module m_vector_c2

#if 1
#ifdef HAVE_MAIN
            program test
              use m_vector_c2
              implicit none

              integer,parameter:: n1=3
              TYPE:: v(n1)
                 type(t_vector_c2):: list
                 integer:: i

                 call list%init(n1)

                 do i=1,11
                    write(v(1),'(i5)') i
                    write(v(2),'(i5)') i+1
                    write(v(3),'(i5)') i+2
                    write(*,*) v
                    call list%append(v)
                 enddo
                 write(*,*)'result'
                 write(*,'(3(a,1x))') list%v(:,1:list%last2)

               end program test
#endif
#endif

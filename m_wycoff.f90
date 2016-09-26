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
module m_wycoff

  type t_wycoff
     integer:: xyz(3), shift(2,3),sitemultiplicity, primitivesitemultiplicity
     character:: sitecharacter
     logical:: specialpos 
     logical::uniq(3)
     integer:: num_uniqvar=0
     logical::fix(3)
     real(8):: x,y,z
     logical:: haveset(3)

     integer:: nequivsite=0
     integer xyzwm(3,48),jam(2,3,48)  ! wycoff equivalent sites

     integer:: infolevel=0
   contains
     procedure:: init=>wycoff_init
     procedure:: set_param=> wycoff_set_param
     procedure:: set_x=>wycoff_set_x
     procedure:: set_y=>wycoff_set_y
     procedure:: set_z=>wycoff_set_z
     procedure:: set_i=>wycoff_set_i
     procedure:: calc_xyz=>wycoff_calc_xyz
     procedure:: exam_property =>wycoff_exam_property
  end type t_wycoff

  private:: wycoff_set_i

contains
  subroutine wycoff_set_param(self, sitemultiplicity, primitivesitemultiplicity, sitecharacter, xyz,shift)
    implicit none
    class(t_wycoff):: self
    integer:: xyz(3), shift(2,3),sitemultiplicity, primitivesitemultiplicity
    character:: sitecharacter
    self%sitemultiplicity= sitemultiplicity
    self%primitivesitemultiplicity= primitivesitemultiplicity
    self%sitecharacter= sitecharacter
    self%xyz=xyz
    self%shift=shift
  end  subroutine wycoff_set_param

  subroutine wycoff_init(self)
    implicit none
    class(t_wycoff):: self
    self%haveset=.false.
    self%uniq=.false.
    self%num_uniqvar=0
  end subroutine wycoff_init

  subroutine wycoff_set_i(self,i,x)
    implicit none
    class(t_wycoff):: self
    real(8):: x
    integer:: i
    if (self%uniq(i)) then
       select case(i)
       case(1); self%x=x
       case(2); self%y=x
       case(3); self%z=x
       case default; write(6,*)'wycoff_set_i, error, default',i; stop ERROR_CODE
       end select
       self%haveset(i)=.true.
    else
       write(6,*) 'wycoff_set_i, error, self%uniq(i)=.false., i=',i
       write(6,*) 'debug self%uniq=',self%uniq
       stop   ERROR_CODE
    endif
  end subroutine wycoff_set_i

  subroutine wycoff_set_x(self,x)
    implicit none
    class(t_wycoff):: self
    real(8):: x
    call self%set_i(1,x)
  end subroutine wycoff_set_x

  subroutine wycoff_set_z(self,x)
    implicit none
    class(t_wycoff):: self
    real(8):: x
    call self%set_i(3,x)
  end subroutine wycoff_set_z

  subroutine wycoff_set_y(self,x)
    implicit none
    class(t_wycoff):: self
    real(8):: x
    call self%set_i(2,x)
  end subroutine wycoff_set_y

  subroutine wycoff_exam_property(self)
    implicit none
    class(t_wycoff):: self
    integer:: xyzwm(3)
    character(len('wycoff_exam_property: ')) :: thisfunc='wycoff_exam_property: '

    integer:: i,add(3)
    xyzwm=self%xyz
    add=0
    if (self%infolevel>10) &
         write(6,*)thisfunc,': xyzwm',xyzwm
    do i=1,3
       select case (xyzwm(i))
       case(-5); add(1)=add(1)+1
       case(-4); add(1:2)=add(1:2)+1
       case(-3); add(3)=add(3)+1
       case(-2); add(2)=add(2)+1
       case(-1); add(1)=add(1)+1
       case(0); continue
       case(5); add(1)=add(1)+1
       case(4); add(1:2)=add(1:2)+1
       case(3); add(3)=add(3)+1
       case(2); add(2)=add(2)+1
       case(1); add(1)=add(1)+1
       case default;
          write(*,*) thisfunc,'exam_niq_pos error'; stop  ERROR_CODE
       end select
    enddo

!!!  write(6,*) thisfunc,': add=',add
    do i=1,3
       if(add(i)>0) then
          self%uniq(i)= .true.
       else
          self%uniq(i)= .false.
       endif
    enddo
    if (self%infolevel>10) &
         write(6,*) thisfunc,': uniq=',self%uniq
    self%num_uniqvar=0
    do i=1,3
       if (self%uniq(i)) self%num_uniqvar= self%num_uniqvar+1
    enddo
    if (self%infolevel>10) &
         write(6,*)thisfunc,': uniqvar=',self%num_uniqvar

    self%fix=.false.
    do i=1,3
       if (xyzwm(i)==0) self%fix(i)=.true.
    enddo
    if (self%infolevel>10) &
         write(6,*)thisfunc,': fix=',self%fix

    if (xyzwm(1)==0 .and. xyzwm(2)==0  .and. xyzwm(3)==0) then
       self%specialpos=.true.
    else
       self%specialpos=.false.
    endif

  end subroutine wycoff_exam_property


  subroutine wycoff_set_xyz(self,x,y,z)
    implicit none
    class(t_wycoff):: self
    real(8)::x,y,z

    self%x=x
    self%y=y
    self%z=z

  end subroutine wycoff_set_xyz

  function wycoff_calc_xyz(self,xyzwm,shift)
    implicit none
    class(t_wycoff):: self
    integer,intent(in):: xyzwm(3)
    integer,intent(in):: shift(2,3)
    real(8):: wycoff_calc_xyz(3)
    integer:: i,j
    real(8):: val1,val2
    do i=1,3
       select case(xyzwm(i))
       case(-5); wycoff_calc_xyz(i)=-2.0d0*self%x
       case( 5); wycoff_calc_xyz(i)=-2.0d0*self%x
       case(-4); wycoff_calc_xyz(i)=-self%x+self%y
       case( 4); wycoff_calc_xyz(i)=self%x-self%y
       case(-3); wycoff_calc_xyz(i)=-self%z
       case( 3); wycoff_calc_xyz(i)=self%z
       case(-2); wycoff_calc_xyz(i)=-self%y
       case( 2); wycoff_calc_xyz(i)=self%y
       case(-1); wycoff_calc_xyz(i)=-self%x
       case( 1); wycoff_calc_xyz(i)=self%x
       case( 0); wycoff_calc_xyz(i)=0.0d0
       case default;
          write(6,*)' error in wycoff_calc_xyz'; stop ERROR_CODE
       end select

       val1 =  shift(1,i); val2=shift(2,i)
       wycoff_calc_xyz(i)=wycoff_calc_xyz(i)+val1/val2
    enddo
  end function wycoff_calc_xyz

end  module m_wycoff

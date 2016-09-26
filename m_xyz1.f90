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


module m_xyz1
  use m_fixedparam
  type t_xyz1
     character(len_specie_name):: name
     real(8):: frac(3)
     real(8):: cart(3)
   contains
     procedure:: frac2cart  => t_xyz1_frac2cart
     procedure:: init
     procedure:: set
     procedure:: is_null
     procedure:: str
  end type t_xyz1

  private:: init, set,is_null, str

contains

  subroutine init(self)
    implicit none
    class(t_xyz1):: self
    self%name=''
    self%frac=0.0d0
    self%cart=0.0d0
  end subroutine init

  subroutine set(self,name,frac,cart)
    implicit none
    class(t_xyz1):: self
    character(len_specie_name)::name
    real(8),optional:: frac(3),cart(3)
    if (present(frac)) self%frac=frac
    if (present(cart)) self%cart=cart
    self%name=name
  end subroutine set


  function is_null(self) result(val)
    implicit none
    class(t_xyz1):: self
    logical::val
    if (self%name=='')  then 
       val=.true.
    else
       val=.false.
    endif
  end function is_null

  function str(self) result(s)
    implicit none
    class(t_xyz1):: self
    character(1000):: s
    write(s,'(a,1x,3F20.10,1x,3F20.10)') self%name,self%frac,self%cart
  end function str

  function t_xyz1_frac2cart(self,clat)  result( cart )
    implicit none
    class(t_xyz1):: self
    real(8):: v(3)
    real(8):: clat(3,3)
    real(8):: cart(3)
    self%cart=matmul(clat,self%frac)
    cart=self%cart
  end function  t_xyz1_frac2cart

end  module m_xyz1

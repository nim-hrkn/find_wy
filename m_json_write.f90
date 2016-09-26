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


module m_json_write
  implicit none

contains

  subroutine json_write_wy(id,a,b,c,alpha,beta,gamma, fid_in,fid_out)
    use m_xyz1
    implicit none
    integer::id
    real(8):: a,b,c,alpha,beta,gamma 
    integer::fid_in,fid_out
    character(1000)::str
    character(10)::symbol,wy
    integer:: iv(2),mul
    real(8):: frac(3)

    type(t_xyz1),allocatable:: atoms(:)

    integer::ios,itot,natoms,i


    rewind(fid_in)
    itot=0
    do while (.true.)
       read(fid_in,'(a1000)',iostat=ios) str
       if (ios/=0) exit 
       if (str(1:1)=="#") cycle
       itot=itot+1
       read(str,*) symbol,wy,iv,frac,mul
    enddo

    natoms=itot
    allocate( atoms(natoms) )

    rewind(fid_in)
    itot=0
    do while (.true.)
       read(fid_in,'(a1000)',iostat=ios) str
       if (ios/=0) exit 
       if (str(1:1)=="#") cycle
       itot=itot+1
       read(str,*) symbol,wy,iv,frac,mul
       atoms(itot)%name=symbol
       atoms(itot)%frac=frac 
    enddo


    write(fid_out,*)'{'
    write(fid_out,*)'"id" : ',id,","
    write(fid_out,*)'"lat" : [',a,',',b,',',c,',',alpha,',',beta,',',gamma ,'],'
    write(fid_out,*) '"elements" : ['
    do itot=1,natoms
       if (itot==natoms) then
          write(fid_out,*) '"'//trim(atoms(itot)%name)//'"'
       else
          write(fid_out,*) '"'//trim(atoms(itot)%name)//'",'
       endif
    enddo
    write(fid_out,*)'],'

    write(fid_out,*) '"positions" : ['
    do itot=1,natoms
       if (itot==natoms) then
          write(fid_out,'("[",F20.10,",",F20.10,",",F20.10,"]")') atoms(itot)%frac
       else
          write(fid_out,'("[",F20.10,",",F20.10,",",F20.10,"],")') atoms(itot)%frac
       endif
    enddo
    write(fid_out,*) ']'
    write(fid_out,*)'}'

  end subroutine json_write_wy

end module m_json_write

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
  call main


contains
  subroutine main
    use m_tsp
    implicit none
    type(t_tsp):: tsp

    integer:: spgid,iorigin,iequiv

    integer:: natom

    natom=10

    call tsp%init()

    do spgid=230,1,-1

       call tsp%set_id(spgid)

       do iorigin=1,tsp%norigin

          call tsp%set_origin(iorigin)
          ! read wycoff position from the file
          call tsp%read_wycoff()
          call tsp%read_wycoff_equiv ()
          call tsp%set_lattice(0)

          write(6,*) 'nwycoff=', tsp%nwycoff
          ! write wycoff position
          call tsp%write_wycoff

          do iequiv=1,tsp%nwycoff
             call tsp%write_wycoff_equiv(iequiv)
          enddo

       enddo

    enddo

    call tsp%finalize()

  end subroutine main
end program test


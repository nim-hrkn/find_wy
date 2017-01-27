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
  type t_param
     character(10):: mode
     integer:: spcid=0, choice=0
  end type t_param
  call main


contains

  subroutine get_arg(param)
    implicit none
    type(t_param):: param

    integer :: narg, iarg
    character(100):: arg
   
    integer:: COMMAND_ARGUMENT_COUNT

    narg = COMMAND_ARGUMENT_COUNT()

    iarg=1
    call GET_COMMAND_ARGUMENT(iarg, param%mode)
         iarg=2
         call GET_COMMAND_ARGUMENT(iarg, arg)
         read(arg,*) param%spcid
 
    if (param%mode=='query') return 

        iarg=3
         call GET_COMMAND_ARGUMENT(iarg, arg)
         read(arg,*) param%choice

  end subroutine get_arg
  subroutine show_arg(param)
    implicit none
    type(t_param):: param
    write(*,*)param
  end subroutine show_arg


  subroutine write_nchoice(i)
    integer::i
    integer:: fid = 300
    open(fid,file='choice.json',status="unknown") 
    write(fid,*) '{ "choice": ', i, "}"
    close(fid) 
  end subroutine write_nchoice

  subroutine main
    use m_tsp
    implicit none
    type(t_param):: param
    type(t_tsp):: tsp

    integer:: spgid,iorigin,iequiv


    call get_arg(param)
    call show_arg(param)

    call tsp%init()

    tsp%file_out_json = 250
    open(tsp%file_out_json,file="wy.json",status="unknown")

     spgid= param%spcid

       call tsp%set_id(spgid)

          if (param%mode=='query') then 
             call write_nchoice(tsp%norigin)
             stop 0 
          endif 


        iorigin= param%choice

        write(tsp%file_out_json,*) '{' 
          call tsp%set_origin(iorigin)
          ! read wycoff position from the file
          call tsp%read_wycoff()
          call tsp%read_wycoff_equiv ()
          call tsp%set_lattice(0)

          call tsp%write_json()

          write(6,*) 'nwycoff=', tsp%nwycoff
          ! write wycoff position
          call tsp%write_wycoff

          write(tsp%file_out_json,*) ', "wyq": ['
          do iequiv=1,tsp%nwycoff
             call tsp%write_wycoff_equiv(iequiv)
             if (iequiv /= tsp%nwycoff) then 
                write(tsp%file_out_json,*) ","
             endif
          enddo
          write(tsp%file_out_json,*) ']'
          

        write(tsp%file_out_json,*) '}'

    close( tsp%file_out_json)
    call tsp%finalize()


  end subroutine main
end program test


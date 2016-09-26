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

c     
c     
c     
      module m_keyvalue
      implicit none
      integer,private :: stdout=6

      character(120),private :: buf
      character(120),private :: token(2)
      character(120),allocatable,private :: vtoken(:)

      integer,private,parameter:: nlogical=14
      character(7),private:: logicalstr(nlogical)=
     i     (/'TRUE   ', 'OK     ', '.TRUE. ', 'YES    ','ON     ',
     i     '1      ', 'T      ',
     i     'FALSAE ', 'NG     ', '.FALSE.', 'NO     ','OFF    ',
     i     '0      ', 'F      '/)
      logical,private :: logicalvalue(nlogical) =
     i     (/.true.,.true.,.true.,.true.,.true.,.true.,.true.,
     i     .false.,.false.,.false.,.false.,.false.,.false.,.false./)
c     i    (/1,1,1,1,1,1,1,0,0,0,0,0,0,0/)
c     debug
      integer,private:: printlevel=0
c     
#if 0
      interface getkeyvalue
      module procedure 
     i     getkeyvalue_i , getkeyvalue_r8 , getkeyvalue_l 
     i     , getkeyvalue_c
     i     , getkeyvalue_iv , getkeyvalue_rv 
     i     , getkeyvalue_cv 
      end interface
#endif 


      type t_keyvalue
      character(100):: filename=''
      integer:: errorexit=10
      contains
      procedure :: set_filename => keyvalue_set_filename
      procedure :: get_block=> getkeyblock
      procedure :: get_i => getkeyvalue_i
      procedure :: get_iv => getkeyvalue_iv
      procedure :: get_r => getkeyvalue_r8
      procedure :: get_rv => getkeyvalue_rv
      procedure :: get_l => getkeyvalue_l
      procedure :: get_c => getkeyvalue_c
      procedure :: get_cv => getkeyvalue_cv
      generic:: get => get_i ,get_iv,  get_r , get_rv,  get_l, get_c, get_cv 
      end type 


      type(t_keyvalue):: keyvalue 

      contains


      subroutine keyvalue_set_filename(self,filename)
      implicit none
      class(t_keyvalue):: self
      character(*):: filename
      self%filename=filename
      end subroutine keyvalue_set_filename

c----------------------------------------------------------
c     unusedfid>0 if success
c     <0 if NG
c     
      integer function unusedfid()
      integer:: i
      logical ::L
c     find new unused file id

      do i=10,99
         inquire(i,opened=L)
         if (.not.L) then
            unusedfid=i
            return
         endif
      enddo

      unusedfid=-1

      end function unusedfid

c----------------------------------------------------------
      subroutine input_delfirstspc(buf)
      implicit none
      character(*):: buf


      buf=adjustl(buf)
c     cut comment
      if (buf(1:1).eq.'#' .or. buf(1:1).eq.'!') then
         buf=''
         return
      endif

c     do i=1,len_trim(buf)
c     if (buf(i:i).eq.'#' .or. buf(i:i).eq.'!') buf(i:)=''
c     enddo

      end subroutine input_delfirstspc

c-----------------------------------------------------------

      subroutine input_toupper(buf)
      implicit none
      character(*) :: buf
      integer:: i,n

      n=len_trim(buf)
      do i=1,n
         if (  ichar(buf(i:i)).ge.ichar('a') .and. 
     i        ichar(buf(i:i)).le.ichar('z')) then
            buf(i:i) = char(ichar(buf(i:i))+ichar('Z')-ichar('z'))
         endif
      enddo
      end subroutine input_toupper
      

c----------------------------------------------------------
      subroutine input_septoken(line,n,bufs)
      implicit none
      integer,intent(in):: n
      character(*),intent(in):: line
      character(*),intent(out):: bufs(n)

      integer::i,j,max,ibuf

      do i=1,n
         bufs(i)=' '
      enddo

      max=len_trim(line)
      if (printlevel>0)then
         write(*,*) max,line(:max)
      endif

      ibuf=0
      i=1
      do 
         do while (line(i:i).eq.' ') 
            i=i+1
            if (i>max) goto 1000
         enddo
         j=i+1
         do  while (line(j:j).ne.' ') 
            j=j+1
            if (j>max) exit
         enddo 
         j=j-1
         if (printlevel>0) then
            write(*,*) i,j,line(i:j)
         endif
         
         ibuf=ibuf+1
         bufs(ibuf)=line(i:j)
         if (ibuf>=n) goto 1000

         i=j+1
         if (i>max) goto 1000

      enddo

 1000 continue
      end  subroutine input_septoken
c-------------------------------------------------------------
      subroutine input_endkey(key)
      character(*) :: key
      integer::i,n

      n= len_trim(key)

      do i=n,1,-1
         key(i+1:i+1)= key(i:i)
      enddo

      key(1:1)=key(2:2)
      key(2:2)='/'

      end  subroutine input_endkey
c-------------------------------------------------------------


c----------------------------------------------------------
      subroutine getkeyvalue_i(self, key0,var, default, status)
      class(t_keyvalue):: self
      character(*),intent(in) :: key0
      integer,intent(out):: var
      integer,optional :: default
      integer,optional :: status
      
      character(120):: key 
      integer:: fid
      character(15):: this='getkeyvalue_i: '

      fid = unusedfid()

      if (fid<0) then
         write(*,*) this,'can not find fileID ',self%filename
         if (present(status)) status=-1
         return
      endif

      open(file=self%filename,unit=fid,status='old',action='read',err=9000)

      if (present(default))then
         var = default
      endif

      key=key0
      call input_toupper(key)
      
      do while (.true.)

         read(fid,'(a120)',end=9100,err=9100)  buf
         call input_delfirstspc(buf)        
         if (buf(1:1)=='#') cycle

         call input_septoken(buf,2,token)
         call input_toupper(token(1))
c     write(*,*) token(1) 
         if ( token(1).eq.key ) then
            read(token(2),*) var
            goto 9200
         endif

      enddo

 9100 if (present(default)) then
         if (present(status)) status=0
      else
         write(stdout,*) this,'can not find keyword ', key0
         if (present(status)) status=-1
      endif
      close(fid)
      return

 9200 close(fid)
      if (present(status)) status=1
      
      return

 9000 write(*,*) this,'can not open ',self%filename
      if (present(status)) status=-1
      if (self%errorexit>0) stop  ERROR_CODE

      return


      end subroutine getkeyvalue_i

c----------------------------------------------------------
      subroutine getkeyvalue_r8(self, key0,var, default, status)
      class(t_keyvalue):: self
      character(*),intent(in) :: key0
      real(8),intent(out):: var
      real(8),optional :: default
      integer,optional :: status
      
      character(120):: key 
      integer:: fid
      character(16)::  this='getkeyvalue_r8: '

      fid = unusedfid()

      if (fid<0) then
         write(*,*) this,'can not find fileID ',self%filename
         if (present(status)) status=-1
         return
      endif

      open(file=self%filename,unit=fid,status='old',err=9000)

      if (present(default))then
         var = default
      endif

      key=key0
      call input_toupper(key)
      
      do while (.true.)

         read(fid,'(a120)',end=9100,err=9100)  buf
         call input_delfirstspc(buf)        
         if (buf(1:1)=='#') cycle

         call input_septoken(buf,2,token)
         call input_toupper(token(1))
c     write(*,*) token(1) 
         if ( token(1).eq.key ) then
            read(token(2),*) var
            goto 9200
         endif

      enddo

 9100 if (present(default)) then
         if (present(status)) status=0
      else 
         write(stdout,*) this,'can not find keyword ', key0
         if (present(status)) status=-1
      endif
      close(fid)
      return

 9200 close(fid)
      if (present(status)) status=1

      return

 9000 write(*,*)this, 'can not open ',self%filename
      if (present(status)) status=-1
      if (self%errorexit>0) stop  ERROR_CODE

      return


      end subroutine getkeyvalue_r8

c----------------------------------------------------------
      subroutine getkeyvalue_l(self, key0,var, default, status)
      class(t_keyvalue):: self

      character(*),intent(in) :: key0
      logical,intent(out):: var
      logical,optional :: default
      integer,optional :: status

      character(120):: key
      integer:: fid,iname
      character(15):: this='getkeyvalue_l: '

      fid = unusedfid()

      if (fid<0) then
         write(*,*) this,'can not find fileID ',self%filename
         if (present(status)) status=-1
         return
      endif

      open(file=self%filename,unit=fid,status='old',err=9000)

      if (present(default))then
         var = default
      endif

      key=key0
      call input_toupper(key)

      do while (.true.)

         read(fid,'(a120)',end=9100,err=9100)  buf
         call input_delfirstspc(buf)
         if (buf(1:1)=='#') cycle

         call input_septoken(buf,2,token)
         call input_toupper(token(1))
c     write(*,*) token(1)
         if ( token(1).eq.key ) then
            do iname=1, nlogical
               call input_toupper(token(2))
               if ( token(2) .eq. logicalstr(iname) ) then
                  var=  logicalvalue(iname) 
                  if (present(status)) status=1
                  goto 9200
               endif
            enddo
            write(stdout,*) this,'value is not logical for key ',key
            if (present(status)) status=-1
            close(fid)
         endif

      enddo

 9100 if (present(default)) then
         if (present(status)) status=0
      else 
         write(stdout,*) this,': can not find keyword ', key0
         if (present(status)) status=-1
      endif
      close(fid)
      return

 9200 close(fid)
      if (present(status)) status=1

      return

 9000 write(*,*) this,'can not open ',self%filename
      if (present(status)) status=-1
      if (self%errorexit>0) stop  ERROR_CODE

      return


      end subroutine getkeyvalue_l

c----------------------------------------------------------

      subroutine getkeyvalue_c(self,key0,var, default,status)
      class(t_keyvalue):: self

      character(*),intent(in) :: key0
      character(*),optional:: var
      character(*),optional :: default
      integer,optional :: status

      character(120):: key 
      integer:: fid
      character(15):: this='getkeyvalue_c: '

      fid = unusedfid()

      if (fid<0) then
         write(*,*) this,'can not find fileID ',self%filename
         if (present(status)) status=-1
         return
      endif

      open(file=self%filename,unit=fid,status='old',err=9000)

      var=' '
      if (present(default))then
         var = default
      endif

      key=key0
      call input_toupper(key)
      
      do while (.true.)

         read(fid,'(a120)',end=9100,err=9100)  buf
         call input_delfirstspc(buf)        
         if (buf(1:1)=='#') cycle

         call input_septoken(buf,2,token)
         call input_toupper(token(1))
c     write(*,*) token(1) 
         if ( token(1).eq.key ) then
            var=token(2)
            goto 9200
         endif

      enddo

 9100 if (present(default)) then
         if (present(status)) status=0
      else
         write(stdout,*) this,'can not find keyword ', key0
         if (present(status)) status=-1
      endif
      close(fid)
      return

 9200 close(fid)
      if (present(status)) status=1

      return

 9000 write(*,*) this,'can not open ',self%filename
      if (present(status)) status=-1
      if (self%errorexit>0) stop  ERROR_CODE

      return
      
      end subroutine getkeyvalue_c
c-------------------------------------------------------------
      subroutine getkeyblock(self,key0,unit,n,status)
      class(t_keyvalue):: self 
      character(*),intent(in) :: key0
      integer,intent(out):: unit
      integer,intent(out)::  n
      integer,optional:: status

      character(120):: key
      integer:: fid

      character(19)::this='getkeyvalue_block: '

      fid = unusedfid()

      if (fid<0) then
         write(*,*) this,'can not find fileID ',self%filename
         if (present(status)) status=-1
         return
      endif

      open(file=self%filename,unit=fid,status='old',err=9000)

      key=key0
      call input_toupper(key)

      do while (.true.)

         read(fid,'(a120)',end=9100,err=9100)  buf
         call input_delfirstspc(buf)
         if (buf(1:1)=='#') cycle

         call input_septoken(buf,2,token)
         call input_toupper(token(1))
c     write(*,*) token(1)
         if ( token(1).eq.key ) then
            goto 9200
         endif

      enddo

 9100 continue
      close(fid)
      write(*,*) this,'can not find key ', key0
      return

 9200 key=key0
      call input_toupper(key)
      call input_endkey(key)
      n=0
      do while (.true.)
         read(fid,'(a120)',end=9100,err=9100)  buf
         call input_delfirstspc(buf)
         call input_septoken(buf,2,token)
         call input_toupper(token(1))
         if ( token(1).eq.key ) exit
         n=n+1
      enddo

c     rewind and find the key again
      key=key0
      call input_toupper(key)
      rewind(fid)
      do while (.true.)

         read(fid,'(a120)',end=9100,err=9100)  buf
         call input_delfirstspc(buf)
         if (buf(1:1)=='#') cycle

         call input_septoken(buf,2,token)
         call input_toupper(token(1))
         if ( token(1).eq.key ) exit
      enddo

      if (present(status)) status=1
      unit = fid

      return

 9000 write(*,*) this,'can not open ',self%filename
      if (present(status)) status=-1
      if (self%errorexit>0) stop  ERROR_CODE

      return

      end subroutine getkeyblock


c------------------------------------------------------------
      subroutine getkeyvalue_iv(self, key0,var,n, default, status)
      class(t_keyvalue):: self

      character(*),intent(in) :: key0
      integer,intent(out):: var(:)
      integer,optional:: n
      integer,optional :: default(:)
      integer,optional :: status
      
      character(120):: key 
      integer:: fid,i,n1
      character(16):: this='getkeyvalue_iv: '

      fid = unusedfid()

      if (fid<0) then
         write(*,*) this,'can not find fileID ',self%filename
         if (present(status)) status=-1
         return
      endif

      open(file=self%filename,unit=fid,status='old',err=9000)


      if (present(n))then
         n1=n
      else
         n1=size(var)
      endif
      if (present(default))then
         var(1:n1) = default(1:n1)
      endif

      key=key0
      call input_toupper(key)

      allocate(vtoken(n1+1) )
      
      do while (.true.)

         read(fid,'(a120)',end=9100,err=9100)  buf
         call input_delfirstspc(buf)        
         if (buf(1:1)=='#') cycle

         call input_septoken(buf,n1+1,vtoken)
         call input_toupper(vtoken(1))
c     write(*,*) token(1) 
         if ( vtoken(1).eq.key ) then
            do i=1,n1
c     write(*,*)i, vtoken(i+1) 
               read(vtoken(i+1),*,end=9500,err=9500) var(i)
            enddo
            goto 9200
         endif

      enddo

 9100 if (present(default)) then
         if (present(status)) status=0
      else
         write(stdout,*) this,'can not find keyword ', key0
         if (present(status)) status=-1
      endif
      close(fid)
      if (allocated(vtoken)) deallocate(vtoken)
      return

 9200 close(fid)
      if (present(status)) status=n1
      if (allocated(vtoken)) deallocate(vtoken)
      return

 9000 write(*,*) this,'can not open ',self%filename
      if (present(status)) status=-1
      if (allocated(vtoken)) deallocate(vtoken)
      if (self%errorexit>0) stop  ERROR_CODE
      return

 9500 write(stdout,*) this, 'can not read all the items for key ', key0
      if (present(status)) status= i
      close(fid)
      if (allocated(vtoken)) deallocate(vtoken)

      end subroutine getkeyvalue_iv

c-------------------------------------------------------------
      subroutine getkeyvalue_rv(self, key0,var,n, default, status)
      class(t_keyvalue):: self

      character(*),intent(in) :: key0
      integer,optional,intent(in):: n
      real(8),intent(out):: var(:)
      real(8),optional :: default(:)
      integer,optional :: status
      
      character(120):: key 
      integer:: fid,i,n1
      character(16):: this='getkeyvalue_rv: '

      fid = unusedfid()

      if (fid<0) then
         write(*,*) this,'can not find fileID ',self%filename
         if (present(status)) status=-1
         return
      endif

      open(file=self%filename,unit=fid,status='old',err=9000)

      if (present(n))then
         n1=n
      else
         n1=size(var)
      endif 
      if (present(default))then
         var(1:n1) = default(1:n1)
      endif

      key=key0
      call input_toupper(key)

      allocate(vtoken(n1+1) )
      
      do while (.true.)

         read(fid,'(a120)',end=9100,err=9100)  buf
         call input_delfirstspc(buf)        
         if (buf(1:1)=='#') cycle

         call input_septoken(buf,n1+1,vtoken)
         call input_toupper(vtoken(1))
c     write(*,*) token(1) 
         if ( vtoken(1).eq.key ) then
            do i=1,n1
c     write(*,*)i, vtoken(i+1) 
               read(vtoken(i+1),*,end=9500,err=9500) var(i)
            enddo
            goto 9200
         endif

      enddo

 9100 if ( present(default) ) then
         if (present(status)) status=0
      else
         write(stdout,*) this,'can not find keyword ', key0
         if (present(status)) status=-1
      endif
      close(fid)
      if (allocated(vtoken)) deallocate(vtoken)
      return

 9200 close(fid)
      if (present(status)) status=n
      if (allocated(vtoken)) deallocate(vtoken)
      return

 9000 write(*,*)this, 'can not open ',self%filename
      if (present(status)) status=-1
      if (allocated(vtoken)) deallocate(vtoken)
      if (self%errorexit>0) stop  ERROR_CODE
      return

 9500 write(stdout,*) this,'can not read all the items for key ', key0
      if (present(status)) status= i
      close(fid)
      if (allocated(vtoken)) deallocate(vtoken)

      end subroutine getkeyvalue_rv


c-------------------------------------------------------------
      subroutine getkeyvalue_cv(self, key0,var,n, default, status)
      class(t_keyvalue):: self

      character(*),intent(in) :: key0
      integer,optional,intent(in):: n
      character(*),intent(out):: var(:)
      character(*),optional :: default(:)
      integer,optional :: status
      
      character(120):: key 
      integer:: fid,i,n1
      character(16):: this='getkeyvalue_rv: '

      fid = unusedfid()

      if (fid<0) then
         write(*,*) this,'can not find fileID ',self%filename
         if (present(status)) status=-1
         return
      endif

      open(file=self%filename,unit=fid,status='old',err=9000)


      if (present(n))then
         n1 =  n
      else
         n1 = size(var)
      endif
      if (present(default))then
         var(1:n1) = default(1:n1)
      endif

      key=key0
      call input_toupper(key)

      allocate(vtoken(n1+1) )
      
      do while (.true.)

         read(fid,'(a120)',end=9100,err=9100)  buf
         call input_delfirstspc(buf)        
         if (buf(1:1)=='#') cycle

         call input_septoken(buf,n1+1,vtoken)
         call input_toupper(vtoken(1))
c     write(*,*) token(1) 
         if ( vtoken(1).eq.key ) then
            do i=1,n1
!!!   write(*,*)"getkeyvalue_cv",i, vtoken(i+1) 
               read(vtoken(i+1),*,end=9500,err=9500) var(i)
            enddo
            goto 9200
         endif

      enddo

 9100 if ( present(default) ) then
         if (present(status)) status=0
      else
         write(stdout,*) this,'can not find keyword ', key0
         if (present(status)) status=-1
      endif
      close(fid)
      if (allocated(vtoken)) deallocate(vtoken)
      return

 9200 close(fid)
      if (present(status)) status=n1
      if (allocated(vtoken)) deallocate(vtoken)
      return

 9000 write(*,*)this, 'can not open ',1H",trim(self%filename),1H"
      if (present(status)) status=-1
      if (allocated(vtoken)) deallocate(vtoken)
      if (self%errorexit>0) stop  ERROR_CODE
      return

 9500 write(stdout,*) this,'can not read all the items for key ', key0
      if (present(status)) status= i
      close(fid)
      if (allocated(vtoken)) deallocate(vtoken)

      end subroutine getkeyvalue_cv







c------------------------------------------------------------

      end module m_keyvalue

c-----------------------------------------------------------

#if  0
      program main
      use keyvalue
      implicit none
      integer :: ikey, ret, ivkey(10),unit,i
      real(8):: rkey, rvkey(10),rvkeydefault(10)
      logical :: lkey
      character(200):: ckey

      character(20)::filename

      filename='IN'


      call getkeyvalue(filename,"intvalue",ikey,status=ret)
      write(*,*) "intvalue",ret,ikey

      call getkeyvalue(filename,"realvalue",rkey,status=ret)
      write(*,*) "realvalue",ret,rkey

      call getkeyvalue(filename,"logical",lkey,status=ret )
      write(*,*) "logical",ret,lkey

      call getkeyvalue(filename,"char",ckey,status=ret)
      write(*,*) "char",ret,ckey(:len_trim(ckey))

      call getkeyvalue(filename,"char2",ckey,status=ret)
      write(*,*) "char2",ret,ckey(:len_trim(ckey))
      call getkeyvalue(filename,"real2",rkey,status=ret)
      write(*,*) "real2",ret,rkey


      call getkeyvalue(filename,"intv",ivkey,n=5,status=ret )
      write(*,*) "intv",ret,ivkey(1:5)

      call getkeyvalue(filename,"realv",rvkey,n=3,status=ret )
      write(*,*) "realv",ret,rvkey(1:3)

      rvkeydefault(1:3)=(/-1.0d0, -2.0d0, -3.0d0/)
      call getkeyvalue(filename,"realv2",rvkey,n=3,default=rvkeydefault,status=ret )
      write(*,*) "realv2",ret,rvkey(1:3)

      call getkeyvalue(filename,"real3", rkey,default=100.01d0,status=ret)
      write(*,*) "real3",ret,rkey


      call getkeyblock(filename,"<block>", unit=unit, n=ikey, status=ret)
      write(*,*) "<block>",ret, ikey, unit
      do i=1,ikey
         read(unit,'(a80)') ckey
         write(*,*)ckey(:len_trim(ckey))
      enddo
      close(unit)

      call getkeyvalue(filename,"Temperature",rkey,default=500.0d0,status=ret)
      write(*,*) "temperature",ret,rkey
      call getkeyvalue(filename,"Velocity",rkey,default=-500.0d0,status=ret)
      write(*,*) "velocity",ret,rkey


      end 

#endif

c     The data in the <blockname> is a bit special because getkeyvalue just returns
c     file hundle (and line numbers.) to read the contents. 
c     In order to use getkeyvalue, you first have to declear "use keyvalue".
c     See main/kino_input_test.f with filename file to see how it works.
c     
c     1.      call getkeyvalue(filename,"int_1",ikey,default=999,status=ret )
c     2.      call getkeyvalue(filename,"delta",rkey,default=1430d0,status=ret)
c     
c     3.     logical:: lkey
c     call getkeyvalue(filename,"L_test",lkey,status=ret )
c     
c     4.     integer(4):: ivkey(5),nsize=5
c     call getkeyvalue(filename,"n1n2n3",ivkey,nsize,status=ret )
c     
c     5.     real(8):: rvkey(3) 
c     integer(4)::nsize=3
c     call getkeyvalue(filename,"vv", rvkey,nsize,status=ret,
c     &                 default=(/10.0d0,10.0d0,10.0d0/))
c     : Note nsize is required. If default is not assigned, this cause stop 
c     within getkeyvalue.
c     
c     6.    call getkeyvalue(filename,"char", ckey, status=ret)
c     7.    call getkeyblock(filename,"<blockname>",unit=file,n=nlines,status=ret)
c     : unit is the file handle to read contants sandwitched by
c     <blockname> and </blockname>. nlines= number of the lines.
c     
c     
c     ret>0: ret is the number of variables succesfully read
c     ret=0: no key found in filename, but variables are set from "default" 
c     
c     ret<0: no key found in "IN, and no default variables are set. (This case is an error.)
c     
c     


c     a sample IN file
#if 0
      int 1
      real 2.0
      logical true
      intv    1 2 3  4 5 6 7
      realv  1.0 2.0 3.0 4.0
      <block>
      test
      test22
      </block> 

## method.scf    simple   #<- comment
c     har test1.dat

      filename Kondo.dat
      temperature 300

#endif




!----------------------------------------------------------------------------------------
subroutine OUTPUT(atype, pos, v, q, fileNameBase)
use atoms 
!----------------------------------------------------------------------------------------
implicit none

real(8),intent(in) :: atype(NBUFFER), q(NBUFFER)
real(8),intent(in) :: pos(NBUFFER,3),v(NBUFFER,3)
character(MAXPATHLENGTH),intent(in) :: fileNameBase

if(isBinary) then
  call WriteBIN(atype,pos,v,q,fileNameBase)
endif

if(isBondFile) call WriteBND(fileNameBase)
if(isPDB) call WritePDB(fileNameBase)

return

Contains 

!--------------------------------------------------------------------------
subroutine WriteBND(fileNameBase)
!--------------------------------------------------------------------------
implicit none

character(MAXPATHLENGTH),intent(in) :: fileNameBase

integer :: i, ity, j, j1, jty, m
integer :: l2g
real(8) :: bndordr(MAXNEIGHBS)
integer :: igd,jgd,bndlist(0:MAXNEIGHBS)

integer (kind=MPI_OFFSET_KIND) :: offset
integer (kind=MPI_OFFSET_KIND) :: fileSize
integer :: localDataSize
integer :: fh ! file handler

integer :: BNDLineSize, baseCharPerAtom
integer,parameter :: MaxBNDLineSize=4096
character(MaxBNDLineSize) :: BNDOneLine
real(8),parameter :: BNDcutoff=0.3d0

character(len=:),allocatable :: BNDAllLines

integer :: scanbuf

integer :: ti,tj,tk
call system_clock(ti,tk)

! precompute the total # of neighbors
m=0
do i=1, NATOMS
   do j1 = 1, nbrlist(i,0)
!--- don't count if BO is less than BNDcutoff.
       if(BO(0,i,j1) > BNDcutoff) then 
           m=m+1
       endif
   enddo
enddo

200 format(i12.12,1x,3f12.3,1x,2i3,20(1x,i12.12,f6.3)) 

! get local datasize based on above format and the total # of neighbors
baseCharPerAtom=12+1+3*12+1+2*3 +1 ! last 1 for newline
localDataSize=NATOMS*(baseCharPerAtom)+m*(1+12+6)

if( (baseCharPerAtom+MAXNEIGHBS*(1+12+6)) > MaxBNDLineSize) then
    print'(a,i6,2i12)', 'ERROR: MaxBNDLineSize is too small @ WriteBND', &
                    myid, baseCharPerAtom+MAXNEIGHBS*(1+12+6), MaxBNDLineSize
endif

call MPI_File_Open(MPI_COMM_WORLD,trim(fileNameBase)//".bnd", &
    MPI_MODE_WRONLY+MPI_MODE_CREATE,MPI_INFO_NULL,fh,ierr)

! offset will point the end of local write after the scan
call MPI_Scan(localDataSize,scanbuf,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)

! since offset is MPI_OFFSET_KIND and localDataSize is integer, use an integer as buffer
offset=scanbuf

! nprocs-1 rank has the total data size
call MPI_Bcast(scanbuf,1,MPI_INTEGER,nprocs-1,MPI_COMM_WORLD,ierr)
fileSize=scanbuf

call MPI_File_set_size(fh, fileSize, ierr)

! set offset at the beginning of the local write
offset=offset-localDataSize

call MPI_File_Seek(fh,offset,MPI_SEEK_SET,ierr)

allocate(character(len=localDataSize) :: BNDAllLines)
BNDALLLines=""

BNDLineSize=0
do i=1, NATOMS
   ity = nint(atype(i))
!--- get global ID for i-atom
   igd = l2g(atype(i))

!--- count the number bonds to be shown.
   bndlist(0)=0
   do j1 = 1, nbrlist(i,0)
      j = nbrlist(i,j1)
      jty = nint(atype(j))

!--- get global ID for j-atom
      jgd = l2g(atype(j))

!--- if bond order is less than 0.3, ignore the bond.
      if( BO(0,i,j1) < 0.3d0) cycle

      bndlist(0) = bndlist(0) + 1
      bndlist(bndlist(0)) = jgd
      bndordr(bndlist(0)) = BO(0,i,j1)
   enddo

   BNDOneLine=""
   write(BNDOneLine,200) igd, pos(i,1:3),nint(atype(i)),bndlist(0), &
         (bndlist(j1),bndordr(j1),j1=1,bndlist(0))

   ! remove space and add new_line
   BNDOneLine=trim(adjustl(BNDOneLine))//NEW_LINE('A')
   BNDLineSize=BNDLineSize+len(trim(BNDOneLine))

   BNDAllLines=trim(BNDAllLines)//trim(BNDOneLine)
enddo

if(localDataSize>0) then
   call MPI_File_Write(fh,BNDAllLines,localDataSize, &
        MPI_CHARACTER,MPI_STATUS_IGNORE,ierr)
endif

deallocate(BNDAllLines) 

call MPI_BARRIER(MPI_COMM_WORLD, ierr)
call MPI_File_Close(fh,ierr)

call system_clock(tj,tk)
it_timer(20)=it_timer(20)+(tj-ti)

return
end subroutine

!--------------------------------------------------------------------------
subroutine WritePDB(fileNameBase)
use parameters
!--------------------------------------------------------------------------
implicit none

character(MAXPATHLENGTH),intent(in) :: fileNameBase

integer :: i, ity, igd, l2g
real(8) :: tt=0.d0, ss=0.d0

integer (kind=MPI_OFFSET_KIND) :: offset
integer (kind=MPI_OFFSET_KIND) :: fileSize
integer :: localDataSize
integer :: fh ! file handler

integer,parameter :: PDBLineSize=67
character(PDBLineSize) :: PDBOneLine

character(len=:),allocatable :: PDBAllLines

integer :: scanbuf

integer :: ti,tj,tk
call system_clock(ti,tk)

! get local datasize
localDataSize=NATOMS*PDBLineSize

call MPI_File_Open(MPI_COMM_WORLD,trim(fileNameBase)//".pdb", &
     MPI_MODE_WRONLY+MPI_MODE_CREATE,MPI_INFO_NULL,fh,ierr)

! offset will point the end of local write after the scan
call MPI_Scan(localDataSize,scanbuf,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)

! since offset is MPI_OFFSET_KIND and localDataSize is integer, use an integer as buffer
offset=scanbuf

! nprocs-1 rank has the total data size
call MPI_Bcast(scanbuf,1,MPI_INTEGER,nprocs-1,MPI_COMM_WORLD,ierr)
fileSize=scanbuf

call MPI_File_set_size(fh, fileSize, ierr)

! set offset at the beginning of the local write
offset=offset-localDataSize

allocate(character(len=localDataSize) :: PDBAllLines)
PDBAllLines=""

call MPI_File_Seek(fh,offset,MPI_SEEK_SET,ierr)

do i=1, NATOMS

  ity = nint(atype(i))
!--- calculate atomic temperature 
  tt = hmas(ity)*sum(v(i,1:3)*v(i,1:3))
  tt = tt*UTEMP*1d-2 !scale down to use two decimals in PDB format 

!--- sum up diagonal atomic stress components 
#ifdef STRESS
  ss = sum(astr(1:3,i))/3.d0
#endif
  ss = ss*USTRS

  ss = q(i)*10 ! 10x atomic charge

  igd = l2g(atype(i))
  write(PDBOneLine,100)'ATOM  ',0, atmname(ity), igd, pos(i,1:3), tt, ss

  PDBOneLine(PDBLineSize:PDBLineSize)=NEW_LINE('A')
  PDBAllLines=trim(PDBAllLines)//trim(PDBOneLine)

enddo

if(localDataSize>0) then
    call MPI_File_Write(fh,PDBAllLines,localDataSize, &
         MPI_CHARACTER,MPI_STATUS_IGNORE,ierr)
endif

deallocate(PDBAllLines)

call MPI_BARRIER(MPI_COMM_WORLD, ierr)
call MPI_File_Close(fh,ierr)

100 format(A6,I5,1x,A2,i12,4x,3f8.3,f6.2,f6.2)

call system_clock(tj,tk)
it_timer(21)=it_timer(21)+(tj-ti)


end subroutine

end subroutine OUTPUT

!--------------------------------------------------------------------------
subroutine ReadMoS2(atype, rreal, v, q, f, fileName)
use atoms; use MemoryAllocator
!--------------------------------------------------------------------------
implicit none

character(*),intent(in) :: fileName
real(8),allocatable,dimension(:),intent(inout) :: atype,q
real(8),allocatable,dimension(:,:),intent(inout) :: rreal,v,f

integer :: i,i1

integer (kind=MPI_OFFSET_KIND) :: offset, offsettmp
integer (kind=MPI_OFFSET_KIND) :: fileSize
integer :: localDataSize, metaDataSize, scanbuf
integer :: fh ! file handler

integer :: nmeta
integer,allocatable :: idata(:)
real(8),allocatable :: dbuf(:)
real(8) :: ddata(6), d10(10)

real(8) :: rnorm(NBUFFER,3), mat(3,3)
integer :: j

!=== # of unit cells ===
integer :: mx=4,my=4,mz=4

integer :: ix,iy,iz,ntot, imos2, iigd

integer :: ti,tj,tk

integer,parameter :: nMoS2=24
real(8) :: pos0(nMoS2*3)
integer :: atype0(nMoS2)

call system_clock(ti,tk)

!--- allocate arrays
if(.not.allocated(atype)) call allocatord1d(atype,1,NBUFFER)
if(.not.allocated(q)) call allocatord1d(q,1,NBUFFER)
if(.not.allocated(rreal)) call allocatord2d(rreal,1,NBUFFER,1,3)
if(.not.allocated(v)) call allocatord2d(v,1,NBUFFER,1,3)
if(.not.allocated(f)) call allocatord2d(f,1,NBUFFER,1,3)
if(.not.allocated(qsfp)) call allocatord1d(qsfp,1,NBUFFER)
if(.not.allocated(qsfv)) call allocatord1d(qsfv,1,NBUFFER)
f(:,:)=0.0

pos0=(/ &
0.499975,0.333350,0.250000,0.749975,0.833350,0.250000,&
0.250025,0.166650,0.750000,0.000025,0.666650,0.750000,&
0.750025,0.166650,0.750000,0.500025,0.666650,0.750000,&
0.249975,0.833350,0.250000,0.999975,0.333350,0.250000,&
0.500025,0.666650,0.121000,0.250025,0.166650,0.379000,&
0.000025,0.666650,0.379000,0.750025,0.166650,0.379000,&
0.500025,0.666650,0.379000,0.499975,0.333350,0.879000,&
0.249975,0.833350,0.879000,0.999975,0.333350,0.879000,&
0.749975,0.833350,0.879000,0.499975,0.333350,0.621000,&
0.249975,0.833350,0.621000,0.999975,0.333350,0.621000,&
0.749975,0.833350,0.621000,0.250025,0.166650,0.121000,&
0.000025,0.666650,0.121000,0.750025,0.166650,0.121000/)

atype0=(/ &
3,3,3,3, 3,3,3,3, &
2,2,2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2 /)

!--- local unit cell parameters
lata=6.30000d0
latb=5.45596d0
latc=12.3000d0
lalpha=90.0000d0
lbeta=90.0000d0
lgamma=90.0000d0

iigd = mx*my*mz*nMoS2*myid ! for global ID
ntot=0
do ix=0,mx-1
do iy=0,my-1
do iz=0,mz-1
   do imos2=1,nMoS2
      ntot=ntot+1
      rreal(ntot,1:3) = pos0(3*imos2-2:3*imos2)+(/ix,iy,iz/)  ! repeat unit cell
      rreal(ntot,1:3) = rreal(ntot,1:3)+vID(1:3)*(/mx,my,mz/) ! adding the box origin
      rreal(ntot,1:3) = rreal(ntot,1:3)*(/lata,latb,latc/) ! real coords
      atype(ntot) = dble(atype0(imos2)) + (iigd+ntot)*1d-13
   enddo 
enddo; enddo; enddo
NATOMS=ntot

!--- update to glocal cell parameters
lata=lata*mx*vprocs(1)
latb=latb*my*vprocs(2)
latc=latc*mz*vprocs(3)

call GetBoxParams(mat,lata,latb,latc,lalpha,lbeta,lgamma)
do i=1, 3
do j=1, 3
   HH(i,j,0)=mat(i,j)
enddo; enddo
call UpdateBoxParams()

call system_clock(tj,tk)
it_timer(22)=it_timer(22)+(tj-ti)

return
end

!--------------------------------------------------------------------------
subroutine ReadBIN(atype, rreal, v, q, f, fileName)
use atoms; use MemoryAllocator
!--------------------------------------------------------------------------
implicit none

character(*),intent(in) :: fileName
real(8),allocatable,dimension(:) :: atype,q
real(8),allocatable,dimension(:,:) :: rreal,v,f

integer :: i,i1

integer (kind=MPI_OFFSET_KIND) :: offset, offsettmp
integer (kind=MPI_OFFSET_KIND) :: fileSize
integer :: localDataSize, metaDataSize, scanbuf
integer :: fh ! file handler

integer :: nmeta
integer,allocatable :: idata(:)
real(8),allocatable :: dbuf(:)
real(8) :: ddata(6), d10(10)

real(8) :: rnorm(NBUFFER,3), mat(3,3)
integer :: j

integer :: ti,tj,tk
call system_clock(ti,tk)


! Meta Data: 
!  Total Number of MPI ranks and MPI ranks in xyz (4 integers)
!  Number of resident atoms per each MPI rank (nprocs integers) 
!  current step (1 integer) + lattice parameters (6 doubles)

nmeta=4+nprocs+1
allocate(idata(nmeta))
metaDataSize = 4*nmeta + 8*6

call MPI_File_Open(MPI_COMM_WORLD,trim(fileName),MPI_MODE_RDONLY,MPI_INFO_NULL,fh,ierr)

! read metadata at the beginning of file
offsettmp=0
call MPI_File_Seek(fh,offsettmp,MPI_SEEK_SET,ierr)
call MPI_File_Read(fh,idata,nmeta,MPI_INTEGER,MPI_STATUS_IGNORE,ierr)

offsettmp=4*nmeta
call MPI_File_Seek(fh,offsettmp,MPI_SEEK_SET,ierr)
call MPI_File_Read(fh,ddata,6,MPI_DOUBLE_PRECISION,MPI_STATUS_IGNORE,ierr)

NATOMS = idata(4+myid+1)
current_step = idata(nmeta)
deallocate(idata)
lata=ddata(1); latb=ddata(2); latc=ddata(3)
lalpha=ddata(4); lbeta=ddata(5); lgamma=ddata(6)

! Get local datasize: 10 doubles for each atoms
localDataSize = 8*NATOMS*10

! offset will point the end of local write after the scan
call MPI_Scan(localDataSize,scanbuf,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)

! Since offset is MPI_OFFSET_KIND and localDataSize is integer, use an integer as buffer
offset = scanbuf + metaDataSize

! nprocs-1 rank has the total data size
fileSize = offset
!call MPI_Bcast(fileSize,1,MPI_INTEGER,nprocs-1,MPI_COMM_WORLD,ierr)
!call MPI_File_set_size(fh, fileSize, ierr)

! set offset at the beginning of the local write
offset=offset-localDataSize
call MPI_File_Seek(fh,offset,MPI_SEEK_SET,ierr)

allocate(dbuf(10*NATOMS))
call MPI_File_Read(fh,dbuf,10*NATOMS,MPI_DOUBLE_PRECISION,MPI_STATUS_IGNORE,ierr)

if(.not.allocated(atype)) call allocatord1d(atype,1,NBUFFER)
if(.not.allocated(q)) call allocatord1d(q,1,NBUFFER)
if(.not.allocated(rreal)) call allocatord2d(rreal,1,NBUFFER,1,3)
if(.not.allocated(v)) call allocatord2d(v,1,NBUFFER,1,3)
if(.not.allocated(f)) call allocatord2d(f,1,NBUFFER,1,3)
if(.not.allocated(qsfp)) call allocatord1d(qsfp,1,NBUFFER)
if(.not.allocated(qsfv)) call allocatord1d(qsfv,1,NBUFFER)
f(:,:)=0.0

do i=1, NATOMS
    i1=10*(i-1)
    rnorm(i,1:3)=dbuf(i1+1:i1+3)
    v(i,1:3)=dbuf(i1+4:i1+6)
    q(i)=dbuf(i1+7)
    atype(i)=dbuf(i1+8)
    qsfp(i)=dbuf(i1+9)
    qsfv(i)=dbuf(i1+10)
enddo
deallocate(dbuf)

call MPI_BARRIER(MPI_COMM_WORLD, ierr)
call MPI_File_Close(fh,ierr)

call GetBoxParams(mat,lata,latb,latc,lalpha,lbeta,lgamma)
do i=1, 3
do j=1, 3
   HH(i,j,0)=mat(i,j)
enddo; enddo
call UpdateBoxParams()

call xs2xu(rnorm,rreal,NATOMS)

call system_clock(tj,tk)
it_timer(22)=it_timer(22)+(tj-ti)

return
end

!--------------------------------------------------------------------------
subroutine WriteBIN(atype, rreal, v, q, fileNameBase)
use atoms
!--------------------------------------------------------------------------
implicit none

real(8),intent(in) :: atype(NBUFFER), q(NBUFFER)
real(8),intent(in) :: rreal(NBUFFER,3),v(NBUFFER,3)
character(MAXPATHLENGTH),intent(in) :: fileNameBase

integer :: i,j

integer (kind=MPI_OFFSET_KIND) :: offset, offsettmp
integer :: localDataSize, metaDataSize, scanbuf
integer :: fh ! file handler

integer :: nmeta
integer,allocatable :: ldata(:),gdata(:)
real(8) :: ddata(6)
real(8),allocatable :: dbuf(:)

real(8) :: rnorm(NBUFFER,3)

integer :: ti,tj,tk
call system_clock(ti,tk)

call xu2xs(rreal,rnorm,NATOMS)

if(.not. isBinary) return

! Meta Data: 
!  Total Number of MPI ranks and MPI ranks in xyz (4 integers)
!  Number of resident atoms per each MPI rank (nprocs integers) 
!  current step (1 integer) + lattice parameters (6 doubles)
nmeta=4+nprocs+1
metaDataSize = 4*nmeta + 8*6

! Get local datasize: 10 doubles for each atoms
localDataSize = 8*NATOMS*10

call MPI_File_Open(MPI_COMM_WORLD,trim(fileNameBase)//".bin",MPI_MODE_WRONLY+MPI_MODE_CREATE,MPI_INFO_NULL,fh,ierr)

! offset will point the end of local write after the scan
call MPI_Scan(localDataSize,scanbuf,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)

! Since offset is MPI_OFFSET_KIND and localDataSize is integer, use an integer as buffer
offset = scanbuf + metaDataSize

! save metadata at the beginning of file
allocate(ldata(nmeta),gdata(nmeta))
ldata(:)=0
ldata(4+myid+1)=NATOMS
call MPI_ALLREDUCE(ldata,gdata,nmeta,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)
gdata(1)=nprocs
gdata(2:4)=vprocs
gdata(nmeta)=nstep+current_step

ddata(1)=lata; ddata(2)=latb; ddata(3)=latc
ddata(4)=lalpha; ddata(5)=lbeta; ddata(6)=lgamma

if(myid==0) then
   offsettmp=0
   call MPI_File_Seek(fh,offsettmp,MPI_SEEK_SET,ierr)
   call MPI_File_Write(fh,gdata,nmeta,MPI_INTEGER,MPI_STATUS_IGNORE,ierr)

   offsettmp=4*nmeta
   call MPI_File_Seek(fh,offsettmp,MPI_SEEK_SET,ierr)
   call MPI_File_Write(fh,ddata,6,MPI_DOUBLE_PRECISION,MPI_STATUS_IGNORE,ierr)
endif
deallocate(ldata,gdata)

! set offset at the beginning of the local write
offset=offset-localDataSize
call MPI_File_Seek(fh,offset,MPI_SEEK_SET,ierr)

allocate(dbuf(10*NATOMS))
do i=1, NATOMS
   j = (i - 1)*10
   dbuf(j+1:j+3)=rnorm(i,1:3)
   dbuf(j+4:j+6)=v(i,1:3)
   dbuf(j+7)=q(i)
   dbuf(j+8)=atype(i)
   dbuf(j+9)=qsfp(i)
   dbuf(j+10)=qsfv(i)
enddo
call MPI_File_Write(fh,dbuf,10*NATOMS,MPI_DOUBLE_PRECISION,MPI_STATUS_IGNORE,ierr)
deallocate(dbuf)

call MPI_BARRIER(MPI_COMM_WORLD, ierr)
call MPI_File_Close(fh,ierr)

call system_clock(tj,tk)
it_timer(23)=it_timer(23)+(tj-ti)

return
end


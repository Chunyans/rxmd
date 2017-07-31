module multitask_funcs
use mpi_vars

integer,parameter :: NUMTASKS=1

type multitask_var_type
   integer :: taskId = 0
   integer :: teamId = 0
   type(mpi_var_type) :: world
   type(mpi_var_type) :: team
   type(mpi_var_type) :: task 
end type

contains

!-------------------------------------------------------------------------------------------
subroutine initialize_multitask(mpt, mtt, ntasks)
use mpi_vars
!-------------------------------------------------------------------------------------------
implicit none

type(mpi_var_type),intent(in) :: mpt
type(multitask_var_type),intent(inout) :: mtt
integer,intent(in) :: ntasks 

integer :: ierr

!--- sanity check
if(mod(mpt%nprocs,ntasks)/=0) then
  if(mpt%myid==0) print'(a,2i6)', & 
    'Total number of procs is not multiple of ntasks',mpt%nprocs,ntasks
  call MPI_Finalize(ierr)
  stop
endif

!--- save the global communicator
call copy_mpi_var_type(mpt, mtt%world)

!--- split MPI ranks into two-ways, i.e. multi-tasking teams & tasks.
mtt%taskId = mod(mtt%world%myid,ntasks)
mtt%teamId = mtt%world%myid/ntasks
print'(a,2i6)','mtt%taskId, mtt%teamId',mtt%taskId, mtt%teamId

!--- create communicator for team group
call MPI_Comm_Split(mtt%world%mycomm, mtt%teamId, mtt%taskId, mtt%team%mycomm, ierr)
call get_rank_and_size(mtt%team, mtt%team%mycomm)

!--- create communicator for task group
call MPI_Comm_Split(mtt%world%mycomm, mtt%taskId, mtt%teamId, mtt%task%mycomm, ierr)
call get_rank_and_size(mtt%task, mtt%task%mycomm)

end subroutine

end module

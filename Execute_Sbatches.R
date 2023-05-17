args = commandArgs(trailingOnly=TRUE)
library(data.table)
library(flock)
# the idea is that this script is supposed to check which of a list of sbatch files has been completed or is in progress and then queue the next uncompleted one. for that, we also need a datetime at which the first file has begun running.

inprogressSinceInitialDate=function(x)
    {
    if(file.exists(x))
        {
            x_read<-readLines(con = x)
            if(length(x_read)==0){
                return(FALSE)
            }
            if(as.numeric(initial_timedate-as.POSIXct(x_read)) < 0)
            {
            return(TRUE)
            }else{
            return(FALSE)
        }
    }else{
        return(FALSE)
    }
}

initial_timedate=as.POSIXct(args[1])
now=Sys.time()
sbatch_list=readLines(args[2])
concurrent_sbatches=as.numeric(args[3])
jobname=args[4] # important to check how many jobs are actually running right now, because else it will increase exponentially if each job runs **concurrent_sbatches** new jobs
user=args[5]

only_run_1=FALSE
maximum_sbatches_total=9999999
if(length(args) > 5){
    print("Max Sbatches Total set. Checking...")
    maximum_sbatches_total=as.numeric(args[6])
    count_of_running_jobs_total=as.numeric(system(command=paste("squeue -u ",user," | wc -l",sep=""), intern=TRUE))
    if(count_of_running_jobs_total > maximum_sbatches_total){
        only_run_1=TRUE
        }
    }

getToRunJobs=function(user,jobname,concurrent_sbatches){
    count_of_running_jobs=as.numeric(system(command=paste("squeue -u ",user," -n ",jobname," | grep -v JOBID | wc -l",sep=""), intern=TRUE))
    
can_still_run_x_sbatches=concurrent_sbatches-count_of_running_jobs+1 # currently running job "substracted" by adding 1
print(paste("Currently running ", count_of_running_jobs, " jobs. Can schedule ",can_still_run_x_sbatches," more.", sep=""))
return(can_still_run_x_sbatches)
}

can_still_run_x_sbatches<-getToRunJobs(user,jobname,concurrent_sbatches)

end_script=FALSE

if(can_still_run_x_sbatches > 0){

    sbatch_in_progress=paste(sbatch_list,".inprogress",sep="")

    sbatch_todo=sbatch_list[!(unlist(lapply(sbatch_in_progress, inprogressSinceInitialDate)))]

    print(paste(length(sbatch_todo), " sbatches remaining! Running ", can_still_run_x_sbatches, " more...",sep=""))
    
    if(length(sbatch_todo)==0){
        print("No more sbatches to run!")
        end_script=TRUE
    }

    if(end_script==FALSE){
        if(can_still_run_x_sbatches>length(sbatch_todo)){
        can_still_run_x_sbatches=length(sbatch_todo)
        }

    sbatch_todo=sbatch_todo[1:can_still_run_x_sbatches]

    for(sbt in sbatch_todo)
        {
        sbatch_in_progress_check=paste(sbt,".inprogress",sep="")
        print(paste0("Checking ",sbatch_in_progress_check))
        file_lock<-lock(sbatch_in_progress_check)
        sbatch_list_lock<-flock::lock(args[2])
        if(!inprogressSinceInitialDate(sbatch_in_progress_check) & !end_script)
            {
                if((can_still_run_x_sbatches<-getToRunJobs(user,jobname,concurrent_sbatches))>0){
                    system(command=paste("echo ", now, " > ",sbt,".inprogress", sep=""), intern=TRUE)
                    print(system(command=paste("sbatch ",sbt,sep=""), intern=TRUE))
                    if(only_run_1==TRUE){print("Max Sbatches Total Reached. Only running 1 new job.")
                    end_script=TRUE}
                    Sys.sleep(1)
                }
            
            }
        unlock(file_lock)
        flock::unlock(sbatch_list_lock)
        }
    }
    
    
    }else{
    print("No capacity for further runs.")
    }
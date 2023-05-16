args = commandArgs(trailingOnly=TRUE)
library(data.table)
library(flock)
# the idea is that this script is supposed to check which of a list of sbatch files has been completed or is in progress and then queue the next uncompleted one. for that, we also need a datetime at which the first file has begun running.

inprogressSinceInitialDate=function(x)
    {
    if(file.exists(x))
        {
        if(as.numeric(initial_timedate-as.POSIXct(readLines(con = x))) < 0)
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
    maximum_sbatches_total=args[6]
    count_of_running_jobs_total=as.numeric(system(command=paste("squeue -u ",user," | wc -l",sep=""), intern=TRUE))
    if(count_of_running_jobs_total > maximum_sbatches_total){
        only_run_1=TRUE
        }
    }

getToRunJobs=function(user,jobname){
    count_of_running_jobs=as.numeric(system(command=paste("squeue -u ",user," -n ",jobname," | wc -l",sep=""), intern=TRUE))-1 # header line
    print(paste("Currently running ", count_of_running_jobs, " jobs.", sep=""))
concurrent_sbatches=concurrent_sbatches-count_of_running_jobs+1 # currently running job "substracted" by adding 1
return(concurrent_sbatches)
}

concurrent_sbatches<-getToRunJobs(user,jobname)

if(concurrent_sbatches > 0){

    sbatch_in_progress=paste(sbatch_list,".inprogress",sep="")

    sbatch_todo=sbatch_list[!(unlist(lapply(sbatch_in_progress, inprogressSinceInitialDate)))]

    print(paste(length(sbatch_todo), " sbatches remaining! Running ", concurrent_sbatches, " more...",sep=""))
    
    if(length(sbatch_todo)==0){
        stop("No more sbatches to run!")
    }
    
    if(concurrent_sbatches>length(sbatch_todo)){
        concurrent_sbatches=length(sbatch_todo)
        }

    sbatch_todo=sbatch_todo[1:concurrent_sbatches]

    for(sbt in sbatch_todo)
        {
        sbatch_in_progress_check=paste(sbt,".inprogress",sep="")
        print(paste0("Checking ",sbatch_in_progress_check))
        file_lock<-lock(sbatch_in_progress_check)
        if(!inprogressSinceInitialDate(sbatch_in_progress_check))
            {
                if(getToRunJobs(user,jobname)>0){
                    system(command=paste("echo ", now, " > ",sbt,".inprogress", sep=""))
                    print(system(command=paste("sbatch ",sbt,sep=""), intern=TRUE))
                    if(only_run_1==TRUE){stop("Max Sbatches Total Reached. Only running 1 new job.")}
                    Sys.sleep(3)
                }
            
            }
        unlock(file_lock)
        }
    }else{
    print("No capacity for further runs.")
    }
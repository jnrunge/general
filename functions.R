options(repr.plot.width=10, repr.plot.height=10)
if(!exists("jnr_general_scripts_dir")){
    if(as.numeric(R.Version()["major"])<4){
        stop("Please use R>=4 if you do not set 'jnr_general_scripts_dir' before invoking this script.")
    }
    if (suppressWarnings(!require('this.path'))) install.packages('this.path', repos='https://ftp.gwdg.de/pub/misc/cran/')
library("this.path")
td=this.dir()
    }else{
        td=jnr_general_scripts_dir
    }
library(tidytable)

library(readr)
library(stringr)
library(readxl)

fread_and_bind_files=function(list_of_files_full_path)
    {
    for(file in list_of_files_full_path)
        {
        if(file == list_of_files_full_path[1])
            {
            df=fread(file, data.table = FALSE)
            df$file=file
        }else
            {
            df_tmp=fread(file, data.table = FALSE)
            df_tmp$file=file
            df=bind_rows(df, df_tmp)
        }
    }
    return(df)
    }

execute_cmd_sbatch=function(cmd,mem="4G",cpu="1",time="short",acc="ziab",env="samtools",jobname="wrap", activateEnvScript){
    if(time=="short"){
        time="11:59:00"
        }else{
        time="119:59:00"
        }
    cmd<-paste("sbatch -c ",cpu," --mem=",mem," --job-name=",jobname," -A ",acc," -t ",time," --wrap '. ",activateEnvScript," ",env,"; ",cmd,"'",sep="")
    print(cmd)
    print(system(command=cmd, intern=TRUE))
    }


getFirst=function(x){
    return(x[[1]][1])
}
getFirst_v2=function(x, split="/"){
    return(strsplit(x, split, fixed = TRUE)[[1]][1])
}
getLast=function(x, split="/"){
    return(strsplit(x, split, fixed = TRUE)[[1]][length(strsplit(x, split, fixed = TRUE)[[1]])])
}
getWhich=function(x, split="/",which=1){
    return(strsplit(x, split, fixed = TRUE)[[1]][which])
}

fancy_scientific <- function(l, r=2) {
    # https://stackoverflow.com/a/24241954/20937783
     # turn in to character string in scientific notation
    
    
     exponent=as.numeric(strsplit(as.character(format(l, scientific = TRUE)), "e", fixed=TRUE)[[1]][2])
    
    if(exponent<0){
        r=r+abs(exponent)
        }
    
    l = round(l, digits=r)
    
     l <- format(l, scientific = TRUE)
    
     l <- gsub("0e\\+00","0",l)
     # quote the part before the exponent to keep all the digits
     l <- gsub("^(.*)e", "'\\1'e", l)
     # turn the 'e+' into plotmath format
    
    # remove + after exponent, if exists. E.g.: (3x10^+2 -> 3x10^2)  
       l <- gsub("e\\+","e",l)
    # convert 1x10^ or 1.000x10^ -> 10^  
    l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l)
    
     l <- gsub("e", "%*%10^", l)
     # return this as an expression
     return(parse(text=l))
}

fancy_scientific_plot=function(l){
 l <- format(l, scientific = TRUE)
    
     l <- gsub("0e\\+00","0",l)
     # quote the part before the exponent to keep all the digits
     l <- gsub("^(.*)e", "'\\1'e", l)
     # turn the 'e+' into plotmath format
    
    # remove + after exponent, if exists. E.g.: (3x10^+2 -> 3x10^2)  
       l <- gsub("e\\+","e",l)
    # convert 1x10^ or 1.000x10^ -> 10^  
    l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l)
    
     l <- gsub("e", "%*%10^", l)
     # return this as an expression
     return(parse(text=l))    }

getArgs=function(){
    args = commandArgs(trailingOnly=TRUE)
    return(args)
}

execute_complex_sbatch=function(
                    list_of_cmds,jobname,scripts_dir,uniqueRunID,
                    cores,mem,time,env,initial_timedate,
                    jobs_simul,jobs_total=999,list_of_additional_flags, activateEnvScript,Execute_Sbatches_Env,username){
    
    if(time=="short"){
        time="11:59:00"
        }else{
        time="119:59:00"
        }
    
    
    
    if(!endsWith(scripts_dir,"/")){
        scripts_dir=paste0(scripts_dir,"/")
    }
    
    if(!dir.exists(scripts_dir)){
        dir.create(scripts_dir)
        dir.create(paste0(scripts_dir,"logs"))
    }
    
    sbatch_list=paste0(scripts_dir,jobname,".list")
    
    if(file.exists(sbatch_list)){
        if(file.mtime(sbatch_list)<initial_timedate){
            file.remove(sbatch_list)
        }
    }
    
    
    cmds=paste(list_of_cmds,collapse="
")
    
    sbatch=paste("#!/bin/bash
#SBATCH --job-name=",jobname,"  # The job name
#SBATCH -o ",scripts_dir,"logs/",jobname,"-",uniqueRunID,".out
#SBATCH -e ",scripts_dir,"logs/",jobname,"-",uniqueRunID,".err
#SBATCH -c ",cores,"                 # The number of cpu cores to use
#SBATCH --time=",time,"       # The time the job will take to run 
#SBATCH --mem=",mem,"
",paste(paste("#SBATCH",list_of_additional_flags,sep=" "),collapse="
    "),"

set -xe

date

. ",activateEnvScript, " ",env,"

",cmds,"


cd ",td,"
. ",activateEnvScript, " ",Execute_Sbatches_Env,"
Rscript Execute_Sbatches.R '",initial_timedate,"' ",sbatch_list," ",jobs_simul," ",jobname," ",username," ",jobs_total,"

date
    ",sep="")

            sbatch_file=paste(scripts_dir,jobname,"-",uniqueRunID,".sbatch",sep="")

            writeLines(sbatch, sbatch_file)
            
            write(sbatch_file,file=sbatch_list,append=TRUE)
    
    print(sbatch_file)
    
    return(sbatch_list)
    
    
}

start_sbatch_list=function(sbatch_list, jobs_simul, jobname, initial_timedate, username, jobs_total=999){
    if(file.exists(sbatch_list)){
    print(system(command=paste("wc -l ", sbatch_list, sep=""),intern=TRUE))
    print(system(command=paste("Rscript ",td,"Execute_Sbatches.R '",initial_timedate,"' ",sbatch_list," ",jobs_simul," ",jobname, " ", username, " ", jobs_total, sep=""), intern=TRUE))
    }
}

slurm_check_jobs_still_running_numeric=function(username,jobname){
    return(length(suppressWarnings(system(command=paste0("sacct -u ",username," --name=",jobname," | grep -v COM | grep -v FAI | grep -v CAN | cut -f 1 -d' ' | grep -v ba | grep '^[0-9]'"),intern=TRUE))))
}

slurm_check_jobs_still_running=function(username,jobname){
    return(slurm_check_jobs_still_running_numeric(username,jobname)>0)
}


library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
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
plate_to_labid=fread("~/columbia2019/firstplate_to_csv.tsv", data.table=FALSE)
plate_to_labid$Sample=str_replace(plate_to_labid$Sample, "m", "")
getLabID = function(x, plate_to_labid)
    {
    if(length(plate_to_labid$Sample[plate_to_labid$Final_ID == x | plate_to_labid$Final_ID_2 == x | plate_to_labid$Final_ID_3 == x | plate_to_labid$Final_ID_4 == x])==0){
        return(NA)
        }
    if(suppressWarnings(as.numeric(x) %in% 1:12))
        {
        return(as.character(x))
        }else
        {
        return(as.character(plate_to_labid$Sample[plate_to_labid$Final_ID == x | plate_to_labid$Final_ID_2 == x | plate_to_labid$Final_ID_3 == x | plate_to_labid$Final_ID_4 == x]))
        }
}
execute_cmd_sbatch=function(cmd,mem="4G",cpu="1",time="short",acc="ziab",env="samtools",jobname="wrap"){
    if(time=="short"){
        time="11:59:00"
        }else{
        time="119:59:00"
        }
    cmd<-paste("sbatch -c ",cpu," --mem=",mem," --job-name=",jobname," -A ",acc," -t ",time," --wrap '. ~/",env,".sh; ",cmd,"'",sep="")
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
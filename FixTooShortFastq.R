# this R script is supposed to make the most out of unexpectedly ending fastq.gz files. this can happen when the file was not transferred fully and the original file does not exist anymore.

library(devtools)
source_url('https://raw.githubusercontent.com/jnrunge/general/main/functions.R')
args=getArgs() # two arguments: full paths to the two files (paired-end) or just one file (single end)
if(length(args)>2){
    stop("too many arguments")
}
if(length(args)==0){
    stop("no arguments")
}
file1=args[1]
if(length(args)==2){
    file2=args[2]
}else{
    stop("single end not yet implemented")
}


# file2 can be optional and bam I have a general script

for(f in c(file1,file2)){
    if(!file.exists(paste0(f,".gziptest"))){
        print(system(command=paste0("gzip -t ",f," >> ",f,".gziptest 2>&1"),intern=TRUE))
    }
}

test1=readLines(paste0(file1,".gziptest"))
test2=readLines(paste0(file2,".gziptest"))
test1
test2

isUnexpectedEndOfFile=function(x){ 
    # have to see if the file has the problem I am trying to solve
    if(length(x)>0){
        return(sum(grepl("unexpected end of file",x,fixed=TRUE)|
            grepl("invalid compressed data",x,fixed=TRUE))>0)
    }
    return(FALSE)
}

WhereFastqIsBroken=function(x){
    # since some files are messed up much more than just a premature stop
    # I need to find where the issue is. I am looking at which lines start
    # with @ (first line), or + (third line)
    
    which_at=which(startsWith(x,"@"))
    which_plus=which(startsWith(x,"+"))
    
    if(length(which_at)==0|length(which_plus)==0){
        return(rep(TRUE,length(x)))
    }
    
    dist=length(which_at)-length(which_plus)
    
    if(dist<0){
        which_at=c(which_at,rep(-999,abs(dist)))
    }
    if(dist>0){
        which_plus=c(which_plus,rep(-999,abs(dist)))
    }
    
    error1=which_at!=(which_plus-2)
    
    # lets compare the length of seq and qual (another possible error)
    nchar_seq=nchar(x[which_at[which_at!=-999]+1])
    nchar_qual=nchar(x[which_at[which_at!=-999]+3])
    
    error2=nchar_seq!=nchar_qual
    
    if(length(error2)<length(error1)){
        error2=c(error2, rep(FALSE,length(error1)-length(error2)))
    }
    
    return(error1 | error2)
    
}

getCutoff=function(x){
    system(command=paste0("zcat ",x," > ",str_replace(x, fixed(".gz"), ""),""))
    system(command=paste0("echo -e '\\n' >> ",str_replace(x, fixed(".gz"), "")))

    # Open a connection to the gzipped file
    con <- file(str_replace(x, fixed(".gz"), ""),open="rb")
    batch_size=4*100000
    length_t=batch_size
    i=1
    while(length_t==(batch_size)){
        test=readLines(con, n = batch_size)
        length_t=length(test)
        scan=WhereFastqIsBroken(test)
        if(sum(scan)>0){
            break
        }   
        i=i+1
    }


    # Close the connection
    close(con)

    if(sum(scan)>0){
        line_cutoff=(batch_size*(i-1))+((which(scan)[1]*4)-8) #broken past here

        return(line_cutoff)

    }else{
        #what to do when I dont find an issue but should?
        return(NA)
    }
}

FixFile=function(x,x2,cutoff){
    # now head and gzip overwrite the og file and zcat head gzip other
    # file to temp file and then mv -f the 2nd og file
    # this should work even if both files are corrupt
    
    cmd1=paste0("head -n ",cutoff, " ",str_replace(x, fixed(".gz"), ""),
               " | gzip > ",x," && ",
               "rm -f ",str_replace(x, fixed(".gz"),""))
    
    cmd2=paste0("zcat ",x2," | head -n ",cutoff,
               " | gzip > ",x2,".tmp && ",
               "mv -f ",x2,".tmp ",x2)
    
    print(cmd1)
    print(cmd2)
    
    print(system(command=cmd1,intern=TRUE))
    print(system(command=cmd2,intern=TRUE))
}

if(isUnexpectedEndOfFile(test1)){
    line_cutoff=getCutoff(file1)
    FixFile(file1,file2,line_cutoff)
}
if(isUnexpectedEndOfFile(test2)){
    line_cutoff=getCutoff(file2)
    FixFile(file2,file1,line_cutoff)
}

for(f in c(file1,file2)){
    file.remove(paste0(f,".gziptest"))
    print(system(command=paste0("gzip -t ",f," >> ",f,".gziptest 2>&1"),intern=TRUE))
}
test1=readLines(paste0(file1,".gziptest"))
test2=readLines(paste0(file2,".gziptest"))

test1
test2
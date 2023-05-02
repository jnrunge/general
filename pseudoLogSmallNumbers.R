pseudoLogSmallNumbers=function(which,original_level_break_values,multipliedby){
    breaks_=as.numeric(original_level_break_values)*multipliedby
    if(which == "x"){
        return(scale_x_log10(breaks=breaks_, labels=as.character(original_level_break_values)))
    }
    if(which == "y"){
        return(scale_y_log10(breaks=breaks_, labels=as.character(original_level_break_values)))
    }
}
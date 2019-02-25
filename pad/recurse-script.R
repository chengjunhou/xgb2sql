fun_recurse_tree <- function(g,local_dump,dump_index,branch_index){
  #query <- NULL

  if(grepl("leaf",local_dump[dump_index])==TRUE){
    #query <- paste0(query, as.numeric(sub(".*leaf= *(.*?)", "\\1", local_dump[dump_index])))
    cat(as.numeric(sub(".*leaf= *(.*?)", "\\1", local_dump[dump_index])))
  } else{
    cur_var_name <- g$feature_names[as.numeric(regmatches(local_dump[dump_index],regexec("f(.*?)[<]",local_dump[dump_index]))[[1]][2])+1]
    cur_var_val <- as.numeric(regmatches(local_dump[dump_index],regexec("[<](.*?)[]]",local_dump[dump_index]))[[1]][2])

    # if YES
    left_local_index <-  as.numeric(regmatches(local_dump[dump_index],regexec("yes=(.*?)[,]",local_dump[dump_index]))[[1]][2])
    left_dump_index <- which(branch_index==left_local_index)

    # if NO
    right_local_index <-  as.numeric(regmatches(local_dump[dump_index],regexec("no=(.*?)[,]",local_dump[dump_index]))[[1]][2])
    right_dump_index <- which(branch_index == right_local_index)

    # if missing
    missing_local_index <-  as.numeric(regmatches(local_dump[dump_index],regexec("missing=(.*?)$",local_dump[dump_index]))[[1]][2])
    missing_dump_index <- which(branch_index == missing_local_index)

    # query <- paste0(
    #   query,
    #   paste0("\n ( CASE WHEN ",paste0("[",cur_var_name,"]"),"< ",cur_var_val, "THEN "),
    #   fun_recurse_tree(g,local_dump,left_dump_index,branch_index),
    #   paste0("\n  WHEN ",paste0("[",cur_var_name,"]"),">= ",cur_var_val, "THEN "),
    #   fun_recurse_tree(g,local_dump,right_dump_index,branch_index),
    #   paste0("\n  WHEN ",paste0("[",cur_var_name,"]"),"IS NULL", "THEN "),
    #   fun_recurse_tree(g,local_dump,missing_dump_index,branch_index),
    #   " END)"
    # )
    cat("\n ( CASE WHEN ",paste0("[",cur_var_name,"]"),"< ",cur_var_val, "THEN ")
    cat(fun_recurse_tree(g,local_dump,left_dump_index,branch_index))
    cat("\n  WHEN ",paste0("[",cur_var_name,"]"),">= ",cur_var_val, "THEN ")
    cat(fun_recurse_tree(g,local_dump,right_dump_index,branch_index))
    cat("\n  WHEN ",paste0("[",cur_var_name,"]"),"IS NULL", "THEN ")
    cat(fun_recurse_tree(g,local_dump,missing_dump_index,branch_index))
    cat(" END)")
  }

}



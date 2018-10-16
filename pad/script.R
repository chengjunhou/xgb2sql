#' fun_xgboost_to_sql(): view a XGBoost model as SQL
#' v0.1
#' USAGE:
#' fun_xgboost_to_sql(xgb_fit, output_file_name="model_output.SQL", input_table_name="[database].[table]","unique_id")


fun_xgboost_to_sql <- function(xgbModel,output_file_name,input_table_name,unique_id){

  xgb_dump <- xgboost::xgb.dump(xgbModel)
  first_letter <- substring(xgb_dump,1,1)


  all_tree_index <-which(first_letter == "b")




  fun_recurse_tree <- function(g,local_dump,dump_index,branch_index,out){

    if(grepl("leaf",local_dump[dump_index])==TRUE){
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



      cat("\n ( CASE WHEN ",cur_var_name,"< ",cur_var_val, "THEN ")
      cat(fun_recurse_tree(g,local_dump,left_dump_index,branch_index,""))
      cat("\n  WHEN ",cur_var_name,">= ",cur_var_val, "THEN ")
      cat(fun_recurse_tree(g,local_dump,right_dump_index,branch_index,""))
      cat("\n  WHEN ",cur_var_name,"IS NULL", "THEN ")
      cat(fun_recurse_tree(g,local_dump,missing_dump_index,branch_index,""))
      cat(" END)")
    }

  }

  sink(output_file_name,type = "output")

  cat("SELECT ",unique_id,", ")
  if(xgbModel$params$objective == "binary:logistic" | xgbModel$params$objective =="reg:logistic"){
    p0 <- ifelse(is.null(xgbModel$params$base_score),0.5,xgbModel$params$base_score)
    b0 <- log(p0/(1-p0))
    cat("1/(1+exp(-(",b0,"+ SUM(tree)))) AS PRED")
  } else if(xgbModel$params$objective == "reg:linear"){
    b0 <- ifelse(is.null(xgbModel$params$base_score),0.5,xgbModel$params$base_score)
    cat(b0,"+ SUM(tree) AS PRED")
  } else if(xgbModel$params$objective == "reg:gamma" | xgbModel$params$objective == "count:poisson"){
    mu0 <- ifelse(is.null(xgbModel$params$base_score),0.5,xgbModel$params$base_score)
    b0 <- log(mu0)
    cat("exp(",b0,"+ SUM(tree)) AS PRED")
  } else if(xgbModel$params$objective == "reg:tweedie"){
    b0 <- ifelse(is.null(xgbModel$params$base_score),0.5,xgbModel$params$base_score)
    cat("exp(",b0,"+ SUM(tree)) AS PRED")
  }


  cat("\n FROM (  ")

  for(tree_num in 1:length(all_tree_index)){
    cat(paste(" \n SELECT ",unique_id, ", \n"))

    tree_begin <- all_tree_index[tree_num]+1
    if(is.na(all_tree_index[tree_num+1])){
      tree_end <- length(xgb_dump)
    } else {
      tree_end <- all_tree_index[tree_num+1] - 1
    }

    all_branch_index <- as.numeric(sub("\\D*(\\d+).*", "\\1", xgb_dump))

    branch_index <- all_branch_index[tree_begin:tree_end]
    local_dump <- xgb_dump[tree_begin:tree_end]

    dump_index <- 1


    out <- ""
    fun_recurse_tree(xgbModel,local_dump,dump_index,branch_index,out)
    cat(" AS ", paste0("tree"))
    if(tree_num != length(all_tree_index)){
      cat(paste(" FROM ",input_table_name,"\n UNION ALL \n"))
    }
  }

  cat("\n FROM ",input_table_name,") AS TREETABLE \n GROUP BY ",unique_id)

  sink()
}


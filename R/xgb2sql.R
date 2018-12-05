#' Transform xgboost model object trained in R to SQL syntax
#'
#' This function creates SQL syntax aiming for in-database model scoring,
#' providing a robust way of model deployment. It takes in the trained xgboost model \code{xgbModel},
#' name of the input database table \code{input_table_name},
#' and name of a unique identifier within that table \code{unique_id} as input,
#' writes the SQL query to a file specified by \code{output_file_name}.
#'
#' @param xgbModel The trained model object of class \code{xgb.Booster}
#' @param unique_id A unique identifier for each raw that must exist in the raw table, or it needs to be created before model scoring
#' @param output_file_name File name that the SQL syntax will write to
#' @param input_table_name Name of the raw data table in SQL server, the SQL syntax will select from this table
#' @return The SQL syntax will write to the file specified by 'output_file_name'
#'
#' @export
#'
#' @examples
#' # load data
#' df = data.frame(ggplot2::diamonds)
#' df$ID = seq(1, dim(df)[1])
#' head(df)
#'
#' # data processing
#' out <- onehot2sql(df,output_file_name='onehot.txt')
#' x <- out$model.matrix[,-which(colnames(out$model.matrix)=='price')]
#' y <- out$model.matrix[,which(colnames(out$model.matrix)=='price')]
#' # model training
#' bst <- xgboost(data = x,
#'                label = y,
#'                max.depth = 3,
#'                eta = .3,
#'                nround = 5,
#'                nthread = 2,
#'                objective = 'reg:linear',
#'                eval_metric = 'mae')
#'
#' # generate model scoring SQL script
#' xgb2sql(bst, output_file_name='xgb.txt', input_table_name='df_diamonds', unique_id='ID')


xgb2sql <- function(xgbModel, unique_id, output_file_name, input_table_name){

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

      cat("\n ( CASE WHEN ",paste0("[",cur_var_name,"]"),"< ",cur_var_val, "THEN ")
      cat(fun_recurse_tree(g,local_dump,left_dump_index,branch_index,""))
      cat("\n  WHEN ",paste0("[",cur_var_name,"]"),">= ",cur_var_val, "THEN ")
      cat(fun_recurse_tree(g,local_dump,right_dump_index,branch_index,""))
      cat("\n  WHEN ",paste0("[",cur_var_name,"]"),"IS NULL", "THEN ")
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

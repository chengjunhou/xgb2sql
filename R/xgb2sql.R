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
#' xgb2sql(bst, unique_id='ID', output_file_name='xgb.txt', input_table_name='df_diamonds')


xgb2sql <- function(xgbModel, unique_id=NULL, print_progress=FALSE,
                    output_file_name=NULL, input_table_name=NULL, input_onehot_query=NULL) {

  ###### initial setup ######
  xgb_dump <- xgboost::xgb.dump(xgbModel)
  first_letter <- substring(xgb_dump,1,1)
  all_tree_index <- which(first_letter=="b")
  if (is.null(unique_id)) {
    unique_id <- "ROW_KEY"
    warning("query is generated with row unique id named as ROW_KEY")
  }
  if (is.null(output_file_name)) {
    stop("output file not specified")
  }
  if (is.null(input_table_name) & is.null(input_onehot_query)) {
    input_table_name <- "INPUT_TABLE"
    warning("query is generated with input table named as INPUT_TABLE")
  } else if (is.null(input_table_name) & !is.null(input_onehot_query)) {
    input_table_name <- paste0("( \n",input_onehot_query," \n) AS MODREADY ")
  }

  ###### recurse fun ######
  fun_recurse_tree <- function(g,local_dump,dump_index,branch_index){
    if (grepl("leaf",local_dump[dump_index])==TRUE) {
      cat(sub(".*leaf= *(.*?)", "\\1", local_dump[dump_index]))
    } else {
      cur_var_name <- g$feature_names[as.numeric(regmatches(local_dump[dump_index],regexec("f(.*?)[<]",local_dump[dump_index]))[[1]][2])+1]
      cur_var_val <- as.numeric(regmatches(local_dump[dump_index],regexec("[<](.*?)[]]",local_dump[dump_index]))[[1]][2])

      # if YES
      left_dump_index <- which(branch_index==
                                 as.numeric(regmatches(local_dump[dump_index],regexec("yes=(.*?)[,]",local_dump[dump_index]))[[1]][2]))
      # if NO
      right_dump_index <- which(branch_index==
                                  as.numeric(regmatches(local_dump[dump_index],regexec("no=(.*?)[,]",local_dump[dump_index]))[[1]][2]))
      # if missing
      missing_dump_index <- which(branch_index==
                                    as.numeric(regmatches(local_dump[dump_index],regexec("missing=(.*?)$",local_dump[dump_index]))[[1]][2]))

      cat("\n (CASE WHEN", paste0("[",cur_var_name,"] < ",cur_var_val), "THEN ")
      cat(fun_recurse_tree(g,local_dump,left_dump_index,branch_index))
      cat("\n  WHEN ",paste0("[",cur_var_name,"] >= ",cur_var_val), "THEN ")
      cat(fun_recurse_tree(g,local_dump,right_dump_index,branch_index))
      cat("\n  WHEN ",paste0("[",cur_var_name,"] IS NULL"), "THEN ")
      cat(fun_recurse_tree(g,local_dump,missing_dump_index,branch_index))
      cat(" END)")
    }
  }

  ###### generate tree ######
  sink(output_file_name, type ="output")

  cat("SELECT ", unique_id, ", ")
  if(xgbModel$params$objective == "binary:logistic" | xgbModel$params$objective == "reg:logistic" | xgbModel$params$objective == "binary:logitraw"){
    p0 <- ifelse(is.null(xgbModel$params$base_score),0.5,xgbModel$params$base_score)
    b0 <- log(p0/(1-p0))
    if (xgbModel$params$objective == "binary:logitraw") {
      cat(b0,"+ SUM(ONETREE) AS PRED")
    } else {
      cat("1/(1+exp(-(",b0,"+ SUM(ONETREE)))) AS PRED")
    }
  } else if (xgbModel$params$objective == "binary:hinge") {
    b0 <- ifelse(is.null(xgbModel$params$base_score),0.5,xgbModel$params$base_score)
    cat("IF((",b0,"+ SUM(ONETREE) )>0,1,0) AS PRED")
  } else if(xgbModel$params$objective == "reg:linear"){
    b0 <- ifelse(is.null(xgbModel$params$base_score),0.5,xgbModel$params$base_score)
    cat(b0,"+ SUM(ONETREE) AS PRED")
  } else if(xgbModel$params$objective == "reg:gamma" | xgbModel$params$objective == "count:poisson" | xgbModel$params$objective == "reg:tweedie"){
    mu0 <- ifelse(is.null(xgbModel$params$base_score),0.5,xgbModel$params$base_score)
    b0 <- log(mu0)
    cat("exp(",b0,"+ SUM(ONETREE)) AS PRED")
  } else {
    warning("query is generated with unsupported objective")
  }

  cat("\nFROM (  ")
  for (tree_num in 1:length(all_tree_index)) {
    cat(" \n SELECT", unique_id, ",")

    tree_begin <- all_tree_index[tree_num]+1
    if(is.na(all_tree_index[tree_num+1])){
      tree_end <- length(xgb_dump)
    } else {
      tree_end <- all_tree_index[tree_num+1] - 1
    }

    all_branch_index <- as.numeric(sub("\\D*(\\d+).*", "\\1", xgb_dump))

    branch_index <- all_branch_index[tree_begin:tree_end]
    local_dump <- xgb_dump[tree_begin:tree_end]

    fun_recurse_tree(xgbModel,local_dump,1,branch_index)
    cat(" AS ONETREE")
    if(tree_num != length(all_tree_index)){
      cat(" FROM ", input_table_name, "\n UNION ALL \n")
    }

    if (print_progress==TRUE) {
      sink()
      cat("====== Processing", tree_num, "/", length(all_tree_index), "Tree ======\n")
      sink(output_file_name, type ="output", append=TRUE)
    }
  }
  cat(" FROM ",input_table_name,"  \n) AS TREETABLE GROUP BY ",unique_id)

  sink()
}

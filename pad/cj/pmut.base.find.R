#' Obtain Meta Information for Each Column
#'
#' This function finds the meta information for each column within training data,
#' which will be used to process testing and/or new data so that it can be scored without error,
#' check \code{\link{pmut.base.prep}} for the part of testing data processing.
#' Meta information for columns of class \code{factor}, \code{character}, and \code{logical} will be stored in one list.
#' Each element of the list contains three slots: 1st \code{$VarString} is column name, 2nd \code{$LvlVec} is vector of unique levels,
#' 3rd \code{$LvlBase} is base level name which is the level with most counts.
#' Meta information for columns of class \code{integer}, and \code{numeric} will be stored in another list.
#' Each element of the list contains two slots: 1st \code{$VarString} is column name,
#' 2nd \code{$ValueMean} is its value mean (median for integer).
#'
#' @param DATA Object of class \code{data.frame} or \code{data.table}
#' @return A list of two elements, 1st being meta information for categorical columns, 2nd for numeric columns
#'
#' @export
#'
#' @examples
#' temp = pmut.base.find(data.frame(ggplot2::diamonds))
#' temp[[1]]  # categorical meta
#' temp[[2]]  # numeric meta


pmut.base.find <- function(DATA) {
  # meta list
  CAT.list = list()
  NUM.list = list()
  # name vector
  namevec = names(DATA)
  # loop over all categorical vars
  cat("====== ", length(namevec), " Runs ======", "\n")
  cati = 1
  numi = 1

  for (i in 1:length(namevec)) {
    CLASS = class(DATA[[namevec[i]]])
    ## discrete feature
    if (CLASS[1] %in% c("character","factor","logical","ordered")) {
      Var1 = as.character(DATA[[namevec[i]]])
      Var1 = Var1[!is.na(Var1) & Var1!=""]
      modelfreq = data.frame(table(Var1)) # frequency of vars in dev data
      modelfreq$Var1 = as.character(modelfreq$Var1)
      CAT.list[[cati]] = list()
      CAT.list[[cati]]$VarString = c(namevec[i], CLASS)
      CAT.list[[cati]]$LvlVec = modelfreq$Var1
      CAT.list[[cati]]$LvlBase = modelfreq$Var1[which(modelfreq$Freq==max(modelfreq$Freq))[1]]
      cati = cati + 1
      cat("Loop", i, namevec[i], ":", CLASS, "\n")

    ## continuous feature
    } else if (CLASS[1] %in% c("integer","numeric")) {
      NUM.list[[numi]] = list()
      NUM.list[[numi]]$VarString = c(namevec[i], CLASS)
      if (CLASS == "integer") {
        NUM.list[[numi]]$ValueMean = median(DATA[[namevec[i]]], na.rm=T)
      } else {
        NUM.list[[numi]]$ValueMean = mean(DATA[[namevec[i]]], na.rm=T)
      }
      numi = numi + 1
      cat("Loop", i, namevec[i], ":", CLASS, "\n")

    ## other rare class feature
    } else {
      cat("Error", i, namevec[i], ": check feature class", "\n")
    }
  }
  return(list(CAT.list, NUM.list))
}


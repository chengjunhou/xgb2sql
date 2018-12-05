#' Calculate Model Prediction AUC with Visulization
#'
#' This function calculates area under the ROC curve for model prediction, without any package dependency.
#'
#' @param aa Vector of actuals, could be non-binary, but all non-zero will be treated as TRUE
#' @param pp Vector of predictions, could be any value, probability is most ideal
#' @param plot Logical (defualt is FALSE), TRUE indicates plotting the auc curve
#' @return A single numeric value for auc
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' actuals = c(1,1,1,1,0,1,1,0,1,0,1,0,1,0,0,1,0,0,0,0)
#' predicts = rev(seq_along(actuals)); predicts[9:10] = mean(predicts[9:10])
#' pmut.auc(actuals, predicts, plot=TRUE)


pmut.auc <- function(aa, pp, plot=FALSE) {
  ord <- order(pp, decreasing=TRUE)
  aa <- aa[ord]
  pp <- pp[ord]
  x <- cumsum(!aa)/max(1,sum(!aa)) # FPR = x-axis
  y <- cumsum(aa)/max(1,sum(aa))   # TPR = y-axis
  # remove dups to achieve this.
  dup <- c(pp[-1]>=pp[-length(pp)],
           FALSE)
  # add in ideal endpoints just in case
  x <- c(0,x[!dup],1)
  y <- c(0,y[!dup],1)
  # sum areas of segments
  n <- length(y)
  area <- sum( ((y[-1]+y[-n])/2) * (x[-1]-x[-n]) )

  # auc plot
  if (plot==TRUE) {
    plotdat <- data.frame(FP=x,TP=y)
    p1 <- ggplot(plotdat, aes(x=FP,y=TP,col=TP)) + geom_abline(intercept=0,slope=1) + geom_line(lwd=1) +
      scale_colour_gradientn(colours=c("#FEB24C","#FD8D3C","#F03B20","#BD0026")) +
      labs(title="AUC Plot", x="false-positive rate", y="true-positive rate") +
      theme(legend.position="none", plot.title=element_text(hjust = 0.5))
    print(p1)
  }

  return(area)
}


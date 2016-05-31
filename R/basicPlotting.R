#' Plot a single mnist digit
#'
#' \code{ggDigit} plots a single mnist digit with ggplot.
#'
#' This functions returns a ggplot object of a single digit. The goal
#' is to keep the plot as minimal as possible, only showing the true
#' digit information. Additional parameters are given to create a
#' title with the digit label and a predicted label.
#'
#' @param digit A single row from the mnist dataset, i.e. a
#'        1 by 784 vector. This can e.g. be \code{trainSet$x[1,]}.
#' @param trueLab The true label of the digit. Can be a factor or character.
#' @param predLab The predicted label, e.g. from a classifier.
#'        Can be a factor or character.
#'
#' @return Return a ggplot object of the digit.
#'
#' @examples
#' # Create some random data to plot
#' dat <- matrix(sample(0:255,784),nrow = 1)
#' # Get the ggplot object
#' myRandomNonDigit <- ggDigit(dat)
#' \dontrun{
#' # Display the digit
#' (myRandomNonDigit)
#' }
#'
#' @export
ggDigit <- function(digit, trueLab, predLab){
  pDig <- data.frame(value = as.numeric(t(digit)), xVar = rep(1:28,28), yVar = -rep(1:28,each=28)+28)
  ggRet <- ggplot(data = pDig, aes(x=xVar, y=yVar, fill=value)) +
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "white", high = "black", mid = "gray",
                         midpoint = max(pDig$value)/2, limit = c(min(pDig$value),max(pDig$value)),
                         space = "Lab", guide = FALSE) +
    theme_minimal()+
    coord_fixed() + xlab("") + ylab("") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),axis.text.y = element_blank())
  if(missing(trueLab) & missing(predLab)){
    return(ggRet)
  }else if(missing(predLab)){
    ggRet <- ggRet + ggtitle(paste('True label is:',as.character(trueLab)))
  }else if(missing(trueLab)){
    ggRet <- ggRet + ggtitle(paste('Predicted label is:',as.character(predLab)))
  }else{
    ggRet <- ggRet + ggtitle(paste('True label:',as.character(trueLab),
                                   ", Predicted label:", as.character(predLab)))
  }
  return(ggRet)
}

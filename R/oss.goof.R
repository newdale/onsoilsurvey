#' Calculate Model Goodness of Fit Metrics
#'
#' This function allows the user to calculate r-square
#' adjusted r-square, root mean square error, mean
#' absolute error, bias and Lin's concordance
#' correlation coefficient from observed vs predicted
#' data from predictive model outputs for continuous
#' data
#'
#' @param predicted numeric
#' @param observed numeric
#'
#' @return data.frame
#' @export
#'
#' @examples
#' #Calculate fit statistics between 2 numeric vectors
#' observed<- c(3,7,4,9,6,2)
#' predicted<-c(2,5,4,7,3,5)
#' oss.goof(observed,predicted)
#'
#'
oss.goof <- function(predicted,observed){

  #pull in data and create a dataframe, remove NAs, assign data to objects
  data<- stats::na.omit(cbind(as.data.frame(observed),as.data.frame(predicted)))
  observed <-data$observed
  predicted<-data$predicted

  #calculate r square and adjusted r square
  rLM<- stats::lm(predicted~observed)
  adj.r2 <- as.matrix(summary(rLM)$adj.r.squared)
  r2 <- as.matrix(summary(rLM)$r.squared)

  #calculate the root mean square error
  SEP<- mean((observed-predicted)^2)
  RMSE<- sqrt(SEP)

  #calculate the mean absolute error
  MAE<- sum(abs(observed-predicted))/nrow(data)

  #calculate the bias of the estimate
  bias <- (mean(mean(predicted)-observed))

  #calculate Lin's Concordance correlation coefficient
  ux<-mean(observed)
  uy<-mean(predicted)
  varxy<-mean((observed-ux) * (predicted-uy))
  varx<-sum((predicted-mean(predicted))^2)/(length(predicted)-1)
  vary<-sum((observed-mean(observed))^2)/(length(observed)-1)
  concordance <- (2*varxy)/(varx+vary+(ux-uy)^2)

  #assign the outputs to a dataframe and print it in the console for viewing
  goof <-data.frame(r2=c(r2),adj.r2=c(adj.r2),concordance=c(concordance),RMSE=c(RMSE),bias=c(bias),MAE=c(MAE))
  }

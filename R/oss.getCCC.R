#' Determine optimal caret model based on Lin's Concordance Correlation Coefficient
#'
#' This function allows the user to  use  Lin's Concordance
#' Correlation Coefficient as the metric to select the
#' optimal model from cross-validation in the caret package.
#'
#'
#' @param Model caret model object
#'
#' @return list of three dataframes
#' Dataframe 1: Fit statistics for best model based on CCC, including best tuning parameters
#' Dataframe 2: Holds tuning parameters for the model based on CCC
#' Dataframe 3: Fit statistics for all tuning parameters for the cross validation
#'
#' @export
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' #Calculate fit statistics for the best tune model
#'
#' oss.getCCC(KeeneSOC)
#'
#'
oss.getCCC <- function(Model){

  #create some empty objects to store data later
  .<-r2<-adj.r2<-concordance<-RMSE<-bias<-MAE<-Mr2<-Madj.r2<-Mconcordance<-MRMSE<-Mbias<-MMAE<-NULL
  Model<- Model

  # we extract the observed and predicted data from the caret outputs
  x <- Model$pred

  # group the data tuning parameters and calculate the goodness of fit statistics
  df <- x %>%
    dplyr::group_by(dplyr::across(which(colnames(x) %in% c(names(Model$bestTune), "Resample")))) %>%

    dplyr::do(as.data.frame(oss.goof(predicted=.$pred, observed=.$obs)))

  # convert the goodness of fit to data frame
  df <- data.frame(df)

  # summarize the goodness of fit metrics to generate standard deviation for each
  df <- df %>%
    dplyr::group_by(dplyr::across(which(colnames(df) %in% c(names(Model$bestTune))))) %>%
    dplyr::summarize(Mr2 = mean(r2), Madj.r2 = mean(adj.r2), Mconcordance = mean(concordance), MRMSE = mean(RMSE), Mbias = mean(bias), MMAE = mean(MAE),
                     sd_r2 = stats::sd(r2), sd_adj.r2 = stats::sd(adj.r2), sd_concordance = stats::sd(concordance), sd_RMSE = stats::sd(RMSE), sd_bias = stats::sd(bias), sd_MAE = stats::sd(MAE),
                     .groups = 'drop') #the .groups = 'drop' just supresses a warning we don't need to worry about regarding how the tibble is organized

  # some renaming of columns
  df <- data.frame(dplyr::rename(df, r2 = Mr2, adj.r2 = Madj.r2, concordance = Mconcordance, RMSE = MRMSE, bias = Mbias, MAE = MMAE))

  #Save the full data frame for output as list item 3
  df_var <- df

  #Extract information for best model based on CCC and extract the best hyperparamters for output as list item 2
  df <- df[df$concordance == max(df$concordance),]
  final_pars <- df[which(colnames(df) %in% names(Model$bestTune))]

  #Put optimal hyperparameters in a final column as descriptive stats of this best model
  df <- cbind(method = Model$method, df)
  df$final_pars <- paste(utils::capture.output(t(final_pars))[-1], collapse = " ; ")
  #remove individual hyperparamter columns from dataframe
  df[,which(colnames(df) %in% names(Model$bestTune))] = NULL

  #create the output list object
  out<- list(ccc_optimal_model=df, ccc_best_tune=final_pars, ccc_cv_summary=df_var)
  invisible(out)

  # print list item 3, or CV results to console and print statement showing final tuning parameters
  print(out[[3]])
  cat(paste("Lin's Concordance Coerrelation Coefficient was used to select the optimal model.",
            paste0("The final values for the optimal model were ",paste(c(colnames(final_pars)),as.vector(t(final_pars)),sep=" ", collapse=" and "),"."),sep="\n"))
}

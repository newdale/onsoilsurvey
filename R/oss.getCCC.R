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

  .<-r2<-adj.r2<-concordance<-RMSE<-bias<-MAE<-Mr2<-Madj.r2<-Mconcordance<-MRMSE<-Mbias<-MMAE<-NULL
  Model<- Model
  x <- Model$pred

  df <- x %>%
    dplyr::group_by(dplyr::across(which(colnames(x) %in% c(names(Model$bestTune), "Resample")))) %>% #this works generally for all MLM no matter the hyperparameter names

    dplyr::do(as.data.frame(oss.goof(predicted=.$pred, observed=.$obs)))

  df <- data.frame(df)

  df <- df %>%
    dplyr::group_by(dplyr::across(which(colnames(df) %in% c(names(Model$bestTune))))) %>%
    dplyr::summarize(Mr2 = mean(r2), Madj.r2 = mean(adj.r2), Mconcordance = mean(concordance), MRMSE = mean(RMSE), Mbias = mean(bias), MMAE = mean(MAE),
              sd_r2 = stats::sd(r2), sd_adj.r2 = stats::sd(adj.r2), sd_concordance = stats::sd(concordance), sd_RMSE = stats::sd(RMSE), sd_bias = stats::sd(bias), sd_MAE = stats::sd(MAE),
              .groups = 'drop') #the .groups = 'drop' just supresses a warning we don't need to worry about regarding how the tibble is organized

  df <- data.frame(dplyr::rename(df, r2 = Mr2, adj.r2 = Madj.r2, concordance = Mconcordance, RMSE = MRMSE, bias = Mbias, MAE = MMAE))

  #Save the variation in concordance across hyperparameters for the bayesian approach
  df_var <- df

  #Clip to the best hyperparamters and save as a separate object to include in subsequent train function
  df <- df[df$concordance == max(df$concordance),]
  final_pars <- df[which(colnames(df) %in% names(Model$bestTune))]

  #Put optimal hyperparameters in a final column as descriptive stats of this best model
  df <- cbind(method = Model$method, df)
  df$final_pars <- paste(utils::capture.output(t(final_pars))[-1], collapse = " ; ")
  #remove individual hyperparamter columns from dataframe
  df[,which(colnames(df) %in% names(Model$bestTune))] = NULL

  return(list(df, final_pars, df_var))
} ## this function filters caret training outputs and returns GOOF for best model only

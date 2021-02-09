#' Evaluate splines with multiple values of lambda
#'
#' This function is a wrapper for the ea_spline
#' function from the ithir package maintained by
#' Brendan Malone and available on bitbucket.
#' The wrapper allows the user to evaluate and
#' plot equal area quadratic splines using multiple
#' values of lambda.
#'
#' @param obj Object of class "data.frame"
#' @param var.name character; target variable name (must be a numeric variable)
#' @param lam numeric; lambda the smoothing parameter
#' @param d numeric; standard depths that are used to extract values from fitted spline i.e harmonising depths
#' @param vlow numeric; smallest value of the target variable (smaller values will be replaced)
#' @param vhigh numeric; highest value of the target variable (larger values will be replaced)
#' @param dir character; location to store plots if the option is selected, defaults to current working directory
#' @param save.plots logical; TRUE or FALSE, to generate plots or not
#'
#' @return Returns a list with five elements:
#' harmonised
#'       data frame; are spline-estimated values of the target variable at standard depths
#' obs.preds
#'       data frame; are observed values together with associated spline predictions for each profile at each depth.
#' splineFitError
#'       matrix; rmse and rmseiqr between observed and predicted between observed profiles and associated fitted splines.
#' var.1cm
#'       matrix; are spline-estimated values of the target variable using the 1 cm increments
#' lamdf
#'      data frame; returns the optimal value of lambda for each profile
#'
#' @importFrom methods "is"
#' @importFrom graphics "par"
#' @importFrom graphics "mtext"
#' @importFrom grDevices "dev.copy"
#' @importFrom grDevices "dev.list"
#' @importFrom grDevices "dev.off"
#' @importFrom grDevices "png"
#'
#' @export
#'
#' @examples
#' #Generate splines with various lambdas for soil profiles
#' data(profiles)
#' d=t(c(0,5,15,30,60,100))
#' lam=c(10, 1, 0.1, 0.01, 0.001)
#' oss.optSpline(obj=profiles,var.name="CEC",lam=lam,d=d,save.plots=FALSE)

oss.optSpline<- function(obj, var.name, lam= 0.1, d= t(c(0,5,15,30,60,100,200)), vlow= 0, vhigh= 1000, dir=getwd(), save.plots=TRUE){

  # create a directory for output of spline plots if user wants them
  if(save.plots==TRUE){
    output_dir<- paste0(dir,'/Spline_plots')
    dir.create(paste0(dir,'/Spline_plots'))}

  #set the first column, the site identifier, to character
  obj[,1]<- as.character(obj[,1])

  # split the input data frame into a list by site identifier
  datlist<- split.data.frame(obj,obj[,1])

  # create an empty list to hold the outputs of the function
  splinelist<- list()

  #create a vector of the site identifiers for naming plots
  names<- unique(obj[,1])

  #start of the outer loop that will run ea_spline for each site
  for (i in 1:length(datlist)){

    #split the plotting window based on number of lambda values
    dims=c(round(length(lam)+1)/2,2)
    par(mfrow=c(round(length(lam)+1)/2,2))

    # start the inner loop which evaluates the various lambda for each profile in your list
    for(j in 1:length(lam)){
      #select the lambda
      y<- lam[j]

      # Run the spline and compile the RMSE values in a table
      DataSpline <- suppressMessages(ithir::ea_spline(datlist[[i]], var.name= var.name ,d = d,lam = y, vlow=vlow, vhigh = vhigh, show.progress=FALSE))
      l_eval<- cbind(y, sum(DataSpline$splineFitError$rmse, na.rm=TRUE))
      ifelse(exists('lam_sum'), lam_sum<- rbind(lam_sum,l_eval), lam_sum<- l_eval)

      # we need to correct the issue with numbers coming out as characters, a workaround
      DataSpline$obs.preds[,2]<- as.numeric(DataSpline$obs.preds[,2])
      DataSpline$obs.preds[,3]<- as.numeric(DataSpline$obs.preds[,3])
      DataSpline$obs.preds[,4]<- as.numeric(DataSpline$obs.preds[,4])

      # we add the plotting function here
      oss.plot_spl(splineOuts= DataSpline, d = d, maxd = 100, type = 3)
      mtext(paste0("lambda = ",y),cex = 0.8)

      #end of inner loop for testing the specified lambda values
    }
    # once the loop is done we can save the plot to png if the user wants them
    if(save.plots==TRUE){
      dev.copy(png, paste0(output_dir,"/Spline_", var.name, "_", names[i], ".png"), width=600, height=800)
      if(!is.null(dev.list())) dev.off()}

    # then you retrieve the optimal lambda for the site
    if (nrow(lam_sum)==1) {z= as.numeric(lam_sum[1,1])
    } else {
      lam_sum<- lam_sum[order(lam_sum[,2]),]
      z=as.numeric(lam_sum[1,1])}

    # and now run the spline one last time with optimal lambda and commit it to a new list
    splinelist[[i]] <- suppressMessages(ithir::ea_spline(datlist[[i]], var.name= var.name ,d = d,lam = z, vlow=vlow, vhigh = vhigh, show.progress=FALSE))
    rm(lam_sum)

    #store the lambda values being used for future reference
    ifelse(exists('lamdf'), lamdf<- rbind(lamdf,z), lamdf<- z)

    # end of the outer loop which is based on the number of unique sites
  }
  # convert the table of optimal lambda values to dataframe and name the column
  rownames(lamdf)<- c(seq(1,nrow(lamdf),by=1))
  lamdf<- as.data.frame(lamdf)
  colnames(lamdf)<- "lambda"

  #small loop to unlist the data into dataframes
  for (i in 1:length(splinelist)){
    temp1 <- splinelist[[i]][[1]]
    temp2 <- splinelist[[i]][[2]]
    temp3 <- splinelist[[i]][[3]]
    temp4 <- splinelist[[i]][[4]]
    ifelse(exists('harmonized'), harmonized<- rbind(harmonized,temp1), harmonized<- temp1)
    ifelse(exists('obs'), obs<- rbind(obs,temp2), obs<- temp2)
    ifelse(exists('rmse'), rmse<- rbind(rmse,temp3), rmse<- temp3)
    ifelse(exists('var.1cm'), var.1cm<- cbind(var.1cm,temp4), var.1cm<- temp4)
    colnames(harmonized)<- colnames(splinelist[[i]][[1]])
    colnames(obs)<- colnames(splinelist[[i]][[2]])
  }

  # combine the dataframes into an output identical to ea_spline list object, except we add the lambda table
  datlist<- list(harmonized, obs, rmse, var.1cm,lamdf)
  names(datlist)<- c('harmonised', 'obs.preds', 'splineFitError','var.1cm','lambda')
  datlist$obs.preds$SiteID<- as.factor(datlist$obs.preds$SiteID)
  datlist$obs.preds[,2]<- as.numeric(datlist$obs.preds[,2])
  datlist$obs.preds[,3]<- as.numeric(datlist$obs.preds[,3])
  datlist$obs.preds[,4]<- as.numeric(datlist$obs.preds[,4])

  return(datlist)}

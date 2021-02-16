#' Plot the output of optsline function
#'
#' This function is identical to plot_ea_spline
#' function from the ithir package maintained by
#' Brendan Malone and available on bitbucket.
#' The diffrence is in how the xlim argument
#' is set for plotting and provides wider x axis.
#'
#' @param splineOuts list; This is a returned object from the ea_spline function
#' @param d numeric; standard depths that were used during the fitting from ea_spline.
#' @param maxd numeric; Maximum soil depth for generating the outputs plots/s
#' @param type numeric; Different themes of plot may be exported. Type 1 is to return the observed soil data plus the continuous spline (default). Type 2 is to return the observed data plus the averages of the spline at the specified depth intervals. Type 3 is to return the observed data, spline averages and continuous spline.
#' @param label Character; Optional label to put on x-axis of plot.
#' @param plot.which numeric; Integer selction of which plot to produce if multiple soil profiles have been fitted using the ea_spline function.
#'
#' @return Returns a labeled plot of the soil profile information and user selected ea_spline outputs.
#'
#' @importFrom graphics "polygon"
#' @importFrom graphics "lines"
#'
#' @export
#'
#' @examples
#' data(profiles)
#' fit<- oss.optSpline(obj=profiles, var.name="pH_CaCl2",
#' lam= c(10, 1, 0.1, 0.01, 0.001), d=t(c(0,5,15,30,60,100)),
#' vlow= 0, vhigh= 9, save.plots=FALSE)
#' oss.plot_spl(splineOuts= fit, d= t(c(0,5,15,30,60,100)), maxd= 100, type=3, label= "Soil pH")
#'

#Add test comment 1 - CB

oss.plot_spl<- function(splineOuts, d = t(c(0,5,15,30,60,100,200)), maxd, type = 1, label = "", plot.which = 1){
  print("performing oss.plot_spl") #adding test printing statement - CB
  #type 1 (raw data and spline fit)
  if (type==1)
  { #plot the observed
    d1<- splineOuts$obs.preds
    d1<- d1[d1$FID==plot.which,]
    vals<- d1[,4]
    lims<- c(d1[,4],d1[,5])
    depths<- d1[,2:3]
    matX<- matrix(NA, nrow=nrow(d1), ncol= 4)
    matY<- matrix(NA, nrow=nrow(d1), ncol= 4)
    for (i in 1: nrow(d1)){
      matX[i,]<- c(vals[i]-vals[i], vals[i], vals[i], vals[i]-vals[i])
      matY[i,]<- c(depths[i,1], depths[i,1], depths[i,2], depths[i,2]) }
    raw.plot<-plot(matX[1,], matY[1,],type="n",ylim=c(maxd,0),xlim=c((min(lims)-(min(lims)*0.25)), (max(lims)*1.25)),main=paste("soil profile:",d1[1,1], sep=" "), ylab="depth", xlab= label, lty=2, lwd=1.5, xaxs="i", col="black", font.lab=2,cex.lab=1.5)
    for (i in 1: nrow(d1)){
      polygon (matX[i,],matY[i,], lty=1, lwd=2, border="black") }
    #plot the fitted spline
    lines(splineOuts$var.1cm[,plot.which],seq(1,d[length(d)]),lwd=2,col="red" )}

  #type 2 (raw data and spline fitted averages)
  if (type==2)
  {#plot the observed
    d1<- splineOuts$obs.preds
    d1<- d1[d1$FID==plot.which,]
    vals<- d1[,4]
    lims<- c(d1[,4],d1[,5])
    depths<- d1[,2:3]
    matX<- matrix(NA, nrow=nrow(d1), ncol= 4)
    matY<- matrix(NA, nrow=nrow(d1), ncol= 4)
    for (i in 1: nrow(d1)){
      matX[i,]<- c(vals[i]-vals[i], vals[i], vals[i], vals[i]-vals[i])
      matY[i,]<- c(depths[i,1], depths[i,1], depths[i,2], depths[i,2]) }
    raw.plot<-plot(matX[1,], matY[1,],type="n",ylim=c(maxd,0),xlim=c((min(lims)-(min(lims)*0.25)), (max(lims)*1.25)),main=paste("soil profile:",d1[1,1], sep=" "), ylab="depth", xlab= label, lty=2, lwd=1.5, xaxs="i", col="black", font.lab=2,cex.lab=1.5)

    d2<- as.matrix(splineOuts$harmonised[plot.which,2:length(d)])
    matX1<- matrix(NA, nrow=ncol(d2), ncol= 4)
    matY1<- matrix(NA, nrow=ncol(d2), ncol= 4)
    for (i in 1: ncol(d2)){
      matX1[i,]<- c(d2[i]-d2[i], d2[i], d2[i], d2[i]-d2[i])
      matY1[i,]<- c(d[1,i], d[1,i], d[1,i+1], d[1,i+1]) }
    #plot the spline averages
    for (i in 1: ncol(d2)){
      polygon (matX1[i,],matY1[i,], lty=1, lwd=1,col="green", border="green") }

    for (i in 1: nrow(d1)){
      polygon (matX[i,],matY[i,], lty=1, lwd=2, border="black") }}

  #type 3 (raw data and spline fitted averages)
  if (type==3)
  {#plot the observed
    d1<- splineOuts$obs.preds
    d1<- d1[d1$FID==plot.which,]
    vals<- d1[,4]
    lims<- c(d1[,4],d1[,5])
    depths<- d1[,2:3]
    matX<- matrix(NA, nrow=nrow(d1), ncol= 4)
    matY<- matrix(NA, nrow=nrow(d1), ncol= 4)
    for (i in 1: nrow(d1)){
      matX[i,]<- c(vals[i]-vals[i], vals[i], vals[i], vals[i]-vals[i])
      matY[i,]<- c(depths[i,1], depths[i,1], depths[i,2], depths[i,2]) }
    raw.plot<-plot(matX[1,], matY[1,],type="n",ylim=c(maxd,0),xlim=c((min(lims)-(min(lims)*0.25)), (max(lims)*1.25)),main=paste("soil profile:",d1[1,1], sep=" "), ylab="depth", xlab= label, lty=2, lwd=1.5, xaxs="i", col="black", font.lab=2,cex.lab=1.5)

    d2<- as.matrix(splineOuts$harmonised[plot.which,2:length(d)])
    matX1<- matrix(NA, nrow=ncol(d2), ncol= 4)
    matY1<- matrix(NA, nrow=ncol(d2), ncol= 4)
    for (i in 1: ncol(d2)){
      matX1[i,]<- c(d2[i]-d2[i], d2[i], d2[i], d2[i]-d2[i])
      matY1[i,]<- c(d[1,i], d[1,i], d[1,i+1], d[1,i+1]) }

    #plot the spline averages
    for (i in 1: ncol(d2)){
      polygon (matX1[i,],matY1[i,], lty=1, lwd=1,col="green", border="green") }
    #plot the observations
    for (i in 1: nrow(d1)){
      polygon (matX[i,],matY[i,], lty=1, lwd=2, border="black") }
    #plot the spline
    lines(splineOuts$var.1cm[,plot.which],seq(1,d[length(d)]),lwd=2,col="red" )}}

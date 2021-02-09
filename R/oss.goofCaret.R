#' Extract Fit Statistics from best fit caret model
#'
#' This function allows the user to extract additional
#' goodness of fit parameters from predictive models
#' trained in caret for continuous variables; most
#' importantly Lin's concordance correlation coefficient.
#'
#' @param ModelName caret model object
#'
#' @return data.frame
#'
#' @export
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' #Calculate fit statistics for the best tune model
#' data(KeeneSOC)
#' oss.goofCaret(KeeneSOC)
#'
#'
oss.goofCaret<- function(ModelName){
  committees<-neighbors<-mtry<-sigma<-C<-n.trees<-interaction.depth<-shrinkage<-n.minobsinnode<-k<-kmax<-distance<-kernel<-Resample<-.<-NULL

  r<- ModelName

  if(r$method=="cubist"){x<- r$pred %>% dplyr::filter(committees==r$bestTune$committees, neighbors==r$bestTune$neighbors)
  }else if(r$method=="rf"|r$method=="qrf"|r$method=="parRF"){x<- r$pred %>% dplyr::filter(mtry==r$bestTune$mtry)
  }else if(r$method=="svmRadial"){x<- r$pred %>% dplyr::filter(sigma==r$bestTune$sigma, C==r$bestTune$C)
  }else if(r$method=="lm"|r$method=="lmStepAIC"){x<- r$pred
  }else if(r$method=="gbm"){x<- r$pred %>% dplyr::filter(n.trees==r$bestTune$n.trees, interaction.depth==r$bestTune$interaction.depth, shrinkage==r$bestTune$shrinkage, n.minobsinnode==r$bestTune$n.minobsinnode)
  }else if (r$method=="knn"){x<- r$pred %>% dplyr::filter(k==r$bestTune$k)
  }else if (r$method=="kknn"){x<- r$pred %>% dplyr::filter(kmax==r$bestTune$kmax, distance==r$bestTune$distance, kernel==r$bestTune$kernel)
  }else {print("Not programmed for model")}


  y <- x %>%
    dplyr::group_by(Resample)%>%
    dplyr::do(as.data.frame(oss.goof(predicted=.$pred,observed=.$obs)))
  z<- as.data.frame(t(colMeans(y[c(2:7)])))
  w<- as.data.frame(t(sapply(y[c(2:7)],stats::sd,na.rm=TRUE)))
  z<-cbind(z,w)
  rownames(z)<- r$method
  colnames(z)[7:12]<- paste0("sd_",names(z[,1:6]))
  rm(r,w,x,y)
  as.data.frame(z)
  print(z)
}

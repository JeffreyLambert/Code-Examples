
#*******************
# Load libraries involved in the simulation
#*******************
library(plyr)
library(survival)
library(ggplot2)
library(gridExtra)
library(foreach)
library(doParallel)

#******************
# Source user functions
source("ggsurv.R")
source("apb_Rlibrary_2014Oct.R")
source("Code Library JRL.R")



r = c(.5,1,3)                                         #Vector of r parameters to be iterated through

info_output <- list()                                 #initialize list for storing output
  for(w in 1:length(r)){
      #**** Do in parallel the apply statement
      cl <- makeCluster(2)                            #Task # of processors
      registerDoParallel(cores=cl)                    #Register processors
      info_temp <- list() #Initialize list
      #system.time(
      pb <- txtProgressBar(min=0,max=5,style=3)          #Initialize progress bar to track simulation progress
        for(j in 1:5){
          
          C <- cbind(rweibull(320,scale=3.31,shape=1),0) #Follow up times for control
          tx <- cbind(rweibull(320,scale=3.72,shape=2.56),1) #Follow up times for treatment
          plt <- as.data.frame(cbind(gen.entry(n=2*nrow(tx),r=r[w],tau=10),rbind(C,tx))) #Stack the treatment and control groups
          
          names(plt) <- c("E","t","group") #rename the data frame variables
          
          plt <- arrange(plt,plt$E)  #Order data by entry time
          
          list_temp <- ldply(seq(0,2*max(plt$t),by=.1),info_growth_evaluator,set=plt, .parallel=T)
                            #Process in parallel elements of the apply loop and return in a data set
                            # apply statement runs info_growth_evaluator for a sequence of study times
                            
          info_temp[[j]] <-  matrix(unlist(by(list_temp,list_temp[,1],head,n=1)),ncol=4,byrow=T)
                              #unwrap list_temp and store the data matrix into an element of the list
                              # info_temp
          
          setTxtProgressBar(pb,j)                        #Print progress bar
        }
      #)
      stopCluster(cl)       #Stop processor cluster
      
      close(pb)             #End the progress bar tracking
      
      info_output[[w]] <- info_temp     #Store the list info_temp as an element of list info_output
      
      
      
  }
    


info_output_1 <- ldply(1:length(info_output[[1]]),function(x){  return(as.data.frame(info_output[[1]][[x]]))})


info_output_1 <- arrange(info_output_1,info_output_1[,1]) #order the data by number of events
names(info_output_1) <- c("N","I","CIC","PH")  #Rename the dataframe columns
info_summary <- ddply(info_output_1, ~N, summarise, info_mean=mean(I,na.rm=T), cic_freq = sum(!is.na(CIC)),  
                      cic_mean=mean(CIC,na.rm=T), cic_sd = sd(CIC,na.rm=T), ph_freq = sum(!is.na(PH)),
                      ph_mean=mean(PH,na.rm=T),
                      ph_sd = sd(PH,na.rm=T))
                      # Summarize parameters by number of events (means, standar deviations, missing values)
                      #  and output to a data frame


info_summary$I_ph <- info_summary$N/4
I_max <- max(info_summary$N)/4
info_summary$I_ph_prop <- info_summary$I_ph / I_max
info_summary$I_cic_prop <- info_summary$info_mean / I_max
info_summary$I_cic_alt <- info_summary$info_mean / max(info_summary$info_mean)


plot(info_summary$I_ph_prop,info_summary$I_cic_prop,col="green",xlim=c(0,1),ylim=c(0,1),type="l",
     main="r=3, tau = 3, H0: NULL, using parallel for loop")
abline(a=0,b=1,col="purple")
lines(info_summary$I_ph_prop,info_summary$I_cic_alt,col="red") #proportion of expected info under PH

#NOTE: As tau increases it appears that the information growth becomes closer to being proporitonal to D
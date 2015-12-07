
#******************
# Function Library
# Author: JR Lambert
# Description: This file contains a series of user defined functions used
#               to A) simulate survival data under proportional hazards
#               and non-proportional hazards, B) Evaluate parameter estimates
#               C) Evaulated type I error rate and power for for the Cox
#               and CIC estimators under various survival scenarios.
#******************


#******************
# Information Growth Simulation Functions
# Description: The functions in this section are used to
#              generate survival data and evaluate each simulated
#              data set.

  #********
  # gen.entry
  # Decription: This function generates entry times from a powered uniform distribution.  
  #             This dicates both the accrual period and the censoring distributions at each
  #             analysis time
  # Inputs:
  #       tau: Defines the end of the patient accrual period
  #       r: Defines the accrual pattern - r = 1 (uniform), r < 1 (early), r > 1 (late)
  #       n: Defines the number of observations to be generated

    gen.entry <- function(n,r,tau){
      U <- tau*runif(n)**(1/r) 
      return(U)
    }

  #********
  # inform.growth.evaluator
  # Description: This function inputs a value for study time.  From this, survival times are
  #               calculated, and event status is calculated.  The function then returns a parameter
  #               estimate for Beta.CIC, Beta.PH (Cox estimator), I.CIC (information aka 1/Variance(Beta.CIC)),
  #               and the number of events that occurred at this point in the simulated study.
  # Inputs:
  #       x: Defines the point in study time at which to evaluate the parameters
  #       set: Defines the simulated survival data set to be evaluated

    info_growth_evaluator <- function(x,set){
      set$X <- pmax(0,pmin(set[,2],x-set[,1],4))
      set$D <- (set$X > 0)*(x-set$E >= set$t)*(set$X<4)   
      
      if(sum(set$D[set$group==1]) >= 5 & sum(set$D[set$group==0]) >= 5 ){
        bhat.kgb <- uniroot(kgbm.EE,time=set$X,delta=set$D,group=set$group,interval=c(-3,3))$root #Calculate beta for KGB
        #NOTE: updated interval from (-3,3)
        kgbmfit <- kgbm.score.res(loghr=bhat.kgb,time=set$X,group=set$group,delta=set$D)          #Calculate residuals
        vhat.kgb <- (t(kgbmfit$score.resid) %*% (kgbmfit$score.resid))/kgbmfit$ivec^2 #calulate variance of KGB
        bhat.ph <- as.numeric(coxph(Surv(set$X,set$D)~set$group)$coeff) #Calculate beta PH
        
        return(t(matrix(cbind(sum(set$D),1/vhat.kgb,bhat.kgb,bhat.ph))))
      }
      
      else{
        return(t(matrix(cbind(sum(set$D),NA,NA,NA))))
      }  
    }
#******************
# Information Graphics functions
# Description: The functions in this section are used to
#              generate survival data and evaluate each simulated
#              data set.
    
    # information_cleaner
    # Description: information_cleaner orders the input data set by number of events,
    #              summarizes the parameters by number of events and attaches a dummy variable
    #              corresponding to the simulation scenario
    # Inputs:
    #       x: Simulation scenario dummy variable
    #       set: Defines the simulated survival data set to be evaluated
    
    information_cleaner <- function(x,set){
      
      info_temp <- arrange(set,set[,1])
      names(info_temp) <- c("N","I","CIC","PH")
      info_summary <- ddply(info_temp, ~N, summarise, info_mean=mean(I,na.rm=T),  
                            cic_mean=mean(CIC,na.rm=T), ph_mean=mean(PH,na.rm=T),cic_sd = sd(CIC,na.rm=T),
                            ph_sd = sd(PH,na.rm=T))
      
      info_summary$I_ph <- info_summary$N/4
      info_summary$scenario <- x
      return(info_summary)
    }
    
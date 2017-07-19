
library(plyr)
library(gridExtra)
library(ggplot2)
#*********** Load the data from external files *******

#load("500 rep tau10 null list.rdata")
load("500 rep tau10 list.rdata")
tau10_list <- info_output

#load("500 rep tau3 null list.rdata")
load("500 rep tau3 list.rdata")
tau3_list <- info_output

#*********** Unwrap the information data *************
tau3_rhalf_data <- ldply(1:length(tau3_list[[1]]),function(x){  return(as.data.frame(tau3_list[[1]][[x]]))})
    #NOTE: information variables are in order c(Events, I.cic, B.cic, B.ph)

tau3_r1_data <- ldply(1:length(tau3_list[[2]]),function(x){  return(as.data.frame(tau3_list[[2]][[x]]))})

tau3_r3_data <- ldply(1:length(tau3_list[[3]]),function(x){  return(as.data.frame(tau3_list[[3]][[x]]))})

tau10_rhalf_data <- ldply(1:length(tau10_list[[1]]),function(x){  return(as.data.frame(tau10_list[[1]][[x]]))})
#NOTE: information variables are in order c(Events, I.cic, B.cic, B.ph)

tau10_r1_data <- ldply(1:length(tau10_list[[2]]),function(x){  return(as.data.frame(tau10_list[[2]][[x]]))})

tau10_r3_data <- ldply(1:length(tau10_list[[3]]),function(x){  return(as.data.frame(tau10_list[[3]][[x]]))})


  #********
  # Tau3 clean
  #********
    tau3_rhalf_clean <- information_cleaner(x=1,set=tau3_rhalf_data)
    tau3_r1_clean <- information_cleaner(x=2,set=tau3_r1_data)
    tau3_r3_clean <- information_cleaner(x=3,set=tau3_r3_data)
    
    tau3_clean <- rbind(tau3_rhalf_clean,tau3_r1_clean,tau3_r3_clean)
    tau3_clean <- tau3_clean[tau3_clean$N<=435,] #435 was maximum sample size for power calculations
                                                 # thus we will end the information growth curve there

    
    tau3_clean$I_cic_alt <- tau3_clean$info_mean / max(tau3_clean$info_mean,na.rm=T) #CIC as proportion of CIC information
    tau3_clean$support <- c("Expanding")
  #********
  # Tau10 clean
  #********
    tau10_rhalf_clean <- information_cleaner(x=1,set=tau10_rhalf_data)
    tau10_r1_clean <- information_cleaner(x=2,set=tau10_r1_data)
    tau10_r3_clean <- information_cleaner(x=3,set=tau10_r3_data)
    
    tau10_clean <- rbind(tau10_rhalf_clean,tau10_r1_clean,tau10_r3_clean)
    tau10_clean <- tau10_clean[tau10_clean$N<=435,] #435 was maximum sample size for power calculations
                                                    # thus we will end the information growth curve there
    
    tau10_clean$I_cic_alt <- tau10_clean$info_mean / max(tau10_clean$info_mean,na.rm=T) #CIC as proportion of CIC information
    tau10_clean$support <- c("Fixed")

#*********** Plot the information growth curves *************

cleaned <- rbind(tau3_clean,tau10_clean) #combine the tau3 and tau10 information data sets

ggplot(cleaned,aes(x=N/max(N),y=I_cic_alt,colour=factor(scenario))) + stat_smooth(size=1) +
    geom_abline(intercept=0, slope=1) +
    labs(x="Proportion of total events", y="Proportion of total information") +
    scale_colour_discrete(name="Scenario",breaks=c(1,2,3),labels=c("Early accrual", "Uniform accrual", "Late accrual"))+
    facet_wrap(~support)
#**********

#*********** Plot CIC vs information growth curves **********

cic_by_I <- ggplot(cleaned, aes(x=info_mean, y = cic_mean, fill=factor(scenario),colour=factor(scenario))) + 
    stat_smooth() +
    geom_point(size=1) +
    labs(x = "Total information", y="Beta CIC") +
    scale_fill_discrete(name="Scenario", breaks=c(1,2,3),labels=c("Early accrual", "Uniform accrual", "Late accrual")) +
    scale_colour_discrete(guide=FALSE) +
    facet_wrap(~support)

ph_by_I <- ggplot(cleaned, aes(x=I_ph, y = ph_mean, fill=factor(scenario),colour=factor(scenario))) + 
  stat_smooth() +
  geom_point(size=1) +
  labs(x = "Total information", y="Beta PH") +
  scale_fill_discrete(name="Scenario", breaks=c(1,2,3),labels=c("Early accrual", "Uniform accrual", "Late accrual")) +
  scale_colour_discrete(guide=FALSE) +
  facet_wrap(~support)

grid.arrange(cic_by_I,ph_by_I,nrow=2, top="Log hazard ratio estimate versus total information")


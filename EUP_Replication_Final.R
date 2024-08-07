#Replication code for "When does the European Commission Pursue Member State Noncompliance?"
#Sivaram Cheruvu
#University of Texas at Dallas
#sivaram.cheruvu@utdallas.edu

####Loading packages####
library(lubridate)
library(ggplot2)
library(tidyr)
library(broom)
library(fixest)

####Loading Replication data####
dat <- read.csv("EUP_replication.csv")

####Table 1 Models####
mod_1 <- feols(LFN_election ~ out_eu_cont*incorrect, cluster = ~ daysbeforeED + incorrect, 
               data = dat)

mod_2 <- feols(LFN_election ~ out_eu_cont*incorrect + pilot + support + commission + addressee + scope + length + daysbeforeED + 
                 effectiveness + expenditures + workload + influence| member_state + year + directorate_general, cluster = ~ daysbeforeED + incorrect, 
               data = dat)

mod_3 <- feols(LFN_election ~ out_eu_cont*incorrect + pilot + support + commission + addressee + scope + length + daysbeforeED + 
                 bureaucracy_quality + expenditures + workload + influence| member_state + year + directorate_general, cluster = ~ daysbeforeED + incorrect, 
               data = dat)

mod_4 <- feglm(LFN_election ~ out_eu_cont*incorrect, cluster = ~ daysbeforeED + incorrect, family = "binomial",
               data = dat)

mod_5 <- feglm(LFN_election ~ out_eu_cont*incorrect| member_state + year + directorate_general, cluster = ~ daysbeforeED + incorrect, family = "binomial",
               data = dat)

mod_6 <- feglm(LFN_election ~ out_eu_cont*incorrect + pilot + support + commission + addressee + scope + length + daysbeforeED +
                 bureaucracy_quality + expenditures + workload + influence,
               cluster = ~ daysbeforeED + incorrect, family = "binomial",
               data = dat)

dict = c("out_eu_cont" = "EU Ideology", "out_left_cont" = "Right Ideology",
         "pilot" = "Pilot", "support" = "Support", "commission" = "Commission",
         "addressee" = "Addressee", "scope" = "Scope", "length" = "Length",
         "bureaucracy_quality" = "Bureaucracy Quality", "expenditures" = "Expenditures",
         "daysbeforeED" = "DaysbeforeED",
         "workload" = "Workload", "influence"="Influence",
         "incorrect"="Nonconformity", "effectiveness" = "Effectiveness", "member_state" = "Member State",
         "year"="Year", "directorate_general"="Directorate General") #changing variable names for table

etable(mod_1,mod_2,mod_3,mod_4,mod_5,mod_6, dict = dict, style.tex = style.tex("aer"), 
       fitstat = ~ pr2 + n, tex = T, file = "table1.tex") # creating latex table

####Figure 3 Predicted Probability Plot####
dat_cluster <- dat %>% dplyr::filter(!is.na(out_eu_cont)) #removing NAs for plot
reg1 <- feols(LFN_election ~ out_eu_cont*incorrect + pilot + support + commission + addressee + scope + length + daysbeforeED + 
                effectiveness + expenditures + workload + influence| member_state + year + directorate_general, cluster = ~ daysbeforeED + incorrect, 
              data = dat_cluster) #running primary regression
newdata = data.frame(out_eu_cont = rep(seq(min(dat_cluster$out_eu_cont), max(dat_cluster$out_eu_cont), length.out = 1000),1), incorrect = rep(0:1, 1000),
                     support = rep(mean(dat_cluster$support, na.rm = T),2000), effectiveness = rep(mean(dat_cluster$effectiveness, na.rm = T),2000),
                     expenditures = rep(mean(dat_cluster$expenditures, na.rm = T),2000), workload = rep(mean(dat_cluster$workload, na.rm = T),2000),
                     influence = rep(mean(dat_cluster$influence, na.rm = T),2000), member_state = rep("Italy",2000),
                     year = rep(2012,2000), directorate_general = rep("Energy",2000),
                     pilot = rep(mean(dat_cluster$pilot, na.rm = T),2000), length = rep(mean(dat_cluster$length, na.rm = T),2000), commission = rep(mean(dat_cluster$commission, na.rm = T),2000),
                     addressee = rep(mean(dat_cluster$addressee, na.rm = T),2000),
                     scope = rep(mean(dat_cluster$scope, na.rm = T),2000), daysbeforeED = rep(mean(dat_cluster$daysbeforeED, na.rm = T),2000)) #creating data for prediction
pred_reg1 <- predict(reg1, newdata = newdata) #predicting model
pred_mod <- augment(reg1, newdata = newdata) #gathering fitted values
interval <- qnorm((1-0.95)/2)  #creating 95% confidence intervals
ggplot(pred_mod, aes(x = out_eu_cont, y = .fitted, color = factor(incorrect))) + 
  geom_line() + geom_line(aes(y = .fitted - reg1$se["out_eu_cont:incorrect"]*interval), linetype = "dashed") +
  geom_line(aes(y = .fitted + reg1$se["out_eu_cont:incorrect"]*interval), linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-8,9,1)) +
  annotate(geom = "text", label = "Non-Conformity", x = 5.7, y = .55, hjust = 1, color = "black", size = 5) +
  annotate(geom = "text", label = "Non-Communication", x =6.5, y = .3, hjust = 1, color = "black", size = 5) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  xlab("Difference in EU Ideology") + ylab("Probability Commission waits until after Election") 

####Table 2 models####

mod_1 <- feols(LFN_election ~ out_left_cont*incorrect, cluster = ~incorrect + daysbeforeED, 
               data = dat)

mod_2 <- feols(LFN_election ~ out_left_cont*incorrect + pilot + support + commission + addressee + scope + length + daysbeforeED + 
                 effectiveness + expenditures + workload + influence| member_state + year + directorate_general, cluster = ~incorrect + daysbeforeED, 
               data = dat)

mod_3 <- feols(LFN_election ~ out_left_cont*incorrect + pilot + support + commission + addressee + scope + length + daysbeforeED + 
                 bureaucracy_quality + expenditures + workload + influence| member_state + year + directorate_general, cluster = ~incorrect + daysbeforeED, 
               data = dat)

mod_4 <- feglm(LFN_election ~ out_left_cont*incorrect, cluster = ~incorrect + daysbeforeED, family = "binomial",
               data = dat)

mod_5 <- feglm(LFN_election ~ out_left_cont*incorrect| member_state + year + directorate_general, cluster = ~incorrect + daysbeforeED, family = "binomial",
               data = dat)

mod_6 <- feglm(LFN_election ~ out_left_cont*incorrect + pilot + support + commission + addressee + scope + length + daysbeforeED +
                 bureaucracy_quality + expenditures + workload + influence,
               cluster = ~incorrect + daysbeforeED, family = "binomial",
               data = dat)

etable(mod_1,mod_2,mod_3,mod_4,mod_5,mod_6, dict = dict, style.tex = style.tex("aer"), 
       fitstat = ~ pr2 + n, tex = T, file = "table2.tex")

####Figure 4 Predicted Probability plot####

reg1 <- feols(LFN_election ~ out_left_cont*incorrect + pilot + support + commission + addressee + scope + length + daysbeforeED + 
                effectiveness + expenditures + workload + influence| member_state + year + directorate_general, cluster = ~ daysbeforeED + incorrect, 
              data = dat)
newdata = data.frame(out_left_cont = rep(seq(min(dat$out_left_cont), max(dat$out_left_cont), length.out = 1000),1), incorrect = rep(0:1, 1000),
                     support = rep(mean(dat$support, na.rm = T),2000), effectiveness = rep(mean(dat$effectiveness, na.rm = T),2000),
                     expenditures = rep(mean(dat$expenditures, na.rm = T),2000), workload = rep(mean(dat$workload, na.rm = T),2000),
                     influence = rep(mean(dat$influence, na.rm = T),2000), member_state = rep("Italy",2000),
                     year = rep(2008,2000), directorate_general = rep("Health and Consumers",2000),
                     pilot = rep(mean(dat$pilot, na.rm = T),2000), length = rep(mean(dat$length, na.rm = T),2000), commission = rep(mean(dat$commission, na.rm = T),2000),
                     addressee = rep(mean(dat$addressee, na.rm = T),2000), scope = rep(mean(dat$scope, na.rm = T),2000), daysbeforeED = rep(mean(dat$daysbeforeED, na.rm = T),2000))
pred_reg1 <- predict(reg1, newdata = newdata)
pred_mod <- augment(reg1, newdata = newdata)
interval <- qnorm((1-0.95)/2)  
ggplot(pred_mod, aes(x = out_left_cont, y = .fitted, color = factor(incorrect))) + 
  geom_line() + geom_line(aes(y = .fitted - reg1$se["out_left_cont:incorrect"]*interval), linetype = "dashed") +
  geom_line(aes(y = .fitted + reg1$se["out_left_cont:incorrect"]*interval), linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-8,9,1)) +
  annotate(geom = "text", label = "Non-Conformity", x = 5.7, y = .47, hjust = 1, color = "black", size = 5) +
  annotate(geom = "text", label = "Non-Communication", x =6.5, y = .29, hjust = 1, color = "black", size = 5) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  xlab("Difference in Left/Right Ideology") + ylab("Probability Commission waits until after Election") 


####Appendix Section 2####
mod_cluster <- feols(LFN_election ~ out_eu_cont*incorrect,
                     data = dat_cluster) #Running baseline regression model without cluster
fitted_data <- augment(mod_cluster, data = dat_cluster) #obtaining residuals and fitted values
fitted_data$incorrect <- ifelse(fitted_data$incorrect == 1, "Nonconformity", "Noncommunication") #recoding data for plot
fitted_data$far<- ifelse(fitted_data$daysbeforeED > 1460, "Four years", "Less than four years")

#Figure 1
residuals_infringement <- ggplot(fitted_data, aes(x = .fitted, y = .resid)) + 
  geom_point(aes(color = incorrect)) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) + 
  xlab("Fitted Values") + ylab("Residuals") #plotting residuals against fitted values by infringement type

#Figure 2
residuals_full <- ggplot(fitted_data, aes(x = .fitted, y = .resid)) + 
  geom_point(aes(color = far)) +
  facet_wrap(~factor(incorrect)) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) + 
  xlab("Fitted Values") + ylab("Residuals")  #facetting residuals by infringement type and including daysbeforeED

#Figure 3
residuals_ms <- ggplot(fitted_data, aes(x = .fitted, y = .resid)) + 
  geom_point(aes(color = incorrect)) +
  facet_wrap(~factor(member_state)) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        legend.position = "bottom") + 
  xlab("Fitted Values") + ylab("Residuals")  #splitting residuals by member state

rm(list = ls(all = TRUE))


library(data.table)
library(igraph)
library(ggplot2)
library(dplyr)
library(proxy)
library(MASS)
library(scatterplot3d)
library(rgl)
############## Data prep

#Importing all datasets
investor_firms = fread("investor_firms.csv", header = TRUE)
startup_companies = fread("startup_companies.csv", header = TRUE)
investors_and_deals = fread("investors_and_deals.csv", header = TRUE)
startups_and_deals = fread("startups_and_deals.csv", header = TRUE)

#Checking data 
head(investor_firms)
head(startup_companies)
unique(startup_companies$Business_Status)
head(investors_and_deals)
head(startups_and_deals)


### Question 1a#############
all_deals<-merge(x=investors_and_deals,y=startups_and_deals,
                by.x="Deal_Id",by.y ="DealId", allow.cartesian = TRUE)

all_deals_2<-merge(x=all_deals,y=startup_companies,
                 by.x="CompanyId",by.y ="CompanyID", allow.cartesian = TRUE)



#aggregating to get number of deals and number of industries to calculate diversification variable at investor level
DT<-data.table(all_deals_2)
all_deals_agg<-DT[, .(number_of_industries = uniqueN(Primary_Industry_Code),number_of_deals=.N), by = Investor_Id]

#removing investors with successful investments =NA
investor_firms_cleaned<-investor_firms[investor_firms$successful_investments>0]

all_deals_3 <-merge(x=all_deals_agg,y=investor_firms_cleaned,
      by.x="Investor_Id",by.y ="InvestorId", allow.cartesian = TRUE)

###exclude firms with less than 5 investments as this may lead to misleading results##
##Cannot be sure of behavior of firms with only 1 investment##
all_deals_4<-all_deals_3[all_deals_3$number_of_deals>4]
all_deals_4$diversification_per<-(all_deals_4$number_of_industries/all_deals_4$number_of_deals)*100
all_deals_4$success_rate<-(all_deals_4$successful_investments/all_deals_4$number_of_deals)*100
all_deals_5<-all_deals_4[all_deals_4$successful_investments<number_of_deals]
plot(all_deals_5$diversification_per,all_deals_5$success_rate)

ggplot(all_deals_5, aes(x=diversification_per,y=success_rate)) + geom_smooth(method = "loess", se = T)
ggplot(all_deals_5, aes(x=diversification_per,y=successful_investments)) + geom_smooth(method = "loess", se = T)
model1<-glm(successful_investments ~  diversification_per + number_of_deals, data=all_deals_5)

#model1<-glm(success_rate ~  diversification_per + number_of_deals, data=all_deals_5)
summary(model1)
#(Intercept)         -8.183254   0.515263  -15.88   <2e-16 ***
#diversification_per  0.105583   0.008336   12.67   <2e-16 ***
#number_of_deals      0.497972   0.001140  436.88   <2e-16 ***

#For success rate
#Coefficients:
                         #Estimate Std. Error t value Pr(>|t|)    
 # (Intercept)         36.826412   0.746259  49.348  < 2e-16 ***
 #diversification_per  0.071740   0.012073   5.942 2.93e-09 ***
 #number_of_deals      0.007961   0.001651   4.822 1.44e-06 ***

######## Question 2###########################################
#all_deals_2<-merge(x=all_deals,y=startup_companies,
                   #by.x="CompanyId",by.y ="CompanyID", allow.cartesian = TRUE)

#Merging file by itself to get edge list
investors_and_deals_2<-investors_and_deals
colnames(investors_and_deals_2)<-c("Investor_Id_1","Deal_Id","Lead_Investor_1")
all_deals_cartesian<-merge(x=investors_and_deals,y=investors_and_deals_2, allow.cartesian = TRUE)
all_deals_cartesian$flag=ifelse(all_deals_cartesian$Investor_Id_1==all_deals_cartesian$Investor_Id,1,0)

all_deals_cartesian_1<-all_deals_cartesian[all_deals_cartesian$flag==0]
all_deals_cartesian_1$deal_id_concat<-paste(all_deals_cartesian_1$Investor_Id,all_deals_cartesian_1$Investor_Id_1,sep = "_")
all_deals_cartesian_grouped<-all_deals_cartesian_1%>% 
  group_by(deal_id_concat) %>%
  summarise(status=sum(Lead_Investor),total=n())
all_deals_cartesian_grouped$status_fnl<-all_deals_cartesian_grouped$status/all_deals_cartesian_grouped$total
all_deals_cartesian_grouped_1<-merge(x=all_deals_cartesian_grouped,y=unique(all_deals_cartesian_1[,c("deal_id_concat","Investor_Id","Investor_Id_1")]), allow.cartesian = TRUE)

#Creating graph object and using status as edge weights
g<-graph_from_edgelist(as.matrix(all_deals_cartesian_grouped_1[,c("Investor_Id","Investor_Id_1")]))
E(g)$weight=all_deals_cartesian_grouped_1[,"status_fnl"]                    
eigen_all<-eigen_centrality(g)
eigen_all_1<-data.table(names(eigen_all$vector), eigen_all$vector)
colnames(eigen_all_1)<-c("Investor_Id","status_eigen")
nrow(all_deals_5)
status_vs_success<-merge(x=all_deals_5,y=eigen_all_1, allow.cartesian = TRUE)
ggplot(status_vs_success, aes(x=status_eigen,y=successful_investments)) + geom_smooth(method = "loess", se = T)

model2<-glm(successful_investments ~ status_eigen + number_of_deals, family="poisson", data=status_vs_success)
summary(model2)
#(Intercept)     2.715e+00  2.930e-03   926.5   <2e-16 ***
#status_eigen    4.702e+00  1.321e-02   355.9   <2e-16 ***
#number_of_deals 2.847e-04  1.209e-06   235.5   <2e-16 ***

########### Question 3 ######
#Figure out which regression to use
model3 = glm(successful_investments ~ diversification_per + status_eigen + diversification_per*status_eigen, family="poisson",data=status_vs_success)
summary(model3)
#diversification_per              -0.0370097  0.0001534 -241.23   <2e-16 ***
#status_eigen                      3.6518521  0.0235325  155.18   <2e-16 ***
 # diversification_per:status_eigen -0.0322797  0.0019138  -16.87   <2e-16 ***

# set up scaled grid of (x,y) values 
diversification_per = seq(0,1000, by=20) 
status_eigen = seq(0,1000, by=20) 
values = expand.grid(diversification_per=diversification_per, status_eigen=status_eigen)
# prediction from the model 
values$successful_investments = predict(model3,newdata=values)
# regular 3d plot 
scatterplot3d(values$diversification_per, values$status_eigen, values$successful_investments)
# interactive 3d plot you can move around 
plot3d(values$diversification_per, values$status_eigen, values$successful_investments)

########### Question 4 ###########
unique(startup_companies$Business_Status)
startup_companies_1<-startup_companies[startup_companies$Business_Status!=""]

#Creating different categories specified
startup_companies_1$startup_state<-ifelse(startup_companies_1$Business_Status=="Profitable","Profitable",
                                          ifelse(startup_companies_1$Business_Status %in% c("Generating Revenue","Generating Revenue/Not Profitable"),"Generating Revenue",
                                                 ifelse(startup_companies_1$Business_Status %in% c("Out of Business","Bankruptcy: Liquidation","Bankruptcy: Admin/Reorg"),"Failed",
                                                        ifelse(startup_companies_1$Business_Status %in% c("Startup","Clinical Trials - General",
                                                                                                          "Clinical Trials - Phase 1","Clinical Trials - Phase 2",
                                                                                                          "Clinical Trials - Phase 3","Clinical Trials - Phase 4"),"Ramp-Up/Clinical Trial","Other"))))
#excluding undefined categories
#getting all info about startups
#startup_companies_2<-startup_companies_1[startup_companies_1$startup_state<5]
startup_companies_merge<-merge(x=startups_and_deals[,c("DealId","CompanyId")],y=startup_companies_1[,c("CompanyID","startup_state")], 
                               by.x="CompanyId",by.y = "CompanyID" ,allow.cartesian = TRUE)

#getting all required info from investors
investors_merge<-merge(x=status_vs_success,y=investors_and_deals[,c("Investor_Id","Deal_Id")],allow.cartesian = TRUE)

#final merge
final_merge<-merge(x=investors_merge,y=startup_companies_merge[,c("DealId","startup_state")], 
                               by.x="Deal_Id",by.y = "DealId" ,allow.cartesian = TRUE)
model4 = multinom(startup_state ~ diversification_per + status_eigen + diversification_per*status_eigen, final_merge)
z = summary(model4)$coefficients/summary(model4)$standard.errors
(1 - pnorm(abs(z), 0, 1)) * 2
#not able to interpret results, creating binary variables and running logit model to make sure results make sense
final_merge$profitable<-ifelse(final_merge$startup_state=="Profitable",1,0)
model_profitable<-glm(profitable ~ status_eigen + diversification_per+status_eigen*diversification_per , data = final_merge, family = "binomial")
summary(model_profitable)

final_merge$revenue<-ifelse(final_merge$startup_state=="Generating Revenue",1,0)
model_revenue<-glm(revenue ~ status_eigen + diversification_per+status_eigen*diversification_per , data = final_merge, family = "binomial")
summary(model_revenue)

final_merge$out_of_business<-ifelse(final_merge$startup_state=="Failed",1,0)
model_out_of_business<-glm(out_of_business ~ status_eigen + diversification_per+status_eigen*diversification_per , data = final_merge, family = "binomial")
summary(model_out_of_business)


#Estimate Std. Error  z value Pr(>|z|)    
#(Intercept)                      -2.8136656  0.0201501 -139.635   <2e-16 ***
#status_eigen                      0.8514719  0.0522857   16.285   <2e-16 ***
#diversification_per               0.0036134  0.0004141    8.726   <2e-16 ***
#status_eigen:diversification_per -0.0024100  0.0039547   -0.609    0.542    


#This shows that companies without a synergy of status and diversification are more likely to go out of business



rm(list = ls(all = TRUE))


library(data.table)
library(igraph)
library(ggplot2)
library(dplyr)
library(proxy)
library(MASS)
library(scatterplot3d)
library(rgl)
library(anytime)
############## Data prep

#Importing all datasets
reddit = fread("reddit_data.csv", header = TRUE)
head(reddit)
reddit$use_time<-anytime(reddit$utc)
reddit_test<-reddit
#write.csv(reddit_test,"reddit_test.csv")
x <- as.POSIXct(reddit_test$use_time)
reddit_test$year<-format(x, "%Y")
reddit_test$month<-format(x, "%m")
reddit_test$day<-format(x, "%d")
#reddit_test_1<-reddit_test[reddit_test$year==2016 & reddit_test$month==12 & reddit_test$day==12]
#head(reddit_test_1)
#####reddit Prar
reddit_test_Inorai<-reddit_test[reddit_test$username=="Inorai"]
unique(reddit_test_Inorai$subreddit)
reddit_test_Inorai_2<-reddit_test_Inorai %>% 
  group_by(subreddit) %>%
  summarise(activity=n())
reddit_test_Inorai_3<-reddit_test_Inorai[reddit_test_Inorai$subreddit=="AskTrumpSupporters"] %>% 
  group_by(year,month,day) %>%
  summarise(activity=n())
reddit_test_Inorai_test<-reddit_test[reddit_test$year==2016 & reddit_test$month>10 & reddit_test$subreddit=="AskTrumpSupporters"]

reddit_test_Inorai_test$month_day<-paste(reddit_test_Inorai_test$month,reddit_test_Inorai_test$day,sep = "")
reddit_test_Inorai_test<-data.table(reddit_test_Inorai_test)
reddit_Inorai_test_grp<-reddit_test_Inorai_test[, .(unique_users = unique(username),activity=.N), by=month_day]
reddit_Inorai_test_grp_2<-reddit_Inorai_test_grp
colnames(reddit_Inorai_test_grp_2)<-c("month_day","unique_users_2","activity")
reddit_Inorai_test_merge<-merge(x=reddit_Inorai_test_grp,y=reddit_Inorai_test_grp_2,allow.cartesian = TRUE)
reddit_Inorai_test_merge$flag<- ifelse(reddit_Inorai_test_merge$unique_users==reddit_Inorai_test_merge$unique_users_2,1,0)
reddit_Inorai_test_merge<-reddit_Inorai_test_merge[reddit_Inorai_test_merge$flag==0]
#reddit_Inorai_test_merge$month_yr<-paste(reddit_Inorai_test_merge$month,reddit_Inorai_test_merge$day,sep = "_")

reddit_Inorai<-reddit_test_Inorai_test[, .(activity=.N), by=month_day]
plot(reddit_Inorai$activity,type="l")


all_months<-unique(reddit_Inorai_test_merge$month_day)
all_months<- all_months[order(all_months)]
#all_edges_cum<-c()
all_edges_ncum<-c()
for(month_index in 1:length(all_months))
{ 
  month <- all_months[month_index]
  reddit_Inorai_test_merge_2=reddit_Inorai_test_merge[reddit_Inorai_test_merge$month_day==month]
  reddit_Inorai_test_merge_2$date<-month
  all_edges_ncum<-rbind(all_edges_ncum,reddit_Inorai_test_merge_2[,c("date","unique_users","unique_users_2")])
  print(month)
}  


all_edges_ncum<-data.table(all_edges_ncum)
#Coreness
k<-data.frame(matrix(ncol = 2,nrow=length(unique(all_months))))
colnames(k)<-c("Date","Coreness")
i<-1
for(month_index in 1:length(all_months))
{ 
  month <- all_months[month_index]
  all_edges_m<-all_edges_cum[all_edges_cum$date==month] 
  
  p<-graph_from_edgelist(as.matrix(all_edges_m[,2:3]),directed = FALSE) 
  c<-mean(coreness(p))
  k[i,"Date"]<-month
  k[i,"Coreness"]<-c
  i = i + 1
  
}
as.data.frame(k)
plot(k$Coreness,type="l")

### Plots through time
all_months_2<-unique(all_edges_ncum$date)
all_months_2 <- all_months_2[order(all_months_2)]
for(month_index in seq(1,length(all_months_2),1))
{ 
  month <- all_months_2[month_index]
  all_edges_graph_m<-all_edges_ncum[all_edges_ncum$date==month] 
  
  p_graph<-graph_from_edgelist(as.matrix(all_edges_graph_m[,1:2]),directed = FALSE) 
  plot.igraph(p_graph,
              vertex.label=NA,
              layout=layout.fruchterman.reingold, 
              vertex.label.color="black",
              edge.color="black",
              vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
  
  
}

reddit_test_1<-reddit_test[reddit_test$year==2016]
reddit_test_active_users<-reddit_test_1%>% 
  group_by(month,username) %>%
  summarise(activity=n())
reddit_always_active_users<-reddit_test_active_users[reddit_test_active_users$activity<10]
reddit_test_active_users_grp<-reddit_always_active_users%>% 
  group_by(username) %>%
  summarise(activity_months=n())
#Goonboo<-always active
#_Dorkus_Prime_<- intermittently active
#ancientfutureguy<-always inactive

reddit_test_always_active_user<-reddit_test_1[reddit_test_1$username=="Goonboo"]
reddit_test_always_active_user<-data.table(reddit_test_always_active_user)
always_active_user_trend<-reddit_test_always_active_user[, .(unique_subreddits = uniqueN(subreddit),activity=.N), by=month]



# intermittently_active<-reddit_test_1[reddit_test_1$username==""]
# intermittently_active<-data.table(intermittently_active)
# intermittently_active_user_trend<-intermittently_active[, .(unique_subreddits = uniqueN(subreddit),activity=.N), by=month]

always_inactive<-reddit_test_1[reddit_test_1$username=="ancientfutureguy"]
always_inactive<-data.table(always_inactive)
always_inactive_user_trend<-always_inactive[, .(unique_subreddits = uniqueN(subreddit),activity=.N), by=month]



reddit_test_active_users_grp<-reddit_test_active_users%>% 
  group_by(month) %>%
  summarise(activity_mean=median(activity))

reddit_test_active_users<-data.table(reddit_test_active_users)
reddit_test_2<-reddit_test_active_users[reddit_test_active_users$month=="01" & reddit_test_active_users$activity>17]
#1TrueKnight<-inactive
#keldohead<-active


plot(always_active_user_trend$unique_subreddits,type="l")
plot(always_inactive_user_trend$unique_subreddits,type="l")

always_inactive_user_detailed<-always_inactive[, .(unique_subreddits = unique(subreddit),activity=.N), by=month]
always_active_user_detailed<-reddit_test_always_active_user[, .(unique_subreddits = unique(subreddit),activity=.N), by=month]

write.csv(always_inactive_user_detailed,"inactive_user.csv")
write.csv(always_active_user_detailed,"active user.csv")


#### regressions#############
reddit_centrality = fread("reddit_centrality.csv", header = TRUE)
head(reddit_test)
reddit_activity<-reddit_test[reddit_test$year==2016 & reddit_test$month>=10]%>% 
  group_by(username) %>%
  summarise(activity=n())

reddit_age<-reddit_test%>% 
  group_by(username) %>%
  summarise(min_age=min(use_time),max_age=max(use_time))

reddit_age$age_diff<-format(reddit_age$max_age-reddit_age$min_age,"%m")

# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); 
lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
# take it for a spin
reddit_age$age_diff<-mondf(reddit_age$min_age,reddit_age$max_age)
regression_merge<-merge(x=reddit_activity,y=reddit_age)
regression_merge<-merge(x=regression_merge,y=reddit_centrality[reddit_centrality$yearquarter=="2016 Q4"])
regression_merge$std_betw<-scale(regression_merge$betw)
regression_merge$std_close<-scale(regression_merge$close)
regression_merge$degree<-regression_merge$degree_in+regression_merge$degree_out

model1<-glm.nb(activity ~  age_diff + degree+eigenv, data=regression_merge)
summary(model1)

cor(regression_merge$activity,regression_merge$age_diff)


##Insights
#get average comments per user per month
reddit_test_check<-data.table(reddit_test[reddit_test$year==2016])
reddit_test_grp_check<-reddit_test[, .(unique_users = uniqueN(username),activity=.N), by=month]
reddit_test_grp_check$avg_comments_per_user<-reddit_test_grp_check$activity/reddit_test_grp_check$unique_users
#get users who have been perennially active
reddit_test_active_users<-reddit_test_check %>% 
  group_by(username) %>%
  summarise(activity=n())
reddit_test_active_users$average_activity<-reddit_test_active_users$activity/12
reddit_test_active_users<-data.table(reddit_test_active_users)
reddit_test_active_users<-reddit_test_active_users[reddit_test_active_users$average_activity>50]
reddit_test_active_users_2<-reddit_test_active_users[reddit_test_active_users$activity==1000]
reddit_test_active_users_3<-merge(x=reddit_test_active_users_2,y=reddit_test_check)
DT2<-data.table(reddit_test_active_users_3)
reddit_test_active_users_3_grp<-DT2[, .(unique_subreddits = uniqueN(subreddit),activity=.N), by = username]
hist(reddit_test_active_users_3_grp$unique_subreddits)

reddit_test_active_users_fake<-reddit_test_active_users[reddit_test_active_users$activity>1000]
###creating user network based on if they comment on the same subreddit on the same day##
reddit_test_grp<-reddit_test_1 %>% 
  group_by(subreddit,username) %>%
  summarise(usernames=max(day),count=n())

reddit_test_grp_2<-reddit_test_grp %>% 
  group_by(subreddit) %>%
  summarise(users=n())

reddit_test_grp_2<-as.data.table(reddit_test_grp_2)
reddit_test_grp_3<-reddit_test_grp_2[reddit_test_grp_2$users>2]

reddit_test_grp<-as.data.table(reddit_test_grp)
reddit_test_grp_4<-reddit_test_grp[reddit_test_grp$subreddit %in% reddit_test_grp_3$subreddit]
########################


### Create a co-occurence matrix and check again###########
head(reddit_test_grp_4)

edgeList<-cbind(reddit_test_grp_4[1:500,c("username")],reddit_test_grp_4[1:500,c("subreddit")])
m1<-graph.data.frame(edgeList,directed = TRUE)
plot(m1)
V(m1)$type <- bipartite_mapping(m1)$type
bipartite_matrix<-data.frame(as_incidence_matrix(m1))
#write.csv(bipartite_matrix,"bipartite_check.csv")
dis<-as.matrix(dist(bipartite_matrix,'Jaccard')) 
dis[1:5,1:5] 
reddit_test_grp_4[reddit_test_grp_4$username=="Avraham20" ] 
reddit_test_grp_4[reddit_test_grp_4$username=="gummybuns" ] 

#####################################################
DT<-data.table(reddit_test_1)
reddit_test_grp<-DT[, .(unique_subreddits = uniqueN(subreddit),activity=.N), by = username]
plot(reddit_test_grp$activity,reddit_test_grp$unique_subreddits)
element_text(family = NULL, face = NULL, colour = NULL, size = NULL,
             hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
             color = NULL)
ggplot(reddit_test_grp, aes(x=unique_subreddits,y=activity)) + geom_smooth(method = "loess", se = T)+theme_bw() +theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                                                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+labs(x = "Number of unique subreddits", y = "Activity") 


edgeList<-reddit_test_1[,c("username","subreddit")]
m1<-graph.data.frame(edgeList,directed = TRUE)
centralities<-as.data.frame(cbind(names(degree(m1)),degree(m1),closeness(m1,normalized = TRUE),betweenness(m1,normalized = TRUE)))
colnames(centralities)<-c("username","degree","closeness","betweenness")

final_merge<-merge(x=reddit_test_grp,y=centralities)
#ggplot(final_merge, aes(x=unique_subreddits,y=betweenness)) + geom_smooth(method = "loess", se = T)
#plot(final_merge$unique_subreddits,final_merge$betweenness)
#,
final_merge$user_group<-ifelse(final_merge$activity<=5,"inactive",
                               ifelse(final_merge$activity<=30,"less active",
                                      ifelse(final_merge$activity<=70,"active","very active")))
write.csv(final_merge,"user_classification.csv")

hist(reddit_test_grp$activity,nclass = 100,xlab = "Comments per user", ylab = "Frequency",main="Histogram of Activity")+theme_bw()


quantile(reddit_test_grp$activity, c(.1,.2,.3,.4, .5,.6,.7,.8,.9,.98)) 

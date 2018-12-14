library(splitstackshape)
library(data.table)
install.packages("readxl")
library(readxl)
install.packages("g.data")
library(g.data)
install.packages("manipulate")
library(igraph)
install.packages("DataCombine")
library(DataCombine)
install.packages(stats)
install.packages("dplyr")
library(dplyr)

#Importing data
venture_0 = fread("Funding_events_7.14.csv", header = TRUE)
venture_1 = fread("Funding_events_7.14_page2.csv",header = TRUE)
venture_2=rbind(venture_0,venture_1)

##Data cleaning steps#############
# Company names such as Inc and Limited were noticed 
venture_2=venture_2[venture_2$Investors!=""]

venture_2$Investors = gsub(pattern = ", Inc", replacement = " Inc", venture_2$Investors)
venture_2$Investors = gsub(pattern = ",Inc", replacement = " Inc", venture_2$Investors)
venture_2$Investors = gsub(pattern = ", Ltd", replacement = " Ltd", venture_2$Investors)
venture_2$Investors = gsub(pattern = ",Ltd", replacement = " Ltd", venture_2$Investors)
venture_2$Investors = gsub(pattern = ", LLC", replacement = " Inc", venture_2$Investors)
venture_2$Investors = gsub(pattern = ", Limited", replacement = " Limited", venture_2$Investors)


outcomes= fread("Venture_capital_firm_outcomes.csv",header = TRUE)
all_edges<-c()
all_edges_ncum<-c()
all_edges_cum<-c()
all_edges_decay<-c()
venture_3=data.table(venture_2)
#There were issues with the date variable being imported and were changed in the spreadsheet itself.
#The Month_Yr variable was created in excel. An alternative method usong data tables was found later
#setDT(venture_3)[, Month_Yr := format(as.Date(venture_3$`Deal Date`,"%m/%d/%y"), paste("%Y%m")) ]
#venture_4<-venture_3[ order(venture_3$Month_Yr , decreasing = FALSE ),]


min(venture_3$Month_Yr)
all_months<-unique(venture_3$Month_Yr)
all_months <- all_months[order(all_months)]
max(all_months)

######### Data creation #####
#Three datasets for cumulative,non-cumulative and decay were created by altering 
#few vital parts of code as mentioned below
#for (month_index in 201309:max(all_months))
for(month_index in 1:length(all_months))
{ 
  month <- all_months[month_index]
  #ifelse(month > 199107, 
   #      (venture_2 = venture_3 %>% filter(Month_Yr <= month & Month_Yr > (month - 1000))),
    #     (venture_2 = venture_3 %>% filter(Month_Yr <= month)))
  #decay
  venture_2<-venture_3[venture_3$Month_Yr==month] #for non-cumulative 
  #venture_2<-venture_3[venture_3$Month_Yr<=month] #for cumulative 

  partners =data.table(venture_2$"Investors") 
  colnames(partners) = "partners"
  
  # names are sepaerated by a semicolon in the file # create a new column for each name 
  partners = cSplit(partners, "partners", ",") 
  partners<-partners[1:200,]
  
  
  
  total_names_in = lapply(seq_len(nrow(partners)), function(i) t(partners[i]))
  total_names_in = data.table(do.call(rbind, total_names_in))
  for(i in seq_along(total_names_in)){
    total_names_in[[i]] = total_names_in[[i]][!is.na(total_names_in[[i]])]
  }
  
  # then take combinations of each of the pairs of people that remain with combn 
  # but only do this when there are pairs (moderate this with the last call to length)
  
  
  total_names_in = lapply(seq_along(total_names_in), function(i) tryCatch(t(combn(total_names_in[[i]], 2)), error = function(e) cbind("", "")))
  total_names_in = data.table(do.call(rbind, total_names_in))
  
  # get rid of blanks one more time
  total_names_in = total_names_in[which(total_names_in[,2]!="")]
  
  # this would have also removed emails to the same person #total_names =total_names[which(total_names[,2]!=total_names[,1])]
  colnames(total_names_in)<-c("Partner_1","Partner_2")
  total_names_in$date<-month
  all_edges_ncum<-rbind(total_names_in,all_edges_ncum)
  #all_edges_decay
  #all_edges_cum
  print(month_index)
}
####  Question 1 ##############

all_edges_ncum_1<-all_edges_ncum[,1:2]
all_edges_ncum_1<-unique(all_edges_ncum_1)

#Calculate closeness,distances
p_1<-graph_from_edgelist(as.matrix(all_edges_ncum_1),directed = FALSE)
plot(p_1)
which.max(closeness(p_1))
distance_p_1<-distances(p_1)
distance_p_1_2<-as.matrix(ifelse(distance_p_1=="Inf",nrow(distance_p_1)-1,distance_p_1))
which.min((rowSums(distance_p_1_2)))
mean(rowMeans(distance_p_1_2))
################# Question 2##########
#all_months_2<-unique(all_edges$date)
# all_months_2 <- all_months[order(all_months_2)]
#The cumulative edge list created in the data creation step is used
all_edges_dup=all_edges_cum

k<-data.frame(matrix(ncol = 2,nrow=length(unique(all_months))))
colnames(k)<-c("Date","Coreness")
i<-1
for(month_index in 1:length(all_months))
{ 
  month <- all_months[month_index]
  all_edges_m<-all_edges_dup[all_edges_dup$date==month] 
  
  p<-graph_from_edgelist(as.matrix(all_edges_m[,1:2]),directed = FALSE) 
  c<-mean(coreness(p))
  k[i,"Date"]<-month
  k[i,"Coreness"]<-c
  i = i + 1
  
}
as.data.frame(k)
plot(k$Date,k$Coreness,type="l")

###################  2b ##################
##The dataset obtained by running the decay conditions in the initial datset creationstep is used
all_edges_dup_decay=all_edges_decay

decay<-data.frame(matrix(ncol = 2,nrow=length(unique(all_months))))
colnames(decay)<-c("Date","Coreness")
i<-1
for(month_index in 1:length(all_months))
{ 
  month <- all_months[month_index]
  all_edges_m_decay<-all_edges_dup_decay[all_edges_dup_decay$date==month] 
  
  p<-graph_from_edgelist(as.matrix(all_edges_m_decay[,1:2]),directed = FALSE) 
  c<-mean(coreness(p))
  decay[i,"Date"]<-month
  decay[i,"Coreness"]<-c
  i = i + 1
  
}
as.data.frame(decay)
plot(decay$Date,decay$Coreness,type="l")

################## Question 3############################
#The non cumulative edge list is used to see how the graph varies over time
all_months_2<-unique(all_edges_ncum$date)
all_months_2 <- all_months_2[order(all_months_2)]
for(month_index in seq(1,length(all_months_2),10))
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
##Descriptive statistics to confirm core-periphery network
p_graph_2<-graph_from_edgelist(as.matrix(all_edges_ncum[,1:2]),directed = FALSE) 
cluster_walktrap(p_graph_2, steps=10)
transitivity(p_graph_2)

plot(p_graph_2)







######### Question 4 ###############
##USe non-cumulative edge list
### Let us calculate centrality measures for the nodes and then merge the file with
#the outcomes to get relevant statistics
all_edges_m=all_edges_ncum

g<-all_edges_m[,1:2]
g_1<-graph_from_edgelist(as.matrix(g))
centrality_stats<-cbind(closeness(g_1),degree(g_1),betweenness(g_1))
centrality_stats<-as.data.frame(cbind(names(degree(g_1)),centrality_stats))
colnames(centrality_stats)<-c("firm_name","closeness","degree","betweenness")



success<-outcomes %>% 
  group_by(firm_name) %>%
  summarise(successful_investments_avg = mean(successful_investments),failed=mean(out_of_business))

success_table <-merge(x=success,y=centrality_stats,by="firm_name",all.x =TRUE )
success_table <- subset(success_table, closeness!=0)


cor(success_table$closeness,success_table$successful_investments_avg)
cor(success_table$degree,success_table$successful_investments_avg)
cor(success_table$betweenness,success_table$successful_investments_avg)


cor(success_table$closeness,success_table$failed)
cor(success_table$degree,success_table$failed)
cor(success_table$betweenness,success_table$failed)



#edges_unique_success=edges_unique[!(edges_unique$Partner_1 %in% outcomes_1$firm_name)]
#edges_unique_success_1=edges_unique_success[!(edges_unique_success$Partner_2 %in% outcomes_1$firm_name)]
#Reading our data into R and doing some EDA
table(duplicated(data))
str(Data88)
data8<-Data88[,-1]
head(data8)
colSums(is.na(data8))
data2<-laterite_mobilemoney_data
table(unique(Data88))
#Looking for unique items in the data
library(dplyr)
data3<-distinct(data43,hhid,.keep_all = T)
head(data3)
View(data3)
#Adding new variables to the data set
data3$financially_excluded<-ifelse(data3$account_type=="None","yes","no")
data3$digital_financial_inclusion<-ifelse(data3$account_type=="None","No","Yes")
#Visualizing finacial exclusion according to districts
library(ggplot2)
ggplot(data3,aes(x=district,y=financially_excluded,fill=district))+
  geom_bar(stat = "identity",width=1)
#Answering the question what are the overall rates for the combines population of these three distrcts???
ggplot(data=data3,aes(x=factor(district),y=prop.table(stat(count)),fill=factor(financially_excluded),label=scales::percent(prop.table(stat(count)))))+
  geom_bar(position="dodge")+geom_text(stat='count',position = position_dodge(.9),vjust=-0.5,size=3)+scale_y_continuous(labels = scales::percent)+
  labs(x="district",y="financially Included",fill="district")+ggtitle("RATES OF FINANCIAL EXCLUSION")
#digitally finacially  included
ggplot(data=data3,aes(x=factor(district),y=prop.table(stat(count)),fill=factor(digital_financial_inclusion),label=scales::percent(prop.table(stat(count)))))+
  geom_bar(position="dodge")+geom_text(stat='count',position = position_dodge(.9),vjust=-0.5,size=3)+scale_y_continuous(labels = scales::percent)+
  labs(x="district",y="financially Included",fill="district")+ggtitle("RATES OF FINANCIAL INCLUSION")

head(data3)
View(data3)
#Describe how the mobile money market is divided between is divided between the three companies include atleast one chart or table to i
#illustrate your findings
pie(table(data3$account_type=="Mobile Money",data3$mm_account_telco))
data3$mobile_money<-ifelse(data3$account_type=="Mobile Money","Mobile__Money","0")
View(data3)
ggplot(data3,aes(x=mm_account_telco,y=mobile_money,fill=mm_account_telco))+geom_bar(stat = "identity",width=1)+labs(x="COMPANY",y="DISTRIBUTION OF MOBILE MONEY",fill="district")+ggtitle("DISTRIBUTION OF MOBILE MONEY AMONG COMPANY A,B and C")
#Determining the share of customers who have experienced failed money transactiona both in urban and rural and urban villages
#we shall use T test since our sample size is big
 data3$Urban_failed<-ifelse(data3$urban=="Urban"& data3$v240=="yes","1","0")
View(data3) 
data3$Rural_failed<-ifelse(data3$urban=="Rural"& data3$v240=="yes","1","0")

sd(data3$Urban_failed)
sd(Rural_failed)
z.test(Urban_failed,Rural_failed,alternative="two.sided")
#calculating diffrent means of the diffrent variables using t.test
x<-as.numeric(data3$Urban_failed)

y<-as.numeric(data3$Rural_failed)
t.test(x,y,paired = F,alternative = "two.sided")
#What variables are good predictors that someone will stop using their mobile money account?
#Discuss what causes a customer to stop using their mobile money account incuding how strong the evidence is?
data3$mm_account_cancelled<-ifelse(data3$mm_account_cancelled=="yes","1","0")
if(data3$highest_grade_completed){
    
data4<-data3  
library(caret)  
 dmy<-dummyVars("~.",data = data4,fullRank = T)
 dat_transformed<-data.frame(predict(dmy,newdata=data3))
 str(dat_transformed)
  
 View(dat_transformed)
  
  
}

model<-lm(mm_account_cancelled~weight+account_num+district+urban+gender+age+hh_members)

head(data3[6,])
#We will convert all the x variables i.e categorical variables so as to be able to feed them to the 
#linear model
data4<-data3
head(data4)
#we take care of our multilvel variables by binning process
bins<-c(-inf,District_A,District_B,District_C,inf)
bin_names<-c("1","2","3")

data4$district2<-as.numeric(data4$district)
View(data4)  
cut(data4$district,breaks = 3,labels = c("1","2","3"))
#Creating an encoding function 
encode_ordinal<-function(x,order=unique(x)){
  x<-as.numeric(factor(x, levels = order,exclude = NULL))
  x
}
#The ordinal encoding function will automatically replace the NA values into 0 hence no need to deal with missing values
data4$district2<-encode_ordinal(data4$district) 
data4$account_type2<-encode_ordinal(data4$account_type)
data4$highest_grade_completed2<-encode_ordinal(data4$highest_grade_completed)
data4$mm_account_telco2<-encode_ordinal(data4$mm_account_telco)
data4$mm_account_telco_main2<-encode_ordinal(data4$mm_account_telco_main)
colSums(is.na(data4))
 #inserting the median 
data4$v234[which(is.na(data4$v234))]=mode(data4$v234)
data4$agent_trust[which(is.na(data4$agent_trust))]=mode(data4$agent_trust)
data4$v236[which(is.na(data4$v236))]=mode(data4$v236)
data4$v237[which(is.na(data4$v237))]=mode(data4$v237)
data4$v238[which(is.na(data4$v238))]=mode(data4$v238)
data4$v240[which(is.na(data4$v240))]=mode(data4$v240)
data4$v241[which(is.na(data4$v241))]=mode(data4$v241)
data4$v242[which(is.na(data4$v242))]=mode(data4$v242)
data4$v243[which(is.na(data4$v243))]=mode(data4$v243)
data4$v244[which(is.na(data4$v244))]=mode(data4$v244)
data4$v245[which(is.na(data4$v245))]=mode(data4$v245)
data4$v246[which(is.na(data4$v246))]=mode(data4$v246)
data4$mm_trust[which(is.na(data4$mm_trust))]=mode(data4$mm_trust)
data4$prefer_cash[which(is.na(data4$prefer_cash))]=mode(data4$prefer_cash)

#We will use multilinear regression to determine which variables are the best predictors that someone will cancel their mobile mone account 
set.seed(1234)
model<-lm(mm_account_cancelled~weight+account_num+district2+urban+gender+age+hh_members+mm_account_telco_main2+mm_account_telco2
          +highest_grade_completed2+account_type2+mobile_money+digital_financial_inclusion+financially_excluded+v236+v237+v238
          +v240+v241+v242+v243+v245+v246+mm_trust+agent_trust+prefer_cash,data = data4
            )
summary(model)  

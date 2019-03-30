#Authors: 
#1 Sarthak
#2 Aman
#3 Vijay
#4 Anand Vaishnao

library(stringr)
library(tidyr)
#library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot) #for plot_grid
library(lubridate)
library(corrplot)
library(gridExtra)
library(caTools) #sample.split
library(MASS) #stepAIC
library(caret) #caret::confusionMatrix
library(car) #vif
library(Information) #IV values
library(InformationValue)  #ks plot
library(fuzzyjoin) #fuzzy_innter_join
library(DMwR) #smote technique
library(randomForest)
library(woeBinning) #scorecard cutoff using woe value


#set working directory if required
#loading the dataframes and treating blank as NA
credit_bureau <- read.csv("Credit Bureau data.csv",stringsAsFactors = FALSE,na.strings = c("NA",'',"Na",' '))
demographic <- read.csv("Demographic data.csv",stringsAsFactors = FALSE,na.strings = c("NA",'',"Na",' '))

#checking dimensional structure of dataframes
dim(demographic) 
#71295x12
dim(credit_bureau) 
#71295x19

#structure of data
str(demographic)
str(credit_bureau)
##application id looks like the applicants, lets look at unique applicants as it should be equal to df size

#unique rows
length(unique(demographic$Application.ID))  
#71292.

length(unique(credit_bureau$Application.ID)) 
#71292

#removing repeated application ids
demographic <- distinct(demographic, Application.ID,.keep_all = TRUE)
credit_bureau <- distinct(credit_bureau, Application.ID,.keep_all = TRUE)

#merging both the datasets using application_id key
merged_dataset <- merge(x = demographic, y = credit_bureau, by = "Application.ID")

#keeping one performance tag column
merged_dataset <- merged_dataset[ , !(names(merged_dataset) %in% 'Performance.Tag.x')]

box_theme_x<-theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())
box_theme_y<-theme(axis.title.y=element_blank())
plot_fun_continuous <- function(cont_col_name,var_name){
  
  plot<-  ggplot(merged_dataset, aes(x="",y=cont_col_name))+ geom_boxplot(width=0.1) + ggtitle(var_name)  + box_theme_x + box_theme_y
  return(plot)
}

plot_cont_1 <-plot_fun_continuous(merged_dataset$Age,"Age")
plot_cont_2 <-plot_fun_continuous(merged_dataset$No.of.dependents,"No.of.dependents")
plot_cont_3 <-plot_fun_continuous(merged_dataset$Income,"Income")
plot_cont_4 <-plot_fun_continuous(merged_dataset$No.of.months.in.current.residence,"No.of.months.in.current.residence")
plot_cont_5 <-plot_fun_continuous(merged_dataset$No.of.months.in.current.company,"No.of.months.in.current.company")
plot_cont_6 <-plot_fun_continuous(merged_dataset$No.of.times.90.DPD.or.worse.in.last.6.months,"No.of.times.90.DPD.or.worse.in.last.6.months")
plot_cont_7 <-plot_fun_continuous(merged_dataset$No.of.times.60.DPD.or.worse.in.last.6.months,"No.of.times.60.DPD.or.worse.in.last.6.months")
plot_cont_8 <-plot_fun_continuous(merged_dataset$No.of.times.30.DPD.or.worse.in.last.6.months,"No.of.times.30.DPD.or.worse.in.last.6.months")
plot_cont_9 <-plot_fun_continuous(merged_dataset$No.of.times.90.DPD.or.worse.in.last.12.months,"No.of.times.90.DPD.or.worse.in.last.12.months")
plot_cont_10 <-plot_fun_continuous(merged_dataset$No.of.times.60.DPD.or.worse.in.last.12.months,"No.of.times.60.DPD.or.worse.in.last.12.months")
plot_cont_11<-plot_fun_continuous(merged_dataset$No.of.times.30.DPD.or.worse.in.last.12.months,"No.of.times.30.DPD.or.worse.in.last.12.months")
plot_cont_12 <-plot_fun_continuous(merged_dataset$Avgas.CC.Utilization.in.last.12.months,"Avgas.CC.Utilization.in.last.12.months")
plot_cont_13 <-plot_fun_continuous(merged_dataset$No.of.trades.opened.in.last.6.months,"No.of.trades.opened.in.last.6.months")
plot_cont_14 <-plot_fun_continuous(merged_dataset$No.of.trades.opened.in.last.12.months,"No.of.trades.opened.in.last.12.months")
plot_cont_15 <-plot_fun_continuous(merged_dataset$No.of.PL.trades.opened.in.last.6.months,"No.of.PL.trades.opened.in.last.6.months")
plot_cont_16 <-plot_fun_continuous(merged_dataset$No.of.PL.trades.opened.in.last.12.months,"No.of.PL.trades.opened.in.last.12.months")
plot_cont_17 <-plot_fun_continuous(merged_dataset$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans")
plot_cont_18 <- plot_fun_continuous(merged_dataset$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans")
plot_cont_19 <- plot_fun_continuous(merged_dataset$Outstanding.Balance,"Outstanding.Balance")
plot_cont_20 <-plot_fun_continuous(merged_dataset$Total.No.of.Trades,"Total.No.of.Trades")

grid.arrange(plot_cont_1,plot_cont_2,plot_cont_3,plot_cont_4)
grid.arrange(plot_cont_5,plot_cont_6,plot_cont_7,plot_cont_8)
grid.arrange(plot_cont_9,plot_cont_10,plot_cont_11,plot_cont_12)
grid.arrange(plot_cont_13,plot_cont_14,plot_cont_15,plot_cont_16)
grid.arrange(plot_cont_17,plot_cont_18,plot_cont_19,plot_cont_20)

######################################DATA CLEANING

#Checking how many minors have applied for a credit card
nrow(merged_dataset %>% filter(Age < 18))
#only 65 rows: 0.09%

#Removing all ages which are less than 18 considering them as minors
merged_dataset <-merged_dataset %>% filter(Age>=18)

#Income variable. 
nrow(merged_dataset %>% filter(Income <= 0))
#only 106 rows after removing age else it was 107:  0.15%

#As the % is below 2, Removing all incomes which are less equal to 0
merged_dataset <- merged_dataset %>% filter(Income > 0)

#checking NA's in the data
sum(is.na(merged_dataset)) #3168
summary(merged_dataset) 
#most NA values corresponding to Avgas.CC.Utilization.in.last.12.months(1051) & Performance.Tag.y (1425)

#keeping a dataframe for NA performance tag values
merged_dataset_NA <- merged_dataset[which(is.na(merged_dataset$Performance.Tag.y)),]

#Removing NA values corresponding to Performance.Tag.y from base dataset and examining it
merged_dataset <- merged_dataset[!(is.na(merged_dataset$Performance.Tag.y)),]
summary(merged_dataset)
#Avgas.CC.Utilization.in.last.12.months is still having 1016 records corresponding to NA values.
#It accounts to 1% of values. So will just remove the NA values

merged_dataset <- na.omit(merged_dataset)
#######Outlier treatment of numeric variables

str(merged_dataset)


#Outlier treatment and imputing missing value
# The Boxplots showed outliers for No.of.months.in.current.company, 
# Avgas.CC.Utilization.in.last.12.months and Total.No.of.Trades. 
#Here confirming it with percentiles for all the continuous variables
sapply(merged_dataset[,c("Income", "No.of.months.in.current.residence", 
                           "No.of.months.in.current.company", "Avgas.CC.Utilization.in.last.12.months",
                           "Outstanding.Balance","Total.No.of.Trades")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T))

#There is a sudden jump in No.of.months.in.current.company values between 99% and 100%.
#So, all values above 74 (99%) are capped to 74.
merged_dataset$No.of.months.in.current.company[which(merged_dataset$No.of.months.in.current.company>74)] <- 74

#There is a sudden jump in Avgas.CC.Utilization.in.last.12.months values between 94% and 95%.
#So, all values above 91 (94%) are capped to 91.
merged_dataset$Avgas.CC.Utilization.in.last.12.months[which(merged_dataset$Avgas.CC.Utilization.in.last.12.months>91)] <- 91

#There is a sudden jump in Total.No.of.Trades values between 99% and 100%.
#So, all values above 31 (99%) are capped to 31.
merged_dataset$Total.No.of.Trades[which(merged_dataset$Total.No.of.Trades>31)] <- 31

############################################################################
#binning
#merged_dataset <- mutate(merged_dataset, age_grp = ifelse(Age <=30, 'Young',if_else(Age > 30 & Age <= 60, 'MiddleAge','SeniorCitizen')))
#merged_dataset$age_grp <- factor(merged_dataset$age_grp, levels = c("Young","MiddleAge","SeniorCitizen"))
merged_dataset$age_grp <- cut(merged_dataset$Age, breaks=c(10,20,30,40,50,60,70))

#merged_dataset <- mutate(merged_dataset, salary_grp = ifelse(Income <=10, 'Low Income',if_else(Income > 10 & Income <= 30, 'Middle Income','High Income')))
#merged_dataset$salary_grp <- factor(merged_dataset$salary_grp, levels = c("Low Income","Middle Income","High Income"))
merged_dataset$salary_grp <- cut(merged_dataset$Income, breaks=c(0,10,20,30,40,50,60))

merged_dataset$No.of.months.in.current.residence_grp <- cut(merged_dataset$No.of.months.in.current.residence, breaks=c(0,20,40,60,80,100,120,140))
merged_dataset$No.of.months.in.current.company_grp <- cut(merged_dataset$No.of.months.in.current.company, breaks=c(0,20,40,60,80,100,120,140))

merged_dataset$Avgas.CC.Utilization.in.last.12.months_grp <- cut(merged_dataset$Avgas.CC.Utilization.in.last.12.months, breaks=c(-1,40,80,120))

merged_dataset$Outstanding.Balance_grp <- cut(merged_dataset$Outstanding.Balance,5)


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

ggplot_func <- function(p,q) {
  plot <- ggplot(merged_dataset, 
                 aes(x = as.matrix(merged_dataset[,p]),fill = factor(Performance.Tag.y) )) + 
    geom_bar(stat="count",position = "dodge") + 
    scale_fill_manual(labels = c("non default","default"), values = c(gg_color_hue(2) )) +
    guides(fill=guide_legend("Performance Tag")) +
    labs(title= paste0(q,' Plot'), x=q) +
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
  return (plot)
}



#ggplots
#"Age
#plot_2 <- ggplot_func(2,"Age")
#"Gender
plot_3 <- ggplot_func(3,"Gender")
#"Marital.Status..at.the.time.of.application.
plot_4 <- ggplot_func(4,"Marital.Status.")
#"No.of.dependents
plot_5 <- ggplot_func(5,"No.of.dependents")
#"Income
#plot_6 <- ggplot_func(6,"Income")
#"Education
plot_7 <- ggplot_func(7,"Education")
#"Profession
plot_8 <- ggplot_func(8,"Profession")
#"Type.of.residence
plot_9 <- ggplot_func(9,"Type.of.residence")

#"No.of.months.in.current.residence
#plot_10 <- ggplot_func(10,"No.of.months.in.current.residence")

#"No.of.months.in.current.company
#plot_11 <- ggplot_func(11,"No.of.months.in.current.company")

#"No.of.times.90.DPD.or.worse.in.last.6.months
plot_12 <- ggplot_func(12,"90.DPD.or.worse.in.last.6.months")
#"No.of.times.60.DPD.or.worse.in.last.6.months
plot_13 <- ggplot_func(13,"60.DPD.or.worse.in.last.6.months")
#"No.of.times.30.DPD.or.worse.in.last.6.months
plot_14 <- ggplot_func(14,"30.DPD.or.worse.in.last.6.months")
#"No.of.times.90.DPD.or.worse.in.last.12.months
plot_15 <- ggplot_func(15,"90.DPD.or.worse.in.last.12.months")
#"No.of.times.60.DPD.or.worse.in.last.12.months
plot_16 <- ggplot_func(16,"60.DPD.or.worse.in.last.12.months")
#"No.of.times.30.DPD.or.worse.in.last.12.months
plot_17 <- ggplot_func(17,"30.DPD.or.worse.in.last.12.months")
#"Avgas.CC.Utilization.in.last.12.months
#plot_18 <- ggplot_func(18,"Avgas.CC.Utilization.in.last.12.months")
#"No.of.trades.opened.in.last.6.months
plot_19 <- ggplot_func(19,"trades.opened.in.last.6.months")
#"No.of.trades.opened.in.last.12.months
plot_20 <- ggplot_func(20,"trades.opened.in.last.12.months")
#"No.of.PL.trades.opened.in.last.6.months
plot_21 <- ggplot_func(21,"PL.trades.opened.in.last.6.months")
#"No.of.PL.trades.opened.in.last.12.months
plot_22 <- ggplot_func(22,"PL.trades.opened.in.last.12.months")
#"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
plot_23 <- ggplot_func(23,"Inquiries.in.last.6.months..excluding.home...auto.loans.")
#"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
plot_24 <- ggplot_func(24,"Inquiries.in.last.12.months..excluding.home...auto.loans.")
#"Presence.of.open.home.loan
plot_25 <- ggplot_func(25,"Presence.of.open.home.loan")
#"Outstanding.Balance
#plot_26 <- ggplot_func(26,"Outstanding.Balance")
#"Total.No.of.Trades
plot_27 <- ggplot_func(27,"Total.No.of.Trades")
#"Presence.of.open.auto.loan
plot_28 <- ggplot_func(28,"Presence.of.open.auto.loan")
#"age_grp
plot_30 <- ggplot_func(30,"Bin Age")
#"salary_grp
plot_31 <- ggplot_func(31,"Bin Income")

plot_32 <- ggplot_func(32,"Bin No.of.months.in.current.residence")
#"salary_grp
plot_33 <- ggplot_func(33,"Bin No.of.months.in.current.company")

#"salary_grp
plot_34 <- ggplot_func(34,"Bin Avgas.CC.Utilization")

#utstanding.Balance
plot_35 <- ggplot_func(35,"Bin outstanding.Balance")

grid.arrange(plot_3,plot_4,plot_30,plot_31)
grid.arrange(plot_5,plot_7,plot_8,plot_9)
grid.arrange(plot_32,plot_33,plot_12,plot_13)
grid.arrange(plot_14,plot_15,plot_16,plot_17)
grid.arrange(plot_34,plot_19,plot_20,plot_21)
grid.arrange(plot_22,plot_23,plot_24,plot_35)
grid.arrange(plot_25,plot_27,plot_28)

plot_6 <- NULL
plot_10 <- NULL
plot_11 <- NULL
plot_18 <- NULL
plot_26 <- NULL

#renaming the performance tag colname correctly
colnames(merged_dataset)[which(names(merged_dataset) == "Performance.Tag.y")] <- "Performance.Tag"
str(merged_dataset)


#Removing the derived columns as they will result in multicollinearity
merged_dataset <- merged_dataset[ , -which(names(merged_dataset) %in% c("age_grp","salary_grp","No.of.months.in.current.residence_grp","No.of.months.in.current.company_grp","Avgas.CC.Utilization.in.last.12.months_grp","Outstanding.Balance_grp"))]



###############
###
##
#let check corelation between continous variables, using ggplot, but before that we need to create a DF of continuous variable

continous_variable_dataset <- merged_dataset %>% dplyr::select(-c("Gender","Marital.Status..at.the.time.of.application.","Education","Profession","Type.of.residence"))

#now we will check the coorelation
corr_plot <- cor(continous_variable_dataset)
corrplot(corr_plot, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.45, tl.col = 'black',
         order = "hclust", diag = FALSE)

#we see that   2   sets of clustures formed where below variables seem to be correlated

#2:6 variables Number of times 60-30 pd in 12 months, number of tims 30-60-90 dpd in 6 months and number of times 90 dpd in 12 months
#3: 6 variables 6-12 months exluding home autoloans, pl trades 6-12 months, trades opened 6-12 months


plot_corr_1 <- ggplot(merged_dataset,aes(x = No.of.trades.opened.in.last.12.months,
                                         y = No.of.trades.opened.in.last.6.months,
                                         col = factor(Performance.Tag))) + 
  geom_smooth() + 
  ggtitle("Corr: No of Trades open in last 6 vs 12 months") +
  labs(col = "Performance Tag")

#both increase simultaneously
plot_corr_2 <- ggplot(merged_dataset,aes(
                          x = No.of.PL.trades.opened.in.last.12.months,
                          y = No.of.trades.opened.in.last.12.months,
                          col = factor(Performance.Tag))) +
  geom_smooth() +
  ggtitle("Corr: No of Trades open in last 12 months vs PL trades in 12 months") + labs(col = "Performance Tag")


#both increase simultaneously

plot_corr_3 <- ggplot(merged_dataset,aes(
    x = Total.No.of.Trades,
    y = No.of.trades.opened.in.last.12.months,
    col = factor(Performance.Tag)))+ 
  geom_smooth() + 
  ggtitle("Corr: Total No of Trades and trades in 12 months") + 
  labs(col = "Performance Tag")

#both change simultaneously

plot_corr_4 <- ggplot(merged_dataset,aes(
  x = No.of.PL.trades.opened.in.last.12.months,
  y = No.of.PL.trades.opened.in.last.6.months,
  col = factor(Performance.Tag))) +
  geom_smooth() +
  ggtitle("Corr: No of Trades open in last 12 vs 6 months") + 
  labs(col = "Performance Tag")

#both change simultaneously
plot_corr_5 <- ggplot(merged_dataset,aes(
  x = No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
  y = No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,
  col = factor(Performance.Tag))) +
  geom_smooth() +
  ggtitle("Inquiries excluding home, autoloans last 12 vs 6 months") + 
  labs(col = "Performance Tag")

#both change simultaneously
plot_corr_6 <- ggplot(merged_dataset,aes(
  x = No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
  y = No.of.trades.opened.in.last.12.months,
  col = factor(Performance.Tag))) +
  geom_smooth() +
  ggtitle("Inquiries excluding home, autoloans vs trades opened in last 12 ") + 
  labs(col = "Performance Tag")


# Bivariant comparision 
grid.arrange(plot_corr_6,plot_corr_5,plot_corr_4,plot_corr_3,plot_corr_2,plot_corr_1)
grid.arrange(plot_corr_2,plot_corr_1)

## keeping these correlations in mind we will build and assess the Models in future
#############
###########
#######
#WOE and IV Analysis:
merged_dataset_woe <-  merged_dataset

str(merged_dataset_woe)
#removing application_id column as it is primary key and is redundant from now on

merged_dataset_woe <- merged_dataset_woe[,-which(names(merged_dataset_woe) %in% c("Application.ID"))]

#now we will create a Information value table using create_infotables command

IV <- Information::create_infotables(data=merged_dataset_woe,y = "Performance.Tag",parallel = FALSE)

#now we will look at the most important variables 

head(IV$Summary,10)

# Below features have > 0.3 IV value
  #"No of Inquiries in last 12 months excluding home auto loans"
  #"Avgas CC Utilization in last 12 months"
  #"No of PL trades opened in last 12 months"
  #"No of trades opened in last 12 months" 
#this indicates that these variable have Strong predictive Power 

# Below features have > 0.25 IV value 
  #"Outstanding Balance"
  #"Total No of Trade" 
#It means that they have Medium predictive Power.


#Creating separate data frame after replacing the values with woe_values using a custom function as mentioned in the stackoverflow link
##to replace data values with woe values
#https://stackoverflow.com/questions/48860641/how-to-replace-values-of-a-column-with-its-woe-values-in-1-shot-in-r

#####Although IV wise only 7 variables are strong, inorder to understand woe value impact we will replace all the values in dataset with WOE values and assess the model

woe_replace <- function(df_orig, IV) {
  df <- cbind(df_orig)
  df_clmtyp <- data.frame(clmtyp = sapply(df, class))
  df_col_typ <-
    data.frame(clmnm = colnames(df), clmtyp = df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    colmn_nm <- toString(df_col_typ[rownm, "clmnm"])    
    if(colmn_nm %in% names(IV$Tables)){
      column_woe_df <- cbind(data.frame(IV$Tables[[toString(df_col_typ[rownm, "clmnm"])]]))
      if (df_col_typ[rownm, "clmtyp"] == "factor" | df_col_typ[rownm, "clmtyp"] == "character") {
        df <-
          dplyr::inner_join(
            df,
            column_woe_df[,c(colmn_nm,"WOE")],
            by = colmn_nm,
            type = "inner",
            match = "all"
          )
        df[colmn_nm]<-NULL
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm
      } else if (df_col_typ[rownm, "clmtyp"] == "numeric" | df_col_typ[rownm, "clmtyp"] == "integer") {
        column_woe_df$lv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr("\\[", column_woe_df[,colmn_nm]) + 1,
          regexpr(",", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df$uv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr(",", column_woe_df[,colmn_nm]) + 1,
          regexpr("\\]", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df[colmn_nm]<-NULL      
        column_woe_df<-column_woe_df[,c("lv","uv","WOE")]      
        colnames(df)[colnames(df)==colmn_nm]<-"WOE_temp2381111111111111697"      
        df <-
          fuzzy_inner_join(
            df,
            column_woe_df[,c("lv","uv","WOE")],
            by = c("WOE_temp2381111111111111697"="lv","WOE_temp2381111111111111697"="uv"),
            match_fun=list(`>=`,`<=`) 
          )      
        df["WOE_temp2381111111111111697"]<-NULL      
        df["lv"]<-NULL      
        df["uv"]<-NULL      
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm      
      }}
  }
  return(df)
}



merged_dataset_woe_values <- woe_replace(merged_dataset_woe,IV)

#############IV infotable analysis
##we have 27 columns and 1 dependent variable, so 27 IV plot visualisation/analysis ahead

#1st table
IV$Tables[1]
#IV distribution for AGE
plot_IV_1<- plot_infotables(IV,"Age")
#Note: Age group between 51-53 higher chance of default as the woe value is lowest.

#2nd table
IV$Tables[2]
#IV distribution for Gender
plot_IV_2 <- plot_infotables(IV,"Gender")
#Note: Male has higher change of default

#3rd table
IV$Tables[3]
#IV distribution for 'Marital.Status..at.the.time.of.application.'
plot_IV_3 <-plot_infotables(IV,"Marital.Status..at.the.time.of.application.")
#Note: Married Applicants have higher change of default

#4th table
IV$Tables[4]
#IV distribution for 'No.of.dependents'
plot_IV_4 <- plot_infotables(IV,"No.of.dependents")
#Note: 2 dependents having Applicants have higher change of default


#5th table
IV$Tables[5]
#IV distribution for 'Income'
plot_IV_5 <-plot_infotables(IV,"Income")
#Note: As the income bin value range increases, the customer starts defaulting more, highest being for 49-60 income group

#6th table
IV$Tables[6]
#IV distribution for 'Education'
plot_IV_6 <-plot_infotables(IV,"Education")
#Note: Masters have 0 woe, means both masters doing as well as not doing applicants have similar default rates and woe weightages
##others education seem to be a very good choice for bank as they have high woe, meaning non-default but it has only 113 applicants

#7th table
IV$Tables[7]
#IV distribution for 'Profession'
plot_IV_7 <-plot_infotables(IV,"Profession")
#Note: Sal profession holding customers default more

#8th table
IV$Tables[8]
#IV distribution for 'Type.of.residence'
plot_IV_8 <-plot_infotables(IV,"Type.of.residence")
#Note: Others residence_type applicants have higher chance of default

#9th table
IV$Tables[9]
#IV distribution for 'No.of.months.in.current.residence'
plot_IV_9 <- plot_infotables(IV,"No.of.months.in.current.residence")
#Note: 6-9 months 'No.of.months.in.current.residence' applicants have higher chance of default

#10th table
IV$Tables[10]
#IV distribution for 'No.of.months.in.current.company'
plot_IV_10 <- plot_infotables(IV,"No.of.months.in.current.company")
#Note: 3 bins ranging from 41-47, 48-53, 54-61 have high chances of default compared to other bins

#11th table
IV$Tables[11]
#IV distribution for 'No.of.times.90.DPD.or.worse.in.last.6.months'
plot_IV_11 <- plot_infotables(IV,"No.of.times.90.DPD.or.worse.in.last.6.months")
#Note:0 'No.of.times.90.DPD.or.worse.in.last.6.months' applicants have higher chance of default

#12th table
IV$Tables[12]
#IV distribution for 'No.of.times.60.DPD.or.worse.in.last.6.months'
plot_IV_12 <- plot_infotables(IV,"No.of.times.60.DPD.or.worse.in.last.6.months")
#Note: 0 'No.of.times.6.DPD.or.worse.in.last.6.months' applicants have higher chance of default

#13th table
IV$Tables[13]
#IV distribution for 'No.of.times.30.DPD.or.worse.in.last.6.months'
plot_IV_13 <- plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.6.months")
#Note: 0 'No.of.times.30.DPD.or.worse.in.last.6.months' applicants have higher chance of default

#14th table
IV$Tables[14]
#IV distribution for 'No.of.times.90.DPD.or.worse.in.last.12.months'
plot_IV_14 <- plot_infotables(IV,"No.of.times.90.DPD.or.worse.in.last.12.months")
#Note: 0 'No.of.times.90.DPD.or.worse.in.last.12.months' applicants have higher chance of default

#15th table
IV$Tables[15]
#IV distribution for 'No.of.times.60.DPD.or.worse.in.last.12.months'
plot_IV_15 <- plot_infotables(IV,"No.of.times.60.DPD.or.worse.in.last.12.months")
#Note: 0 'No.of.times.60.DPD.or.worse.in.last.12.months' applicants have higher chance of default

#16th table
IV$Tables[16]
#IV distribution for 'No.of.times.30.DPD.or.worse.in.last.12.months'
plot_IV_16 <- plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.12.months")
#Note: 0 'No.of.times.30.DPD.or.worse.in.last.12.months' applicants have higher chance of default

#17th table
IV$Tables[17]
#IV distribution for 'Avgas.CC.Utilization.in.last.12.months'
plot_IV_17 <- plot_infotables(IV,"Avgas.CC.Utilization.in.last.12.months")
#Note: 5 bins ranging from 0-14 Avgas.CC.Utilization.in.last.12.months applicants have higher chance of default

#18th table
IV$Tables[18]
#IV distribution for 'No.of.trades.opened.in.last.6.months'
plot_IV_18 <- plot_infotables(IV,"No.of.trades.opened.in.last.6.months")
#Note: 2 bins: 0 and 1  'No.of.trades.opened.in.last.6.months' applicants have higher chance of default

#19th table
IV$Tables[19]
#IV distribution for 'No.of.trades.opened.in.last.12.months'
plot_IV_19 <- plot_infotables(IV,"No.of.trades.opened.in.last.12.months")
#Note: 3 bins: 0, 1 and 2  'No.of.trades.opened.in.last.12.months' applicants have higher chance of default

#20th table
IV$Tables[20]
#IV distribution for 'No.of.PL.trades.opened.in.last.6.months'
plot_IV_20 <- plot_infotables(IV,"No.of.PL.trades.opened.in.last.6.months")
#Note: 0  value holding bin for 'No.of.trades.opened.in.last.12.months' applicants have higher chance of default

#21st table
IV$Tables[21]
#IV distribution for 'No.of.PL.trades.opened.in.last.12.months'
plot_IV_21 <- plot_infotables(IV,"No.of.PL.trades.opened.in.last.12.months")
#Note: 0  value holding bin for 'No.of.PL.trades.opened.in.last.12.months' applicants have higher chance of default

#22nd table
IV$Tables[22]
#IV distribution for 'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.'
plot_IV_22 <- plot_infotables(IV,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")
#Note: 0  value holding bin for 'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.' applicants have higher chance of default

#23th table
IV$Tables[23]
#IV distribution for 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.'
plot_IV_23 <- plot_infotables(IV,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")
#Note: 0  value holding bin for 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.' applicants have higher chance of default

#24th table
IV$Tables[24]
#IV distribution for 'Presence.of.open.home.loan'
plot_IV_24 <- plot_infotables(IV,"Presence.of.open.home.loan")
#Note: 1 value holding bin for 'Presence.of.open.home.loan' applicants have higher chance of default

#25th table
IV$Tables[25]
#IV distribution for 'Outstanding.Balance'
plot_IV_25 <- plot_infotables(IV,"Outstanding.Balance")
#Note: 3 bins: 0-7792, 7793-60285 and 2961788-3290266 'Outstanding.Balance' applicants have higher chance of default

#26th table
IV$Tables[26]
#IV distribution for 'Total.No.of.Trades'
plot_IV_26 <- plot_infotables(IV,"Total.No.of.Trades")
#Note: 4 bins: 1,2,3,4 'Total.No.of.Trades' applicants have higher chance of default

#27th table
IV$Tables[27]
#IV distribution for 'Presence.of.open.auto.loan'
plot_IV_27 <- plot_infotables(IV,"Presence.of.open.auto.loan")
#Note: 1 value holding bin of 'Presence.of.open.auto.loan' applicants have higher chance of default

#grid.arrange(plot_IV_1,plot_IV_2,plot_IV_3,plot_IV_4)
#grid.arrange(plot_IV_5,plot_IV_6,plot_IV_7,plot_IV_9)
#grid.arrange(plot_IV_10,plot_IV_11,plot_IV_12,plot_IV_13)
#grid.arrange(plot_IV_14,plot_IV_15,plot_IV_16,plot_IV_17)
#grid.arrange(plot_IV_18,plot_IV_19,plot_IV_20,plot_IV_21)
#grid.arrange(plot_IV_22,plot_IV_23,plot_IV_24,plot_IV_26)
#grid.arrange(plot_IV_8,plot_IV_27,plot_IV_25)
##WOE analysis done

###########
#########
######

#######################
merged_dataset_non_woe <- merged_dataset
#4 datasets for modelling 

#1 cleaned demodata without application_id
demographic_cleaned <-  merged_dataset_non_woe %>% dplyr::select(colnames(demographic)[2:length(demographic)])

#2 woe_demodata cleaned without application_id
demographic_woe_cleaned <-  merged_dataset_woe_values %>% dplyr::select(colnames(demographic)[2:length(demographic)])

#3 merged_data cleaned without application_id
merged_dataset_non_woe_cleaned  <- merged_dataset_non_woe %>% dplyr::select(colnames(merged_dataset_non_woe)[2:length(merged_dataset_non_woe)])

#4 merged_woe_data cleaned without application_id
merged_dataset_woe_values_cleaned <- merged_dataset_woe_values


#######################
#creating dummy variables for columns with optimum levels

#for woe data all the columns are numeric so it is model ready
#we need to do this scaling and ummy variable process for non_woe values merged_data and demographics data

#for merged_dataset_non_woe_cleaned dataframe creating dummy variables
merged_dataset_non_woe_cleaned$Gender <- as.factor(merged_dataset_non_woe_cleaned$Gender)
merged_dataset_non_woe_cleaned$Marital.Status..at.the.time.of.application. <- as.factor(merged_dataset_non_woe_cleaned$Marital.Status..at.the.time.of.application.)
merged_dataset_non_woe_cleaned$Education <- as.factor(merged_dataset$Education)
merged_dataset_non_woe_cleaned$Profession <- as.factor(merged_dataset_non_woe_cleaned$Profession)
merged_dataset_non_woe_cleaned$Type.of.residence <- as.factor(merged_dataset_non_woe_cleaned$Type.of.residence)
merged_dataset_non_woe_cleaned$Presence.of.open.auto.loan <- as.factor(merged_dataset_non_woe_cleaned$Presence.of.open.auto.loan)
merged_dataset_non_woe_cleaned$Presence.of.open.home.loan <- as.factor(merged_dataset_non_woe_cleaned$Presence.of.open.home.loan)

factor_variables <- merged_dataset_non_woe_cleaned[ , which(names(merged_dataset_non_woe_cleaned) %in% c("Gender","Marital.Status..at.the.time.of.application.","Education","Profession","Type.of.residence","Presence.of.open.auto.loan","Presence.of.open.home.loan"))]
dummies<- data.frame(sapply(factor_variables,function(x) data.frame(model.matrix(~x-1,data =factor_variables))[,-1]))
merged_dataset_non_woe_cleaned <- cbind(dummies,merged_dataset_non_woe_cleaned[ , -which(names(merged_dataset_non_woe_cleaned) %in% c("Gender","Marital.Status..at.the.time.of.application.","Education","Profession","Type.of.residence","Presence.of.open.auto.loan","Presence.of.open.home.loan"))])

colnames(merged_dataset_non_woe_cleaned)

#scaling all the values from age column till the last 2nd column
#i.e. 15th column to 34th column
scaled_merged_dataset_non_woe_cleaned <-  sapply(merged_dataset_non_woe_cleaned[,15:34],function(x) scale(x))

model_data_non_woe_cleaned <-  cbind(merged_dataset_non_woe_cleaned[,c(35,1:14)],scaled_merged_dataset_non_woe_cleaned)
model_data_non_woe_cleaned$Performance.Tag <- as.factor(model_data_non_woe_cleaned$Performance.Tag)

#################similarly for demographics data
demographic_cleaned$Gender <- as.factor(demographic_cleaned$Gender)
demographic_cleaned$Marital.Status..at.the.time.of.application. <- as.factor(demographic_cleaned$Marital.Status..at.the.time.of.application.)
demographic_cleaned$Education <- as.factor(merged_dataset$Education)
demographic_cleaned$Profession <- as.factor(demographic_cleaned$Profession)
demographic_cleaned$Type.of.residence <- as.factor(demographic_cleaned$Type.of.residence)

factor_variables <- demographic_cleaned[ , which(names(demographic_cleaned) %in% c("Gender","Marital.Status..at.the.time.of.application.","Education","Profession","Type.of.residence"))]
dummies<- data.frame(sapply(factor_variables,function(x) data.frame(model.matrix(~x-1,data =factor_variables))[,-1]))
demographic_cleaned <- cbind(dummies,demographic_cleaned[ , -which(names(demographic_cleaned) %in% c("Gender","Marital.Status..at.the.time.of.application.","Education","Profession","Type.of.residence"))])

colnames(demographic_cleaned)

#scaling all the values from age column till the last 2nd column
#i.e. 13th column to 17th column

scaled_demographic_cleaned <-  sapply(demographic_cleaned[,13:17],function(x) scale(x))

model_data_non_woe_demographic_cleaned <-  cbind(demographic_cleaned[,c(18,1:12)],scaled_demographic_cleaned)

model_data_non_woe_demographic_cleaned$Performance.Tag <- as.factor(model_data_non_woe_demographic_cleaned$Performance.Tag)

############
demographic_woe_cleaned$Performance.Tag <- as.factor(demographic_woe_cleaned$Performance.Tag)
merged_dataset_woe_values_cleaned$Performance.Tag <- as.factor(merged_dataset_woe_values_cleaned$Performance.Tag)
#####################
#####################
#model_datasets
  #1 model_data_non_woe_cleaned
  #2 model_data_non_woe_demographic_cleaned
  #3 demographic_woe_cleaned
  #4 merged_dataset_woe_values_cleaned


#splitting the datasets into train and test 70-30 split

#######################################Data Modelling process
#logistic regression model for each data set will be made and optimum cutoff will be calculated where accuracy, sensitivity and specificity is maximum
#total models 1 for each

#for each dataset
set.seed(100)
split_indices_demo_non_woe <- sample.split(model_data_non_woe_demographic_cleaned$Performance.Tag, SplitRatio = 0.70)

#train and test datasets
train_demo_non_woe <- model_data_non_woe_demographic_cleaned[split_indices_demo_non_woe, ]
test_demo_non_woe <- model_data_non_woe_demographic_cleaned[!split_indices_demo_non_woe, ]
###############1 by 1 modelling of this train-test data sets


##########################MODELLLING-1: Logistic regression on Demographics data Non-WOE
#datasets
#train_demo_non_woe
#test_demo_non_woe

#Initial Model 
logistic_demo_non_woe_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_demo_non_woe) 
summary(logistic_demo_non_woe_1) #AIC: 16703

#StepAIC run
stepaic_demo_non_woe <- stepAIC(logistic_demo_non_woe_1, direction = "both")
summary(stepaic_demo_non_woe) #AIC: 16686

vif(stepaic_demo_non_woe)
#all vif is below 4, hence removing features having highest pvalue

#-Education.xPhd

logistic_demo_non_woe_2 <- glm(formula = Performance.Tag ~ Education.xOthers + 
                                 Profession.xSE + Income + No.of.months.in.current.residence + 
                                 No.of.months.in.current.company, family = "binomial", data = train_demo_non_woe)


summary(logistic_demo_non_woe_2) #AIC: 16686 to 16686
#all the pvalues are < 0.05 hence  prediction on train data with logistic_demo_non_woe_2 model will be done now


#####################Model 1 Evaluation
#predicted probabilities of Performance.tag for test data
test_demo_non_woe_pred = predict(logistic_demo_non_woe_2, type = "response", 
                    newdata = test_demo_non_woe[,-1])
#summary of predicted probabilities
summary(test_demo_non_woe_pred)
#0.021 to 0.13
##note the probabilities are not going till 0.90+, this is an indicator of unbalanced distribution of dependent variable
##Hence we need to train the model with balanced data later

# Let's use the probability cutoff of 0.1
test_pred_performance <- factor(ifelse(test_demo_non_woe_pred >= 0.10, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_demo_non_woe$Performance.Tag==1,"Yes","No"))


table(test_actual_performance,test_pred_performance)
#test_pred_performance
#test_actual_performance    No   Yes
#No  19683    10
#Yes   866     1

test_conf <- caret::confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

#Accuracy : 95%
#Sensitivity : 1.153e-03
#Specificity : 9.995e-01

#Need to get a better cutoff where all 3 are at optimum levels

#optimal cutoff code
cutff_decider <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(test_demo_non_woe_pred >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_performance, test_actual_performance, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test_demo_non_woe_pred)

# # Creating cutoff values from 0.025 to 0.13 for plotting and initiallizing a matrix of 100 X 3.
# in each iteration to find out optimal value.
cutoff_data = seq(.025,.13,length=100)
cmdata = matrix(0,100,3) # matrix to hold model performance data

for(i in 1:100){
  cmdata[i,] = cutff_decider(cutoff_data[i])
}

plot(cutoff_data, cmdata[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff_data,cmdata[,2],col="darkgreen",lwd=2)
lines(cutoff_data,cmdata[,3],col=4,lwd=2)
box()
legend(0.1,0.5,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- cutoff_data[which(abs(cmdata[,1]-cmdata[,2])<0.05)]
cutoff
#0.0430303

test_cutoff_performance <- factor(ifelse(test_demo_non_woe_pred >=cutoff[1], "Yes", "No"))
conf_final <- caret::confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

#Accuracy, Sensitivity and Specificity 
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
#Accuracy
acc
#0.57

#Sensitivity
sens
#0.55

# Show Specificity
spec
#0.57

#Logistic regression on unbalance demo non woe data gave below results
# Accuracy : 0.57
# Sensitivity : 0.55
#Specificity : 0.57
####################################################################################3
###########################################3

###MODEL 2: dataset: demographics_woe values

set.seed(100)
split_indices_demo_woe <- sample.split(demographic_woe_cleaned$Performance.Tag, SplitRatio = 0.70)

#train and test datasets
train_demo_woe <- demographic_woe_cleaned[split_indices_demo_woe, ]
test_demo_woe <- demographic_woe_cleaned[!split_indices_demo_woe, ]
###############1 by 1 modelling of this train-test data sets

##########################MODELLLING-2: Logistic regression on Demographics data WOE
#datasets
#train_demo_woe
#test_demo_woe

#Initial Model 
logistic_demo_woe_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_demo_woe) 
summary(logistic_demo_woe_1) #AIC: 16531

#StepAIC run
stepaic_demo_woe <- stepAIC(logistic_demo_woe_1, direction = "both")
summary(stepaic_demo_woe) #AIC: 16528

vif(stepaic_demo_woe)
#all vif is below 4, hence removing features having highest pvalue (>0.05)

#-No.of.dependents

logistic_demo_woe_2 <- glm(formula = Performance.Tag ~ Age + Income + 
                                 Education + Profession + No.of.months.in.current.residence + 
                                 No.of.months.in.current.company, family = "binomial", data = train_demo_woe)


summary(logistic_demo_woe_2) #AIC: 16528 to 16529
#all the pvalues are < 0.05 hence  prediction on train data with logistic_demo_woe_2 model will be done now


#####################Model 2 Evaluation
#predicted probabilities of Performance.tag for test data

test_demo_woe_pred = predict(logistic_demo_woe_2, type = "response", 
                             newdata = test_demo_woe[,-11])
#summary of predicted probabilities
summary(test_demo_woe_pred)
#0.018 to 0.179
##note the probabilities are not going till 0.90+, this is an indicator of unbalanced distribution of dependent variable
##Hence we need to train the model with balanced data later

# Let's use the probability cutoff of 10%.

test_pred_performance <- factor(ifelse(test_demo_woe_pred >= 0.10, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_demo_woe$Performance.Tag==1,"Yes","No"))


table(test_actual_performance,test_pred_performance)
#test_pred_performance
#test_actual_performance    No   Yes
#No  19675    18
#Yes   863     4

test_conf <- caret::confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

#Accuracy : 96%
#Sensitivity : 0.0046136 
#Specificity : 0.9990860

#Need to get a better cutoff where all 3 are at optimum levels
#optimal cutoff code

cutff_decider <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(test_demo_woe_pred >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_performance, test_actual_performance, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test_demo_woe_pred)

# # Creating cutoff values from 0.018 to 0.175 for plotting and initiallizing a matrix of 100 X 3.
# in each iteration to find out optimal value.
cutoff_data = seq(.018,.175,length=100)
cmdata = matrix(0,100,3) # matrix to hold model performance data

for(i in 1:100){
  cmdata[i,] = cutff_decider(cutoff_data[i])
}

plot(cutoff_data, cmdata[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff_data,cmdata[,2],col="darkgreen",lwd=2)
lines(cutoff_data,cmdata[,3],col=4,lwd=2)
box()
legend(0.1,0.5,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- cutoff_data[which(abs(cmdata[,1]-cmdata[,2])<0.05)]
cutoff
#0.04178788

# Let's choose a cutoff value of 0.0498 for final model

test_cutoff_performance <- factor(ifelse(test_demo_woe_pred >=cutoff[1], "Yes", "No"))
conf_final <- caret::confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

#Accuracy, Sensitivity and Specificity 
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
#Accuracy
acc
#0.59

#Sensitivity
sens
#0.57

# Show Specificity
spec
#0.59

#Logistic regression on unbalanced demo woe data gave below results
# Accuracy : 0.59
# Sensitivity : 0.57
#Specificity : 0.59

##NOTE: woe values demo model has better accuracy,sens and specificity combination on unbalanced dataset.

#Demographics only wise important variables
summary(logistic_demo_woe_2)
#Age, Income, Education, Profession, No.of.months.in.current.residence, No.of.months.in.current.company
####################################################################################3
###########################################

###MODEL 3: dataset: merged_data cleaned

set.seed(100)
split_indices_all_non_woe <- sample.split(model_data_non_woe_cleaned$Performance.Tag, SplitRatio = 0.70)

#train and test datasets
train_all_non_woe <- model_data_non_woe_cleaned[split_indices_all_non_woe, ]
test_all_non_woe <- model_data_non_woe_cleaned[!split_indices_all_non_woe, ]
###############1 by 1 modelling of this train-test data sets

##########################MODELLLING-3: Logistic regression on full merged data non WOE
#datasets
#train_all_non_woe
#test_all_non_woe

#Initial Model 
logistic_all_non_woe_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_all_non_woe) 
summary(logistic_all_non_woe_1) #AIC: 16159

#StepAIC run
stepaic_all_non_woe <- stepAIC(logistic_all_non_woe_1, direction = "both")
summary(stepaic_all_non_woe) #AIC: 16128

vif(stepaic_all_non_woe)
#-No.of.times.60.DPD.or.worse.in.last.6.months: vif is highest with higher pvalue among the similar vif grp

#-No.of.times.60.DPD.or.worse.in.last.6.months

logistic_all_non_woe_2 <-  glm(formula = Performance.Tag ~ Education.xOthers + Profession.xSE + 
                                 No.of.months.in.current.residence + 
                                 No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                                 Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                                 Total.No.of.Trades, family = "binomial", data = train_all_non_woe)


summary(logistic_all_non_woe_2) #AIC: 16128 to 16131
vif(logistic_all_non_woe_2)
#=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
##vif is highest (>4) with higher pvalue among the similar vif grp

logistic_all_non_woe_3 <- glm(formula = Performance.Tag ~ Education.xOthers + Profession.xSE + 
                                No.of.months.in.current.residence + 
                                No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                                Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                                Total.No.of.Trades, family = "binomial", data = train_all_non_woe)


summary(logistic_all_non_woe_3) #AIC: 16128 to 16131 to 16132
vif(logistic_all_non_woe_3)
#-Total.No.of.Trades
##vif is highest (>4) with higher pvalue among the similar vif grp

logistic_all_non_woe_4 <- glm(formula = Performance.Tag ~ Education.xOthers + Profession.xSE + 
                                No.of.months.in.current.residence + 
                                No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                                Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , family = "binomial", data = train_all_non_woe)


summary(logistic_all_non_woe_4) #AIC: 16128 to 16131 to 16132 to 16138
vif(logistic_all_non_woe_4)
#-No.of.times.90.DPD.or.worse.in.last.12.months 
##vif is highest with higher pvalue among the similar vif grp

logistic_all_non_woe_5 <- glm(formula = Performance.Tag ~ Education.xOthers + Profession.xSE + 
                                No.of.months.in.current.residence + 
                                No.of.times.30.DPD.or.worse.in.last.6.months + 
                                Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , family = "binomial", data = train_all_non_woe)


summary(logistic_all_non_woe_5) #AIC: 16128 to 16131 to 16132 to 16138
vif(logistic_all_non_woe_5)
#all vif < 3 now
##any removal will be pvalue based from now on
#-No.of.months.in.current.residence
##higher pvalue among the similar vif grp

logistic_all_non_woe_6 <- glm(formula = Performance.Tag ~ Education.xOthers + Profession.xSE + 
                                No.of.times.30.DPD.or.worse.in.last.6.months + 
                                Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , family = "binomial", data = train_all_non_woe)


summary(logistic_all_non_woe_6) #AIC: 16128 to 16131 to 16132 to 16138 to 16140

#-Profession.xSE
##higher pvalue among the similar vif grp

logistic_all_non_woe_7 <- glm(formula = Performance.Tag ~ Education.xOthers +
                                No.of.times.30.DPD.or.worse.in.last.6.months + 
                                Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , family = "binomial", data = train_all_non_woe)


summary(logistic_all_non_woe_7) #AIC: 16128 to 16131 to 16132 to 16138 to 16140 to 16141

#-Education.xOthers
##higher pvalue among the similar vif grp


logistic_all_non_woe_8 <- glm(formula = Performance.Tag ~ 
                                No.of.times.30.DPD.or.worse.in.last.6.months + 
                                Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , family = "binomial", data = train_all_non_woe)


summary(logistic_all_non_woe_8) #AIC: 16128 to 16131 to 16132 to 16138 to 16140 to 16141 to 16142

#final_model: logistic_all_non_woe_8
#####################Model 3 Evaluation
#predicted probabilities of Performance.tag for test data

test_all_non_woe_pred = predict(logistic_all_non_woe_8, type = "response", 
                                newdata = test_all_non_woe[,-1])
#summary of predicted probabilities
summary(test_all_non_woe_pred)
#0.019 to 0.25
##note the probabilities are not going till 0.90+, this is an indicator of unbalanced distribution of dependent variable
##Hence we need to train the model with balanced data later

# Let's use the probability cutoff of 0.1.

test_pred_performance <- factor(ifelse(test_all_non_woe_pred >= 0.1, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_all_non_woe$Performance.Tag==1,"Yes","No"))


table(test_actual_performance,test_pred_performance)
#test_pred_performance
#test_actual_performance    No   Yes
#No  19079   614
#Yes   800    67

test_conf <- caret::confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

#Accuracy : 0.9312
#Sensitivity : 0.077278
#Specificity : 0.968821

#optimal cutoff code

cutff_decider <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(test_all_non_woe_pred >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_performance, test_actual_performance, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test_all_non_woe_pred)

# # Creating cutoff values from 0.0195 to 0.249 for plotting and initiallizing a matrix of 100 X 3.
# in each iteration to find out optimal value.
cutoff_data = seq(.0195,.249,length=100)
cmdata = matrix(0,100,3) # matrix to hold model performance data

for(i in 1:100){
  cmdata[i,] = cutff_decider(cutoff_data[i])
}

plot(cutoff_data, cmdata[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff_data,cmdata[,2],col="darkgreen",lwd=2)
lines(cutoff_data,cmdata[,3],col=4,lwd=2)
box()
legend(0.1,0.5,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- cutoff_data[which(abs(cmdata[,1]-cmdata[,2])<0.05)]
cutoff

# Let's choose a cutoff value of 0.045 for final model

test_cutoff_performance <- factor(ifelse(test_all_non_woe_pred >=cutoff[1], "Yes", "No"))
conf_final <- caret::confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

#Accuracy, Sensitivity and Specificity 
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
#Accuracy
acc
#0.62

#Sensitivity
sens
#0.62

# Show Specificity
spec
#0.62

#Logistic regression on unbalanced full data gave below results
# Accuracy : 0.62
# Sensitivity : 0.62
#Specificity : 0.62

#####################################################################
######################
#################################################

###MODEL 4: dataset: merged_data woe_values cleaned

set.seed(100)
split_indices_all_woe <- sample.split(merged_dataset_woe_values_cleaned$Performance.Tag, SplitRatio = 0.70)

#train and test datasets
train_all_woe <- merged_dataset_woe_values_cleaned[split_indices_all_woe, ]
test_all_woe <- merged_dataset_woe_values_cleaned[!split_indices_all_woe, ]
###############1 by 1 modelling of this train-test data sets

##########################MODELLLING-4: Logistic regression on full merged data WOE values
#datasets
#train_all_woe
#test_all_woe

#Initial Model 
logistic_all_woe_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_all_woe) 
summary(logistic_all_woe_1) #AIC: 16043

#StepAIC run
stepaic_all_woe <- stepAIC(logistic_all_woe_1, direction = "both")
summary(stepaic_all_woe) #AIC: 16022

vif(stepaic_all_woe)
#-No.of.months.in.current.company:  higher pvalue among the grp

#-No.of.months.in.current.company

logistic_all_woe_2 <-   glm(formula = Performance.Tag ~ Age + No.of.dependents + Education + 
                              Profession + No.of.times.30.DPD.or.worse.in.last.6.months + 
                              Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                              No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                            family = "binomial", data = train_all_woe)


summary(logistic_all_woe_2) #AIC: 16022 to 16022
vif(logistic_all_woe_2)
#-No.of.dependents higher pvalue among the grp


logistic_all_woe_3 <- glm(formula = Performance.Tag ~ Age +  Education + 
                                Profession + No.of.times.30.DPD.or.worse.in.last.6.months + 
                                Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                              family = "binomial", data = train_all_woe)


summary(logistic_all_woe_3) #AIC: 16128 to 16023 to 16023
#-Profession: higher pvalue among the grp


logistic_all_woe_4 <- glm(formula = Performance.Tag ~ Age +  Education 
                                 + No.of.times.30.DPD.or.worse.in.last.6.months + 
                                Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                              family = "binomial", data = train_all_woe)


summary(logistic_all_woe_4) #AIC: 16128 to 16023 to 16023 to 16024
#-Age : higher pvalue among the grp

logistic_all_woe_5 <- glm(formula = Performance.Tag ~    Education 
                              + No.of.times.30.DPD.or.worse.in.last.6.months + 
                                Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                              family = "binomial", data = train_all_woe)


summary(logistic_all_woe_5) #AIC: 16128 to 16023 to 16023 to 16024

#final_model: logistic_all_woe_5
#####################Model 4 Evaluation
#predicted probabilities of Performance.tag for test data

test_all_woe_pred = predict(logistic_all_woe_5, type = "response", 
                            newdata = test_all_woe[,-1])
#summary of predicted probabilities
summary(test_all_woe_pred)
#0.0135 to 0.185
##note the probabilities are not going till 0.90+, this is an indicator of unbalanced distribution of dependent variable
##Hence we need to train the model with balanced data later

# Let's use the probability cutoff of 0.1.

test_pred_performance <- factor(ifelse(test_all_woe_pred >= 0.10, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_all_woe$Performance.Tag==1,"Yes","No"))


table(test_actual_performance,test_pred_performance)
#test_pred_performance
#test_actual_performance    No   Yes
#No  19680    13
#Yes   866     1

test_conf <- caret::confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

#Accuracy : 0.9572 
#Sensitivity : 1.153e-03      
#Specificity : 9.993e-01

#optimal cutoff code
cutff_decider <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(test_all_woe_pred >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_performance, test_actual_performance, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test_all_woe_pred)

# # Creating cutoff values from 0.0136 to 0.185 for plotting and initiallizing a matrix of 100 X 3.
# in each iteration to find out optimal value.
cutoff_data = seq(.0136,.185,length=100)
cmdata = matrix(0,100,3) # matrix to hold model performance data

for(i in 1:100){
  cmdata[i,] = cutff_decider(cutoff_data[i])
}

plot(cutoff_data, cmdata[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff_data,cmdata[,2],col="darkgreen",lwd=2)
lines(cutoff_data,cmdata[,3],col=4,lwd=2)
box()
legend(0.1,0.5,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- cutoff_data[which(abs(cmdata[,1]-cmdata[,2])<0.05)]
cutoff

# Let's choose a cutoff value of 0.0498 for final model

test_cutoff_performance <- factor(ifelse(test_all_woe_pred >=cutoff[1], "Yes", "No"))
conf_final <- caret::confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

#Accuracy, Sensitivity and Specificity 
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
#Accuracy
acc
#0.62

#Sensitivity
sens
#0.64

# Show Specificity
spec
#0.62

#Logistic regression on unbalanced full data gave below results
# Accuracy : 0.62
# Sensitivity : 0.64
#Specificity : 0.62

###############
##MODEL 5 Random forest on all_non_woe data
rforest <- randomForest(Performance.Tag ~ ., train_all_non_woe,ntree=100,importance=T)

summary(rforest)

# prediction on test data
predictions_rf <- predict(rforest, newdata = test_all_non_woe[-1], type = "prob")[,2]

summary(predictions_rf)
#0 to 0.41

predicted_response <- factor(ifelse(predictions_rf >= 0.250, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_all_non_woe$Performance.Tag==1,"Yes","No"))

#Confusion metric
conf <- caret::confusionMatrix(predicted_response, test_actual_performance, positive = "Yes")
conf

#Accuracy 0.9565
#Sensi 2.307e-03
#Spec 9.985e-01 


# Let's find out the optimal probalility cutoff 
cutff_decider <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(predictions_rf >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_performance, test_actual_performance, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(predictions_rf)

# # Creating cutoff values from 0.01 to 0.41 for plotting and initiallizing a matrix of 100 X 3.
# in each iteration to find out optimal value.
cutoff_data = seq(.01,.41,length=100)
cmdata = matrix(0,100,3) # matrix to hold model performance data

for(i in 1:100){
  cmdata[i,] = cutff_decider(cutoff_data[i])
}

plot(cutoff_data, cmdata[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff_data,cmdata[,2],col="darkgreen",lwd=2)
lines(cutoff_data,cmdata[,3],col=4,lwd=2)
box()
legend(0.1,0.5,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- cutoff_data[which(abs(cmdata[,1]-cmdata[,2])<0.055)]
cutoff

#optimum cut off set
predicted_response <- factor(ifelse(predictions_rf >=cutoff[1], "Yes", "No"))
conf_final <- caret::confusionMatrix(predicted_response, test_actual_performance, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#58

sens
#63

spec
#58




###############################
#Model 7: All data non_woe SMOTE LR model
# Logistic Regression: using SMOTE analysis (to account for the data imbalance)
set.seed(100)
train_smote <- DMwR::SMOTE(Performance.Tag ~ ., train_all_non_woe, perc.over = 100, perc.under=200)

summary(train_smote$Performance.Tag)

# Tag= 1 implies default, 0 implies good
train_smote_model_1 = glm(Performance.Tag ~ ., data = train_smote, family = "binomial")
summary(train_smote_model_1)
#aic: 10514

# used STEPAIC to find the best model
train_smote_model_2 <- stepAIC(train_smote_model_1,direction = "both")
summary(train_smote_model_2)
#aic 10491
# Removing multicollinearity through VIF check
sort(vif(train_smote_model_2))

#captured the STEPAIC output and built model2

#Excluding No.of.PL.trades.opened.in.last.6.months  due to low significance and high VIF
train_smote_model_3<-glm(formula = Performance.Tag ~ Marital.Status..at.the.time.of.application. + 
                           Education.xOthers + Profession.xSE + Profession.xSE_PROF + 
                           Presence.of.open.auto.loan + No.of.months.in.current.residence + 
                           No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                           No.of.times.60.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                           No.of.PL.trades.opened.in.last.12.months + 
                           No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                           No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                           Total.No.of.Trades, family = "binomial", data = train_smote)

summary(train_smote_model_3)
sort(vif(train_smote_model_3))

#-No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
train_smote_model_4<-glm(formula = Performance.Tag ~ Marital.Status..at.the.time.of.application. + 
                           Education.xOthers + Profession.xSE + Profession.xSE_PROF + 
                           Presence.of.open.auto.loan + No.of.months.in.current.residence + 
                           No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                           No.of.times.60.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                           No.of.PL.trades.opened.in.last.12.months + 
                           No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                           Total.No.of.Trades, family = "binomial", data = train_smote)


summary(train_smote_model_4)
sort(vif(train_smote_model_4))
#-No.of.times.60.DPD.or.worse.in.last.12.months

train_smote_model_5<-glm(formula = Performance.Tag ~ Marital.Status..at.the.time.of.application. + 
                           Education.xOthers + Profession.xSE + Profession.xSE_PROF + 
                           Presence.of.open.auto.loan + No.of.months.in.current.residence + 
                           No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                           Avgas.CC.Utilization.in.last.12.months + 
                           No.of.PL.trades.opened.in.last.12.months + 
                           No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                           Total.No.of.Trades, family = "binomial", data = train_smote)


summary(train_smote_model_5)
sort(vif(train_smote_model_5))
#-No.of.times.90.DPD.or.worse.in.last.12.months

train_smote_model_6<-glm(formula = Performance.Tag ~ Marital.Status..at.the.time.of.application. + 
                           Education.xOthers + Profession.xSE + Profession.xSE_PROF + 
                           Presence.of.open.auto.loan + No.of.months.in.current.residence + 
                           No.of.times.30.DPD.or.worse.in.last.6.months + 
                           Avgas.CC.Utilization.in.last.12.months + 
                           No.of.PL.trades.opened.in.last.12.months + 
                           No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                           Total.No.of.Trades, family = "binomial", data = train_smote)


summary(train_smote_model_6)
sort(vif(train_smote_model_6))
#-Total.No.of.Trades

train_smote_model_7<-glm(formula = Performance.Tag ~ Marital.Status..at.the.time.of.application. + 
                           Education.xOthers + Profession.xSE + Profession.xSE_PROF + 
                           Presence.of.open.auto.loan + No.of.months.in.current.residence + 
                           No.of.times.30.DPD.or.worse.in.last.6.months + 
                           Avgas.CC.Utilization.in.last.12.months + 
                           No.of.PL.trades.opened.in.last.12.months + 
                           No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 
                           , family = "binomial", data = train_smote)


summary(train_smote_model_7)
sort(vif(train_smote_model_7))
#-Marital.Status..at.the.time.of.application.

train_smote_model_8<-glm(formula = Performance.Tag ~ 
                           Education.xOthers + Profession.xSE + Profession.xSE_PROF + 
                           Presence.of.open.auto.loan + No.of.months.in.current.residence + 
                           No.of.times.30.DPD.or.worse.in.last.6.months + 
                           Avgas.CC.Utilization.in.last.12.months + 
                           No.of.PL.trades.opened.in.last.12.months + 
                           No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 
                         , family = "binomial", data = train_smote)

summary(train_smote_model_8)
sort(vif(train_smote_model_8))
#-Education.xOthers

train_smote_model_9<-glm(formula = Performance.Tag ~ 
                           Profession.xSE + Profession.xSE_PROF + 
                           Presence.of.open.auto.loan + No.of.months.in.current.residence + 
                           No.of.times.30.DPD.or.worse.in.last.6.months + 
                           Avgas.CC.Utilization.in.last.12.months + 
                           No.of.PL.trades.opened.in.last.12.months + 
                           No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 
                         , family = "binomial", data = train_smote)

summary(train_smote_model_9)
sort(vif(train_smote_model_9))
#-No.of.times.60.DPD.or.worse.in.last.12.months


train_smote_model_10<-glm(formula = Performance.Tag ~ Education.xPhd + 
                           Presence.of.open.home.loan + Income + No.of.months.in.current.residence + 
                           No.of.months.in.current.company + No.of.times.30.DPD.or.worse.in.last.6.months + 
                           Avgas.CC.Utilization.in.last.12.months + 
                           No.of.PL.trades.opened.in.last.12.months + 
                           No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                           Total.No.of.Trades, family = "binomial", 
                         data = train_smote)

summary(train_smote_model_10)
sort(vif(train_smote_model_10))
#-No.of.times.60.DPD.or.worse.in.last.12.months
test_pred_smote<-predict(train_smote_model_9, type = "response",newdata = test_all_non_woe[,-1])

summary(test_pred_smote)


test_actual_smote<- factor(ifelse(test_all_non_woe$Performance.Tag==1,"Yes","No"))

s = seq(.22,.91,length=100)

OUT = matrix(0,100,3)

perform_fn <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(test_pred_smote >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_performance, test_actual_performance, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

for(i in 1:100){  
  OUT[i,] = perform_fn(s[i])
}

#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

best_cutoff_smote <- s[which(abs(OUT[,1]-OUT[,2])<0.015)]
best_cutoff_smote
#best_cutoff_smote <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
#best_cutoff_smote

test_pred_default_smote <- as.factor(ifelse(test_pred_smote >= best_cutoff_smote, "Yes", "No"))
confusionmartix_final_smote<-caret::confusionMatrix(test_pred_default_smote, test_actual_smote, positive = "Yes")
confusionmartix_final_smote
#62,62,62

##########################
#Model 8: All data WOE SMOTE LR model
# Logistic Regression: using SMOTE analysis (to account for the data imbalance)
#Initial model - Build model 1 containing all variables
set.seed(100)
train_smote_woe <- DMwR::SMOTE(Performance.Tag ~ ., train_all_woe, perc.over = 100, perc.under=200)

summary(train_smote_woe$Performance.Tag)

# Tag= 1 implies default, 0 implies good
train_smote_woe_model_1 = glm(Performance.Tag ~ ., data = train_smote_woe, family = "binomial")
summary(train_smote_model_1)
#10464

# used STEPAIC to find the best model
train_smote_woe_model_2 <- stepAIC(train_smote_woe_model_1,direction = "both")
summary(train_smote_woe_model_2)
vif(train_smote_woe_model_2)
#-No.of.PL.trades.opened.in.last.12.months : high vif and pvalue

#10444

train_smote_woe_model_3 <- glm(formula = Performance.Tag ~ Age + No.of.dependents + Income + 
      No.of.months.in.current.residence + No.of.months.in.current.company + 
      No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.12.months + 
      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
      Outstanding.Balance + Presence.of.open.auto.loan, family = "binomial", 
    data = train_smote_woe)

summary(train_smote_woe_model_3)
#AIC: 10445
#-No.of.times.60.DPD.or.worse.in.last.12.months: high vif and pvalue
vif(train_smote_woe_model_3)

train_smote_woe_model_4 <- glm(formula = Performance.Tag ~ Age + No.of.dependents + Income + 
                                 No.of.months.in.current.residence + No.of.months.in.current.company + 
                                 No.of.times.30.DPD.or.worse.in.last.6.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                                 Outstanding.Balance + Presence.of.open.auto.loan, family = "binomial", 
                               data = train_smote_woe)


summary(train_smote_woe_model_4)
#10445 to 10449
vif(train_smote_woe_model_4)
#-No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. high vif and pvalue

train_smote_woe_model_5 <- glm(formula = Performance.Tag ~ Age + No.of.dependents + Income + 
                                 No.of.months.in.current.residence + No.of.months.in.current.company + 
                                 No.of.times.30.DPD.or.worse.in.last.6.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                                 Outstanding.Balance + Presence.of.open.auto.loan, family = "binomial", 
                               data = train_smote_woe)


summary(train_smote_woe_model_5)
#10445 to 10449 to 10451
vif(train_smote_woe_model_5)
#-Income : high pvalue

train_smote_woe_model_6 <- glm(formula = Performance.Tag ~ Age + No.of.dependents  + 
                                 No.of.months.in.current.residence + No.of.months.in.current.company + 
                                 No.of.times.30.DPD.or.worse.in.last.6.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                                 Outstanding.Balance + Presence.of.open.auto.loan, family = "binomial", 
                               data = train_smote_woe)


summary(train_smote_woe_model_6)
#10445 to 10449 to 10451 to 10451
vif(train_smote_woe_model_6)
#-No.of.dependents : high pvalue

train_smote_woe_model_7 <- glm(formula = Performance.Tag ~ Age +
                                 No.of.months.in.current.residence + No.of.months.in.current.company + 
                                 No.of.times.30.DPD.or.worse.in.last.6.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                                 Outstanding.Balance + Presence.of.open.auto.loan, family = "binomial", 
                               data = train_smote_woe)


summary(train_smote_woe_model_7)
#10445 to 10449 to 10451 to 10451 to 10452
vif(train_smote_woe_model_7)
#-No.of.months.in.current.company

train_smote_woe_model_8 <- glm(formula = Performance.Tag ~ Age +
                                 No.of.months.in.current.residence + 
                                 No.of.times.30.DPD.or.worse.in.last.6.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                                 Outstanding.Balance + Presence.of.open.auto.loan, family = "binomial", 
                               data = train_smote_woe)


summary(train_smote_woe_model_8)
#10445 to 10449 to 10451 to 10451 to 10452 to 10452
vif(train_smote_woe_model_8)
#-Age: high pvalue (> 0.05)

train_smote_woe_model_9 <- glm(formula = Performance.Tag ~ 
                                 No.of.months.in.current.residence + 
                                 No.of.times.30.DPD.or.worse.in.last.6.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                                 Outstanding.Balance + Presence.of.open.auto.loan, family = "binomial", 
                               data = train_smote_woe)


summary(train_smote_woe_model_9)
#10445 to 10449 to 10451 to 10451 to 10452 to 10452 to 10454
vif(train_smote_woe_model_9)
##final model: train_smote_woe_model_9

test_pred_woe_smote<-predict(train_smote_woe_model_9, type = "response",newdata = test_all_woe[,-1])

summary(test_pred_woe_smote)

#0.19 to 0.737
test_actual_smote_woe<- factor(ifelse(test_all_woe$Performance.Tag==1,"Yes","No"))

s = seq(.19,.73,length=100)

OUT = matrix(0,100,3)

perform_fn <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(test_pred_woe_smote >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_performance, test_actual_smote_woe, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

for(i in 1:100){  
  OUT[i,] = perform_fn(s[i])
}

#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

best_cutoff_smote <- s[which(abs(OUT[,1]-OUT[,2])<0.015)]
best_cutoff_smote

test_pred_default_woe_smote <- as.factor(ifelse(test_pred_woe_smote >= best_cutoff_smote, "Yes", "No"))
confusionmatix_final_smote_woe <-caret::confusionMatrix(test_pred_default_woe_smote, test_actual_smote, positive = "Yes")
confusionmatix_final_smote_woe
#64,63,64

#this is by far the best model we have


#########################
###MODEL 9: Random forest on smote test all non woe data
set.seed(100)
train_smote_non_woe_rf <- DMwR::SMOTE(Performance.Tag ~ ., train_all_non_woe, perc.over = 100, perc.under=200)

rforest_smote_non_woe <- randomForest(Performance.Tag ~ ., train_smote_non_woe_rf,ntree=100,importance=T)

summary(rforest_smote_non_woe)

# prediction on test data
predictions_rf_smote_nonwoe <- predict(rforest_smote_non_woe, newdata = test_all_non_woe[-1], type = "prob")[,2]

summary(predictions_rf_smote_nonwoe)
#0.01 to 0.84

predicted_response <- factor(ifelse(predictions_rf_smote_nonwoe >= 0.250, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_all_non_woe$Performance.Tag==1,"Yes","No"))

#Confusion metric
conf <- caret::confusionMatrix(predicted_response, test_actual_performance, positive = "Yes")
conf

#Accuracy 0.33
#Sensi 0.89
#Spec 0.30


# Let's find out the optimal probalility cutoff 
cutff_decider <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(predictions_rf_smote_nonwoe >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_performance, test_actual_performance, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(predictions_rf_smote_nonwoe)

# # Creating cutoff values from 0.02 to 0.855 for plotting and initiallizing a matrix of 100 X 3.
# in each iteration to find out optimal value.
cutoff_data = seq(.02,.84,length=100)
cmdata = matrix(0,100,3) # matrix to hold model performance data

for(i in 1:100){
  cmdata[i,] = cutff_decider(cutoff_data[i])
}

plot(cutoff_data, cmdata[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff_data,cmdata[,2],col="darkgreen",lwd=2)
lines(cutoff_data,cmdata[,3],col=4,lwd=2)
box()
legend(0.1,0.5,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- cutoff_data[which(abs(cmdata[,1]-cmdata[,2])<0.01)]
cutoff

#optimum cut off set
predicted_response <- factor(ifelse(predictions_rf_smote_nonwoe >=cutoff[1], "Yes", "No"))
conf_final <- caret::confusionMatrix(predicted_response, test_actual_performance, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#61

sens
#62

spec
#61

##similar performance as logistic smote non woe
#61 62 61

      ######################################## Model Selection ########################
#logistic performed better

#########################
#best model: logistic smote with woe values
confusionmatix_final_smote_woe 
# Model 8
#cutoff:
#acc:64, sens: 63, spec:64

#model 8
#train_smote_woe_model_9
#summary(train_smote_woe_model_9)

#glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
#      No.of.times.30.DPD.or.worse.in.last.6.months + Avgas.CC.Utilization.in.last.12.months + 
#      No.of.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
#      Outstanding.Balance + Presence.of.open.auto.loan, family = "binomial", 
#    data = train_smote_woe)
final_model <- train_smote_woe_model_9

###########################################
############
##KS AND GAIN Chart Analysis for best model
##########Lift & Gain analysis and plots##########
#test_pred_woe_smote
require(dplyr)
library(dplyr)
test_actual_smote<- test_all_woe$Performance.Tag
test_pred_woe_smote <- as.numeric(test_pred_woe_smote)


#helper function to calculate gain & lift
lift_calculation <- function(labels , predticted_prob, groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predticted_prob)) predicted.prob <- as.integer(as.character(predticted_prob))
  helper = data.frame(cbind(labels , predticted_prob))
  helper[,"bucket"] = ntile(-helper[,"predticted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}


default_decile  <-  lift_calculation(test_actual_smote, test_pred_woe_smote, groups = 10)

#######Lift chart plot#########

plot_grid(ggplot(default_decile,aes(x=default_decile$bucket ,y=default_decile$Gain, color=""))+geom_line()+geom_point(),
          ggplot(default_decile,aes(x=default_decile$bucket,y=default_decile$Cumlift))+geom_line()+geom_point(), 
          align = "h",ncol = 1)

#Gain at 4th decile =66.3%
#Lift at 4th decile =1.66%

######################################################
#############Application_Scorecard###########
##########################################

#Calculating the probabilities based on the final model on merged dataset - final_model: logistict woe smote
score_card_data <- merged_dataset_woe_values_cleaned

score_card_data$predict_default<-predict(final_model, type = "response",newdata = score_card_data[,-1])
score_card_data$predict_non_default <- (1-(score_card_data$predict_default))

score_card_data$odds <-  log(score_card_data$predict_non_default/score_card_data$predict_default)

#score_card_data_pred <- as.factor(ifelse(score_card_data$predict_default >= best_cutoff_smote, "Yes", "No"))
#score_card_actual <-  as.factor(ifelse(score_card_data$Performance.Tag == 1,"Yes","No"))
#conf_full_data_final <-caret::confusionMatrix(score_card_data_pred, score_card_actual, positive = "Yes")
#conf_full_data_final
#64, 62 64


PDO <- 20
BaseScore <- 400
Odds <- 10

#Calculating Factor & Offset
Factor=PDO/log(2)

Offset=BaseScore-(Factor*log(Odds))

Offset
#333.5614

Factor
#28.8539

print("equation is : score = 333.5614 + (28.8539 * log(odds))")

score_card_data$score <- 333.5614 + (28.8539 * score_card_data$odds)

summary(score_card_data$score)
#303 to 376 scores
quantile(score_card_data$score,seq(0,1,0.2))

cuttoff <- woe.binning(score_card_data, "Performance.Tag", "score")
woe.binning.plot(cuttoff)
#From the above, it is evident that the cut off 
##1 could be set to 334  or
##2 could be set to 355

##Assumping that 
#1 bank losses 1,000 INR per credit card per customer for a rejected applicant who didnt default
#2 per credit card default bank losses 15 times what it makes as revenue (as default generally happens when amount is high)
##i.e 15,000 per default

##below analysis is done

plot_score_vs_perf_tag <- ggplot(score_card_data, aes(x = score,fill=Performance.Tag ))+
  geom_histogram(position = "fill") 
plot_score_vs_perf_tag
########################################
cutoff_scorecard = 334

#Calculating the percentage of defaults below the cutoff value of 334
no_of_defaults_under_cutoff <- length(which(score_card_data$Performance.Tag==1 & score_card_data$score <cutoff_scorecard))
total_no_of_defaults<-length(which(score_card_data$Performance.Tag==1))

percentage_of_defaults_under_cutoff <- ceiling((no_of_defaults_under_cutoff/total_no_of_defaults)*100)

model_outcome <-  ifelse(score_card_data$score >= 334,'approve','reject')
actual_outcome <- ifelse(score_card_data$Performance.Tag == 1, "defaulted","didnt_default")

table(model_outcome,actual_outcome)
#            defaulted didnt_default
#approve       937         38543
#reject       1954         27102

##-we have approved credit cards for 937 applicants who had defaulted lated
##+Additionally we rejected 1954 applicants who defauled later
##-we rejected 27.1k applicants who didnt_default
#Revenue loss = No of candidates rejected by the model who didn't default / Total No of candidates who didn't default
## 27102/(27102+38543) = 0.41, 41%

#correct default rate: 1954/(1954+937) = ~68%

#financial benefit by the model = correctly_defaulted_customer_prediction*default_loss - non_default_rejections*good_customer_loss
#1954*15000 -27102*1000 = +2,208,000 INR
percentage_of_defaults_under_cutoff <- ceiling((no_of_defaults_under_cutoff/total_no_of_defaults)*100)
percentage_of_defaults_under_cutoff
#68
#Plotting the score distribution for all the applicants of the test data
plot_score_334 <- ggplot(score_card_data, aes(x = score,fill=Performance.Tag ))+
  geom_histogram()+geom_vline(aes(xintercept = 334))+
  labs(x="Score",y="Count",title="Score Distribution for all applicants")+
  annotate("text", x=350,y=4000, colour = "black",hjust=0, vjust=0, size=4,label=paste("Defaults at 334: " ,percentage_of_defaults_under_cutoff,"%"))
plot_score_334
################
cutoff_scorecard = 355

#Calculating the percentage of defaults below the cutoff value of 355
no_of_defaults_under_cutoff <- length(which(score_card_data$Performance.Tag==1 & score_card_data$score <cutoff_scorecard))
total_no_of_defaults<-length(which(score_card_data$Performance.Tag==1))

percentage_of_defaults_under_cutoff <- ceiling((no_of_defaults_under_cutoff/total_no_of_defaults)*100)
percentage_of_defaults_under_cutoff

#91%
plot_score_355 <- ggplot(score_card_data, aes(x = score,fill=Performance.Tag ))+
  geom_histogram()+geom_vline(aes(xintercept = 355))+
  labs(x="Score",y="Count",title="Score Distribution for all applicants")+
  annotate("text", x=355,y=4000, colour = "black",hjust=0, vjust=0, size=4,label=paste("Defaults at 355: " ,percentage_of_defaults_under_cutoff,"%"))
plot_score_355

grid.arrange(plot_score_vs_perf_tag,plot_score_334,plot_score_355)
model_outcome <-  ifelse(score_card_data$score >= 355,'approve','reject')
actual_outcome <- ifelse(score_card_data$Performance.Tag == 1, "defaulted","didnt_default")

table(model_outcome,actual_outcome)
#             actual_outcome
#model_outcome defaulted didnt_default
#approve       268         20030
#reject       2623         45615

##-we have approved credit cards for 268 applicants who had defaulted lated
##+Additionally we rejected 2623 applicants who defauled later
##-we rejected 45.6k applicants who didnt_default
#Revenue loss = No of candidates rejected by the model who didn't default / Total No of candidates who didn't default
## 45615/45615+20030 = 0.7, 70%
#correct default rate: 2623/(2623+268) = 90%

#2623*15000 -45615*1000 = -6,270,000 INR

##decision making is needed here:
#financial benefit by the model = correctly_defaulted_customer_prediction*default_loss - non_default_rejections*good_customer_loss

##if we are looking to capture more default rate and can trade off revenue loss for that then
##scorecard cutoff should be 355
##based on assumptions
#2623*15000 -45615*1000 = -6,270,000 INR

##if we are looking to capture defaulters but do not wish to trade off revenue loss heavily and few miss in defaulters is fine then
##scorecard cutoff should be 334
#1954*15000 -27102*1000 = +2,208,000 INR
#########################################################################
#Predicting the application scores for the rejected population
rejected_applicants_df <- merged_dataset_NA
##data formatting
#removing application_id
rejected_applicants_df$Application.ID <- NULL
#as all this is rejected, replacing performance tag with 1
rejected_applicants_df$Performance.Tag.y <- 1
#renaming performance tag column
colnames(rejected_applicants_df)[28] <- 'Performance.Tag'
#getting data in same order as woe
colnames(rejected_applicants_df) <- colnames(merged_dataset_woe)
summary(rejected_applicants_df)
#Nas in Avg cc utilisation
sum(is.na(rejected_applicants_df))
#37 NA's' majority in avg cc utilisation
##removing all Na's as it is less than 1% of this dataset
rejected_applicants_df <-na.omit(rejected_applicants_df)

#putting same filters as applied on full data while data cleaning
rejected_applicants_df$No.of.months.in.current.company[which(rejected_applicants_df$No.of.months.in.current.company>74)] <- 74

rejected_applicants_df$Avgas.CC.Utilization.in.last.12.months[which(rejected_applicants_df$Avgas.CC.Utilization.in.last.12.months>91)] <- 91
rejected_applicants_df$Total.No.of.Trades[which(rejected_applicants_df$Total.No.of.Trades>31)] <- 31

#creating new df with woe_values
rejected_applicants_df_vals <-  woe_replace(rejected_applicants_df,IV)

#predicting based on final model
rejected_applicants_df_vals$predict_default<-predict(final_model, type = "response",newdata = rejected_applicants_df_vals[,-1])
rejected_applicants_df_vals$predict_non_default <- (1-(rejected_applicants_df_vals$predict_default))

rejected_applicants_df_vals$odds <-  log(rejected_applicants_df_vals$predict_non_default/rejected_applicants_df_vals$predict_default)

#getting the score for rejected population
rejected_applicants_df_vals$score <- ceiling(Offset + (Factor*rejected_applicants_df_vals$odds))

summary(rejected_applicants_df_vals$score)
#ranging from 304 to 344
##note our cutoff for score was 355 so all rejected applicants are below the cutoff

########################Cut off 334
cutoff_scorecard = 334

length(which(rejected_applicants_df_vals$score<cutoff_scorecard))/(nrow(rejected_applicants_df_vals)) 
# Using the decided cutoff of 355 we were able to identify 100% actual rejected applicants.

actual_rejections_using_scorecard = "99.1"

#Plotting the score distribution for actual rejected applicants
plot_rejected_score_334 <- ggplot(rejected_applicants_df_vals, aes(x = score,fill=Performance.Tag ))+
  geom_histogram()+geom_vline(aes(xintercept = 334))+
  labs(x="Score",y="Count",title="Score Distribution for all applicants")+
  annotate("text", x=334,y=100, colour = "black", size=3,label=paste("Defaults at 334: " ,actual_rejections_using_scorecard,"%"))
plot_rejected_score_334

#########
########################Cut off 355
cutoff_scorecard = 355

length(which(rejected_applicants_df_vals$score<cutoff_scorecard))/(nrow(rejected_applicants_df_vals)) 
# Using the decided cutoff of 355 we were able to identify 100% actual rejected applicants.

actual_rejections_using_scorecard = "100"

#Plotting the score distribution for actual rejected applicants
plot_rejected_score_355 <- ggplot(rejected_applicants_df_vals, aes(x = score,fill=Performance.Tag ))+
  geom_histogram()+geom_vline(aes(xintercept = 355))+
  labs(x="Score",y="Count",title="Score Distribution for all applicants")+
  annotate("text", x=345,y=100, colour = "black", size=3,label=paste("Defaults at 355: " ,actual_rejections_using_scorecard,"%"))
plot_rejected_score_355

grid.arrange(plot_rejected_score_334,plot_rejected_score_355)
###############################################################
######################################################
#Note: final model: logistic regression with smote technique and Woe_values

##as we dont know how much loss a default can cost the bank we have the take cutoff accordingly
##Score card insights and revenue loss analysis outcome
##if we are looking to capture more default rate and can trade off revenue loss for that then
##scorecard cutoff should be 355
#2623*15000 -45615*1000 = -6,270,000 INR
##revenue_loss: 70%

##if we are looking to capture defaulters but do not wish to trade off revenue loss heavily and few miss in defaulters is fine then
##scorecard cutoff should be 334
#1954*15000 -27102*1000 = +2,208,000 INR
##revenue_loss: 41%


#final chosen score cutoff: 334,
##if below assumptions are done:
#1 bank losses 1,000 INR per credit card per customer for a rejected applicant who didnt default
#2 per credit card default bank losses 15 times what it makes as revenue (as default generally happens when amount is high)
##i.e 15,000 INR per default

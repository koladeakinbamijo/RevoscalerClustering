#Below is the K-means clustering code used for a credit card dataset. 
#The dataset was obtained from KAGGLE : https://www.kaggle.com/arjunbhasin2013/ccdata 
#For this dataset, we want to the cluster the customers while focusing on the Balance in each customer's account


setwd('~/GIT') #set working directory of R
dataset = read.csv('CC GENERAL.csv') #read the CSV file
str(dataset)#get the datatypes of the columns
#CUST_ID                         : Factor w/ 8950 levels "C10001","C10002",..: 1 2 3 4 5 6 7 8 9 10 ...
# BALANCE                         : num  40.9 3202.5 2495.1 1666.7 817.7 ...
# BALANCE_FREQUENCY               : num  0.818 0.909 1 0.636 1 ...
# PURCHASES                       : num  95.4 0 773.2 1499 16 ...
# ONEOFF_PURCHASES                : num  0 0 773 1499 16 ...
# INSTALLMENTS_PURCHASES          : num  95.4 0 0 0 0 ...
# CASH_ADVANCE                    : num  0 6443 0 206 0 ...
# PURCHASES_FREQUENCY             : num  0.1667 0 1 0.0833 0.0833 ...
# ONEOFF_PURCHASES_FREQUENCY      : num  0 0 1 0.0833 0.0833 ...
# PURCHASES_INSTALLMENTS_FREQUENCY: num  0.0833 0 0 0 0 ...
# CASH_ADVANCE_FREQUENCY          : num  0 0.25 0 0.0833 0 ...
# CASH_ADVANCE_TRX                : int  0 4 0 1 0 0 0 0 0 0 ...
# PURCHASES_TRX                   : int  2 0 12 1 1 8 64 12 5 3 ...
# CREDIT_LIMIT                    : num  1000 7000 7500 7500 1200 1800 13500 2300 7000 11000 ...
# PAYMENTS                        : num  202 4103 622 0 678 ...
# MINIMUM_PAYMENTS                : num  140 1072 627 NA 245 ...
# PRC_FULL_PAYMENT                : num  0 0.222 0 0 0 ...
# TENURE                          : int  12 12 12 12 12 12 12 12 12 12 ...

sapply(dataset, function(x) sum(is.na(x))) #identify the number of missing items per column

#CUST_ID                               BALANCE                BALANCE_FREQUENCY 
#0                                      0                                0 
#PURCHASES                             ONEOFF_PURCHASES           INSTALLMENTS_PURCHASES 
#0                                       0                                0 
#CASH_ADVANCE                          PURCHASES_FREQUENCY       ONEOFF_PURCHASES_FREQUENCY 
#0                                       0                                0 
#PURCHASES_INSTALLMENTS_FREQUENCY      CASH_ADVANCE_FREQUENCY       CASH_ADVANCE_TRX 
#0                                           0                                0 
#PURCHASES_TRX                         CREDIT_LIMIT                  PAYMENTS 
#0                                         1                                0 
#MINIMUM_PAYMENTS                      PRC_FULL_PAYMENT                TENURE 
#313                                       0                                0 

#the missing items are in the MINIMUM_PAYMENTS and CREDIT_LIMIT variables which are both numeric variables, we shall use the
#KNNImputation technique to remove this missing items

library('DMwR')
dataset = knnImputation(dataset,2)#use the KNNImputation techinque to remove missing items

sapply(dataset, function(x) sum(is.na(x))) #check to ensure that there are still no missing items

#CUST_ID                                 BALANCE                BALANCE_FREQUENCY 
#0                                        0                                0 
#PURCHASES                              ONEOFF_PURCHASES           INSTALLMENTS_PURCHASES 
#0                                         0                                0 
#CASH_ADVANCE                          PURCHASES_FREQUENCY       ONEOFF_PURCHASES_FREQUENCY 
#0                                         0                                0 
#PURCHASES_INSTALLMENTS_FREQUENCY      CASH_ADVANCE_FREQUENCY        CASH_ADVANCE_TRX 
#0                                        0                                0 
#PURCHASES_TRX                         CREDIT_LIMIT                   PAYMENTS 
#0                                      0                                0 
#MINIMUM_PAYMENTS                     PRC_FULL_PAYMENT                   TENURE 
#0                                       0                              0 

#There are no more missing items


#As earlier stated, we want to the cluster the customers while focusing on the Balance in each customer's
#account, hence we shall run a Linear regression and step it so as to obtain significant variables which would be
#used for the clustering. The BALANCE variable will be used as the dependent variable
#the Linear and Stepwise Regression shall be run in Revoscaler

dataset2 = dataset[,-1] #remove the customer id variable as it does not add any valuable information for the Linear and 
#stepwise regression
dataset2 = scale(dataset2) #scale the data, as there are some variables with large numeric variables and some others
#with smaller values

dataset2_xdf = 'dataset2.xdf' #assign a name to the XDF file in which the scaled dataframe would be saved 

rxImport(inData=as.data.frame(dataset2),outFile=dataset2_xdf,overwrite=TRUE)#convert the scaled dataframe into XDF(i.e dataset2_xdf)

#we obtain a formula to be used in the Linear regresion function
VarNames <- rxGetVarNames(dataset2_xdf)#obtain the variable names from the dataset2_xdf file
toexclude <- CLR<-c("BALANCE")#obtain the dependent variable 
RemVarNames<-VarNames[!VarNames%in%c(toexclude)]#exclude the dependent variable from the VarNames variable and pass this into
# a new variable(i.e RemVarNames)
FormularText<-paste(c('BALANCE',paste(RemVarNames,collapse = "+")),collapse="~")# add the + and ~ characters  
Formula<-as.formula(FormularText)#convert the FormularText variable into a formular object that Revoscaler can use

#To use the Stepwise Regression, we obtain a formular which excludes the dependent variable

StepFormularText<-paste("~",paste(RemVarNames,collapse = "+"))#using the RemVarNames from earlier, we add only + 
#and ~ characters to create a string that can be converted to a formular
StepFormula<-as.formula(StepFormularText) #convert StepFormularText variable into a formular object that Revoscaler can use

StepwiseResult <- rxLinMod(Formula,data = dataset2_xdf,
               variableSelection = rxStepControl(method="stepwise",
                                                 scope = StepFormula )) #run the Stepwise Regression in Revoscaler



StepwiseFormula=StepwiseResult$params$Formula #obtain the signifcant variables(which would be in a formular)
#NOTE that as the StepwiseFormula variable is in formular format, it also has the dependent variable added to it
#"BALANCE~BALANCE_FREQUENCY+CASH_ADVANCE+CASH_ADVANCE_FREQUENCY+CASH_ADVANCE_TRX+CREDIT_LIMIT+INSTALLMENTS_PURCHASES+
#MINIMUM_PAYMENTS+ONEOFF_PURCHASES_FREQUENCY+PAYMENTS+PRC_FULL_PAYMENT+PURCHASES+PURCHASES_FREQUENCY+PURCHASES_TRX+TENURE"

#Next we utilise the significant variables in the Revoscaler K-means function

StepwiseFormula = gsub("BALANCE~", "", StepwiseFormula)#remove the dependent variable from the StepwiseFormula variable

StepwiseFormula = paste("~",StepwiseFormula)#add the ~ character to create a string that can be converted to a formular for 
#Revoscaler to use

StepwiseFormula<-as.formula(StepwiseFormula) #convert StepwiseFormula variable into a formular object that Revoscaler can use

#run the elbow method using the Revoscler K-means function
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(rxKmeans(StepwiseFormula, data=dataset2_xdf, numClusters=i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')


#From the plot, the optimum number of clusters is 6
#carry out kmeans clustering, NOTE that the rxkmeans function will append a new column that contains the cluster each
#customer was assigned to
kmeans_xdf<-rxKmeans(StepwiseFormula,data=dataset2_xdf,outFile = dataset2_xdf ,numClusters=6, outColName="Clusterno",blocksPerRead = 1,
                        overwrite=TRUE)

rxGetInfo(dataset2_xdf, getVarInfo=TRUE)#ensure that a new column has been added to the dataset2_xdf object/file


#convert scaled xdf back to dataframe
dataset_cluster=rxImport(dataset2_xdf)

#add the Clusterno column back to the original dataset
dataset$Cluster = dataset_cluster$Clusterno

#plot the clusters by thier frequency
library(ggplot2)
gg <- ggplot(dataset, aes(x=Cluster,fill=factor(Cluster)))
gg + geom_bar()+ scale_fill_brewer(palette="Blues") + labs(title="Cluster Frequency", x="Clusters", y="Number of Customers")  

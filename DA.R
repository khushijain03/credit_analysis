#Importing the libraries we will be using in the project
library("tidyr")
library('ggvis')
library('tidyverse')
library('ggplot2')
library('dplyr')
library('reshape')
library('lattice')
library('heatmaply')
library('ggcorrplot')
library('patchwork')
library('GGally')
library('data.table')
library('cowplot')
library('knitr')
library('gmodels')
library('reshape2')
#Reading the "previous_app.csv" into the dataframe prev_data
prev_data <- read.csv(file = '/Users/khushijainmahendra/Downloads/previous_app.csv')
#Reading the "current_app.csv" into the dataframe prev_data
curr_data <- read.csv(file = '/Users/khushijainmahendra/Downloads/current_app.csv')

#Printing the head of the dataframe to view the data available
head(prev_data,n=5)
head(curr_data,n=5)

#finding sum of null values in every column
colSums(is.na(prev_data))

#finding sum of null values in every column
colSums(is.na(curr_data))
dim(curr_data)


#getting elaborate details about each attribute in the data 
summary(curr_data)
summary(prev_data)

## Remove columns with more than 90% NA
curr_data <- curr_data[, which(colMeans(!is.na(curr_data)) >= 0.9)]
head(curr_data)

#To check the null values in the dataset
is.null(curr_data)


#The number of columns has gone down to 84 from 122 which means that 38 columns had more than 50% rows with null values.
# Using a box plot to see if we have outliers in the CNT_FAM_MEMBERS column  of the curr_data dataset
ggplot(curr_data, aes(x=CNT_FAM_MEMBERS, y = "CNT_FAM_MEMBERS")) +
  geom_boxplot()

summary(curr_data['CNT_FAM_MEMBERS'])

# use a box plot to see if we have outliers in the AMT_CREDIT column
# of the curr_data dataset
ggplot(curr_data, aes(x=AMT_CREDIT, y = "AMT_CREDIT")) +
  geom_boxplot()

ggplot(data = curr_data) +
  geom_histogram(mapping = aes(x = CNT_FAM_MEMBERS), binwidth = 0.5)

summary(curr_data["AMT_CREDIT"])

# checking for percentage of null values
colMeans(is.na(curr_data))

#Checking for data imbalance for the Target Column
Defaulter_sum <- sum(curr_data$TARGET == 1)
Defaulter_sum

Defaulter <- round(Defaulter_sum/length(curr_data),1)
Defaulter

Non_Defaulter_sum <- sum(curr_data$TARGET==0)
Non_Defaulter_sum
Non_Defaulter <- round(Non_Defaulter_sum/length(curr_data),1)
Non_Defaulter

#Piechart
x <- c(Defaulter,Non_Defaulter)
labels<- c("Defaulter", "Non_Defaulter")
piepercent<- round(100*x/sum(x),1)
piepercent

pie(x,labels = piepercent, col= rainbow(length(x)))
legend("topright",c(labels), cex=0.8, fill=rainbow((length(x))))

# We can see from the map that there is a severe imbalance in the client list for the TARGET columns, with an Imbalance ratio of 11.5 (i.e., 8% of clients are defaulters compared to 92% of non-defaulters).
# We now analyze the dataset with respect to Inferential Statistics
#splitting the dataframe into two data sets

OriginalDT <- data.table(curr_data)
curr_data0 <- OriginalDT[TARGET == 0]
curr_data1 <- OriginalDT[TARGET == 1]

curr_data0


dt2 <- prev_data %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
head(dt2)

#correlation matrix
cormat <- cor(prev_data[sapply(prev_data,is.numeric)])
melted_data <- melt(cormat)
head(melted_data)
melted_data

ggplot(data= melted_data, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()

#closer at the distribution of TARGET column
# Histogram with kernel density curve
ggplot(curr_data,aes(x=TARGET)) +
  geom_density(alpha=.2, fill="#FF6666")


##plotting the graph
melted_data_num <- data.frame(
 lapply(curr_data, as.integer))
ggcorr(curr_data,
            method=c("pairwise","spearman"),
            nbreaks=6,
           hjust = 0.5,
            label = TRUE,
           label_size=2,
           color="grey50")

#lower triangle of the correlation matrix
get_lower_tri <- function(cormat)
{
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

#upper triangle of the correlation matrix
get_upper_tri <-function(cormat) {
  
  cormat[lower.tri(cormat)]<- NA
  return (cormat)
}

upper_tri<- get_upper_tri(cormat)
upper_tri

#correlation heatmap
melted_data <- melt(upper_tri, na.rm=TRUE)
ggplot(data=melted_data, aes(Var2,Var1, fill=value)) +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Univariate Categorical Features
plot_unnivariate_categorical <- function(i) {
  g1 <-ggplot(curr_data0, aes_string(x=i)) +geom_bar()
  g2 <-ggplot(curr_data1, aes_string(x=i)) +geom_bar()
  print(cowplot::plot_grid(g1,g2))
}

plot_unnivariate_categorical('CODE_GENDER')
plot_unnivariate_categorical('NAME_CONTRACT_TYPE')
plot_unnivariate_categorical('NAME_FAMILY_STATUS')
plot_unnivariate_categorical('NAME_HOUSING_TYPE')
plot_unnivariate_categorical('NAME_EDUCATION_TYPE')

#Univariate Continuous Features
plot_unnivariate_continuous <- function(i)
{
h1<- ggplot(curr_data0, aes_string(x=i)) +geom_histogram(alpha=0.5, position="identity",bins='30')
h2<- ggplot(curr_data1, aes_string(x=i)) +geom_histogram(alpha=0.5, position="identity",bins='30')
  print(cowplot::plot_grid(h1,h2))
}
#Count of Children
plot_unnivariate_continuous('CNT_CHILDREN')
#Inference: The number of people taking out loans before they have children are more than the number of people taking loans after having children

#Total Income
plot_unnivariate_continuous('AMT_INCOME_TOTAL')
#Credit Amount
plot_unnivariate_continuous('AMT_CREDIT')
#Annuity Amount
plot_unnivariate_continuous('AMT_ANNUITY')
#Age
plot_unnivariate_continuous('DAYS_BIRTH')

#Bivariate-categorical-categorical
plot_Bivariate_cat_cat <- function(i,j)
{
  g3<- ggplot(curr_data0, aes_string(x=i, fill=j)) +geom_bar(position = position_dodge(preserve = "single"))
  g4<- ggplot(curr_data1, aes_string(x=i,fill=j))+geom_bar(position = position_dodge(preserve = "single"))
  print(cowplot::plot_grid(g3,g4))
}

#Following plots were plotted:
#Family Status VS Education
plot_Bivariate_cat_cat('NAME_FAMILY_STATUS','NAME_EDUCATION_TYPE')
#Housing Type VS Family Status
plot_Bivariate_cat_cat('NAME_HOUSING_TYPE','NAME_FAMILY_STATUS')
#Education VS Gender
plot_Bivariate_cat_cat('NAME_EDUCATION_TYPE','CODE_GENDER')
#Education VS Gender
plot_Bivariate_cat_cat('NAME_HOUSING_TYPE','NAME_EDUCATION_TYPE')
#Housing Type VS Gender
plot_Bivariate_cat_cat('NAME_FAMILY_STATUS','CODE_GENDER')

#Bivariate Categorical-Continuous
plot_Bivariate_cat_con <- function(i,j)
{
  b1<- ggplot(curr_data0, aes_string(x=i, y=j)) +geom_boxplot()
  b2<- ggplot(curr_data1, aes_string(x=i,y=j))+geom_boxplot()
  print(cowplot::plot_grid(b1,b2))
}

#Education VS Income
plot_Bivariate_cat_con('NAME_EDUCATION_TYPE','AMT_INCOME_TOTAL')
#Education VS Credit Amount
plot_Bivariate_cat_con('NAME_EDUCATION_TYPE','AMT_CREDIT')
# Inference: The overall mean for the Credit Amount is higher for people with Academic degree.

#Gender VS Credit Amount
plot_Bivariate_cat_con('CODE_GENDER','AMT_CREDIT')
#Inference: Both Men and Women have the same anount of credit.

#Housing Type VS Count of Children
plot_Bivariate_cat_con('NAME_HOUSING_TYPE','CNT_CHILDREN')
#Education VS Count of Children
plot_Bivariate_cat_con('NAME_EDUCATION_TYPE','CNT_CHILDREN')


#Bivariate Continuous-Continuous
plot_Bivariate_con_con <- function(i,j)
{
  s1<- ggplot(curr_data0, aes_string(x=i, y=j)) +geom_point(color="cornflowerblue", 
                                                            size = 2, 
                                                            alpha=.8)
  s2<- ggplot(curr_data1, aes_string(x=i,y=j))+geom_point(color="cornflowerblue", 
                                                          size = 2, 
                                                          alpha=.8)
  print(cowplot::plot_grid(s1,s2))
}
#Credit Amount VS Age
plot_Bivariate_con_con('AMT_CREDIT','DAYS_BIRTH')
#Income VS Credit Amount
plot_Bivariate_con_con('AMT_INCOME_TOTAL','AMT_CREDIT')
#Income VS Annuity
plot_Bivariate_con_con('AMT_INCOME_TOTAL','AMT_ANNUITY')
#Income VS Age
plot_Bivariate_con_con('AMT_INCOME_TOTAL','DAYS_BIRTH')
#Age VS Annuity
plot_Bivariate_con_con('DAYS_BIRTH','AMT_ANNUITY')


#correlation heatmap for non defaulters
subset_data0 <- select(curr_data0, CNT_CHILDREN, AMT_INCOME_TOTAL, AMT_CREDIT, AMT_ANNUITY, DAYS_BIRTH, DAYS_EMPLOYED, AMT_GOODS_PRICE)

corr_mat0 <- round(cor(subset_data0),2)
# reduce the size of correlation matrix
melted_corr_mat0 <- melt(corr_mat0)
head(melted_corr_mat0)

# plotting the correlation heatmap
ggplot(data = melted_corr_mat0, aes(x=Var1, y=Var2,
                                    fill=value)) +
  geom_tile()

#correlation heatmap for defaulters
subset_data1 <- select(curr_data1, CNT_CHILDREN, AMT_INCOME_TOTAL, AMT_CREDIT, AMT_ANNUITY, DAYS_BIRTH, DAYS_EMPLOYED, AMT_GOODS_PRICE)

corr_mat1 <- round(cor(subset_data1),2)
# reduce the size of correlation matrix
melted_corr_mat1 <- melt(corr_mat1)
head(melted_corr_mat1)

# plotting the correlation heatmap
ggplot(data = melted_corr_mat1, aes(x=Var1, y=Var2,
                                    fill=value)) +
  geom_tile()


#Decision tree
library(party)
output.tree <- ctree(
  TARGET ~  AMT_ANNUITY, 
  data = curr_data)
plot(output.tree)


#RegressionTree
#test_curr <- sample(1:nrow(curr_data), size = 102503)
#train_curr<-(1:nrow(curr_data))[-test_curr]
#set.seed(1)
library(rpart)
#Treemodel<-rpart(TARGET~ ., data=curr_data[train_curr, ],method = "class")
library(rpart.plot)
#rpart.plot(Treemodel, extra = 106)
# Splitting the data into train and test
split1<- sample(c(rep(0, 0.7 * nrow(encoded_data)), rep(1, 0.3 * nrow(encoded_data))))
train <- encoded_data[split1 == 0, ]            

test <- encoded_data[split1== 1, ]    


# rpart(): Function to fit the model. The arguments are:
# survived ~.: Formula of the Decision Trees
# data = data_train: Dataset
# method = ‘class’: Fit a binary model
fit <- rpart(TARGET~., data = train, method = 'class')
rpart.plot(fit, extra = 106)

predict_unseen <-predict(fit, test, type = 'class')
#Confusion Matrix
table_mat <- table(test$TARGET, predict_unseen)
table_mat
#Calculating the accuracy

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test*100))


#rpart.plot(Treemodel,extra =1)
#library(ROCR)
#prp(Treemodel, type=2, extra=1)


#Fitting the model using glm() function
model<- glm(TARGET ~., family = binomial(link='logit'),data=train)
summary(model)
train_prev <- prev_data


# printing the first 3 elements
lapply(model,class)[1:3]
model$aic

#Accessing the performance of the model:
predict<- predict(model,test, type='response')
test_new <-test
test_new$CODE_GENDER[which(!(test_new$CODE_GENDER %in% unique(train$CODE_GENDER)))]<- NA
test_new$NAME_INCOME_TYPE[which(!(test_new$NAME_INCOME_TYPE %in% unique(train$NAME_INCOME_TYPE)))]<- NA
test_new$NAME_FAMILY_STATUS[which(!(test_new$NAME_FAMILY_STATUS %in% unique(train$NAME_FAMILY_STATUS)))]<- NA
test_new$ORGANIZATION_TYPE[which(!(test_new$ORGANIZATION_TYPE %in% unique(train$ORGANIZATION_TYPE)))]<- NA
test_new$ORGANIZATION_TYPE[which(!(test_new$ORGANIZATION_TYPE %in% unique(test$ORGANIZATION_TYPE)))]<- NA
test_new
predict<- predict(model,test_new, type='response')
summary(model)


#Now we can run the anova() function on the model to analyze the table of deviance
anova(model, test="Chisq")

#While no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
pR2(model)
levels(as.factor(train$CODE_GENDER)) 

#Assessing the predictive ability of the model
#a <- predict(model,newdata = test_new)
#fitted.results <- predict(model,newdata=subset(test_new,select=c(2)),type='response')
#fitted.results <- ifelse(fitted.results > 0.5,1,0)
#misClasificError <- mean(fitted.results != test$TARGET)
#print(paste('Accuracy',1-misClasificError))
#confusion matrix

table_mat<-table(test$TARGET, predict >0.5)
table_mat
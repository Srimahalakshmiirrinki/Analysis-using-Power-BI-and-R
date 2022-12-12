# Loading csv file to df

df<- read.csv("/cloud/project/HollywoodsMostProfitableStories.csv")

#Take a look at the data

View(df)

# look for rows and columns
dim(df)

# Loading Liabrary

install.packages("tidyverse")

# import Liabrary
library(tidyverse)

#Checking datatypes
str(df)

# Check for missing values:

colSums(is.na(df))

#Drop missing values

df <- na.omit(df)



# check to make sure that the rows have been removed
head(df)

dim(df)

#Check for duplicates
dim(df[duplicated(df$Film),])[1]

#round off values of profitability & Worldwide.Gross to 2 places

df$Profitability <- round(df$Profitability ,digit=2)
View(df)

df$Worldwide.Gross <- round(df$Worldwide.Gross ,digit=2)

View(df)

#Check for outliers using a boxplot

library(ggplot2)

#Create a boxplot that highlights the outliers

ggplot(df,aes(x=Profitability, y=Worldwide.Gross)) + geom_boxplot(outlier.colour = "red",outlier.shape= 2)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,1000))


#Remove outliers in 'Profitability'

Q1 <- quantile(df$Profitability, .25)

Q3 <- quantile(df$Profitability, .75)

IQR <- IQR(df$Profitability)

no_outliers <- subset(df, df$Profitability> (Q1 - 1.5*IQR) & df$Profitability< (Q3 + 1.5*IQR))

dim(no_outliers) 

ggplot(df,aes(x=Profitability, y=Worldwide.Gross)) + geom_boxplot(outlier.colour = "green",outlier.shape= 2)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,1000))

#Remove outliers in 'Worldwide.Gross'

Q1 <- quantile(no_outliers$Worldwide.Gross, .25)

Q3 <- quantile(no_outliers$Worldwide.Gross, .75)

IQR <- IQR(no_outliers$Worldwide.Gross)

df1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))

dim(df1) 
View(df1)

ggplot(df,aes(x=Profitability, y=Worldwide.Gross)) + geom_boxplot(outlier.colour = "pink",outlier.shape= 2)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,1000))


#Summary Statistics /Univariate Analysis

summary(df1)


#bivariate analysis

#scatterplot

ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point(colour = "red")+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

ggplot(df1, aes(x=Genre, y=Worldwide.Gross)) + geom_point(colour = "blue")+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

#bar chart
ggplot(df1, aes(x=Year)) + geom_bar(colour = "blue")

ggplot(df1, aes(x=Genre)) + geom_bar(colour = "red")

#Exporting clean data to csv file

write.csv(df1, "cleanHollywooddata_df.csv")



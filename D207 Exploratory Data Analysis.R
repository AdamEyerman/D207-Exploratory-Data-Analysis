#Install packages and libraries needed
install.packages("tidyverse")
library(ggplot2)
library(rcompanion)

#Load clean data set to RStudio
churn_data_cleaned <- read.csv('churn_data_cleaned.csv')

# Create a table with the Churn and Contract variables
churn_data = table(churn_data_cleaned$Churn, churn_data_cleaned$Contract)
print(churn_data)

# Perform the Chi-Square test.
print(chisq.test(churn_data))

# Create a table with the Churn and InternetService variables
churn_data_2 = table(churn_data_cleaned$Churn, churn_data_cleaned$InternetService)
print(churn_data_2)

# Perform the Chi-Square test.
print(chisq.test(churn_data_2))









# Display table of Churn categorical variable
table(churn_data_cleaned$Churn)


# Display histogram of Churn categorical variable
num_customers = 1
plot<-ggplot(churn_data_cleaned,
             aes(Churn,num_customers)) +
  geom_bar(stat = "identity")
plot

# Display table of InternetService categorical variable
table(churn_data_cleaned$InternetService)

# Display histogram of InternetService categorical variable
num_customers = 1
plot_2<-ggplot(churn_data_cleaned,
             aes(InternetService,num_customers)) +
  geom_bar(stat = "identity")
plot_2

# Display boxplot of Outage_sec_perweek continuous variable
boxplot(churn_data_cleaned$Outage_sec_perweek, ylab = "Outage sec/week" , main = "Outage Time")

# Display histogram of Outage_sec_perweek continuous variable
hist(churn_data_cleaned$Outage_sec_perweek, xlab = "Outage sec/week", ylab = "Num_customers", main = "Outage Time")

# Display boxplot of Bandwidth_GB_Year continuous variable
boxplot(churn_data_cleaned$Bandwidth_GB_Year, ylab = "Bandwidth GB/year", main = "Bandwidth Usage")

# Display histogram of Bandwidth_GB_Year continuous variable
hist(churn_data_cleaned$Bandwidth_GB_Year, xlab = "Bandwidth GB/Year", ylab = "Num_customers", main = "Bandwidth Usage")







  

# Display table of Churn/Internet Service categorical variables
table(churn_data_cleaned$Churn, churn_data_cleaned$InternetService)

# Create data set for CramerV Correlation
Churn_InternetService = table(churn_data_cleaned$Churn, churn_data_cleaned$InternetService)

# Display CramerV Correlation 
cramerV(Churn_InternetService)

# Display barplot of Churn rate by Internet Service
plot <- ggplot(churn_data_cleaned, aes(InternetService, fill = Churn)) + geom_bar(position = "dodge", stat = "count")

plot +labs(title="Churn Rate by Internet Service",
           x ="Internet Service", y = "Customer Count")

# Display scatterplot of Outage_sec_perweek/Bandwidth_GB_Year continuous variables 
plot(churn_data_cleaned$Outage_sec_perweek, churn_data_cleaned$Bandwidth_GB_Year, pch=16, col='steelblue',
     main='Outage_sec_perweek vs. Bandwidth_GB_Year',
     xlab='Outage', ylab='Bandwidth')

# Display correlation coefficient of Outage_sec_perweek/Bandwidth_GB_Year continuous variables
cor(churn_data_cleaned$Outage_sec_perweek, churn_data_cleaned$Bandwidth_GB_Year)





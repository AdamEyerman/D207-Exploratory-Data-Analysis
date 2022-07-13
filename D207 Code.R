#Install needed packages and library for analysis
install.packages("tidyverse")
library(tidyverse)

#Load clean data set into R
churn_clean <- read.csv('churn_clean.csv')

# Create a table with the Churn and Contract variables
churn_contract = table(churn_clean$Churn, churn_clean$Contract)
print(churn_contract)

# Perform the Chi-Square test.
print(chisq.test(churn_contract))

# Create a table with the Churn and InternetService variables
churn_internet = table(churn_clean$Churn, churn_clean$InternetService)
print(churn_internet)

# Perform the Chi-Square test.
print(chisq.test(churn_internet))




#Univariate Stats

# Display table of Churn categorical variable
table(churn_clean$Churn)

# Display histogram of Churn categorical variable

num_customers = 1

plot<-ggplot(churn_clean,
             aes(Churn,num_customers)) +
  geom_bar(stat = "identity")

plot


# Display table of InternetService categorical variable
table(churn_clean$InternetService)

# Display histogram of InternetService categorical variable

num_customers = 1

plot_2<-ggplot(churn_clean,
               aes(InternetService,num_customers)) +
  geom_bar(stat = "identity")

plot_2


# Display boxplot of Outage_sec_perweek continuous variable
boxplot(churn_clean$Outage_sec_perweek, ylab = "Outage sec/week" , main = "Outage Time")

# Display histogram of Outage_sec_perweek continuous variable
hist(churn_clean$Outage_sec_perweek, xlab = "Outage sec/week", ylab = "Num_customers", main = "Outage Time")

# Display boxplot of Bandwidth_GB_Year continuous variable
boxplot(churn_clean$Bandwidth_GB_Year, ylab = "Bandwidth GB/year", main = "Bandwidth Usage")

# Display histogram of Bandwidth_GB_Year continuous variable
hist(churn_clean$Bandwidth_GB_Year, xlab = "Bandwidth GB/Year", ylab = "Num_customers", main = "Bandwidth Usage")




#Bivariate Stats

# Display table of Churn/Internet Service categorical variables
table(churn_clean$Churn, churn_clean$InternetService)

# Display barplot of Churn rate by Internet Service
plot_3 <- ggplot(churn_clean, aes(InternetService, fill = Churn)) + geom_bar(position = "dodge", stat = "count")

plot_3 +labs(title="Churn Rate by Internet Service",
             x ="Internet Service", y = "Customer Count")

# Display scatterplot of Outage_sec_perweek/Bandwidth_GB_Year continuous variables 
plot(churn_clean$Outage_sec_perweek, churn_clean$Bandwidth_GB_Year, pch=16, col='steelblue',
     main='Outage_sec_perweek vs. Bandwidth_GB_Year',
     xlab='Outage', ylab='Bandwidth')

# Display correlation coefficient of Outage_sec_perweek/Bandwidth_GB_Year continuous variables
cor(churn_clean$Outage_sec_perweek, churn_clean$Bandwidth_GB_Year)




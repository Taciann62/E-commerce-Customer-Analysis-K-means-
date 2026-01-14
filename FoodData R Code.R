## Preview dataset

library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)
library(janitor)
library(tibble)
library(ggplot2)

head(ifood_df)

Food_Data<-ifood_df %>% 
  rename(Household_income = Income, 
         Kids= Kidhome, 
         Teen_in_Household = Teenhome,
         Days_AfterPurchase = Recency, 
         Wine_Sales = MntWines, 
         Fruits_Sales = MntFruits, 
         Meat_Sales = MntMeatProducts,
         Fish_Sales = MntFishProducts,
         Sweet_Sales = MntSweetProducts, 
         Gold_Sales= MntGoldProds, 
         Total_Amt= MntTotal, 
         Divorced = marital_Divorced, 
         Single = marital_Single,
         Married = marital_Married, 
         Dating = marital_Together, 
         Widow = marital_Widow,
         Cmp_1 = AcceptedCmp1,
         Cmp_2 = AcceptedCmp2,
         Cmp_3 = AcceptedCmp3,
         Cmp_4 = AcceptedCmp4,
         Cmp_5 = AcceptedCmp5,
         Last_Cmp = Response,
         Cmp_overall = AcceptedCmpOverall,
         Complain = Complain,
         Cust_Age =Age,
         Cust_Days = Customer_Days,
         High_School = `education_2n Cycle`,
         Basic_Edu = education_Basic,
         BSC = education_Graduation,
         MSC = education_Master,
         PHD = education_PhD,
         Discounted_Purchase = NumDealsPurchases, 
         In_Store_Purchase =NumStorePurchases , 
         Web_Purchases = NumWebPurchases, 
         Web_Traffic =NumWebVisitsMonth, 
         Catalogue_Purchases= NumCatalogPurchases,
         Z_Cos = Z_CostContact,
         Z_Rev = Z_Revenue,
         Regular_Prods = MntRegularProds,
  )
## Ensure the total amount for the sold items is accuarretely summed
Food_Data<-Food_Data %>% 
  mutate(Total_Amt =rowSums(select(., Fish_Sales, Fruits_Sales, Meat_Sales, Wine_Sales, Gold_Sales, Sweet_Sales), na.rm = TRUE))

## Ensure numeric rows are numeric
Food_Data <- Food_Data %>% 
  mutate(across(where(is.numeric), abs))

Food_Data<-Food_Data %>% 
  select(-Z_Cos, -Z_Rev)

## Sum campaign acceptance
Food_Data<-Food_Data %>% 
  mutate(Cmp_overall = rowSums(select(., Cmp_2, Cmp_1, Cmp_3, Cmp_4, Cmp_5, Last_Cmp), na.rm = TRUE))

## Remove Duplicates
Food_Data<-Food_Data %>% 
  distinct()

## Review the data properly'
summary(Food_Data)

colSums(is.na(Food_Data))

### Total on each column
mean(Food_Data$Total_Amt)
median(Food_Data$Total_Amt)
sum(Food_Data$Household_income)
sd(Food_Data$Total_Amt)

Total_Column <- Food_Data %>% 
  summarise(Total_income = sum(Household_income), Avg_income =mean(Household_income), Med_Income = median(Household_income), 
            Standard_Deviation = sd(Household_income), Highest = max(Household_income), Least = min(Household_income), Transaction_count = n(), Total_Amt = sum(Total_Amt),
            Avg_total =mean(Total_Amt), Median =median(Total_Amt))

## Per product

Wine_Sales<- Food_Data %>% 
  summarise(Total_Amt_Spent = sum(Wine_Sales), Avg_Amt_spent = mean(Wine_Sales), Sales_COunt = n(), SD = sd(Wine_Sales), Med = median(Wine_Sales))

Fruit_Sales<- Food_Data %>% 
  summarise(Total_Amt_Spent = sum(Fruits_Sales), Avg_Amt_spent = mean(Fruits_Sales), Sales_COunt = n(), SD = sd(Fruits_Sales), Med = median(Fruits_Sales))

Sweet_Sales <- Food_Data %>% 
  summarise(Total_Amt_Spent = sum(Sweet_Sales), Avg_Amt_spent = mean(Sweet_Sales),Sales_Count =n(), SD = sd(Sweet_Sales), Med = median(Sweet_Sales))

Fish_sales <- Food_Data %>% 
  summarise(Total_Amt_Spent = sum(Fish_Sales), Avg_Amt_spent = mean(Fish_Sales),Sales_Count =n(), SD = sd(Fish_Sales), Med = median(Fish_Sales))

Gold_Sales <- Food_Data %>% 
  summarise(Total_Amt_Spent = sum(Gold_Sales), Avg_Amt_spent = mean(Gold_Sales),Sales_Count =n(), SD = sd(Gold_Sales), Med = median(Gold_Sales))

Meat_Sales <- Food_Data %>% 
  summarise(Total_Amt_Spent = sum(Meat_Sales), Avg_Amt_spent = mean(Meat_Sales),Sales_Count =n(), SD = sd(Meat_Sales), Med = median(Meat_Sales))


Foodclustered_data <- Food_Data %>%
  select(Household_income, Total_Amt, Fish_Sales, Wine_Sales, Meat_Sales, Gold_Sales,
         Fruits_Sales, Sweet_Sales, Web_Purchases, Web_Traffic, Discounted_Purchase, Catalogue_Purchases, In_Store_Purchase, Cust_Age) %>%
  drop_na() 

FoodScaled_data <- scale(Foodclustered_data)

set.seed(123)
wss<-sapply(1:10, function(k){kmeans(FoodScaled_data, k, nstart = 20)$tot.withinss})

plot(1:10, wss, type ="b", pch = 19, frame = FALSE, xlab = "Number of Clusters K", ylab = "Total within-cluster sum of squares" )


set.seed(123)
Clustred_result<- kmeans(FoodScaled_data, centers = 3, nstart = 25)

Clustred_result$size

Foodclustered_data <- Foodclustered_data %>% 
  mutate(Cluster = as.factor(Clustred_result$cluster))

Food_Analysis <- Foodclustered_data %>%
  group_by(Cluster) %>% 
  summarise(Sales_count = n(),
            Avg_Income = mean(Household_income),
            Avg_Total_Spend = mean(Total_Amt),
            Avg_Wine = mean(Wine_Sales),
            Avg_Sweet = mean(Sweet_Sales),
            Avg_Fruit = mean(Fruits_Sales),
            Avg_Fish = mean(Fish_Sales),
            Avg_web_purchase = mean(Web_Purchases),
            Avg_Discount = mean(Discounted_Purchase),
            Avg_Catalogue = mean(Catalogue_Purchases),
            Avg_Instore = mean(In_Store_Purchase),
            Avg_Age = mean(Cust_Age),
            Avg_Meat = mean(Meat_Sales))

Food_Analysis <- Food_Analysis %>% 
  mutate(Cluster_Level = case_when(Cluster == 2 ~"High_Spenders", Cluster == 3 ~"Mid_Spenders", Cluster == 1 ~"Low_Spenders", TRUE    ~"Unknown"))


ggplot(Foodclustered_data, aes(x= Household_income, y = Total_Amt, colour = Cluster)) +
  geom_point(alpha= 0.6) + theme_minimal() + labs(title = "customer segments : income vs Total spending", x = "Household income", y = " Total Amount Spent")

# Join the labels back to the main plotting data
Foodclustered_data <- Foodclustered_data %>%
  left_join(Food_Analysis %>% select(Cluster, Cluster_Level), by = "Cluster")



# Updated Plot with descriptive names
ggplot(Foodclustered_data, aes(x = Household_income, y = Total_Amt, colour = Cluster_Level)) +
  geom_point(alpha = 0.6) + 
  theme_minimal() + 
  labs(title = "Customer Segments: Income vs Total Spending", 
       subtitle = "Segments defined by K-means clustering",
       x = "Household Income", 
       y = "Total Amount Spent",
       colour = "Segment Name")

## Product Preference
Foodclustered_data %>% 
  group_by(Cluster_Level) %>% 
  summarise(across(c(Wine_Sales, Meat_Sales, Sweet_Sales, Fish_Sales, Fruits_Sales, Gold_Sales,
                     Web_Purchases, In_Store_Purchase, Discounted_Purchase, Catalogue_Purchases), mean))%>%
  pivot_longer(-Cluster_Level, names_to = "Metric", values_to = "Average") %>% 
  ggplot(aes(x=Cluster_Level, y = Average, fill = Metric)) + geom_col(position = "dodge")+
  theme_light() + labs(title = "AVerage Spending Patterns per CLuster")


ggplot(Foodclustered_data, aes(x = Household_income, fill = Cluster_Level)) +
  geom_histogram(bins = 30, alpha = 0.7, color = "white") +
  facet_wrap(~Cluster_Level) +
  theme_minimal() +
  labs(title = "Income Distribution per Segment",
       x = "Household Income",
       y = "Number of Customers") +
  scale_fill_brewer(palette = "Set1")

ggplot(Foodclustered_data, aes(x = Cust_Age, fill = Cluster_Level)) +
  geom_histogram(bins = 30, alpha = 0.7, color = "white") +
  facet_wrap(~Cluster_Level) +
  theme_minimal() +
  labs(title = "Income Distribution per Segment",
       x = "Household Income",
       y = "Number of Customers") +
  scale_fill_brewer(palette = "Set1")



Food_Final_Export <- Foodclustered_data %>%
  mutate(Age_Group = case_when(
    Cust_Age>=24 & Cust_Age<=40  ~ "Young Adult",
    Cust_Age>=41 & Cust_Age<=53  ~ "Middle Aged",
    Cust_Age>=54 & Cust_Age<=60 ~ "Late Career",
    Cust_Age>=61 & Cust_Age<=73 ~"Retirement" ,
    Cust_Age >=74 & Cust_Age<=80 ~ "Seniors",
    TRUE           ~ "Unknown"
  ))
> View(Food_Final_Export)

sum(Foodclustered_data$In_Store_Purchase)
sum(Foodclustered_data$Catalogue_Purchases)
sum(Foodclustered_data$Web_Purchases)
sum(Foodclustered_data$Discounted_Purchase)


Age_Segment<-Food_Final_Export %>% 
  group_by(Age_Group) %>% 
  arrange(Total_Amt) %>% 
  summarise(Total_Spent = sum(Total_Amt), Total_Transaction = n(), Avg_Amount = mean(Total_Amt), Avg_Income = mean(Household_income), total_income = sum(Household_income))

Cluster_Level<-Foodclustered_data %>% 
  group_by(Cluster_Level) %>% 
  summarise(Total_Transaction = n())

Cluster_Level<-Foodclustered_data %>% 
  group_by(Cluster_Level) %>% 
  summarise(Total_Transaction = n())


Food_Data_Clusterlevel<-Foodclustered_data %>% 
  group_by(Cluster_Level) %>% 
  summarise(Total_SPent = sum(Total_Amt), AVg_Spent = mean(Total_Amt), Total_Income = sum(Household_income), AVg_Income = mean(Household_income))
 
Food_Data_Segments<-Foodclustered_data %>% 
  group_by(Wine_Sales, Meat_Sales, Sweet_Sales, Fruits_Sales, Fish_Sales, Gold_Sales) %>% 
  summarise(Total_SPent = sum(Total_Amt), AVg_Spent = mean(Total_Amt), Total_Income = sum(Household_income), AVg_Income = mean(Household_income))

Food_Data_Segments<-Foodclustered_data %>% 
  group_by(Catalogue_Purchases, In_Store_Purchase, Web_Purchases, Discounted_Purchase) %>% 
  summarise(Total_SPent = sum(Catalogue_Purchases), AVg_Spent = mean(Catalogue_Purchases), Total_Income = sum(Household_income), AVg_Income = mean(Household_income))



write.csv(Food_Final_Export, "Food_Final_Data.csv")

write.csv(Food_Analysis, "Food_Analysis_Data.csv")

write.csv(Total_Column, "Overall_Stats.csv")


#By: Kenen Corea
#Purpose: PwC Inventory Analysis Case Study


#Loading packages
library(tidyverse)
library(skimr)
library(janitor)
library(magrittr)


#Setting working directory
setwd("~/Desktop/Data/PwC_Case_Study/CSVs")


########################
### Cleaning & Tests ###
########################


#Loading in data and cleaning/standardizing column names
beg_inv <- read_csv("BegInvDec.csv") %>% clean_names()
end_inv <- read_csv("EndInvDec.csv") %>% clean_names()
pricing_purchases <- read_csv("PricingPurchasesDec.csv") %>% clean_names()
purchases <- read_csv("PurchasesDec.csv") %>% clean_names()
sales <- read_csv("SalesDec.csv") %>% clean_names()
vendor_invoices <- read_csv("VendorInvoicesDec.csv") %>% clean_names()


#Checking how many stores are in each table
n_unique(beg_inv$store) #79
n_unique(end_inv$store) #80
n_unique(purchases$store) #80
n_unique(sales$store) #80
#The beginning inventory table is missing one store (#81 - PEMBROKE), most likely
#a new store that got acquired sometime within the year


#Checking for duplicates in each table
sum(duplicated(beg_inv)) #0
sum(duplicated(end_inv)) #0
sum(duplicated(pricing_purchases)) #0
sum(duplicated(purchases)) #0
sum(duplicated(sales)) #0
sum(duplicated(vendor_invoices)) #0
#No duplicates


####################################################
### Transformation, creating columns & subtables ###
####################################################


#Creating a new column that calculates the gross profit of each product
pricing_purchases %<>% mutate(profit_per_unit = (price - purchase_price))


#Creating a new sub-table that only contains relevant columns to merge to the sales table
units_profit <- pricing_purchases %>% select(brand, description, size, profit_per_unit)


#Merging this new table to the sales table
sales %<>% left_join(units_profit, by = c("brand", "description", "size"))


#Creating a new column that calculates the total profit per sale
sales %<>% mutate(profit = (profit_per_unit * sales_quantity))


#Creating a sub-table that lists the top 10 most profitable units across all stores
highest_profit_items <- sales %>% 
                        group_by(brand, description) %>% 
                        summarize(profits = sum(profit)) %>% 
                        arrange(desc(profits)) %>% 
                        head(10)


#Creating a sub-table that lists the top 10 units in sales
highest_sales_items <- sales %>%
                       group_by(brand, description) %>%
                       summarize(gross_sales = sum(sales_dollars)) %>%
                       arrange(desc(gross_sales)) %>%
                       head(10)


#Creating a sub-table that lists the top 10 vendors by sales
top_ten_vendors_sales <- purchases %>%
                         group_by(vendor_number, vendor_name) %>%
                         summarize(total_payments = sum(dollars)) %>%
                         arrange(desc(total_payments)) %>%
                         head(10)


#Creating a sub-table that lists the top 10 vendors by units purchased
top_ten_vendors_units <- purchases %>%
                         group_by(vendor_number, vendor_name) %>%
                         summarize(total_units_purchased = sum(quantity)) %>%
                         arrange(desc(total_units_purchased)) %>%
                         head(10)


#Creating a summary table that groups the total units sold by each month, the total orders per month,
#and then merging both into one table
sales_per_month <- sales %>% mutate(month = month(sales_date)) %>%
                             group_by(month) %>%
                             summarize(quantity_sold = sum(sales_quantity))

orders_per_month <- purchases %>% mutate(month = month(receiving_date)) %>% 
                                  group_by(month) %>% 
                                  summarize(quantity_ordered = sum(quantity))

sales_and_orders_per_month <- orders_per_month %>% left_join(sales_per_month, by = c("month"))


###########################################
### Exporting data for Tableau analysis ###
###########################################

write_csv(highest_profit_items, "~/Desktop/Data/PwC_Case_Study/highest_profit_items.csv")
write_csv(highest_sales_items, "~/Desktop/Data/PwC_Case_Study/highest_sales_items.csv")
write_csv(top_ten_vendors_sales, "~/Desktop/Data/PwC_Case_Study/top_ten_vendors_sales.csv")
write_csv(top_ten_vendors_units, "~/Desktop/Data/PwC_Case_Study/top_ten_vendors_units.csv")
write_csv(sales_and_orders_per_month, "~/Desktop/Data/PwC_Case_Study/sales_and_orders_per_month.csv")









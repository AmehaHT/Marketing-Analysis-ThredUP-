library(expss)
library(tidyverse)
library(dplyr)
library(ggplot2)

#----------------------------------- Exploratory Data Analysis (EDA)------------------------- 

#Importing the dataset 
thred <- thredUP_2023_Marketing_Analyst_Intern_Project_Ameha_Teshome_raw_data
str(thred)

#Summary of the dataset
summary(thred)

#Changing the name of the columns
new_names <- c("Spend_Date","Total_Spend", "Acquisition_Spend", "Total_Sessions", "Acquisition_Sessions", "1d_Gross_Rev", "1d_Orders", "1d_Count_Items", "1d_discount", "1d_buyers", "7d_Gross_Rev", "7d_Orders", "7d_Count_Items", "7d_discount", "7d_buyers", "30d_Gross_Rev", "30d_Orders", "30d_Count_Items", "30d_discount", "30d_buyers")
thred <- thred %>%
  rename_at(vars("Spend Date","Total Spend", "Acquisition Spend", "Total Sessions", "Acquisition Sessions", "1d Gross Rev", "1d Orders", "1d Count of Items", "1d discount $", "1d buyers", "7d Gross Rev", "7d Orders", "7d Count of Items", "7d discount $", "7d buyers", "30d Gross Rev", "30d Orders", "30d Count of Items", "30d discount $", "30d buyers"), ~ new_names)

#remove all the dollar ($) signs from the whole dataset 
thred[] <- lapply(thred, function(x) gsub("\\$", "", x))

# Removing duplicates: Remove duplicate rows
thred <- unique(thred)

thred <- na.omit(thred)

thred <- thred %>% mutate_all(~gsub(",", "", .))

#--------------------- ROAS 1D, 7D, 30D Calculations ---------------------

thred$`1d_Gross_Rev` <- as.numeric(thred$`1d_Gross_Rev`)
thred$`7d_Gross_Rev` <- as.numeric(thred$`7d_Gross_Rev`)
thred$`30d_Gross_Rev` <- as.numeric(thred$`30d_Gross_Rev`)
thred$Acquisition_Spend <- as.numeric(thred$Acquisition_Spend)

#ROAS_1D
roas_1d <- thred$`1d_Gross_Rev` / thred$Acquisition_Spend 
roas_1d <- roas_1d[!is.na(roas_1d)]
roas_1d1 <- mean(roas_1d)
roas_1d1
# ROAS 1D is 45%

#ROAS_7D
roas_7d <- thred$`7d_Gross_Rev` / thred$Acquisition_Spend 
roas_7d <- roas_7d[!is.na(roas_7d)]
roas_7d1 <- mean(roas_7d)
roas_7d1
# ROAS 7D is 78%

#ROAS_30D
roas_30d <- thred$`30d_Gross_Rev` / thred$Acquisition_Spend 
roas_30d <- roas_30d[!is.na(roas_30d)]
roas_30d_finite <- mean(roas_30d[is.finite(roas_30d)])
roas_30d_finite
# ROAS 30D is 118%

# Convert columns to numeric
thred$`1d_Gross_Rev` <- as.numeric(thred$`1d_Gross_Rev`)
thred$`7d_Gross_Rev` <- as.numeric(thred$`7d_Gross_Rev`)
thred$`30d_Gross_Rev` <- as.numeric(thred$`30d_Gross_Rev`)
thred$Acquisition_Spend <- as.numeric(thred$Acquisition_Spend)

# Group by Channel and compute ROAS for each group
roas_by_channel <- thred %>%
  group_by(Channel) %>%
  summarise(
    roas_1d = mean(`1d_Gross_Rev` / Acquisition_Spend * 100, na.rm = TRUE),
    roas_7d = mean(`7d_Gross_Rev` / Acquisition_Spend * 100, na.rm = TRUE),
    roas_30d = mean(`30d_Gross_Rev` / Acquisition_Spend * 100, na.rm = TRUE)
  )
roas_by_channel <- roas_by_channel %>%
  drop_na()
# View the results
roas_by_channel

#---------------------- Calculates CR, CPV, CAC, GRND, CPA, CPS --------------------------------

# Remove commas and dollar signs from the monetary columns

thred <- thred %>%
 mutate_at(vars(`Total_Spend`, `Acquisition_Spend`, `1d_Orders`, `1d_discount`, 
                `1d_Gross_Rev`, `7d_Gross_Rev`, `30d_Gross_Rev`), 
            function(x) as.numeric(gsub("[^0-9.]", "", x))) %>%
  mutate(`Total_Sessions` = as.numeric(`Total_Sessions`),
       `Acquisition_Sessions` = as.numeric(gsub("[^0-9.]", "", `Acquisition_Sessions`)))


# Calculate cost per acquisition (CPA) for each channel
thred$CPA <- thred$Acquisition_Spend / thred$Acquisition_Sessions * 100
# Calculate cost per session (CPS) for each channel
thred$CPS <- thred$Total_Spend / thred$Total_Sessions * 100
# Calculate CR
thred$CR <- thred$`1d_Orders` / thred$`1d_Gross_Rev` * 100
# Calculate CPV
thred$CPV <- thred$`Total_Spend` / thred$`Total_Sessions`
# Calculate CAC
thred$CAC <- thred$`Acquisition_Spend` / thred$`Acquisition_Sessions`
# Calculate GRND
thred$GRND <- thred$`1d_Gross_Rev` - thred$`1d_discount`
# Display the results
print(thred[, c("Channel", "CR", "CPV", "CAC", "GRND", "CPA", "CPS")], n = 338)



# Calculate mean CR by channel, excluding NaN values
mean_CR <- aggregate(CR ~ Channel, data = thred, FUN = function(x) mean(x, na.rm = TRUE))
# Calculate mean CPV by channel, excluding NaN values
mean_CPV <- aggregate(CPV ~ Channel, data = thred, FUN = function(x) mean(x, na.rm = TRUE))
# Calculate mean CAC by channel, excluding NaN values
mean_CAC <- aggregate(CAC ~ Channel, data = thred, FUN = function(x) mean(x, na.rm = TRUE))
# Calculate mean GRND by channel, excluding NaN values
mean_GRND <- aggregate(GRND ~ Channel, data = thred, FUN = function(x) mean(x, na.rm = TRUE))

mean_CPA <- aggregate(CPA ~ Channel, data = thred, FUN = function(x) mean(x, na.rm = TRUE))

mean_CPS <- aggregate(CPS ~ Channel, data = thred, FUN = function(x) mean(x, na.rm = TRUE))


# Combine the results into a single dataframe
mean_by_channel <- merge(mean_CR, mean_CPV, by = "Channel")
mean_by_channel <- merge(mean_by_channel, mean_CAC, by = "Channel")
mean_by_channel <- merge(mean_by_channel, mean_GRND, by = "Channel")
mean_by_channel <- merge(mean_by_channel, mean_CPA, by = "Channel")
mean_by_channel <- merge(mean_by_channel, mean_CPS, by = "Channel")

# Display the results
print(mean_by_channel)

#--------------------------------- Plots to gain more insight ------------------------

#Plot 1: Acquisition Sessions by Channel

# Group the data by Channel and Date
sessions_by_channel <- thred %>% 
  group_by(Channel, Spend_Date, .drop = TRUE) %>% 
  summarize(Total_Sessions = sum(Total_Sessions),
            Acquisition_Sessions = sum(Acquisition_Sessions), 
            .groups = "drop")

ggplot(sessions_by_channel, aes(x = Channel, y = Acquisition_Sessions, fill = Channel)) +
  geom_col() +
  geom_text(aes(label = Acquisition_Sessions), position = position_stack(vjust = 0.5)) +  # Add this line
  ggtitle("Acquisition Sessions by Channel") +
  xlab("Channel") +
  ylab("Acquisition Sessions") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#---------------------------------------

#Plot 2: Revenue by Channel and Time Period

# Calculate revenue for each channel for 1 day, 7 days, and 30 days
thred$Spend_Date <- as.Date(thred$Spend_Date, format = "%Y-%m-%d")

revenue_by_channel <- thred %>%
  group_by(Channel) %>%
  summarize(Revenue_1_day = sum(`1d_Gross_Rev`, na.rm = TRUE, 
                                subset = as.Date(Spend_Date) >= max(as.Date(Spend_Date)) - 1),
            Revenue_7_days = sum(`7d_Gross_Rev`, na.rm = TRUE, 
                                 subset = as.Date(Spend_Date) >= (max(as.Date(Spend_Date)) - 7)),
            Revenue_30_days = sum(`30d_Gross_Rev`, na.rm = TRUE, 
                                  subset = as.Date(Spend_Date) >= (max(as.Date(Spend_Date)) - 30)))

# Melt data to create stacked bar chart
revenue_by_channel_melt <- melt(revenue_by_channel, id.vars = "Channel", 
                                variable.name = "Time_Period", 
                                value.name = "Revenue")

# Create stacked bar chart
ggplot(revenue_by_channel_melt, aes(x = Channel, y = Revenue, fill = Time_Period)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Revenue by Channel and Time Period", x = "Channel", y = "Revenue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------

#Plot 3:Total Discount Percentage by Channel and Discount Type

discount_data <- thred %>%
  select(Channel, `1d_discount`, `7d_discount`, `30d_discount`) %>%
  mutate(`7d_discount` = as.numeric(`7d_discount`),
         `30d_discount` = as.numeric(`30d_discount`)) %>%
  pivot_longer(cols = `1d_discount`:`30d_discount`, names_to = "discount_type", values_to = "discount_value") %>%
  drop_na()

discount_data <- discount_data %>%
  group_by(Channel, discount_type) %>%
  summarize(total_discount = sum(discount_value), .groups = "drop") %>%
  ungroup()


ggplot(discount_data, aes(x = Channel, y = total_discount, fill = discount_type)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(title = "Total Discount Percentage by Channel and Discount Type", x = "Channel", y = "Total Discount (%)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------------------------------------------------------

#Plot 4: Channel Performance by Revenue per Session


# Group data by Channel and calculate total spend, orders, revenue, and sessions
channel_performance <- thred %>%
  group_by(Channel) %>%
  summarize(total_spend = sum(Total_Spend),
            total_orders = sum(`1d_Orders`),
            total_revenue = sum(`1d_Gross_Rev`),
            total_sessions = sum(Total_Sessions))

# Calculate metrics per session for each channel
channel_performance <- channel_performance %>%
  mutate(avg_order_value = total_revenue / total_orders,
         conversion_rate = total_orders / total_sessions,
         revenue_per_session = total_revenue / total_sessions,
         spend_per_session = total_spend / total_sessions)

# Rank channels by revenue per session
channel_performance <- channel_performance %>%
  mutate(rank = rank(-revenue_per_session))

# Visualize channel performance by revenue per session
ggplot(channel_performance, aes(x = reorder(Channel, revenue_per_session), y = revenue_per_session)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  geom_text(aes(label = round(revenue_per_session, 2)), vjust = -0.5, size = 3, color = "black") + # Add this line
  labs(title = "Channel Performance by Revenue per Session",
       x = "Channel",
       y = "Revenue per Session ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------------------------------------

#Plot 5: Total Spend by Channel
thred_summary <- thred %>% 
  group_by(Channel) %>% 
  summarise(Total_Spend = sum(Total_Spend))

ggplot(thred, aes(x = reorder(Channel, desc(Total_Spend)), y = Total_Spend, fill = Channel)) +
  geom_col()  +
  geom_text(data = thred_summary, aes(label = Total_Spend, y = Total_Spend + max(Total_Spend) * 0.05), 
            size = 5, color = "black") + # Add this line
  labs(title = "Total Spend by Channel",
       x = "Channel", y = "Total Spend ($)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

# Install required packages (run only once)
install.packages("tidyverse")   # includes readxl, dplyr, ggplot2, etc.
install.packages("janitor")     # for clean_names()
install.packages("lubridate")   # for date handling

# Load the libraries
library(tidyverse)    # main tidyverse tools (dplyr, ggplot2, readr/readxl, etc.)
library(janitor)      # for clean_names()
library(lubridate)    # for working with dates

# Read the Excel file into R
# Make sure the file is in your project folder
retail_raw <- readxl::read_excel("Online Retail.xlsx")

# Quick exploration of the data
dim(retail_raw)          # shows number of rows and columns
glimpse(retail_raw)      # shows column names, types, and a preview of values
head(retail_raw, 10)     # shows first 10 rows
summary(retail_raw)      # shows basic statistics for each column

# Clean column names to make them easier to work with (snake_case)
retail_clean <- retail_raw %>%
  clean_names()

# Check the cleaned data
glimpse(retail_clean)

# Further cleaning: remove cancellations, negative/zero values, missing customer_id
retail_clean <- retail_clean %>%
  filter(
    !str_starts(invoice_no, "C"),      # Remove cancelled invoices (start with C)
    quantity > 0,                      # Only positive quantities
    unit_price > 0,                    # Only positive prices
    !is.na(customer_id)                # Remove rows with missing customer ID
  ) %>%
  mutate(
    total_price   = quantity * unit_price,              # Add total price per row
    invoice_date  = as.Date(invoice_date),              # Convert to Date only
    year_month    = format(invoice_date, "%Y-%m"),      # e.g., "2010-12"
    month         = month(invoice_date, label = TRUE),  # Jan, Feb, ...
    day_of_week   = wday(invoice_date, label = TRUE)    # Mon, Tue, ...
  )

# Check the cleaned data again
glimpse(retail_clean)
dim(retail_clean)
summary(retail_clean)

# Total Sales, Orders, and Unique Customers
total_revenue <- sum(retail_clean$total_price)
total_orders  <- n_distinct(retail_clean$invoice_no)
total_customers <- n_distinct(retail_clean$customer_id)

cat("Total Revenue: ", format(round(total_revenue, 2), big.mark = ","), " GBP\n")
cat("Total Orders: ", format(total_orders, big.mark = ","), "\n")
cat("Unique Customers: ", format(total_customers, big.mark = ","), "\n\n")

# Top 10 Products by Revenue
top_products <- retail_clean %>%
  group_by(stock_code, description) %>%
  summarise(total_revenue = sum(total_price),
            total_quantity = sum(quantity)) %>%
  arrange(desc(total_revenue)) %>%
  head(10)

print("Top 10 Products by Revenue:")
print(top_products)

# Monthly Sales Trend
monthly_sales <- retail_clean %>%
  group_by(year_month, month) %>%
  summarise(total_revenue = sum(total_price)) %>%
  arrange(year_month)

print("Monthly Sales:")
print(monthly_sales)

# Top 10 Countries by Revenue
sales_by_country <- retail_clean %>%
  group_by(country) %>%
  summarise(total_revenue = sum(total_price)) %>%
  arrange(desc(total_revenue)) %>%
  head(10)

print("Top 10 Countries by Revenue:")
print(sales_by_country)

# RFM Analysis - Top 10 Customers by Monetary Value
rfm <- retail_clean %>%
  group_by(customer_id) %>%
  summarise(
    recency_days = as.numeric(max(invoice_date) - min(invoice_date)),  # Simple recency example (can be improved)
    frequency    = n_distinct(invoice_no),
    monetary     = sum(total_price)
  ) %>%
  arrange(desc(monetary)) %>%
  head(10)

print("Top 10 Customers by Monetary Value (RFM):")
print(rfm)

# Load ggplot2 if not already loaded (it's in tidyverse)
library(ggplot2)
library(scales)  # for better number formatting in plots
library(scales)

# Top 10 Products by Revenue - Bar Chart
ggplot(top_products, aes(x = reorder(description, total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # flip to make product names readable
  labs(title = "Top 10 Products by Revenue",
       subtitle = "Online Retail Dataset (2010-2011)",
       x = "Product Description",
       y = "Total Revenue (GBP)") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Save the plot as PNG
ggsave("top_10_products.png", width = 10, height = 7, dpi = 300)

# Monthly Sales Trend - Line Chart
ggplot(monthly_sales, aes(x = year_month, y = total_revenue, group = 1)) +
  geom_line(color = "#2ca02c", linewidth = 1.2) +
  geom_point(color = "#2ca02c", size = 3) +
  labs(title = "Monthly Sales Trend",
       subtitle = "Clear seasonality visible",
       x = "Year-Month",
       y = "Total Revenue (GBP)") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as PNG
ggsave("monthly_sales_trend.png", width = 10, height = 6, dpi = 300)

# Top 10 Countries by Revenue - Bar Chart
ggplot(sales_by_country, aes(x = reorder(country, total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "#d62728") +
  coord_flip() +
  labs(title = "Top 10 Countries by Revenue",
       x = "Country",
       y = "Total Revenue (GBP)") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# Save the plot as PNG
ggsave("top_10_countries.png", width = 10, height = 7, dpi = 300)

# Sales Distribution by Day of the Week - Bar Chart

# First: calculate total revenue per day of week
sales_by_day <- retail_clean %>%
  group_by(day_of_week) %>%
  summarise(total_revenue = sum(total_price)) %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

# Plot
ggplot(sales_by_day, aes(x = day_of_week, y = total_revenue, fill = day_of_week)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales Distribution by Day of the Week",
       subtitle = "Online Retail Dataset (2010-2011)",
       x = "Day of the Week",
       y = "Total Revenue (GBP)") +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set2") +   # ألوان حلوة ومتناسقة
  theme_minimal() +
  theme(legend.position = "none",          # مش محتاجين legend هنا
        axis.text.x = element_text(size = 12))

# Save the plot as PNG
ggsave("sales_by_day_of_week.png", width = 9, height = 6, dpi = 300)

# Scatter Plot: Frequency vs Monetary
ggplot(rfm, aes(x = frequency, y = monetary)) +
  geom_point(alpha = 0.6, color = "#1f77b4", size = 3) +   # نقاط شفافة شوية عشان ما تتراكمش
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +  # خط اتجاه خطي (اختياري)
  labs(
    title = "RFM Analysis: Frequency vs Monetary Value",
    subtitle = "Relationship between number of purchases and total spending",
    x = "Frequency (Number of Orders)",
    y = "Monetary Value (Total Spending in GBP)"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(size = 12)
  )

# Save the plot as PNG
ggsave("rfm_frequency_vs_monetary.png", width = 9, height = 6, dpi = 300)

# Create a Dashboard 
install.packages("flexdashboard")
library(flexdashboard)


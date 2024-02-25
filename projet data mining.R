install.packages(c("tidyverse", "lubridate", "ggthemes", "leaflet", "scales", "gridExtra", 
                   "dplyr", "ggplot2", "corrplot", "lavaan", "FactoMineR", "tidyr", 
                   "stringr", "factoextra", "vcd", "gbm", "visdat", "UpSetR", "mice", "Amelia"))


library(tidyverse)
library(lubridate)
library(ggthemes)
library(leaflet)
library(scales)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(corrplot)
library(lavaan)
library(FactoMineR)
library(tidyr)
library(stringr)
library(FactoMineR)
library(factoextra)
library(vcd)
library(gbm)
library(visdat)
library(UpSetR)
library(mice)
library(Amelia)

#telechargement de données :
data <- read.csv("/Users/Radja/Downloads/vehicles_comp.csv.gz", header = TRUE, sep = ",")
data <- data.frame(data)
dim(data)

#visualisation de données :
View(data)
colnames(data)
summary(data)

################## PRICE
price <- data [5:5]

# Count the number of values equal to 0 or greater than 250000 in the "price" variable
num_zeros <- sum(data$price == 0)
num_greater <- sum(data$price > 250000)

# Print the results
cat("Number of values equal to 0:", num_zeros, "\n")
cat("Number of values greater than 250000:", num_greater, "\n")


# Create box plot for price with outliers shown
bp <- boxplot(price$price, main = "Box plot of Price", xlab = "Price", outline = TRUE)

# Count the number of values equal to 0 or greater than 250000 in the "price" variable
num_zeros <- sum(data$price == 0)
num_greater <- sum(data$price > 250000)

# Print the results
cat("Number of values equal to 0:", num_zeros, "\n")
cat("Number of values greater than 250000:", num_greater, "\n")
# Subset the dataset to keep only rows with price over 100000
priceunder100k <- subset(price, price < 100000)
View(priceunder100k)
summary(priceunder100k)

# Count the number of observations
n_obs <- nrow(priceunder100k)

# Print the number of observations
cat("Number of observations:", n_obs)

# Create a vector of colors using the rainbow function
num_bins <- 20
hist_colors <- rainbow(num_bins)

# Set the size of the axis labels and turn off scientific notation
par(cex.axis = 0.8)
options(scipen = 999)

# Define the breaks for the histogram
breaks <- seq(min(priceunder100k$price), max(priceunder100k$price), length.out = num_bins + 1)

# Plot a histogram of the price data
hist(priceunder100k$price, breaks = breaks, col = hist_colors,
     main = "Distribution of Prices", xlab = "Price")
library(dplyr)
install.packages('highcharter')
library(highcharter)
priceunder100k %>% 
  ggplot(aes(price)) + 
  stat_ecdf(geom="step", color="blue") +
  scale_x_continuous(breaks = seq(0,100000,10000)) + 
  scale_y_continuous(breaks = c(0,.25,.5,.75,1), labels = c("0", "25", "50", "75", "100")) + 
  labs(x = "Price", y = "Percentage of Vehicles under Price")


######### Manufacturer 

# Count vehicules per manufacturer 
vehicle_counts <-data %>%
  filter(!is.na(manufacturer), manufacturer != '') %>%
  group_by(manufacturer) %>%
  summarize(vehicle_count = n())
View(vehicle_counts)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(vehicle_counts, aes(x = reorder(manufacturer, -vehicle_count), y = vehicle_count, fill = vehicle_count)) +
  geom_bar(stat = "identity") +
  labs(x = "Manufacturer", y = "Vehicle Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_gradientn(colors = hist_colors)


######### PRICE-Manufacturer

price_manufacturer <- data[, c(5, 7)]
View(price_manufacturer)

price_manufacturer_under250k <- subset(price_manufacturer, price < 250000)

mean_price_by_manufacturer <- price_manufacturer_under250k %>%
  group_by(manufacturer) %>%
  summarize(mean_price = mean(price))

View(mean_price_by_manufacturer)

# Plot a bar chart of mean price by manufacturer
num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(mean_price_by_manufacturer, aes(x = reorder(manufacturer, -mean_price), y = mean_price)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Mean Price by Manufacturer", x = "Manufacturer", y = "Mean Price") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
  panel.background = element_rect(fill = "white", color = NA) +
  scale_fill_gradientn(colors = hist_colors))

# Set the margin size to a smaller value
par(mar = c(2, 3, 2, 1) + 0.1)

# Create a box plot for the "price" variable in the "data" dataset
boxplot(data$price, main = "Box Plot of Price", xlab = "Price", col = "lightblue", border = "darkblue")

# Subset the dataset to keep only rows with price over 250000
priceunder250k <- subset(price, price < 250000)

# Create a box plot for the "price" variable in the "data" dataset
boxplot(priceunder250k$price, main = "Box Plot of Prices under 250k$", xlab = "Price", col = "lightblue", border = "darkblue")


######### YEAR

# Create box plot for price with outliers shown
bp <- boxplot(data$year, main = "Box plot of year", xlab = "year", outline = TRUE)

# Count vehicules per year 
vehicle_counts2 <-data %>%
  filter(!is.na(year), year != '') %>%
  group_by(year) %>%
  summarize(vehicle_count = n())
View(vehicle_counts2)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(vehicle_counts2, aes(x = vehicle_count , y =reorder(year, desc(vehicle_count)), fill = cut(vehicle_count, breaks = num_bins))) +
  geom_bar(stat = "identity") +
  labs(x = "Vehicle Count", y = "Year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=0.5), 
        axis.text.y = element_text(size = 4),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = hist_colors) +
  guides(fill = FALSE)


######### PRICE-year 
price_year <- data[, c(6, 5)]
View(price_year)

price_year_under250k <- subset(price_year, price < 250000)

mean_price_by_year <- price_year_under250k %>%
  group_by(year) %>%
  summarize(mean_price = mean(price))

mean_price_by_year <- na.omit(mean_price_by_year)

# create a new column with the year intervals
mean_price_by_year$year_intervals <- cut(mean_price_by_year$year, breaks = seq(min(mean_price_by_year$year), max(mean_price_by_year$year) + 10, by = 10), include.lowest = TRUE)

# summarize the mean price by year intervals
mean_price_by_year_intervals <- mean_price_by_year %>%
  group_by(year_intervals) %>%
  summarize(mean_price = mean(mean_price))

View(mean_price_by_year_intervals)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(mean_price_by_year_intervals, aes(x = reorder(year_intervals, -mean_price), y = mean_price, fill = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by Year Intervals", x = "Year Intervals", y = "Mean Price") +
  scale_fill_gradientn(colors = hist_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA))

######### State

# Count vehicules per state 
vehicle_counts4 <-data %>%
  filter(!is.na(state), state != '') %>%
  group_by(state) %>%
  summarize(vehicle_count = n())
View(vehicle_counts4)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(vehicle_counts4, aes(x = vehicle_count , y =reorder(state, desc(vehicle_count)), fill = cut(vehicle_count, breaks = num_bins))) +
  geom_bar(stat = "identity") +
  labs(x = "Vehicle Count", y = "State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=0.5), 
        axis.text.y = element_text(size = 4),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = hist_colors) +
  guides(fill = FALSE)


######### PRICE-state 

price_state <- data[, c(23, 5)]
View(price_state)

price_state_under250k <- subset(price_state, price < 250000)

mean_price_by_state <- price_state_under250k %>%
  group_by(state) %>%
  summarize(mean_price = mean(price))

View(mean_price_by_state)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(mean_price_by_state, aes(x = reorder(state, -mean_price), y = mean_price, fill = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by State", x = "state", y = "Mean Price") +
  scale_fill_gradientn(colors = hist_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA))


######### FUEL

vehicle_counts5 <-data %>%
  filter(!is.na(fuel), fuel != '') %>%
  group_by(fuel) %>%
  summarize(vehicle_count = n())
View(vehicle_counts5)


library(ggplot2)

ggplot(vehicle_counts5, aes(x = "", y = vehicle_count, fill = fuel)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des carburants", fill = "Carburant", x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = "right")


######### PRICE-FUEL
price_fuel <- data[, c(11, 5)]
View(price_fuel)

price_fuel_under250k <- subset(price_fuel, price < 250000)

mean_price_by_fuel <- price_fuel_under250k %>%
  group_by(fuel) %>%
  summarize(mean_price = mean(price))

View(mean_price_by_fuel)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(mean_price_by_fuel, aes(x = reorder(fuel, -mean_price), y = mean_price, fill = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by Fuel", x = "fuel", y = "Mean Price") +
  scale_fill_gradientn(colors = hist_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA))

gc()
######### CYLINDER

vehicle_counts5 <-data %>%
  filter(!is.na(cylinders), cylinders != '', cylinders != 'Other') %>% 
  group_by(cylinders) %>%
  summarize(vehicle_count = n())
View(vehicle_counts5)


num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(vehicle_counts5, aes(x = vehicle_count , y =reorder(cylinders, desc(vehicle_count)), fill = cut(vehicle_count, breaks = num_bins))) +
  geom_bar(stat = "identity") +
  labs(x = "Vehicle Count", y = "cylinders") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=0.5), 
        axis.text.y = element_text(size = 4),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = hist_colors) +
  guides(fill = FALSE)


######### PRICE-cylinders
price_cylinders <- data[, c(10, 5)]
View(price_cylinders)

price_cylinders_under250k <- subset(price_cylinders, price < 250000)

mean_price_by_cylinders <- price_cylinders_under250k %>%
  group_by(cylinders) %>%
  summarize(mean_price = mean(price))
mean_price_by_cylinders <- mean_price_by_cylinders %>%
  filter(cylinders != "")
View(mean_price_by_cylinders)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(mean_price_by_cylinders, aes(x = reorder(cylinders, -mean_price), y = mean_price, fill = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by cylinders", x = "cylinders", y = "Mean Price") +
  scale_fill_gradientn(colors = hist_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA))

######### Odometer 

vehicle_counts5 <-data %>%
  filter(!is.na(odometer), odometer != '') %>%
  group_by(odometer) %>%
  summarize(vehicle_count = n())
View(vehicle_counts5)


# Create intervals for odometer values
vehicle_counts5 <- data %>%
  filter(!is.na(odometer), odometer != '') %>%
  mutate(odometer_interval = cut(odometer, breaks = c(0, 99999, 249999, 499999, Inf), 
                                 labels = c("0-99999", "100000-249999", "250000-499999", ">500000"),
                                 include.lowest = TRUE)) %>%
  group_by(odometer_interval) %>%
  summarize(vehicle_count = n())

# Create barplot of vehicle counts by odometer interval
ggplot(vehicle_counts5, aes(x = odometer_interval, y = vehicle_count, fill = odometer_interval)) +
  geom_bar(stat = "identity") +
  labs(x = "Odometer Interval", y = "Vehicle Count", title = "Vehicle Count by Odometer Interval")

# Create a circular plot of vehicle counts by odometer interval
ggplot(vehicle_counts5, aes(x = "", y = vehicle_count, fill = odometer_interval)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, title = "Vehicle Count by Odometer Interval") +
  scale_fill_hue(name = "Odometer Interval", labels = c("0-99999", "100000-249999", "250000-499999", ">500000")) +
  theme_void()

######### PRICE-odometer
price_odometer <- data[, c(12, 5)]
View(price_odometer)

price_odometer_under250k <- subset(price_odometer, price < 250000)


# Create intervals for odometer readings
odometer_intervals <- cut(price_odometer_under250k$odometer, 
                          breaks = c(0, 99999, 249999, 499999, Inf), 
                          labels = c("0-99999", "100000-249999", "250000-499999", ">500000"), 
                          include.lowest = TRUE)


# Add odometer intervals to the data frame
price_odometer_under250k <- cbind(price_odometer_under250k, odometer_intervals)
View(price_odometer_under250k)


# Group by odometer intervals and calculate mean price
mean_price_by_odometer <- price_odometer_under250k %>%
  group_by(odometer_intervals) %>%
  summarize(mean_price = mean(price))

View(mean_price_by_odometer)

mean_price_by_odometer <- na.omit(mean_price_by_odometer)

num_bins <- 20
hist_colors <- rainbow(num_bins)


ggplot(mean_price_by_odometer, aes(x = reorder(odometer_intervals, -mean_price), y = mean_price, fill = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by odometer_intervals", x = "odometer_intervals", y = "Mean Price") +
  scale_fill_gradientn(colors = hist_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA))


######### TITLE_STATUS

vehicle_counts5 <-data %>%
  filter(!is.na(title_status), title_status != '') %>%
  group_by(title_status) %>%
  summarize(vehicle_count = n())
View(vehicle_counts5)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(vehicle_counts5, aes(x = vehicle_count , y =reorder(title_status, desc(vehicle_count)), fill = cut(vehicle_count, breaks = num_bins))) +
  geom_bar(stat = "identity") +
  labs(x = "Vehicle Count", y = "title_status") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=0.5), 
        axis.text.y = element_text(size = 4),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = hist_colors) +
  guides(fill = FALSE)


######### PRICE-title_status
price_title_status<- data[, c(13, 5)]
View(price_title_status)

price_title_status_under250k <- subset(price_title_status, price < 250000)

mean_price_by_title_status <- price_title_status_under250k %>%
  group_by(title_status) %>%
  summarize(mean_price = mean(price))
mean_price_by_title_status<- mean_price_by_title_status %>%
  filter(title_status != "")
View(mean_price_by_title_status)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(mean_price_by_title_status, aes(x = reorder(title_status, -mean_price), y = mean_price, fill = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by title_status", x = "title_status", y = "Mean Price") +
  scale_fill_gradientn(colors = hist_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA))


######### PAINT_COLOR

vehicle_counts5 <-data %>%
  filter(!is.na(paint_color), paint_color != '') %>%
  group_by(paint_color) %>%
  summarize(vehicle_count = n())
View(vehicle_counts5)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(vehicle_counts5, aes(x = vehicle_count , y =reorder(paint_color, desc(vehicle_count)), fill = cut(vehicle_count, breaks = num_bins))) +
  geom_bar(stat = "identity") +
  labs(x = "Vehicle Count", y = "paint_color") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=0.5), 
        axis.text.y = element_text(size = 4),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = hist_colors) +
  guides(fill = FALSE)


######### PRICE-PAINT_COLOR
price_paint_color<- data[, c(19, 5)]
View(price_paint_color)

price_paint_color_under250k <- subset(price_paint_color, price < 250000)

mean_price_by_paint_color <- price_paint_color_under250k %>%
  group_by(paint_color) %>%
  summarize(mean_price = mean(price))
mean_price_by_paint_color<- mean_price_by_paint_color %>%
  filter(paint_color != "")
View(mean_price_by_paint_color)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(mean_price_by_paint_color, aes(x = reorder(paint_color, -mean_price), y = mean_price, fill = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by paint_color", x = "paint_color", y = "Mean Price") +
  scale_fill_gradientn(colors = hist_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA))


######### TYPE

vehicle_counts5 <-data %>%
  filter(!is.na(type), type != '') %>%
  group_by(type) %>%
  summarize(vehicle_count = n())
View(vehicle_counts5)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(vehicle_counts5, aes(x = vehicle_count , y =reorder(type, desc(vehicle_count)), fill = cut(vehicle_count, breaks = num_bins))) +
  geom_bar(stat = "identity") +
  labs(x = "Vehicle Count", y = "type") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=0.5), 
        axis.text.y = element_text(size = 4),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = hist_colors) +
  guides(fill = FALSE)


######### PRICE-TYPE
price_type<- data[, c(18, 5)]
View(price_type)

price_type_under250k <- subset(price_type, price < 250000)

mean_price_by_type <- price_type_under250k %>%
  group_by(type) %>%
  summarize(mean_price = mean(price))

mean_price_by_type <- mean_price_by_type %>%
  filter(type != "")

View(mean_price_by_type)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(mean_price_by_type, aes(x = reorder(type, -mean_price), y = mean_price, fill = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by Type", x = "type", y = "Mean Price") +
  scale_fill_gradientn(colors = hist_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA))


######### CONDITION

vehicle_counts5 <-data %>%
  filter(!is.na(condition), type != '') %>%
  group_by(condition) %>%
  summarize(vehicle_count = n())
vehicle_counts5 <- vehicle_counts5 %>%
  filter(condition != "")
View(vehicle_counts5)


num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(vehicle_counts5, aes(x = vehicle_count , y =reorder(condition, desc(vehicle_count)), fill = cut(vehicle_count, breaks = num_bins))) +
  geom_bar(stat = "identity") +
  labs(x = "Vehicle Count", y = "condition") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=0.5), 
        axis.text.y = element_text(size = 8),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = hist_colors) +
  guides(fill = FALSE)


######### PRICE-CONDITION
price_condition<- data[, c(9, 5)]
View(price_condition)

price_condition_under250k <- subset(price_condition, price < 250000)

mean_price_by_condition <- price_condition_under250k %>%
  group_by(condition) %>%
  summarize(mean_price = mean(price))

mean_price_by_condition <- mean_price_by_condition %>%
  filter(condition != "")

View(mean_price_by_condition)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(mean_price_by_condition, aes(x = reorder(condition, -mean_price), y = mean_price, fill = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by condition", x = "condition", y = "Mean Price") +
  scale_fill_gradientn(colors = hist_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA))



######### date posted 

vehicle_counts5 <-data %>%
  filter(!is.na(posting_date), type != '') %>%
  group_by(posting_date) %>%
  summarize(vehicle_count = n())
vehicle_counts5 <- vehicle_counts5 %>%
  filter(posting_date != "")
View(vehicle_counts5)
# Extract the date portion of the "fish" column into a new column "date"
vehicle_counts5 <- vehicle_counts5 %>% mutate(date = as.Date(substr(posting_date, 1, 10)))
vehicle_counts5[ , c('posting_date')] <- list(NULL)
# Print the modified data frame
View(vehicle_counts5)

vehicle_counts_daily <- vehicle_counts5 %>%
  group_by(month_day = paste(lubridate::month(date, label = TRUE), lubridate::day(date), sep = "-")) %>%
  summarize(total_vehicle_count = sum(vehicle_count))

View(vehicle_counts_daily)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(vehicle_counts_daily, aes(x = total_vehicle_count , y =reorder(month_day, desc(total_vehicle_count)), fill = cut(total_vehicle_count, breaks = num_bins))) +
  geom_bar(stat = "identity") +
  labs(x = "total vehicle count", y = "month_day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=0.5), 
        axis.text.y = element_text(size = 8),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = hist_colors) +
  guides(fill = FALSE)


######### PRICE-CONDITION
price_condition<- data[, c(9, 5)]
View(price_condition)

price_condition_under250k <- subset(price_condition, price < 250000)

mean_price_by_condition <- price_condition_under250k %>%
  group_by(condition) %>%
  summarize(mean_price = mean(price))

mean_price_by_condition <- mean_price_by_condition %>%
  filter(condition != "")

View(mean_price_by_condition)

num_bins <- 20
hist_colors <- rainbow(num_bins)

ggplot(mean_price_by_condition, aes(x = reorder(condition, -mean_price), y = mean_price, fill = mean_price)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by condition", x = "condition", y = "Mean Price") +
  scale_fill_gradientn(colors = hist_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA))

##################   correlation matrix 

# Select columns that are numeric only
numeric_cols <- sapply(data, is.numeric)
numeric_data <- data[, numeric_cols]
library(corrplot)
# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
cor_matrix
corrplot(cor_matrix, method = "color", order = "hclust", addrect = 2, col = col1(100), tl.cex = 0.5)


##################   chi-squared statistic 

# Calculate the chi-squared statistic for the association between two categorical variables

library(vcd)
t1 <- table(data$manufacturer, data$model)
assocstats(t1)

t2 <- table(data$fuel, data$cylinders)
assocstats(t2)

t3 <- table(data$drive, data$transmission)
assocstats(t3)

t4 <- table(data$state, data$region)
assocstats(t4)

gc()

################### Analyse des valeurs manquantes
install.packages('naniar')
library(naniar)
gg_miss_upset(data,nsets = 20)

n_miss(data)
prop_miss(data)
prop_complete(data)

# Profil des VM
library(mice)
plot(md.pattern(data))

#Compter le nombre de valeurs manquantes dans chacune des colonnes
sapply(data,function(x) sum(is.na(x)))
install.packages("Amelia")
library(Amelia)
#Afficher un graph avec les valeurs manquantes vs les valeurs observées
missmap(data, main = "Missing values vs observed")

################## Selection d'un echantillon  

#Creation et supression de colonnes :
data[ , c('model','region', 'url',"id", 'region_url', 'image_url','county','ID',"VIN","description","size")] <- list(NULL)
data <- data[data$price <= 250000 & data$price > 0, ]

data$manufacturer <- ifelse(data$manufacturer == "", NA, data$manufacturer)
data$year <- ifelse(data$year == "", NA, data$year)
#data$model <- ifelse(data$model == "", NA, data$model)
data$condition <- ifelse(data$condition == "", NA, data$condition)
data$cylinders <- ifelse(data$cylinders == "", NA, data$cylinders)
data$fuel <- ifelse(data$fuel == "", NA, data$fuel)
data$odometer <- ifelse(data$odometer == "", NA, data$odometer)
data$title_status <- ifelse(data$title_status == "", NA, data$title_status)
data$transmission <- ifelse(data$transmission == "", NA, data$transmission)
data$drive <- ifelse(data$drive == "", NA, data$drive)
data$type <- ifelse(data$type == "", NA, data$type)
data$paint_color <- ifelse(data$paint_color == "", NA, data$paint_color)
data$lat <- ifelse(data$lat == "", NA, data$lat)
data$long <- ifelse(data$long == "", NA, data$long)
data$posting_date <- ifelse(data$posting_date == "", NA, data$posting_date)
data$state <- ifelse(data$state == "", NA, data$state)
#data$region <- ifelse(data$region == "", NA, data$region)
library(stringr)
#scinder la colonne posting date
data$posting_date1 =str_split_fixed(data$posting_date, "T", 2)
data[ ,"posting_date"] <- list(NULL)
data$posting_date1 <- ifelse(data$posting_date1 == "", NA, data$posting_date1)

dim(data)

#Calculer le nombre de VM par colonne de chaque obserations :

data$nb <- apply(data, MARGIN = 1, function(x){sum(is.na(x))})

#Construction d'un echantillon avec zero variable manquantes par obseravtion :
echanti <-data[data$nb==0 ,]
data[ ,"nb"] <- list(NULL)
echanti[ , c("nb")] <- list(NULL)
dim(echanti)
colnames(echanti)

#Connaitre le nombre de valeurs différentes dans chaque colonnes
sapply(echanti, function(x) length(unique(x)))

#Compter le nombre de valeurs manquantes dans chacune des colonnes
sapply(echanti,function(x) sum(is.na(x)))

# Sélectionner un échantillon aléatoire de 40% de ce dataframe
#set.seed(555)
echantillon <- echanti[sample(nrow(echanti), round(0.2*nrow(echanti))), ]

#colnames(echantillon)
#dim(echantillon)

################## MCA 

# Select the qualitative variables for the MCA
qual_vars <- c("manufacturer", "condition", "cylinders", 
               "fuel", "title_status", "transmission", "drive", 
               "type", "paint_color", 'state')

library(FactoMineR)

qual_data<- echantillon[,qual_vars]
qual_data[] <- lapply(qual_data, factor)
mca <- MCA(qual_data, graph = T)

fviz_eig(mca, addlabels = TRUE, ylim = c(0, 30))
var <- get_mca_var(mca)
var

# Contributions aux composantes principales
var$contrib

fviz_pca_var(mca, col.var = "black")

corrplot(var$cos2, is.corr=FALSE)
# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(mca, choice = "var", axes = 1:5)
# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(mca, col.var = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )

#####Visualiser les valeurs propre Montre le pourcentage de variances expliquées par chaque axe principal.
library(FactoMineR)
eig.val <- mca$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")

# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")
install.packages("FactoMineR")
plot(mca, invisible = "ind", cex = 0.8, autoLab = "yes")
corcircle(mca)

##Biplot  des variables :
plot(mca, 
     invisible = c("ind"),
     cex = 0.8,
     autoLab = "yes")

#Biplot des individus et des variables montrant leurs associations
plot(mca, autoLab = "yes")

# Valeurs propres
mca$eig

plotind(fmin, axes = c(1,2), clabel = 0.8)

# Résultats des variables actives
res.var <- mca$var
res.var$coord          # Coordonnées
res.var$contrib        # Contributions 
res.var$cos2           # Qualité de représentation 

# Résultats des individus actifs
res.ind <- mca$var
res.ind$coord          # Coordonnées
res.ind$contrib        # Contributions 
res.ind$cos2           # Qualités de représentation

#Colorer les individus par groupes et ajouter les ellipses de confiance autour de la moyenne des groupes :
plotellipses(mca, keepvar = c("cylinders", "condition"))
plotellipses(mca, keepvar = c("manufacturer", "drive"))
plotellipses(mca, keepvar = c("fuel", "title_status"))
plotellipses(mca, keepvar = c("paint_color", 'state'))
plotellipses(mca, keepvar = c("transmission", 'type'))

gc()

################## ACP


#creation d'un base de données pour les variables quantitatives
var_quanti <- echantillon[,c(2,3,8,14,15)]

View(var_quanti)
#Analyse de ACP 
library("FactoMineR")
library("factoextra")

res.pca = PCA(var_quanti, scale.unit=T, ncp=5, graph=T)
eig.val <- get_eigenvalue(res.pca)

eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 30))
var <- get_pca_var(res.pca)
var

# Coordonnées
var$coord
# Cos2: qualité de répresentation
var$cos2
# Contributions aux composantes principales
var$contrib
fviz_pca_var(res.pca, col.var = "black")
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:5)
# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )
ind <- get_pca_ind(res.pca)
ind
# Coordonnées des individus
head(ind$coord)
# Qualité des individus
head(ind$cos2)
# Contributions des individus
head(ind$contrib)
plot.PCA(res.pca, axes=c(1, 2), choix="ind")



################# Model non supervise classification hiérarchique

qual_vars <- c("manufacturer", "condition", "cylinders", 
               "fuel", "title_status", "transmission", "drive", 
               "type", "paint_color", 'state')

qual_data<- echantillon[,qual_vars]
qual_data[] <- lapply(qual_data, factor)
quanti = echantillon[,c(2,3,9,16,17)]
dim(quanti)
View(quanti)
install.packages("ClustOfVar")
require(ClustOfVar)
tree<-hclustvar(quanti,qual_data)
plot(tree,main="Dendrogram of ClustOfVar")
stab<-stability(tree,B=3)
plot(stab,main="Stability of the partitions")
###fixe k
plot(tree)
rect.hclust(tree,k=6,border=2:5)
partition1 <- cutreevar(tree,k=6)

summary(partition1)

round(partition1$scores,digit=2)
View(echantillon)


################# Model supervise regression multiple 

# Sélectionner un échantillon aléatoire de 20% de ce dataframe
echantillon <- echanti[sample(nrow(echanti), round(0.2*nrow(echanti))), ]
dim(echantillon)


# Split the dataset into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(nrow(echantillon), round(0.6 * nrow(echantillon)))
train_data <- echantillon[train_index, ]
test_data <- echantillon[-train_index, ]

# Train a linear regression model
lm_model <- lm(price ~year+ condition + cylinders + fuel + odometer + title_status + transmission + drive + type + paint_color + state + lat + long, data = train_data)

summary(lm_model)
# Evaluate the performance of the model on the testing set
y_pred <- predict(lm_model, newdata = test_data)
rmse <- sqrt(mean((y_pred - test_data$price)^2))
r_squared <- summary(lm_model)$r.squared

# Print the results
cat("RMSE:", round(rmse, 3), "\n")
cat("R-squared:", round(r_squared, 3), "\n")


################# Model supervise regression logistique  

# Create a binary variable for car prices
threshold <- 20000 # set the threshold for car prices
echantillon$car_binary <- ifelse(echantillon$price > threshold, 1, 0)

# Split the dataset into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(nrow(echantillon), round(0.6 * nrow(echantillon)))
train_data <- echantillon[train_index, ]
test_data <- echantillon[-train_index, ]

# Train a logistic regression model
glm_model <- glm(car_binary ~year+ condition + cylinders + fuel + odometer + title_status + transmission + drive + type + paint_color + state + lat + long, data = train_data, family = binomial())

# Evaluate the performance of the model on the testing set
y_pred <- predict(glm_model, newdata = test_data, type = "response")
y_pred_binary <- ifelse(y_pred > 0.5, 1, 0)
accuracy <- mean(y_pred_binary == test_data$car_binary)

# Calculate the accuracy
accuracy <- mean(y_pred_binary == test_data$car_binary)

# Calculate the root mean squared error (RMSE)
rmse <- sqrt(mean((y_pred - test_data$car_binary)^2))

# Calculate the R-squared
SSR <- sum((y_pred_binary - mean(test_data$car_binary))^2)
SST <- sum((test_data$car_binary - mean(test_data$car_binary))^2)
r_squared <- SSR / SST

# Print the results
cat("Accuracy:", round(accuracy, 3), "\n")
cat("RMSE:", round(rmse, 3), "\n")
cat("R-squared:", round(r_squared, 3), "\n")



############### Train a Gradient boosting 


# Load required packages
library(gbm)

# Convert categorical variables to factors
vars <- c("condition", "fuel", "title_status", "transmission", "drive", "type", "paint_color", "state", "cylinders")
for (var in vars) {
  echantillon[[var]] <- as.factor(echantillon[[var]])
}

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(nrow(echantillon), round(0.6 * nrow(echantillon)))
train_data <- echantillon[train_index, ]
test_data <- echantillon[-train_index, ]

# Specify the variables to include in the model
vars <- c("year", "condition", "cylinders", "fuel", "odometer", "title_status", "transmission", "drive", "type", "paint_color", "state", "lat", "long")

# Build the gradient boosting model
gbm_model <- gbm(price ~ year + condition + cylinders + fuel + odometer + title_status + transmission + drive + type + paint_color + state + lat + long, data = train_data[, c(vars, "price")], n.trees = 1000, interaction.depth = 4, shrinkage = 0.01, distribution = "gaussian")

# Generate predictions for the test set
y_pred <- predict(gbm_model, newdata = test_data[, vars], n.trees = 1000)

# Compute the root mean squared error (RMSE) and R-squared for the model
rmse <- sqrt(mean((y_pred - test_data$price)^2))
r_squared <- cor(y_pred, test_data$price)^2

# Print the results
cat("Accuracy:", accuracy, "\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

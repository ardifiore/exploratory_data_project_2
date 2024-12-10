# Load dplyr and ggplot2 packages
library(dplyr)
library(ggplot2)

# Read the PM2.5 emissions and source classification code data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Filter for motor vehicle-related sources
vehicle_sources <- SCC %>%
  filter(grepl("vehicle", EI.Sector, ignore.case = TRUE)) 

# Merge emissions data with motor vehicle sources for Baltimore and Los Angeles
vehicle_emissions <- NEI %>%
  filter(fips %in% c("24510", "06037") & SCC %in% vehicle_sources$SCC) %>%
  group_by(year, fips) %>%
  summarize(Emissions = sum(Emissions, na.rm = TRUE)) 

# Map FIPS codes to city names
vehicle_emissions$fips <- recode(vehicle_emissions$fips, "24510" = "Baltimore City, MD", "06037" = "Los Angeles County, CA")

# Define a color palette for the cities
city_colors <- c("Baltimore City, MD" = "#FF4500", "Los Angeles County, CA" = "#4682B4") 

# Create a PNG
png("plot6.png", width = 640, height = 480)

# Create a bar graph with facets for each location
ggplot(data = vehicle_emissions, aes(x = factor(year), y = Emissions, fill = fips)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(Emissions, 2)), vjust = -1.2, size = 4, color = "black") + 
  scale_fill_manual(values = city_colors) + 
  labs(
    title = "PM2.5 Emissions from Motor Vehicles (1999-2008)",
    subtitle = "Comparison between Baltimore City and Los Angeles County",
    x = "Year",
    y = "PM2.5 Emissions (Tons)",
    fill = "City"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10) 
  ) +
  facet_grid(fips ~ ., scales = "free") 

# Close the PNG device
dev.off()
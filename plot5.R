# Load dplyr and ggplot2 packages
library(dplyr)
library(ggplot2)

# Read the PM2.5 emissions and source classification code data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Filter for motor vehicle-related sources
vehicle_sources <- SCC %>%
  filter(grepl("vehicle", EI.Sector, ignore.case = TRUE)) 

# Merge emissions data with motor vehicle sources for Baltimore
baltimore_vehicle_emissions <- NEI %>%
  filter(fips == "24510" & SCC %in% vehicle_sources$SCC) %>% 
  group_by(year) %>%
  summarize(Emissions = sum(Emissions, na.rm = TRUE)) 

# Define a color palette for the bars
bar_colors <- c("#FF4500", "#FF6347", "#FF7F50", "#FFA07A") 

# Create a PNG
png("plot5.png", width = 640, height = 480)

# Create a bar graph with numerical labels and scaled emissions
ggplot(data = baltimore_vehicle_emissions, aes(x = factor(year), y = Emissions, fill = factor(year))) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(Emissions, 2)), vjust = -0.5, size = 5, color = "black") + 
  scale_fill_manual(values = bar_colors) + # Custom colors
  labs(
    title = "PM2.5 Emissions from Motor Vehicles in Baltimore City (1999-2008)",
    x = "Year",
    y = "PM2.5 Emissions (Tons)",
    fill = "Year"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none" 
  )

# Close the PNG device
dev.off()
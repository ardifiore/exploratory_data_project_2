# Load dplyr and ggplot2 packages
library(dplyr)
library(ggplot2)

# Read the PM2.5 emissions and source classification code data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Filter for coal combustion-related sources
coal_combustion_sources <- SCC %>%
  filter(grepl("Comb.*Coal", EI.Sector, ignore.case = TRUE)) 

# Merge emissions data with coal combustion sources
coal_combustion_emissions <- NEI %>%
  filter(SCC %in% coal_combustion_sources$SCC) %>%
  group_by(year) %>%
  summarize(Emissions = sum(Emissions, na.rm = TRUE)) 

# Define a color palette for the bars
bar_colors <- c("#6B8E23", "#2E8B57", "#4682B4", "#8B0000") 

# Create a PNG
png("plot4.png", width = 640, height = 480)

# Create a bar graph with labels and scaled emissions
ggplot(data = coal_combustion_emissions, aes(x = factor(year), y = Emissions / 1000, fill = factor(year))) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(Emissions / 1000, 2)), vjust = -0.5, size = 5, color = "black") + 
  scale_fill_manual(values = bar_colors) + 
  labs(
    title = "Total Coal Combustion-Related PM2.5 Emissions in the US (1999-2008)",
    x = "Year",
    y = "PM2.5 Emissions (Kilotons)",
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
# Load dplyr and ggplot2 packages
library(dplyr)
library(ggplot2)

# Read the PM2.5 emissions data from the RDS file
NEI <- readRDS("summarySCC_PM25.rds")

# Filter and summarize emissions data for Baltimore by year and type
baltimore_emissions_by_type <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year, type) %>%
  summarize(Emissions = sum(Emissions, na.rm = TRUE))

# Define Baltimore Orioles colors for the bars
orioles_colors <- c("#DF4601", "#000000") 

# Create a PNG
png("plot3.png", width = 640, height = 480)

# Create a bar chart with facets for source type
ggplot(data = baltimore_emissions_by_type, aes(x = factor(year), y = Emissions, fill = factor(year))) +
  geom_bar(stat = "identity") + 
  geom_label(aes(label = round(Emissions, 2)), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = rep(orioles_colors, length.out = length(unique(baltimore_emissions_by_type$year)))) + 
  labs(
    title = expression('PM'[2.5]*' Emissions in Baltimore City by Various Source Types (1999-2008)'),
    x = "Year",
    y = expression("Total PM"[2.5]*" Emissions (Tons)"),
    fill = "Year"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none", 
    strip.text = element_text(size = 14, face = "bold") 
  ) +
  facet_wrap(~ type, scales = "free_y", ncol = 2) 

# Close the PNG device
dev.off()
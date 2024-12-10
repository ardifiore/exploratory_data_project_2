# Load the dplyr package
library(dplyr)

# Read the PM2.5 emissions data from the RDS file
NEI <- readRDS("summarySCC_PM25.rds")

# Group the data and summarize total PM2.5 emissions for each year
total_emissions <- NEI %>%
  group_by(year) %>% # Group data by the 'year' column
  summarize(Emissions = sum(Emissions, na.rm = TRUE))

# Define colors for the bars
pastel_colors <- c("#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9")

# Create a PNG file
png("plot1.png", width = 640, height = 480)

# Generate a bar plot to visualize total PM2.5 emissions between 1999-2008. Scale emissions to kilotons, and label the bars.
x_positions <- barplot(
  height = total_emissions$Emissions / 1000, 
  names.arg = total_emissions$year, 
  col = pastel_colors, 
  xlab = "Year", # Label for the X-axis
  ylab = expression('Total PM'[2.5]*' Emission (Kilotons)'), 
  ylim = c(0, max(total_emissions$Emissions / 1000) * 1.1), # Adjust Y-axis limit for space above bars
  main = expression('Total PM'[2.5]*' Emissions in the US (1999-2008)') # Title of the plot
)

# Add labels on top of the bars. Scale y-coordinates to kilotons. Round emission values to 2 decimal places.
text(
  x = x_positions, # X-coordinates from the barplot
  y = total_emissions$Emissions / 1000, 
  labels = round(total_emissions$Emissions / 1000, 2), 
  pos = 3, 
  cex = 0.8, 
  col = "black"
)
  dev.off()
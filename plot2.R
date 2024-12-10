# Load the dplyr package
library(dplyr) # Used for data manipulation

# Read the PM2.5 emissions data from the RDS file
NEI <- readRDS("summarySCC_PM25.rds")

# Filter the data for Baltimore City, Maryland (FIPS == "24510")
baltimore_data <- NEI %>%
  filter(fips == "24510") # Select rows for Baltimore City

# Summarize total PM2.5 emissions for each year
baltimore_emissions <- baltimore_data %>%
  group_by(year) %>%
  summarize(Emissions = sum(Emissions, na.rm = TRUE)) 

# Define Baltimore Ravens colors for the bars
ravens_colors <- c("#241773", "#9E7C0C", "#000000", "#241773") # Purple, Gold, Black, and repeat Purple

# Create a PNG file
png("plot2_ravens_colors.png", width = 640, height = 480) 

# Generate a bar plot for total PM2.5 emissions in Baltimore. Scale emissions to kilotons and label the bars.
x_positions <- barplot(
  height = baltimore_emissions$Emissions / 1000, 
  names.arg = baltimore_emissions$year, 
  col = ravens_colors, # Use Ravens team colors for the bars
  xlab = "Year", # Label for the X-axis
  ylab = expression('Total PM'[2.5]*' Emission (Kilotons)'), 
  ylim = c(0, max(baltimore_emissions$Emissions / 1000) * 1.1), 
  main = expression('Total PM'[2.5]*' Emissions in Baltimore City (1999-2008)') # Title of the plot
)

# Add labels on top of the bars. Scale y-coordinates to kilotons. Round emission values to 2 decimal places.
text(
  x = x_positions, 
  y = baltimore_emissions$Emissions / 1000, 
  labels = round(baltimore_emissions$Emissions / 1000, 2), 
  pos = 3, # Position above the bars
  cex = 0.8, # Text size
  col = "black" # Text color
)

dev.off()
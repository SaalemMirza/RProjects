# Load the libraries of ggplot2, ggthemes and data.table
library(ggplot2)
library(data.table)
library(ggthemes)

# Create a data frame by exporting the csv file of the data using fread function (from data.table library).
# Drop the heading of the csv file.
dataf <- fread(file = 'Economist_Assignment_Data.csv',drop = 1)

# Create a scatter plot by loading data, adding aesthetic layer and adding geometric point layer. 
pl <- ggplot(dataf,aes(x=CPI,y=HDI,color=Region)) + geom_point(shape=1,size=5) 

# Below codes add a trend line to the scatter plot, which uses logarithmic function and display confidence interval.
pl2 <- pl + geom_smooth(aes(group=1),method = lm, formula = y~log(x),se = F, color = 'red')

# Creating a vector which consists of subset of country names which we want to display on the plot as labels.
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", 
                   "Japan", "New Zealand", "Singapore")

# Adding the created list of countries to the visualization using geom_text() function.
# This is done so that our scatter plot does not gets messy with overlapping Country labels.
pl3 <- pl2 + geom_text(data = subset(dataf, Country %in% pointsToLabel), aes(label = Country), color = "gray20", check_overlap = T)

# Labeling the X-Axis, and defining breaks and limits.
pl4 <- pl3 + scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10 = least corrupt)", limits = c(1,10), breaks = 1:10)

# Labeling the Y-Axis, and defining limits.
pl5 <- pl4 + scale_y_continuous(name = "Human Development Index, 2011 (1 = Best)",limits = c(0.2,1.0))

# Adding title to our created scatter plot
pl6 <- pl5 + ggtitle("Corruption and Human Development Correlation")

# Finally adding theme to our fully created visualization. 
pl7 <- pl6 + theme_economist()

print(pl7)


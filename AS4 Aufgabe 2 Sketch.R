library(readr)
library("ggplot2")

## Aufgabe (a)
pricesraw <- read_delim("Handelspreise_per_hour.csv", 
                                     delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d.%m.%Y")), 
                                     locale = locale(date_names = "de", decimal_mark = ",", 
                                                     grouping_mark = "."), trim_ws = TRUE)
pricesraw$StartShow <- format(as.POSIXct(pricesraw$Start), format = "%H:%M")

sel_dates <- subset(pricesraw, (Date == '2022-02-24' | Date == '2022-06-14' | Date == '2022-12-05'), select = c(Date, StartShow, Deutschland))
ggplot(data = sel_dates, aes(x=StartShow, y=Deutschland, group = 1)) + geom_line(colour = "blue") + facet_grid(rows=vars(sel_dates$Date), scales = "fixed") + labs(y="Preis",x="Uhrzeit",title = "Deutschland") + theme(axis.text.x = element_text(angle = 90))


## Aufgabe (b) 
library(reshape2)
# Empty vector to hold the mean value for Danmark
dan <- c()
# Create mean value for all rows in pricesraw
for (i in length(pricesraw)) {
    # Add the mean value to the vector
    dan <- append (dan,(pricesraw$Dänemark_1 + pricesraw$Dänemark_2)/2)
}
# Adding the column with the values
pricesraw$Dänemark <- dan
# Deleting the rows for Dänemark_ and ..._2 in a new data frame
pricesraw_clean <- pricesraw[-c(6,7)]
### Aggregate with max price per day and per country
# Working data set with only needed columns
sel_prices <- subset(pricesraw_clean, select = -c(Start, End, StartShow))
# Aggregate, store in max_prices
max_prices <- aggregate(sel_prices[,colnames(sel_prices) != "Date"], by = list(sel_prices$Date), FUN = max)
# Melting
molted_max_prices <- melt(max_prices, id.vars = "Group.1")

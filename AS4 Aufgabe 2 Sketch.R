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
library(readr)
dataraw <- read_csv("Energieerzeugung_long_format.csv", 
                                         col_types = cols(date = col_date(format = "%d.%m.%Y")))
sel_dates <- subset(dataraw, date == c('2022-12-10','2022-08-01'))

ren <- c()
for (one_source in sel_dates[[4]]) {
  if (one_source %in% c("Biomasse", "Wasserkraft", "Wind_Offshore", "Wind_Onshore", "Photovoltaik", "Sonstige_Erneuerbare_Energien")) {
    ren <- append (ren,"renewable energy")
  } else {
    ren <- append(ren,"non-renewable energy")
  }
}
sel_dates$renewable = ren

ren <- c()
for (one_source in sel_dates[[4]]) {
  if (one_source == "Sonstige_Konventionelle_Energien") {
    ren <- append (ren, "Sonstige konventionelle Energien")
  } else if (one_source == "Sonstige_Erneuerbare_Energien") {
    ren <- append (ren, "Sonstige erneuerbare Energien")
  } else if (one_source == "Wind_Offshore") {
    ren <- append (ren, "Wind (Offshore)")
  } else if (one_source == "Wind_Onshore") {
    ren <- append (ren, "Wind (Onshore)")  } else {
    ren <- append (ren, one_source)
  }
}
sel_dates$source = ren

disp_data <- sel_dates[,c(1,4,5,6)]

# aggr <- aggregate(x=disp_data[,colnames(disp_data) !="source"], by=list(disp_data$date,disp_data$renewable,disp_data$source), FUN=mean)
aggr <- aggregate(x=disp_data[,colnames(disp_data) == "energy"], by=list(disp_data$date,disp_data$renewable,disp_data$source), FUN=mean)

library("ggplot2")
ggplot(data = aggr) + geom_bar(color = "darkred", fill = "darkred", mapping = aes(x=Group.3, y=energy), stat = "identity") + coord_flip() + facet_grid(cols=vars(aggr$Group.1), rows=vars(aggr$Group.2), scales = "free") + labs(y="Energieerzeugung in [MWh]",x="")

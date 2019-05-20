library(devtools)
install_github('R-package','quandl')
library(quantmod)
library(ggplot2)
library(gridExtra)
library(ggdendro)
library(zoo)


# Get some data from quantmod
amazon <- getSymbols('AMZN', from="2013-05-01", to='2019-05-01',  auto.assign = F)
apple <- getSymbols('AAPL', from="2013-05-01", to='2019-05-01',  auto.assign = F)
google <- getSymbols('GOOG', from="2013-05-01", to='2019-05-01',  auto.assign = F)
exxon <- getSymbols('XOM', from="2013-05-01", to='2019-05-01',  auto.assign = F)
sap <- getSymbols('SAP', from="2013-05-01", to='2019-05-01',  auto.assign = F)
walmart <- getSymbols('WMT', from="2013-05-01", to='2019-05-01',  auto.assign = F)
microsoft <- getSymbols('MSFT', from="2013-05-01", to='2019-05-01',  auto.assign = F)
jpm <- getSymbols('JPM', from="2013-05-01", to='2019-05-01',  auto.assign = F)

# Plot the time series
plot(amazon)
# Merge and plot the time series (just the closing price)
joined_ts <- cbind(amazon[,4], apple[,4], google[,4], exxon[,4], sap[,4], walmart[,4], microsoft[,4], jpm[,4])
names(joined_ts) <- c('amazon', 'apple', 'google', 'exxon', 'sap', 'walmart', 'microsoft', 'jpm')
plot(joined_ts)

# Scale the time series and plot
maxs <- apply(joined_ts, 2, max)
mins <- apply(joined_ts, 2, min)
joined_ts_scales <- scale(joined_ts, center = mins, scale = maxs - mins)
plot(joined_ts_scales)

# Sparklines & Dendograms
hc <- hclust(dist(t(joined_ts_scales)), "ave")

### Plot
hcdata <- dendro_data(hc)
names_order <- hcdata$labels$label
# Use the folloing to remove labels from dendogram so not doubling up - but good for checking
hcdata$labels$label <- ''
p1 <- ggdendrogram(hcdata, rotate=TRUE, leaf_labels=FALSE)

new_data <- joined_ts_scales[,rev(as.character(names_order))]

p2 <- autoplot(new_data, facets = Series ~ . ) + 
  xlab('') + ylab('') + theme(legend.position="none")

gp1<-ggplotGrob(p1)
gp2<-ggplotGrob(p2) 


grid.arrange(gp2, gp1, ncol=2, widths=c(4,2))

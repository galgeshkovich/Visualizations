#Ebay database - Analyzing the Close Price for items sold.

#Read file
ebay.df <- read.csv("Cross Auctions.csv", stringsAsFactors = TRUE)
head(ebay.df) 

# histogram
#Most Close Prices are under 200.
hist(ebay.df$ClosePrice,  main = "Histogram", 
     xlab = "Close Price")

# change number of bins for better distribution
hist(ebay.df$ClosePrice, br = 100,  main = "Histogram with 100 Breaks", 
     xlab = "Close Price")

hist(ebay.df$ClosePrice, br = 1000,  main = "Histogram with 1000 Breaks", 
     xlab = "Close Price")

# zoom
#Median of the distribution is around 40.
hist(ebay.df$ClosePrice[ebay.df$ClosePrice < 200],  
     main = "Histogram", xlab = "Close Price")

#density plot
plot (density(ebay.df$ClosePrice), main="Density Plot",
      xlab= "Close Price")

## boxplot
boxplot(ebay.df$ClosePrice, xlab = "Close Price")
boxplot(ebay.df$ClosePrice[ebay.df$ClosePrice < 200], xlab = "Close Price")


## log scale for better results
#As said in the histogram, median around 40.
boxplot(ebay.df$ClosePrice, xlab = "Close Price", log = 'y')

## scatter plot
#2 numeric variables - BuyItNow Price and Close Price
plot(ClosePrice ~ BuyItNow, data = ebay.df)

# Most of the items were sold without reserve price.
plot(ClosePrice ~ BuyItNow, data = ebay.df, 
     col = ifelse(ebay.df$HasReservePrice=="FALSE", "blue", "red"), 
     pch = 1)
legend("topright", 
       legend = c("No reserve prices", "With reserved price"), 
       col = c("blue", "red"), 
       pch = 1)

#log scale - the relationship seems more clear.
plot(ClosePrice ~ BuyItNow, data = ebay.df, 
     col = ifelse(ebay.df$HasReservePrice=="FALSE", "blue", "red"), 
     pch = 1,
     log='xy')
legend("topright", 
       legend = c("No reserve prices", "With reserved price"), 
       col = c("blue", "red"), 
    pch = 1)

## 2 variable barplot
#The longer the duration increases, the larger the average closing price
meanClosePrice <- aggregate(ClosePrice ~ Duration, 
                            data = ebay.df, 
                            FUN = "mean")
barplot(height = meanClosePrice[,2], names.arg = meanClosePrice[,1], 
        xlab = "duration", ylab = "mean close price")



#Visualization using ggplot2 library

library(ggplot2)
#simple scatter plot
ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice)) + geom_point()

## separate layers
p <- ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice)) 
p + geom_point()

## axes names and limits
ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice)) + 
  geom_point() + 
  xlab("x Title") + 
  ylim(c(0, 250))

## aes
ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice, 
                    color = HasReservePrice)) + geom_point()

ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice, 
                    shape = SellerAboutMePage)) + geom_point()

ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice, 
                    size = BidCount)) + geom_point()


## histogram and density
ggplot(ebay.df, aes(x = ClosePrice)) + geom_histogram()
ggplot(ebay.df, aes(x = ClosePrice)) + geom_histogram(bins = 100)
ggplot(ebay.df, aes(x = ClosePrice)) + geom_density(fill = "red")

## boxplot
ggplot(ebay.df, aes(x ="ClosePrice", y = ClosePrice)) + geom_boxplot()
ggplot(ebay.df, aes(x ="ClosePrice", y = ClosePrice)) + geom_boxplot() + scale_y_log10()
ggplot(ebay.df, aes(x = as.factor(Duration), y = ClosePrice)) + geom_boxplot()

## scatter and bar plots
ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice)) + geom_point()
ggplot(ebay.df, aes(x = as.factor(Duration), y = ClosePrice)) + geom_bar(stat = "summary", fun = "mean")

## close price as a function of feedback
p <- ggplot(ebay.df, aes(x = SellerFeedbackScore, y = ClosePrice))

## log scale
p + geom_point() + scale_x_log10() + scale_y_log10()

## jitter
p + geom_jitter(width = 0.1, height = 0.1) + scale_x_log10() + scale_y_log10()

## close price as a function of bid count:
p <- ggplot(ebay.df, aes(x = BidCount, y = ClosePrice)) 

## smooth
p + geom_point() + geom_smooth(method = lm) + ylim(c(0, 250))
p + geom_point() + geom_smooth(method = loess) + ylim(c(0, 250))

## facets
p <- ggplot(ebay.df, aes(x = SellerFeedbackScore, y = ClosePrice)) + scale_x_log10() + scale_y_log10() + geom_jitter(width = 0.1, height = 0.1)
p + facet_grid(. ~ SellerAboutMePage)
p + facet_grid(SellerAboutMePage ~ .)

#Facet split view
ebay.df$isBuyItNow<-ebay.df$BuyItNow>0
p<- ggplot(ebay.df, aes(x=SellerFeedbackScore, y=ClosePrice)) +
  scale_x_log10()+scale_y_log10() + geom_jitter()
p+ facet_grid(isBuyItNow~SellerAboutMePage)

#Close Price as a function of Start Price with distribution by SellerAboutMePage
plot(ClosePrice~StartPrice , data=ebay.df ,
     col=ifelse(ebay.df$SellerAboutMePage== "FALSE","blue", "red") ,
     pch=ifelse(ebay.df$SellerAboutMePage=="FALSE",1,2),
     log='xy')
legend("bottomright", 
       legend=c("No seller page","With selelr page"),
       col=c("blue","red"),
       pch=c(1,2))


#Stock Prices database

library(ggplot2)

#Read the file
stock.df<-read.csv("stock prices.csv")

# Set date 
stock.df$Index<- as.Date(stock.df$Index, "%d/%m/%Y")

#Stock close prices per date
#Apple stock close prices are getting much higher then Kellogg's, after 2020.
ggplot(stock.df, aes(x = Index)) +
  geom_point(aes(y = AAPL.Close, color = "AAPL.Close")) +
  geom_point(aes(y = K.Close, color = "K.Close")) +
  labs(title = "Stock close prices per date",
       x = "date", y = "Stock Price", color = "Stock") +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("AAPL.Close", "K.Close")) 

#Mean apple stock close price before and after covid
# After covid, apple stock close prices are higher.
ggplot(stock.df, aes(x=after.covid, y=AAPL.Close , fill=after.covid)) + 
  geom_bar(stat="summary", fun="mean") +
  labs(title = "Mean Apple stock close price before and after covid")+
  scale_fill_manual(values=c("red", "blue"))

#Histogram of apple stock close price
#After covid, stock close prices were higher, but less stocks were sold.
ggplot(stock.df, aes( x=AAPL.Close , fill=after.covid)) +
  geom_histogram(bins=25) + scale_fill_manual(values=c("red","blue")) +
  labs(title="Histogram of Apple stock close price" , x= "Close Price" )

#Mean kellogg's stock close price before and after the Russia-Ukraine war started
#After the Russia-Ukraine war started, Kellogg's stock close prices increased slightly.
ggplot(stock.df, aes(x=after.war, y=K.Close , fill=after.war)) + 
  geom_bar(stat="summary", fun="mean") +
  labs(title = "Mean Kellogg's stock close price before and after the Russia - Ukraine war started",
       y="Kellogg's stock close price" , x="After Russia - Ukraine war started" )+
  scale_fill_manual(values=c("red", "blue"))





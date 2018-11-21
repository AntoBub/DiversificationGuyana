library(plotly)
library(comtradr)
library(plyr)
library(dplyr)
library(data.table)

setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification/comtrade_data")
file_names <- dir() #where you have your files
TradeGuy <- do.call(rbind,lapply(file_names,read.csv))
TradeGuy <- TradeGuy[which(TradeGuy$Year != 'NA'),]

TradeGuy_Export <- TradeGuy[which(TradeGuy$Trade.Flow == "Export" ), ]

##### Hirshmann - Herfindal Index All products 6 digits
TradeGuy_6digits <- TradeGuy_Export[which(TradeGuy_Export$Aggregate.Level == '6'),]  
TradeGuy_Export_to_W <- TradeGuy_6digits[which(TradeGuy_6digits$Partner == "World" ), ]
Total_Trade_per_year <- aggregate(TradeGuy_Export_to_W$Trade.Value..US.., by=list(Category=TradeGuy_Export_to_W$Year), FUN=sum, na.rm = T)
names(Total_Trade_per_year) <- c("Year","Tot_Export")
TradeGuy_Export_to_W <- merge(TradeGuy_Export_to_W, Total_Trade_per_year, by = "Year")
TradeGuy_Export_to_W$share <- (TradeGuy_Export_to_W$Trade.Value..US../TradeGuy_Export_to_W$Tot_Export)
TradeGuy_Export_to_W_1991 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1991"),]
TradeGuy_Export_to_W_1992 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1992"),]
TradeGuy_Export_to_W_1993 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1993"),]
TradeGuy_Export_to_W_1994 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1994"),]
TradeGuy_Export_to_W_1995 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1995"),]
TradeGuy_Export_to_W_1996 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1996"),]
TradeGuy_Export_to_W_1997 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1997"),]
TradeGuy_Export_to_W_1998 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1998"),]
TradeGuy_Export_to_W_1999 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1999"),]
TradeGuy_Export_to_W_2000 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2000"),]
TradeGuy_Export_to_W_2001 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2001"),]
TradeGuy_Export_to_W_2002 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2002"),]
TradeGuy_Export_to_W_2003 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2003"),]
TradeGuy_Export_to_W_2004 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2004"),]
TradeGuy_Export_to_W_2005 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2005"),]
TradeGuy_Export_to_W_2006 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2006"),]
TradeGuy_Export_to_W_2007 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2007"),]
TradeGuy_Export_to_W_2008 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2008"),]
TradeGuy_Export_to_W_2009 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2009"),]
TradeGuy_Export_to_W_2010 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2010"),]
TradeGuy_Export_to_W_2011 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2011"),]
TradeGuy_Export_to_W_2012 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2012"),]
TradeGuy_Export_to_W_2013 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2013"),]
TradeGuy_Export_to_W_2014 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2014"),]
TradeGuy_Export_to_W_2015 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2015"),]
TradeGuy_Export_to_W_2016 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2016"),]
TradeGuy_Export_to_W_2017 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2017"),]


library(hhi)
HHI_1991 <- sum((TradeGuy_Export_to_W_1991$share)^2)
HHI_1992 <- sum((TradeGuy_Export_to_W_1992$share)^2)
HHI_1993 <- sum((TradeGuy_Export_to_W_1993$share)^2)
HHI_1994 <- sum((TradeGuy_Export_to_W_1994$share)^2)
HHI_1995 <- sum((TradeGuy_Export_to_W_1995$share)^2)
HHI_1996 <- sum((TradeGuy_Export_to_W_1996$share)^2)
HHI_1997 <- sum((TradeGuy_Export_to_W_1997$share)^2)
HHI_1998 <- sum((TradeGuy_Export_to_W_1998$share)^2)
HHI_1999 <- sum((TradeGuy_Export_to_W_1999$share)^2)
HHI_2000 <- sum((TradeGuy_Export_to_W_2000$share)^2)
HHI_2001 <- sum((TradeGuy_Export_to_W_2001$share)^2)
HHI_2002 <- sum((TradeGuy_Export_to_W_2002$share)^2)
HHI_2003 <- sum((TradeGuy_Export_to_W_2003$share)^2)
HHI_2004 <- sum((TradeGuy_Export_to_W_2004$share)^2)
HHI_2005 <- sum((TradeGuy_Export_to_W_2005$share)^2)
HHI_2006 <- sum((TradeGuy_Export_to_W_2006$share)^2)
HHI_2007 <- sum((TradeGuy_Export_to_W_2007$share)^2)
HHI_2008 <- sum((TradeGuy_Export_to_W_2008$share)^2)
HHI_2009 <- sum((TradeGuy_Export_to_W_2009$share)^2)
HHI_2010 <- sum((TradeGuy_Export_to_W_2010$share)^2)
HHI_2011 <- sum((TradeGuy_Export_to_W_2011$share)^2)
HHI_2012 <- sum((TradeGuy_Export_to_W_2012$share)^2)
HHI_2013 <- sum((TradeGuy_Export_to_W_2013$share)^2)
HHI_2014 <- sum((TradeGuy_Export_to_W_2014$share)^2)
HHI_2015 <- sum((TradeGuy_Export_to_W_2015$share)^2)
HHI_2016 <- sum((TradeGuy_Export_to_W_2016$share)^2)
HHI_2017 <- sum((TradeGuy_Export_to_W_2017$share)^2)


TradeGuy_Export_to_W_1991$HHI <- (sqrt(HHI_1991)-sqrt(1/dim(TradeGuy_Export_to_W_1991)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1991)[1]))
TradeGuy_Export_to_W_1992$HHI <- (sqrt(HHI_1992)-sqrt(1/dim(TradeGuy_Export_to_W_1992)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1992)[1]))
TradeGuy_Export_to_W_1993$HHI <- (sqrt(HHI_1993)-sqrt(1/dim(TradeGuy_Export_to_W_1993)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1993)[1]))
TradeGuy_Export_to_W_1994$HHI <- (sqrt(HHI_1994)-sqrt(1/dim(TradeGuy_Export_to_W_1994)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1994)[1]))
TradeGuy_Export_to_W_1995$HHI <- (sqrt(HHI_1995)-sqrt(1/dim(TradeGuy_Export_to_W_1995)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1995)[1]))
TradeGuy_Export_to_W_1996$HHI <- (sqrt(HHI_1996)-sqrt(1/dim(TradeGuy_Export_to_W_1996)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1996)[1]))
TradeGuy_Export_to_W_1997$HHI <- (sqrt(HHI_1997)-sqrt(1/dim(TradeGuy_Export_to_W_1997)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1997)[1]))
TradeGuy_Export_to_W_1998$HHI <- (sqrt(HHI_1998)-sqrt(1/dim(TradeGuy_Export_to_W_1998)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1998)[1]))
TradeGuy_Export_to_W_1999$HHI <- (sqrt(HHI_1999)-sqrt(1/dim(TradeGuy_Export_to_W_1999)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1999)[1]))
TradeGuy_Export_to_W_2000$HHI <- (sqrt(HHI_2000)-sqrt(1/dim(TradeGuy_Export_to_W_2000)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2000)[1]))
TradeGuy_Export_to_W_2001$HHI <- (sqrt(HHI_2001)-sqrt(1/dim(TradeGuy_Export_to_W_2001)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2001)[1]))
TradeGuy_Export_to_W_2002$HHI <- (sqrt(HHI_2002)-sqrt(1/dim(TradeGuy_Export_to_W_2002)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2002)[1]))
TradeGuy_Export_to_W_2003$HHI <- (sqrt(HHI_2003)-sqrt(1/dim(TradeGuy_Export_to_W_2003)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2003)[1]))
TradeGuy_Export_to_W_2004$HHI <- (sqrt(HHI_2004)-sqrt(1/dim(TradeGuy_Export_to_W_2004)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2004)[1]))
TradeGuy_Export_to_W_2005$HHI <- (sqrt(HHI_2005)-sqrt(1/dim(TradeGuy_Export_to_W_2005)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2005)[1]))
TradeGuy_Export_to_W_2006$HHI <- (sqrt(HHI_2006)-sqrt(1/dim(TradeGuy_Export_to_W_2006)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2006)[1]))
TradeGuy_Export_to_W_2007$HHI <- (sqrt(HHI_2007)-sqrt(1/dim(TradeGuy_Export_to_W_2007)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2007)[1]))
TradeGuy_Export_to_W_2008$HHI <- (sqrt(HHI_2008)-sqrt(1/dim(TradeGuy_Export_to_W_2008)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2008)[1]))
TradeGuy_Export_to_W_2009$HHI <- (sqrt(HHI_2009)-sqrt(1/dim(TradeGuy_Export_to_W_2009)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2009)[1]))
TradeGuy_Export_to_W_2010$HHI <- (sqrt(HHI_2010)-sqrt(1/dim(TradeGuy_Export_to_W_2010)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2010)[1]))
TradeGuy_Export_to_W_2011$HHI <- (sqrt(HHI_2011)-sqrt(1/dim(TradeGuy_Export_to_W_2011)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2011)[1]))
TradeGuy_Export_to_W_2012$HHI <- (sqrt(HHI_2012)-sqrt(1/dim(TradeGuy_Export_to_W_2012)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2012)[1]))
TradeGuy_Export_to_W_2013$HHI <- (sqrt(HHI_2013)-sqrt(1/dim(TradeGuy_Export_to_W_2013)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2013)[1]))
TradeGuy_Export_to_W_2014$HHI <- (sqrt(HHI_2014)-sqrt(1/dim(TradeGuy_Export_to_W_2014)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2014)[1]))
TradeGuy_Export_to_W_2015$HHI <- (sqrt(HHI_2015)-sqrt(1/dim(TradeGuy_Export_to_W_2015)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2015)[1]))
TradeGuy_Export_to_W_2016$HHI <- (sqrt(HHI_2016)-sqrt(1/dim(TradeGuy_Export_to_W_2016)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2016)[1]))
TradeGuy_Export_to_W_2017$HHI <- (sqrt(HHI_2017)-sqrt(1/dim(TradeGuy_Export_to_W_2017)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2017)[1]))

TradeGuy_Export_to_W <- rbind(TradeGuy_Export_to_W_1991, 
                              TradeGuy_Export_to_W_1992,
                              TradeGuy_Export_to_W_1993, 
                              TradeGuy_Export_to_W_1994,
                              TradeGuy_Export_to_W_1995, 
                              TradeGuy_Export_to_W_1996,
                              TradeGuy_Export_to_W_1997,
                              TradeGuy_Export_to_W_1998,
                              TradeGuy_Export_to_W_1999,
                              TradeGuy_Export_to_W_2000,
                              TradeGuy_Export_to_W_2001,
                              TradeGuy_Export_to_W_2002,
                              TradeGuy_Export_to_W_2003,
                              TradeGuy_Export_to_W_2004,
                              TradeGuy_Export_to_W_2005,
                              TradeGuy_Export_to_W_2006,
                              TradeGuy_Export_to_W_2007,
                              TradeGuy_Export_to_W_2008,
                              TradeGuy_Export_to_W_2009,
                              TradeGuy_Export_to_W_2010,
                              TradeGuy_Export_to_W_2011,
                              TradeGuy_Export_to_W_2012,
                              TradeGuy_Export_to_W_2013,
                              TradeGuy_Export_to_W_2014,
                              TradeGuy_Export_to_W_2015,
                              TradeGuy_Export_to_W_2016,
                              TradeGuy_Export_to_W_2017)

HirshmanIndex <- TradeGuy_Export_to_W[,c(1,38)]
HirshmanIndex <-unique( HirshmanIndex)





##### Hirshmann - Herfindal Index Agriculture products 6 digits
TradeGuy_6digits <- TradeGuy_Export[which(TradeGuy_Export$Aggregate.Level == '6'),]  
TradeGuy_Export_to_W <- TradeGuy_6digits[which(TradeGuy_6digits$Partner == "World" ), ]
TradeGuy_Export_to_W <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Commodity.Code <= "240399" ),]

Total_Trade_per_year <- aggregate(TradeGuy_Export_to_W$Trade.Value..US.., by=list(Category=TradeGuy_Export_to_W$Year), FUN=sum, na.rm = T)
names(Total_Trade_per_year) <- c("Year","Tot_Export")
TradeGuy_Export_to_W <- merge(TradeGuy_Export_to_W, Total_Trade_per_year, by = "Year")
TradeGuy_Export_to_W$share <- (TradeGuy_Export_to_W$Trade.Value..US../TradeGuy_Export_to_W$Tot_Export)
TradeGuy_Export_to_W_1991 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1991"),]
TradeGuy_Export_to_W_1992 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1992"),]
TradeGuy_Export_to_W_1993 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1993"),]
TradeGuy_Export_to_W_1994 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1994"),]
TradeGuy_Export_to_W_1995 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1995"),]
TradeGuy_Export_to_W_1996 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1996"),]
TradeGuy_Export_to_W_1997 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1997"),]
TradeGuy_Export_to_W_1998 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1998"),]
TradeGuy_Export_to_W_1999 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "1999"),]
TradeGuy_Export_to_W_2000 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2000"),]
TradeGuy_Export_to_W_2001 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2001"),]
TradeGuy_Export_to_W_2002 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2002"),]
TradeGuy_Export_to_W_2003 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2003"),]
TradeGuy_Export_to_W_2004 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2004"),]
TradeGuy_Export_to_W_2005 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2005"),]
TradeGuy_Export_to_W_2006 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2006"),]
TradeGuy_Export_to_W_2007 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2007"),]
TradeGuy_Export_to_W_2008 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2008"),]
TradeGuy_Export_to_W_2009 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2009"),]
TradeGuy_Export_to_W_2010 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2010"),]
TradeGuy_Export_to_W_2011 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2011"),]
TradeGuy_Export_to_W_2012 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2012"),]
TradeGuy_Export_to_W_2013 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2013"),]
TradeGuy_Export_to_W_2014 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2014"),]
TradeGuy_Export_to_W_2015 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2015"),]
TradeGuy_Export_to_W_2016 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2016"),]
TradeGuy_Export_to_W_2017 <- TradeGuy_Export_to_W[which(TradeGuy_Export_to_W$Year == "2017"),]


library(hhi)
HHI_1991 <- sum((TradeGuy_Export_to_W_1991$share)^2)
HHI_1992 <- sum((TradeGuy_Export_to_W_1992$share)^2)
HHI_1993 <- sum((TradeGuy_Export_to_W_1993$share)^2)
HHI_1994 <- sum((TradeGuy_Export_to_W_1994$share)^2)
HHI_1995 <- sum((TradeGuy_Export_to_W_1995$share)^2)
HHI_1996 <- sum((TradeGuy_Export_to_W_1996$share)^2)
HHI_1997 <- sum((TradeGuy_Export_to_W_1997$share)^2)
HHI_1998 <- sum((TradeGuy_Export_to_W_1998$share)^2)
HHI_1999 <- sum((TradeGuy_Export_to_W_1999$share)^2)
HHI_2000 <- sum((TradeGuy_Export_to_W_2000$share)^2)
HHI_2001 <- sum((TradeGuy_Export_to_W_2001$share)^2)
HHI_2002 <- sum((TradeGuy_Export_to_W_2002$share)^2)
HHI_2003 <- sum((TradeGuy_Export_to_W_2003$share)^2)
HHI_2004 <- sum((TradeGuy_Export_to_W_2004$share)^2)
HHI_2005 <- sum((TradeGuy_Export_to_W_2005$share)^2)
HHI_2006 <- sum((TradeGuy_Export_to_W_2006$share)^2)
HHI_2007 <- sum((TradeGuy_Export_to_W_2007$share)^2)
HHI_2008 <- sum((TradeGuy_Export_to_W_2008$share)^2)
HHI_2009 <- sum((TradeGuy_Export_to_W_2009$share)^2)
HHI_2010 <- sum((TradeGuy_Export_to_W_2010$share)^2)
HHI_2011 <- sum((TradeGuy_Export_to_W_2011$share)^2)
HHI_2012 <- sum((TradeGuy_Export_to_W_2012$share)^2)
HHI_2013 <- sum((TradeGuy_Export_to_W_2013$share)^2)
HHI_2014 <- sum((TradeGuy_Export_to_W_2014$share)^2)
HHI_2015 <- sum((TradeGuy_Export_to_W_2015$share)^2)
HHI_2016 <- sum((TradeGuy_Export_to_W_2016$share)^2)
HHI_2017 <- sum((TradeGuy_Export_to_W_2017$share)^2)


TradeGuy_Export_to_W_1991$HHI <- (sqrt(HHI_1991)-sqrt(1/dim(TradeGuy_Export_to_W_1991)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1991)[1]))
TradeGuy_Export_to_W_1992$HHI <- (sqrt(HHI_1992)-sqrt(1/dim(TradeGuy_Export_to_W_1992)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1992)[1]))
TradeGuy_Export_to_W_1993$HHI <- (sqrt(HHI_1993)-sqrt(1/dim(TradeGuy_Export_to_W_1993)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1993)[1]))
TradeGuy_Export_to_W_1994$HHI <- (sqrt(HHI_1994)-sqrt(1/dim(TradeGuy_Export_to_W_1994)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1994)[1]))
TradeGuy_Export_to_W_1995$HHI <- (sqrt(HHI_1995)-sqrt(1/dim(TradeGuy_Export_to_W_1995)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1995)[1]))
TradeGuy_Export_to_W_1996$HHI <- (sqrt(HHI_1996)-sqrt(1/dim(TradeGuy_Export_to_W_1996)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1996)[1]))
TradeGuy_Export_to_W_1997$HHI <- (sqrt(HHI_1997)-sqrt(1/dim(TradeGuy_Export_to_W_1997)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1997)[1]))
TradeGuy_Export_to_W_1998$HHI <- (sqrt(HHI_1998)-sqrt(1/dim(TradeGuy_Export_to_W_1998)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1998)[1]))
TradeGuy_Export_to_W_1999$HHI <- (sqrt(HHI_1999)-sqrt(1/dim(TradeGuy_Export_to_W_1999)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_1999)[1]))
TradeGuy_Export_to_W_2000$HHI <- (sqrt(HHI_2000)-sqrt(1/dim(TradeGuy_Export_to_W_2000)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2000)[1]))
TradeGuy_Export_to_W_2001$HHI <- (sqrt(HHI_2001)-sqrt(1/dim(TradeGuy_Export_to_W_2001)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2001)[1]))
TradeGuy_Export_to_W_2002$HHI <- (sqrt(HHI_2002)-sqrt(1/dim(TradeGuy_Export_to_W_2002)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2002)[1]))
TradeGuy_Export_to_W_2003$HHI <- (sqrt(HHI_2003)-sqrt(1/dim(TradeGuy_Export_to_W_2003)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2003)[1]))
TradeGuy_Export_to_W_2004$HHI <- (sqrt(HHI_2004)-sqrt(1/dim(TradeGuy_Export_to_W_2004)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2004)[1]))
TradeGuy_Export_to_W_2005$HHI <- (sqrt(HHI_2005)-sqrt(1/dim(TradeGuy_Export_to_W_2005)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2005)[1]))
TradeGuy_Export_to_W_2006$HHI <- (sqrt(HHI_2006)-sqrt(1/dim(TradeGuy_Export_to_W_2006)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2006)[1]))
TradeGuy_Export_to_W_2007$HHI <- (sqrt(HHI_2007)-sqrt(1/dim(TradeGuy_Export_to_W_2007)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2007)[1]))
TradeGuy_Export_to_W_2008$HHI <- (sqrt(HHI_2008)-sqrt(1/dim(TradeGuy_Export_to_W_2008)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2008)[1]))
TradeGuy_Export_to_W_2009$HHI <- (sqrt(HHI_2009)-sqrt(1/dim(TradeGuy_Export_to_W_2009)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2009)[1]))
TradeGuy_Export_to_W_2010$HHI <- (sqrt(HHI_2010)-sqrt(1/dim(TradeGuy_Export_to_W_2010)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2010)[1]))
TradeGuy_Export_to_W_2011$HHI <- (sqrt(HHI_2011)-sqrt(1/dim(TradeGuy_Export_to_W_2011)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2011)[1]))
TradeGuy_Export_to_W_2012$HHI <- (sqrt(HHI_2012)-sqrt(1/dim(TradeGuy_Export_to_W_2012)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2012)[1]))
TradeGuy_Export_to_W_2013$HHI <- (sqrt(HHI_2013)-sqrt(1/dim(TradeGuy_Export_to_W_2013)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2013)[1]))
TradeGuy_Export_to_W_2014$HHI <- (sqrt(HHI_2014)-sqrt(1/dim(TradeGuy_Export_to_W_2014)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2014)[1]))
TradeGuy_Export_to_W_2015$HHI <- (sqrt(HHI_2015)-sqrt(1/dim(TradeGuy_Export_to_W_2015)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2015)[1]))
TradeGuy_Export_to_W_2016$HHI <- (sqrt(HHI_2016)-sqrt(1/dim(TradeGuy_Export_to_W_2016)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2016)[1]))
TradeGuy_Export_to_W_2017$HHI <- (sqrt(HHI_2017)-sqrt(1/dim(TradeGuy_Export_to_W_2017)[1]))/(1-sqrt(1/dim(TradeGuy_Export_to_W_2017)[1]))

TradeGuy_Export_to_W <- rbind(TradeGuy_Export_to_W_1991, 
                              TradeGuy_Export_to_W_1992,
                              TradeGuy_Export_to_W_1993, 
                              TradeGuy_Export_to_W_1994,
                              TradeGuy_Export_to_W_1995, 
                              TradeGuy_Export_to_W_1996,
                              TradeGuy_Export_to_W_1997,
                              TradeGuy_Export_to_W_1998,
                              TradeGuy_Export_to_W_1999,
                              TradeGuy_Export_to_W_2000,
                              TradeGuy_Export_to_W_2001,
                              TradeGuy_Export_to_W_2002,
                              TradeGuy_Export_to_W_2003,
                              TradeGuy_Export_to_W_2004,
                              TradeGuy_Export_to_W_2005,
                              TradeGuy_Export_to_W_2006,
                              TradeGuy_Export_to_W_2007,
                              TradeGuy_Export_to_W_2008,
                              TradeGuy_Export_to_W_2009,
                              TradeGuy_Export_to_W_2010,
                              TradeGuy_Export_to_W_2011,
                              TradeGuy_Export_to_W_2012,
                              TradeGuy_Export_to_W_2013,
                              TradeGuy_Export_to_W_2014,
                              TradeGuy_Export_to_W_2015,
                              TradeGuy_Export_to_W_2016,
                              TradeGuy_Export_to_W_2017)

Ag_HirshmanIndex <- TradeGuy_Export_to_W[,c(1,38)]
Ag_HirshmanIndex <-unique( Ag_HirshmanIndex)



library(plotly)
## Graph 1
HirshmanIndex <- list(
  x = as.character(c(HirshmanIndex$Year)),
  y = as.numeric(HirshmanIndex$HHI),
  line = list(color = "rgba(44,160,44,1)", width = 4), 
  mode = "lines", 
  name = "Herfindahl-Hirschmann Index", 
  type = "scatter", 
  uid = "f96c27", 
  xaxis = "x", 
  xsrc = "chnejohnson:0:eddd32", 
  yaxis = "y", 
  ysrc = "chnejohnson:0:2bfc2d"
)

Ag_HirshmanIndex <- list(
  x = as.character(c(Ag_HirshmanIndex$Year)),
  y = as.numeric(Ag_HirshmanIndex$HHI),
  line = list(color = "rgba(214,39,40,1)", width = 4), 
  mode = "lines", 
  name = "Agriculture Herfindahl-Hirschmann Index", 
  type = "scatter", 
  uid = "f96c27", 
  xaxis = "x", 
  xsrc = "chnejohnson:0:eddd32", 
  yaxis = "y", 
  ysrc = "chnejohnson:0:2bfc2d"
)



data <- list(HirshmanIndex, Ag_HirshmanIndex )
layout <- list(
  autosize = TRUE, 
  hovermode = "closest", 
  margin = list(
    r = 10, 
    t = 25, 
    b = 40, 
    l = 60
  ), 
  showlegend = TRUE, 
  title = "Concentration Index", 
  titlefont = list(color ="rgb(49, 61, 183)", size = 46, cex = 30), 
  xaxis = list(
    autorange = TRUE, 
    domain = c(0, 1), 
    range = c("1970", "2017"), 
    title = "date", 
    type = "number"
  ), 
  yaxis = list(
    autorange = TRUE, 
    domain = c(0, 1), 
    range = c(0, 30), 
    title = "", 
    type = "linear"
  )
)


p <- plot_ly()
p <- add_trace(p, x=Ag_HirshmanIndex$x, y=Ag_HirshmanIndex$y, line=Ag_HirshmanIndex$line, mode=Ag_HirshmanIndex$mode, name=Ag_HirshmanIndex$name, type=Ag_HirshmanIndex$type, uid=Ag_HirshmanIndex$uid, xaxis=Ag_HirshmanIndex$xaxis, xsrc=Ag_HirshmanIndex$xsrc, yaxis=Ag_HirshmanIndex$yaxis, ysrc=Ag_HirshmanIndex$ysrc)
p <- add_trace(p, x=HirshmanIndex$x, y=HirshmanIndex$y, line=HirshmanIndex$line, mode=HirshmanIndex$mode, name=HirshmanIndex$name, type=HirshmanIndex$type, uid=HirshmanIndex$uid, xaxis=HirshmanIndex$xaxis, xsrc=HirshmanIndex$xsrc, yaxis=HirshmanIndex$yaxis, ysrc=HirshmanIndex$ysrc)


p <- layout(p, autosize=layout$autosize, hovermode=layout$hovermode, legend = list(orientation = 'h',font = list(size = 32, cex = 30), xanchor = "center",  x = 0.5, y = -.23), title=layout$title, titlefont=layout$titlefont, xaxis = list(autotick = F, dtick = 2,tickangle = 0, ticks="inside",titlefont= list(size = 36, cex = 30), range = c(1997,2017)), yaxis=layout$yaxis, font = list(family = 'Arial', size = 36),margin = list(l = 75, r = 40, t = 73, b = 20))

hide_legend(p)
p


### Hirshman Index and GDP  (Agriculture)


setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/diversification")
data <- read.csv("Ag_GDP.csv", sep =",", header = T)
data <- data[which(data$Year >= 1997),]




##

ay <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = "Herfindahl - Hirschmann Index"
)

p <- plot_ly() %>%
  add_lines(x = as.character(c(1997:2016)), y = as.numeric(data[,2]), name = "GDP Agriculture, Hunting, forestry, Fishing",
            line = list(color = "rgba(0,0,255,1)", width = 4), 
            mode = "lines", 
            type = "scatter", 
            uid = "f96c27", 
            xaxis = "x", 
            xsrc = "chnejohnson:0:eddd32", 
            yaxis = "y", 
            ysrc = "chnejohnson:0:2bfc2d") %>%
  add_lines(x = as.character(c(1997:2017)), y = as.numeric(Ag_HirshmanIndex[,2]), name = "Concentration Index", line = list(color = "rgba(76,153,0,1)", width = 4), 
            mode = "lines", 
            type = "scatter", 
            uid = "f96c27", 
            xaxis = "x", 
            xsrc = "chnejohnson:0:eddd32", 
            yaxis = "y2", 
            ysrc = "chnejohnson:0:2bfc2d"
  ) %>%
  layout(
    autosize = TRUE, 
    hovermode = "closest", 
    margin = list(
      r = 120, 
      t = 30, 
      b = 0, 
      l = 170
    ), 
    showlegend = TRUE,
    legend = list(orientation = 'h',font = list(size = 46, cex = 30), xanchor = "center",  x = 0.5, y = -.08),
    title = "", 
    titlefont = list(color ="rgb(49, 61, 183)", size = 46, cex = 30), 
    xaxis = list(
      autorange = FALSE, 
      domain = c(0, 1), 
      range = c("1997", "2017"), 
      title = "", 
      type = "number", autotick = F, dtick = 3,tickangle = 0, ticks="inside",titlefont= list(size = 36, cex = 30)
    ), 
    yaxis = list(
      autorange = TRUE, 
      domain = c(0, 1), 
      range = c(0, 30), 
      title = "Millions USD", 
      type = "linear"
    ), yaxis2 = ay,
    xaxis = list(autotick = F, dtick = 2,tickangle = 90, titlefont= list(size = 36, cex = 30), range = c(1997, 2017)),  font = list(family = 'Arial', size = 36)
  )

hide_legend(p)
p

yaxis=layout$yaxis,



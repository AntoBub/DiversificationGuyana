

library(rjson)
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file=string)
reporters <- as.data.frame(t(sapply(reporters$results,rbind)))

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}

setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
Comtrade_Classification <- read.csv("UN Comtrade Commodity Classifications.csv", sep =",", header = T, stringsAsFactors = F)



####### LIVE ANIMALS


#Guyana <- get.Comtrade(r="328", p="0", fmt="csv", ps = c("1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,1010,2011,2012,2013,2014,015,2016,2017"),
#                     cc = c("01"))
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                       cc = c("01 ,	010111,	010119,	010120,	0102,	010210,	010290,	0103,	010310,	010391,	010392,	0104,	010410,	010420,	0105,	010511,	010519,	010591,	010599,	0106,	010600"))

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                       cc = c("01 ,	010111,	010119,	010120,	0102,	010210,	010290,	0103,	010310,	010391,	010392,	0104,	010410,	010420,	0105,	010511,	010519,	010591,	010599,	0106,	010600"))

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                       cc = c("01 ,	010111,	010119,	010120,	0102,	010210,	010290,	0103,	010310,	010391,	010392,	0104,	010410,	010420,	0105,	010511,	010519,	010591,	010599,	0106,	010600"))

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                       cc = c("01 ,	010111,	010119,	010120,	0102,	010210,	010290,	0103,	010310,	010391,	010392,	0104,	010410,	010420,	0105,	010511,	010519,	010591,	010599,	0106,	010600"))

World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World <-  rbind(World1a,World2a,World3a,World4a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:17)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-2.492e-06, 0, 1.042e-07, 7.105e-07,  7.807e-05 ), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=16,ncol=16,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-2.49e-06,0]", "(0,1.04e-07]", "(1.04e-07,7.11e-07]", "(7.11e-07,7.81e-05]" ))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2", "#08306B", "#808080")
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 14),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 0, r = -50, t = 10, b = 0))

hide_legend(p)



###################  Meat

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                       cc = c("02,	0201,	020110,	020120,	020130,	0202,	020210,	020220,	020230,	0203,	020311,	020312,	020319,	020321,	020322,	020329,	0204,	020410,	020421,	020422,	020423,	020430,	020441,	020442,	020443,	020450,	0205,	020500,	0206,	020610,	020621,	020622,	020629,	020630,	020641,	020649,	020680,	020690,	0207,	020710,	020721,	020722,	020723,	020731,	020739,	020741,	020742,	020743,	020750,	0208,	020810,	020820,	020890,	0209,	020900,	0210,	021011,	021012,	021019,	021020,	021090"))

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                       cc = c("02,	0201,	020110,	020120,	020130,	0202,	020210,	020220,	020230,	0203,	020311,	020312,	020319,	020321,	020322,	020329,	0204,	020410,	020421,	020422,	020423,	020430,	020441,	020442,	020443,	020450,	0205,	020500,	0206,	020610,	020621,	020622,	020629,	020630,	020641,	020649,	020680,	020690,	0207,	020710,	020721,	020722,	020723,	020731,	020739,	020741,	020742,	020743,	020750,	0208,	020810,	020820,	020890,	0209,	020900,	0210,	021011,	021012,	021019,	021020,	021090"))

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                       cc = c("02,	0201,	020110,	020120,	020130,	0202,	020210,	020220,	020230,	0203,	020311,	020312,	020319,	020321,	020322,	020329,	0204,	020410,	020421,	020422,	020423,	020430,	020441,	020442,	020443,	020450,	0205,	020500,	0206,	020610,	020621,	020622,	020629,	020630,	020641,	020649,	020680,	020690,	0207,	020710,	020721,	020722,	020723,	020731,	020739,	020741,	020742,	020743,	020750,	0208,	020810,	020820,	020890,	0209,	020900,	0210,	021011,	021012,	021019,	021020,	021090"))

World8 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                       cc = c("02,	0201,	020110,	020120,	020130,	0202,	020210,	020220,	020230,	0203,	020311,	020312,	020319,	020321,	020322,	020329,	0204,	020410,	020421,	020422,	020423,	020430,	020441,	020442,	020443,	020450,	0205,	020500,	0206,	020610,	020621,	020622,	020629,	020630,	020641,	020649,	020680,	020690,	0207,	020710,	020721,	020722,	020723,	020731,	020739,	020741,	020742,	020743,	020750,	0208,	020810,	020820,	020890,	0209,	020900,	0210,	021011,	021012,	021019,	021020,	021090"))

World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World8a <- World8$data
World <-  rbind(World5a,World6a,World7a,World8a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:18)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-7.589e-09, 0, 1.179e-10, 1.249e-09,  1.635e-08 ,2.236e-07), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=44,ncol=17,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-7.59e-09,0]", "(0,1.18e-10]", "(1.18e-10,1.25e-09]", "(1.25e-09,1.64e-08]", "(1.64e-08,2.24e-07]" ))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 10),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 400, r = -50, t = 50, b = 0))

hide_legend(p)




###################  Fish

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                       cc = c("03,	0301,	030110,	030191,	030192,	030193,	030199,	0302,	030211,	030212,	030219,	030221,	030222,	030223,	030229,	030231,	030232,	030233,	030239,	030240,	030250,	030261,	030262,
030263,	030264,	030265,	030266,	030269,	030270,	0303,	030310,	030321,	030322,	030329,	030331,	030332,	030333,	030339,	030341,	030342,	030343,	030349,	030350,	030360,	030371,	030372,	030373,	030374,	030375,	030376,	030377,	030378,	030379,	030380,	0304,	030410,	030420,	030490,	0305,	030510,	030520,	030530,	030541,	030542,	030549,	030551,	030559,	030561,	030562,	030563,	030569,	0306,	030611,	030612,	030613,	030614,	030619,	030621,	030622,	030623,	030624,	030629,	0307,	030710,	030721,	030729,	030731,	030739,	030741,	030749,	030751,	030759,	030760,	030791,	030799"))

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("03,	0301,	030110,	030191,	030192,	030193,	030199,	0302,	030211,	030212,	030219,	030221,	030222,	030223,	030229,	030231,	030232,	030233,	030239,	030240,	030250,	030261,	030262,
030263,	030264,	030265,	030266,	030269,	030270,	0303,	030310,	030321,	030322,	030329,	030331,	030332,	030333,	030339,	030341,	030342,	030343,	030349,	030350,	030360,	030371,	030372,	030373,	030374,	030375,	030376,	030377,	030378,	030379,	030380,	0304,	030410,	030420,	030490,	0305,	030510,	030520,	030530,	030541,	030542,	030549,	030551,	030559,	030561,	030562,	030563,	030569,	0306,	030611,	030612,	030613,	030614,	030619,	030621,	030622,	030623,	030624,	030629,	0307,	030710,	030721,	030729,	030731,	030739,	030741,	030749,	030751,	030759,	030760,	030791,	030799"))

World11 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("03,	0301,	030110,	030191,	030192,	030193,	030199,	0302,	030211,	030212,	030219,	030221,	030222,	030223,	030229,	030231,	030232,	030233,	030239,	030240,	030250,	030261,	030262,
030263,	030264,	030265,	030266,	030269,	030270,	0303,	030310,	030321,	030322,	030329,	030331,	030332,	030333,	030339,	030341,	030342,	030343,	030349,	030350,	030360,	030371,	030372,	030373,	030374,	030375,	030376,	030377,	030378,	030379,	030380,	0304,	030410,	030420,	030490,	0305,	030510,	030520,	030530,	030541,	030542,	030549,	030551,	030559,	030561,	030562,	030563,	030569,	0306,	030611,	030612,	030613,	030614,	030619,	030621,	030622,	030623,	030624,	030629,	0307,	030710,	030721,	030729,	030731,	030739,	030741,	030749,	030751,	030759,	030760,	030791,	030799"))

World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("03,	0301,	030110,	030191,	030192,	030193,	030199,	0302,	030211,	030212,	030219,	030221,	030222,	030223,	030229,	030231,	030232,	030233,	030239,	030240,	030250,	030261,	030262,
030263,	030264,	030265,	030266,	030269,	030270,	0303,	030310,	030321,	030322,	030329,	030331,	030332,	030333,	030339,	030341,	030342,	030343,	030349,	030350,	030360,	030371,	030372,	030373,	030374,	030375,	030376,	030377,	030378,	030379,	030380,	0304,	030410,	030420,	030490,	0305,	030510,	030520,	030530,	030541,	030542,	030549,	030551,	030559,	030561,	030562,	030563,	030569,	0306,	030611,	030612,	030613,	030614,	030619,	030621,	030622,	030623,	030624,	030629,	0307,	030710,	030721,	030729,	030731,	030739,	030741,	030749,	030751,	030759,	030760,	030791,	030799"))

World9a <- World9$data
World10a <- World10$data
World11a <- World11$data
World12a <- World12$data
World <-  rbind(World9a,World10a,World11a,World12a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

Guyana_World <- Guyana_World[-c(268,291,301),]


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-1.257e-05, -3.311e-07, 0, 4.840e-09 , 1.120e-06 , 3.073e-04), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=164,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-1.26e-05,-3.31e-07]", "(-3.31e-07,0]", "(0,4.84e-09]", "(4.84e-09,1.12e-06]", "(1.12e-06,0.000307]" ))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 7),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 615, r = -50, t = 50, b = 0))

hide_legend(p)


###################  Dairy Products

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                       cc = c("04,	0401,	040110,	040120,	040130,	0402,	040210,	040221,	040229,	040291,	040299,	0403,	040310,	040390,	0404,	040410,	040490,	0405,	040500,	0406,	040610,	040620,	040630,	040640,	040690,	0407,	040700,	0408,	040811,	040819,	040891,	040899,	0409,	040900,	0410,	041000"))
World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("04,	0401,	040110,	040120,	040130,	0402,	040210,	040221,	040229,	040291,	040299,	0403,	040310,	040390,	0404,	040410,	040490,	0405,	040500,	0406,	040610,	040620,	040630,	040640,	040690,	0407,	040700,	0408,	040811,	040819,	040891,	040899,	0409,	040900,	0410,	041000"))

World15 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("04,	0401,	040110,	040120,	040130,	0402,	040210,	040221,	040229,	040291,	040299,	0403,	040310,	040390,	0404,	040410,	040490,	0405,	040500,	0406,	040610,	040620,	040630,	040640,	040690,	0407,	040700,	0408,	040811,	040819,	040891,	040899,	0409,	040900,	0410,	041000"))

World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("04,	0401,	040110,	040120,	040130,	0402,	040210,	040221,	040229,	040291,	040299,	0403,	040310,	040390,	0404,	040410,	040490,	0405,	040500,	0406,	040610,	040620,	040630,	040640,	040690,	0407,	040700,	0408,	040811,	040819,	040891,	040899,	0409,	040900,	0410,	041000"))

World13a <- World13$data
World14a <- World14$data
World15a <- World15$data
World16a <- World16$data

World <-  rbind(World13a,World14a,World15a,World16a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-1.657e-07, -1.494e-09, 0, 2.226e-09, 2.993e-08, 2.629e-06), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=45,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-1.66e-07,-1.49e-09]", "(-1.49e-09,0]", "(0,2.23e-09]", "(2.23e-09,2.99e-08]", "(2.99e-08,2.63e-06]" ))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 7.5),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 615, r = -50, t = 50, b = 0))

hide_legend(p)




###################  Animal Originated Products

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("05,	0501,	050100,	0502,	050210,	050290,	0503,	050300,	0504,	050400,	0505,	050510,	050590,	0506,	050610,	050690,	0507,	050710,	050790,	0508,	050800,	0509,	050900,	0510,	051000,	0511,	051110,	051191,	051199"))

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("05,	0501,	050100,	0502,	050210,	050290,	0503,	050300,	0504,	050400,	0505,	050510,	050590,	0506,	050610,	050690,	0507,	050710,	050790,	0508,	050800,	0509,	050900,	0510,	051000,	0511,	051110,	051191,	051199"))

World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("05,	0501,	050100,	0502,	050210,	050290,	0503,	050300,	0504,	050400,	0505,	050510,	050590,	0506,	050610,	050690,	0507,	050710,	050790,	0508,	050800,	0509,	050900,	0510,	051000,	0511,	051110,	051191,	051199"))

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("05,	0501,	050100,	0502,	050210,	050290,	0503,	050300,	0504,	050400,	0505,	050510,	050590,	0506,	050610,	050690,	0507,	050710,	050790,	0508,	050800,	0509,	050900,	0510,	051000,	0511,	051110,	051191,	051199"))

World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data

World <-  rbind(World17a,World18a,World19a,World20a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:21)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-5.101e-08, 0, 1.404e-10, 4.742e-08, 3.292e-07, 2.309e-06), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=18,ncol=20,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-5.1e-08,0]", "(0,1.4e-10]", "(1.4e-10,4.74e-08]", "(4.74e-08,3.29e-07]", "(3.29e-07,2.31e-06]" ))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 7.5),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 750, r = -50, t = 50, b = 0))

hide_legend(p)


###################  Trees and other plants

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c(	"06,	0601,	060110,	060120,	0602,	060210,	060220,	060230,	060240,	060291,	060299,	0603,	060310,	060390,	0604,	060410,	060491,	060499"))

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c(	"06,	0601,	060110,	060120,	0602,	060210,	060220,	060230,	060240,	060291,	060299,	0603,	060310,	060390,	0604,	060410,	060491,	060499"))

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c(	"06,	0601,	060110,	060120,	0602,	060210,	060220,	060230,	060240,	060291,	060299,	0603,	060310,	060390,	0604,	060410,	060491,	060499"))

World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c(	"06,	0601,	060110,	060120,	0602,	060210,	060220,	060230,	060240,	060291,	060299,	0603,	060310,	060390,	0604,	060410,	060491,	060499"))

World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data

World <-  rbind(World21a,World22a,World23a,World24a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:21)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-3.680e-09, 0, 1.220e-09, 9.541e-09, 3.490e-08, 5.607e-07), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=19,ncol=20,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-3.68e-09,0]", "(0,1.22e-09]", "(1.22e-09,9.54e-09]", "(9.54e-09,3.49e-08]", "(3.49e-08,5.61e-07]" ))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 5),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)



###################  Vegetables and roots and tubers

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("07,	0701,	070110,	070190,	0702,	070200,	0703,	070310,	070320,	070390,	0704,	070410,	070420,	070490,	0705,	070511,	070519,	070521,	070529,	0706,	070610,	070690,	0707,	070700,	0708,	070810,	070820,	070890,	0709,	070910,	070920,	070930,	070940,	070951,	070952,	070960,	070970,	070990,	0710,	071010,	071021,	071022,	071029,	071030,	071040,	071080,	071090,	0711,	071110,	071120,	071130,	071140,	071190,	0712,	071210,	071220,	071230,	071290,	0713,	071310,	071320,	071331,	071332,	071333,	071339,	071340,	071350,	071390,	0714,	071410,	071420,	071490"))

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("07,	0701,	070110,	070190,	0702,	070200,	0703,	070310,	070320,	070390,	0704,	070410,	070420,	070490,	0705,	070511,	070519,	070521,	070529,	0706,	070610,	070690,	0707,	070700,	0708,	070810,	070820,	070890,	0709,	070910,	070920,	070930,	070940,	070951,	070952,	070960,	070970,	070990,	0710,	071010,	071021,	071022,	071029,	071030,	071040,	071080,	071090,	0711,	071110,	071120,	071130,	071140,	071190,	0712,	071210,	071220,	071230,	071290,	0713,	071310,	071320,	071331,	071332,	071333,	071339,	071340,	071350,	071390,	0714,	071410,	071420,	071490"))

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("07,	0701,	070110,	070190,	0702,	070200,	0703,	070310,	070320,	070390,	0704,	070410,	070420,	070490,	0705,	070511,	070519,	070521,	070529,	0706,	070610,	070690,	0707,	070700,	0708,	070810,	070820,	070890,	0709,	070910,	070920,	070930,	070940,	070951,	070952,	070960,	070970,	070990,	0710,	071010,	071021,	071022,	071029,	071030,	071040,	071080,	071090,	0711,	071110,	071120,	071130,	071140,	071190,	0712,	071210,	071220,	071230,	071290,	0713,	071310,	071320,	071331,	071332,	071333,	071339,	071340,	071350,	071390,	0714,	071410,	071420,	071490"))

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("07,	0701,	070110,	070190,	0702,	070200,	0703,	070310,	070320,	070390,	0704,	070410,	070420,	070490,	0705,	070511,	070519,	070521,	070529,	0706,	070610,	070690,	0707,	070700,	0708,	070810,	070820,	070890,	0709,	070910,	070920,	070930,	070940,	070951,	070952,	070960,	070970,	070990,	0710,	071010,	071021,	071022,	071029,	071030,	071040,	071080,	071090,	0711,	071110,	071120,	071130,	071140,	071190,	0712,	071210,	071220,	071230,	071290,	0713,	071310,	071320,	071331,	071332,	071333,	071339,	071340,	071350,	071390,	0714,	071410,	071420,	071490"))

World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data

World <-  rbind(World25a,World26a,World27a,World28a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-8.158e-07, -1.067e-08, 0, 2.532e-09, 1.281e-07, 7.782e-06), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=108,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-8.16e-07,-1.07e-08]", "(-1.07e-08,0]", "(0,2.53e-09]", "(2.53e-09,1.28e-07]", "(1.28e-07,7.78e-06]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)



###################  Fruits and Nuts

World29 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("08,	0801,	080110,	080120,	080130,	0802,	080211,	080212,	080221,	080222,	080231,	080232,	080240,	080250,	080290,	0803,	080300,	0804,	080410,	080420,	080430,	080440,	080450,	0805,	080510,	080520,	080530,	080540,	080590,	0806,	080610,	080620,	0807,	080710,	080720,	0808,	080810,	080820,	0809,	080910,	080920,	080930,	080940,	0810,	081010,	081020,	081030,	081040,	081090,	0811,	081110,	081120,	081190,	0812,	081210,	081220,	081290,	0813,	081310,	081320,	081330,	081340,	081350,	0814,	081400"))

World30 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("08,	0801,	080110,	080120,	080130,	0802,	080211,	080212,	080221,	080222,	080231,	080232,	080240,	080250,	080290,	0803,	080300,	0804,	080410,	080420,	080430,	080440,	080450,	0805,	080510,	080520,	080530,	080540,	080590,	0806,	080610,	080620,	0807,	080710,	080720,	0808,	080810,	080820,	0809,	080910,	080920,	080930,	080940,	0810,	081010,	081020,	081030,	081040,	081090,	0811,	081110,	081120,	081190,	0812,	081210,	081220,	081290,	0813,	081310,	081320,	081330,	081340,	081350,	0814,	081400"))

World31 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("08,	0801,	080110,	080120,	080130,	0802,	080211,	080212,	080221,	080222,	080231,	080232,	080240,	080250,	080290,	0803,	080300,	0804,	080410,	080420,	080430,	080440,	080450,	0805,	080510,	080520,	080530,	080540,	080590,	0806,	080610,	080620,	0807,	080710,	080720,	0808,	080810,	080820,	0809,	080910,	080920,	080930,	080940,	0810,	081010,	081020,	081030,	081040,	081090,	0811,	081110,	081120,	081190,	0812,	081210,	081220,	081290,	0813,	081310,	081320,	081330,	081340,	081350,	0814,	081400"))

World32 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("08,	0801,	080110,	080120,	080130,	0802,	080211,	080212,	080221,	080222,	080231,	080232,	080240,	080250,	080290,	0803,	080300,	0804,	080410,	080420,	080430,	080440,	080450,	0805,	080510,	080520,	080530,	080540,	080590,	0806,	080610,	080620,	0807,	080710,	080720,	0808,	080810,	080820,	0809,	080910,	080920,	080930,	080940,	0810,	081010,	081020,	081030,	081040,	081090,	0811,	081110,	081120,	081190,	0812,	081210,	081220,	081290,	0813,	081310,	081320,	081330,	081340,	081350,	0814,	081400"))

World29a <- World29$data
World30a <- World30$data
World31a <- World31$data
World32a <- World32$data

World <-  rbind(World29a,World30a,World31a,World32a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-5.667e-07, -1.242e-08, 0, 5.977e-09, 2.181e-07, 5.052e-06), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=75,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-5.67e-07,-1.24e-08]", "(-1.24e-08,0]", "(0,5.98e-09]", "(5.98e-09,2.18e-07]", "(2.18e-07,5.05e-06]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)


###################  Coffe, tea, mate, spice

World33 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("09,	0901,	090111,	090112,	090121,	090122,	090130,	090140,	0902,	090210,	090220,	090230,	090240,	0903,	090300,	0904,	090411,	090412,	090420,	0905,	090500,	0906,	090610,	090620,	0907,	090700,	0908,	090810,	090820,	090830,	0909,	090910,	090920,	090930,	090940,	090950,	0910,	091010,	091020,	091030,	091040,	091050,	091091,	091099"))

World34 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("09,	0901,	090111,	090112,	090121,	090122,	090130,	090140,	0902,	090210,	090220,	090230,	090240,	0903,	090300,	0904,	090411,	090412,	090420,	0905,	090500,	0906,	090610,	090620,	0907,	090700,	0908,	090810,	090820,	090830,	0909,	090910,	090920,	090930,	090940,	090950,	0910,	091010,	091020,	091030,	091040,	091050,	091091,	091099"))

World35 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("09,	0901,	090111,	090112,	090121,	090122,	090130,	090140,	0902,	090210,	090220,	090230,	090240,	0903,	090300,	0904,	090411,	090412,	090420,	0905,	090500,	0906,	090610,	090620,	0907,	090700,	0908,	090810,	090820,	090830,	0909,	090910,	090920,	090930,	090940,	090950,	0910,	091010,	091020,	091030,	091040,	091050,	091091,	091099"))

World36 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("09,	0901,	090111,	090112,	090121,	090122,	090130,	090140,	0902,	090210,	090220,	090230,	090240,	0903,	090300,	0904,	090411,	090412,	090420,	0905,	090500,	0906,	090610,	090620,	0907,	090700,	0908,	090810,	090820,	090830,	0909,	090910,	090920,	090930,	090940,	090950,	0910,	091010,	091020,	091030,	091040,	091050,	091091,	091099"))

World33a <- World33$data
World34a <- World34$data
World35a <- World35$data
World36a <- World36$data

World <-  rbind(World33a,World34a,World35a,World36a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

Guyana_World <- Guyana_World[-164,]
library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-1.468e-06, -8.745e-09, 0, 1.007e-08, 2.865e-07,  6.590e-06 ), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=54,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-1.47e-06,-8.74e-09]", "(-8.74e-09,0]","(0,1.01e-08]","(1.01e-08,2.86e-07]", "(2.86e-07,6.59e-06]" ))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)




###################  Cereals

World37 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("10,	1001,	100110,	100190,	1002,	100200,	1003,	100300,	1004,	100400,	1005,	100510,	100590,	1006,	100610,	100620,	100630,	100640,	1007,	100700,	1008,	100810,	100820,	100830,	100890"))

World38 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("10,	1001,	100110,	100190,	1002,	100200,	1003,	100300,	1004,	100400,	1005,	100510,	100590,	1006,	100610,	100620,	100630,	100640,	1007,	100700,	1008,	100810,	100820,	100830,	100890"))

World39 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("10,	1001,	100110,	100190,	1002,	100200,	1003,	100300,	1004,	100400,	1005,	100510,	100590,	1006,	100610,	100620,	100630,	100640,	1007,	100700,	1008,	100810,	100820,	100830,	100890"))

World40 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("10,	1001,	100110,	100190,	1002,	100200,	1003,	100300,	1004,	100400,	1005,	100510,	100590,	1006,	100610,	100620,	100630,	100640,	1007,	100700,	1008,	100810,	100820,	100830,	100890"))

World37a <- World37$data
World38a <- World38$data
World39a <- World39$data
World40a <- World40$data

World <-  rbind(World37a,World38a,World39a,World40a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-7.600e-05, -2.254e-06, 0,  6.640e-08,  8.904e-05,  7.675e-04), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=25,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-7.6e-05,-2.25e-06]", "(-2.25e-06,0]","(0,6.64e-08]", "(6.64e-08,8.9e-05]", "(8.9e-05,0.000767]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)


###################  Products of the milling industry

World41 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("11,	1101,	110100,	1102,	110210,	110220,	110230,	110290,	1103,	110311,	110312,	110313,	110314,	110319,	110321,	110329,	1104,	110411,	110412,	110419,	110421,	110422,	110423,	110429,	110430,	1105,	110510,	110520,	1106,	110610,	110620,	110630,	1107,	110710,	110720,	1108,	110811,	110812,	110813,	110814,	110819,	110820,	1109,	110900"))

World42 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("11,	1101,	110100,	1102,	110210,	110220,	110230,	110290,	1103,	110311,	110312,	110313,	110314,	110319,	110321,	110329,	1104,	110411,	110412,	110419,	110421,	110422,	110423,	110429,	110430,	1105,	110510,	110520,	1106,	110610,	110620,	110630,	1107,	110710,	110720,	1108,	110811,	110812,	110813,	110814,	110819,	110820,	1109,	110900"))

World43 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("11,	1101,	110100,	1102,	110210,	110220,	110230,	110290,	1103,	110311,	110312,	110313,	110314,	110319,	110321,	110329,	1104,	110411,	110412,	110419,	110421,	110422,	110423,	110429,	110430,	1105,	110510,	110520,	1106,	110610,	110620,	110630,	1107,	110710,	110720,	1108,	110811,	110812,	110813,	110814,	110819,	110820,	1109,	110900"))

World44 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("11,	1101,	110100,	1102,	110210,	110220,	110230,	110290,	1103,	110311,	110312,	110313,	110314,	110319,	110321,	110329,	1104,	110411,	110412,	110419,	110421,	110422,	110423,	110429,	110430,	1105,	110510,	110520,	1106,	110610,	110620,	110630,	1107,	110710,	110720,	1108,	110811,	110812,	110813,	110814,	110819,	110820,	1109,	110900"))

World41a <- World41$data
World42a <- World42$data
World43a <- World43$data
World44a <- World44$data

World <-  rbind(World41a,World42a,World43a,World44a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Guyana$Trade.Value..US.. <- as.numeric(Guyana$Trade.Value..US..)
Total_Trade_Guyana <- aggregate(as.numeric(Guyana$Trade.Value..US..), by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
World$World_Export <- as.numeric(World$World_Export)
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=as.character(World$Year)), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-4.7e-05,  0.0e+00 ,1.0e-06,  8.2e-05), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=50,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-4.7e-05,0]", "(0,1e-06]" ,"(1e-06,8.2e-05]" ))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" )
#, "#D8E7F5",  "#CFE1F2"
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15")
#,"#F0E68C", "#2db66e"
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)


###################  Oil Seeds

World45 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("12,	1201,	120100,	1202,	120210,	120220,	1203,	120300,	1204,	120400,	1205,	120500,	1206,	120600,	1207,	120710,	120720,	120730,	120740,	120750,	120760,	120791,	120792,	120799,	1208,	120810,	120890,	1209,	120911,	120919,	120921,	120922,	120923,	120924,	120925,	120926,	120929,	120930,	120991,	120999,	1210,	121010,	121020,	1211,	121110,	121120,	121190,	1212,	121210,	121220,	121230,	121291,	121292,	121299,	1213,	121300,	1214,	121410,	121490"))

World46 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("12,	1201,	120100,	1202,	120210,	120220,	1203,	120300,	1204,	120400,	1205,	120500,	1206,	120600,	1207,	120710,	120720,	120730,	120740,	120750,	120760,	120791,	120792,	120799,	1208,	120810,	120890,	1209,	120911,	120919,	120921,	120922,	120923,	120924,	120925,	120926,	120929,	120930,	120991,	120999,	1210,	121010,	121020,	1211,	121110,	121120,	121190,	1212,	121210,	121220,	121230,	121291,	121292,	121299,	1213,	121300,	1214,	121410,	121490"))

World47 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("12,	1201,	120100,	1202,	120210,	120220,	1203,	120300,	1204,	120400,	1205,	120500,	1206,	120600,	1207,	120710,	120720,	120730,	120740,	120750,	120760,	120791,	120792,	120799,	1208,	120810,	120890,	1209,	120911,	120919,	120921,	120922,	120923,	120924,	120925,	120926,	120929,	120930,	120991,	120999,	1210,	121010,	121020,	1211,	121110,	121120,	121190,	1212,	121210,	121220,	121230,	121291,	121292,	121299,	1213,	121300,	1214,	121410,	121490"))

World48 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("12,	1201,	120100,	1202,	120210,	120220,	1203,	120300,	1204,	120400,	1205,	120500,	1206,	120600,	1207,	120710,	120720,	120730,	120740,	120750,	120760,	120791,	120792,	120799,	1208,	120810,	120890,	1209,	120911,	120919,	120921,	120922,	120923,	120924,	120925,	120926,	120929,	120930,	120991,	120999,	1210,	121010,	121020,	1211,	121110,	121120,	121190,	1212,	121210,	121220,	121230,	121291,	121292,	121299,	1213,	121300,	1214,	121410,	121490"))

World45a <- World45$data
World46a <- World46$data
World47a <- World47$data
World48a <- World48$data

World <-  rbind(World45a,World46a,World47a,World48a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-1.391e-06, -1.513e-09, 0,  1.083e-08,  1.070e-07,  1.274e-05 ), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=51,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-1.39e-06,-1.51e-09]", "(-1.51e-09,0]", "(0,1.08e-08]","(1.08e-08,1.07e-07]" ,"(1.07e-07,1.27e-05]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)




###################  Lac, Gums, resins

World49 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("13,	1301,	130110,	130120,	130190,	1302,	130211,	130212,	130213,	130214,	130219,	130220,	130231,	130232,	130239"))

World50 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("13,	1301,	130110,	130120,	130190,	1302,	130211,	130212,	130213,	130214,	130219,	130220,	130231,	130232,	130239"))

World51 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("13,	1301,	130110,	130120,	130190,	1302,	130211,	130212,	130213,	130214,	130219,	130220,	130231,	130232,	130239"))

World52 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("13,	1301,	130110,	130120,	130190,	1302,	130211,	130212,	130213,	130214,	130219,	130220,	130231,	130232,	130239"))

World49a <- World49$data
World50a <- World50$data
World51a <- World51$data
World52a <- World52$data

World <-  rbind(World49a,World50a,World51a,World52a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:13)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(0, 1.570e-10, 1.155e-08, 2.129e-06, 6.256e-07, 2.952e-05  ), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=8,ncol=12,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("(1.57e-10,1.15e-08]","(1.15e-08,6.26e-07]", "(6.26e-07,2.13e-06]","(2.13e-06,2.95e-05]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)




###################  Vegetable Plaiting Materials

World53 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("14,	1401,	140110,	140120,	140190,	1402,
                               140210,	140291,	140299,	1403,	140310,	140390,	1404,	140410,	140420,	140490"))

World54 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("14,	1401,	140110,	140120,	140190,	1402,
                               140210,	140291,	140299,	1403,	140310,	140390,	1404,	140410,	140420,	140490"))

World55 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("14,	1401,	140110,	140120,	140190,	1402,
                               140210,	140291,	140299,	1403,	140310,	140390,	1404,	140410,	140420,	140490"))

World56 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("14,	1401,	140110,	140120,	140190,	1402,
                               140210,	140291,	140299,	1403,	140310,	140390,	1404,	140410,	140420,	140490"))

World53a <- World53$data
World54a <- World54$data
World55a <- World55$data
World56a <- World56$data

World <-  rbind(World53a,World54a,World55a,World56a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Trade.Value..US.. <- as.numeric(Guyana_World$Trade.Value..US..)
Guyana_World$Tot_Export_World <- as.numeric(Guyana_World$Tot_Export_World)
Guyana_World$World_Export <- as.numeric(Guyana_World$World_Export)
Guyana_World$Tot_Export_Guyana <- as.numeric(Guyana_World$Tot_Export_Guyana)
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:16)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-0.000004 , 0.000011 ,0.000172  ), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=6,ncol=15,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-4e-06,1.1e-05]", "(1.1e-05,0.000172]" ))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)




###################  Animal or vegetable fats

World57 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("15,	1501,	150100,	1502,	150200,	1503,	150300,	1504,	150410,	150420,	150430,	1505,	150510,	150590,	1506,	150600,	1507,	150710,	150790,	1508,	150810,	150890,	1509,	150910,	150990,	1510,	151000,	1511,	151110,	151190,	1512,	151211,	151219,	151221,	151229,	1513,	151311,	151319,	151321,	151329,	1514,	151410,	151490,	1515,	151511,	151519,	151521,	151529,	151530,	151540,	151550,	151560,	151590,	1516,	151610,	151620,	1517,	151710,	151790,	1518,	151800,	1519,	151911,	151912,	151913,	151919,	151920,	151930,	1520,	152010,	152090,	1521,	152110,	152190,	1522,	152200"))

World58 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("15,	1501,	150100,	1502,	150200,	1503,	150300,	1504,	150410,	150420,	150430,	1505,	150510,	150590,	1506,	150600,	1507,	150710,	150790,	1508,	150810,	150890,	1509,	150910,	150990,	1510,	151000,	1511,	151110,	151190,	1512,	151211,	151219,	151221,	151229,	1513,	151311,	151319,	151321,	151329,	1514,	151410,	151490,	1515,	151511,	151519,	151521,	151529,	151530,	151540,	151550,	151560,	151590,	1516,	151610,	151620,	1517,	151710,	151790,	1518,	151800,	1519,	151911,	151912,	151913,	151919,	151920,	151930,	1520,	152010,	152090,	1521,	152110,	152190,	1522,	152200"))

World59 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("15,	1501,	150100,	1502,	150200,	1503,	150300,	1504,	150410,	150420,	150430,	1505,	150510,	150590,	1506,	150600,	1507,	150710,	150790,	1508,	150810,	150890,	1509,	150910,	150990,	1510,	151000,	1511,	151110,	151190,	1512,	151211,	151219,	151221,	151229,	1513,	151311,	151319,	151321,	151329,	1514,	151410,	151490,	1515,	151511,	151519,	151521,	151529,	151530,	151540,	151550,	151560,	151590,	1516,	151610,	151620,	1517,	151710,	151790,	1518,	151800,	1519,	151911,	151912,	151913,	151919,	151920,	151930,	1520,	152010,	152090,	1521,	152110,	152190,	1522,	152200"))

World60 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("15,	1501,	150100,	1502,	150200,	1503,	150300,	1504,	150410,	150420,	150430,	1505,	150510,	150590,	1506,	150600,	1507,	150710,	150790,	1508,	150810,	150890,	1509,	150910,	150990,	1510,	151000,	1511,	151110,	151190,	1512,	151211,	151219,	151221,	151229,	1513,	151311,	151319,	151321,	151329,	1514,	151410,	151490,	1515,	151511,	151519,	151521,	151529,	151530,	151540,	151550,	151560,	151590,	1516,	151610,	151620,	1517,	151710,	151790,	1518,	151800,	1519,	151911,	151912,	151913,	151919,	151920,	151930,	1520,	152010,	152090,	1521,	152110,	152190,	1522,	152200"))

World57a <- World57$data
World58a <- World58$data
World59a <- World59$data
World60a <- World60$data

World <-  rbind(World57a,World58a,World59a,World60a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-5.356e-07, -1.482e-08,0,4.030e-10,1.342e-07,  1.576e-05), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=69,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-5.36e-07,-1.48e-08]", "(-1.48e-08,0]" ,"(0,4.03e-10]","(4.03e-10,1.34e-07]","(1.34e-07,1.58e-05]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)




###################  Preparations of meat and crustaceans and molluscs

World61 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("16,	1601,	160100,	1602,	160210,	160220,	160231,	160239,	160241,	160242,	160249,	160250,	160290,	1603,	160300,	1604,	160411,	160412,	160413,	160414,	160415,	160416,	160419,	160420,	160430,	1605,	160510,	160520,	160530,	160540,	160590"))

World62 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("16,	1601,	160100,	1602,	160210,	160220,	160231,	160239,	160241,	160242,	160249,	160250,	160290,	1603,	160300,	1604,	160411,	160412,	160413,	160414,	160415,	160416,	160419,	160420,	160430,	1605,	160510,	160520,	160530,	160540,	160590"))

World63 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("16,	1601,	160100,	1602,	160210,	160220,	160231,	160239,	160241,	160242,	160249,	160250,	160290,	1603,	160300,	1604,	160411,	160412,	160413,	160414,	160415,	160416,	160419,	160420,	160430,	1605,	160510,	160520,	160530,	160540,	160590"))

World64 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("16,	1601,	160100,	1602,	160210,	160220,	160231,	160239,	160241,	160242,	160249,	160250,	160290,	1603,	160300,	1604,	160411,	160412,	160413,	160414,	160415,	160416,	160419,	160420,	160430,	1605,	160510,	160520,	160530,	160540,	160590"))

World61a <- World61$data
World62a <- World62$data
World63a <- World63$data
World64a <- World64$data

World <-  rbind(World61a,World62a,World63a,World64a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-2.603e-08,0, 1.930e-10,  2.483e-09,2.100e-08,  5.835e-06 ), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=37,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-2.6e-08,0]", "(0,1.93e-10]", "(1.93e-10,2.48e-09]","(2.48e-09,2.1e-08]","(2.1e-08,5.84e-06]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)



###################  Sugar and sugar confectionary

World65 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("17,	1701,	170111,	170112,	170191,	170199,	1702,	170210,	170220,	170230,	170240,	170250,	170260,	170290,	1703,	170310,	170390,	1704,	170410,	170490"))

World66 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("17,	1701,	170111,	170112,	170191,	170199,	1702,	170210,	170220,	170230,	170240,	170250,	170260,	170290,	1703,	170310,	170390,	1704,	170410,	170490"))

World67 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("17,	1701,	170111,	170112,	170191,	170199,	1702,	170210,	170220,	170230,	170240,	170250,	170260,	170290,	1703,	170310,	170390,	1704,	170410,	170490"))

World68 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("17,	1701,	170111,	170112,	170191,	170199,	1702,	170210,	170220,	170230,	170240,	170250,	170260,	170290,	1703,	170310,	170390,	1704,	170410,	170490"))

World65a <- World65$data
World66a <- World66$data
World67a <- World67$data
World68a <- World68$data

World <-  rbind(World65a,World66a,World67a,World68a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-4.081e-04, -1.010e-05, -1.630e-07,0, 1.196e-05,  3.183e-03 ), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=29,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-0.000408,-1.01e-05]", "(-1.01e-05,-1.63e-07]", "(-1.63e-07,0]","(0,1.2e-05]", "(1.2e-05,0.00318]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)



###################  Cocoa and Cocoa Preparations

World69 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("18,	1801,	180100,	1802,	180200,	1803,	180310,	180320,	1804,	180400,	1805,	180500,	1806,	180610,	180620,	180631,	180632,	180690"))

World70 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("18,	1801,	180100,	1802,	180200,	1803,	180310,	180320,	1804,	180400,	1805,	180500,	1806,	180610,	180620,	180631,	180632,	180690"))

World71 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("18,	1801,	180100,	1802,	180200,	1803,	180310,	180320,	1804,	180400,	1805,	180500,	1806,	180610,	180620,	180631,	180632,	180690"))

World72 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("18,	1801,	180100,	1802,	180200,	1803,	180310,	180320,	1804,	180400,	1805,	180500,	1806,	180610,	180620,	180631,	180632,	180690"))

World69a <- World69$data
World70a <- World70$data
World71a <- World71$data
World72a <- World72$data

World <-  rbind(World69a,World70a,World71a,World72a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:20)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-2.115e-09,0,2.697e-10,1.249e-09,7.671e-09,4.622e-07), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=19,ncol=19,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-2.11e-09,0]", "(0,2.7e-10]", "(2.7e-10,1.25e-09]", "(1.25e-09,7.67e-09]", "(7.67e-09,4.62e-07]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)



###################  Preparations of cereals

World93 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("19,	1901,	190110,	190120,	190190,	1902,	190211,	190219,	190220,	190230,	190240,	1903,	190300,	1904,	190410,	190490,	1905,	190510,	190520,	190530,	190540,	190590"))

World94 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("19,	1901,	190110,	190120,	190190,	1902,	190211,	190219,	190220,	190230,	190240,	1903,	190300,	1904,	190410,	190490,	1905,	190510,	190520,	190530,	190540,	190590"))

World95 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("19,	1901,	190110,	190120,	190190,	1902,	190211,	190219,	190220,	190230,	190240,	1903,	190300,	1904,	190410,	190490,	1905,	190510,	190520,	190530,	190540,	190590"))

World96 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("19,	1901,	190110,	190120,	190190,	1902,	190211,	190219,	190220,	190230,	190240,	1903,	190300,	1904,	190410,	190490,	1905,	190510,	190520,	190530,	190540,	190590"))

World93a <- World93$data
World94a <- World94$data
World95a <- World95$data
World96a <- World96$data

World <-  rbind(World93a,World94a,World95a,World96a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-2.747e-06, -4.395e-08,0,5.810e-10,2.906e-07,  1.873e-05 ), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=37,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-2.75e-06,-4.39e-08]", "(-4.39e-08,0]", "(0,5.81e-10]","(5.81e-10,2.91e-07]", "(2.91e-07,1.87e-05]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)



###################  Preparations of veetables, fruits, nuts

World73 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("20,	2001,	200110,	200120,	200190,	2002,	200210,	200290,	2003,	200310,	200320,	2004,	200410,	200490,	2005,	200510,	200520,	200530,	200540,	200551,	200559,	200560,	200570,	200580,	200590,	2006,	200600,	2007,	200710,	200791,	200799,	2008,	200811,	200819,	200820,	200830,	200840,	200850,	200860,	200870,	200880,	200891,	200892,	200899,	2009,	200911,	200919,	200920,	200930,	200940,	200950,	200960,	200970,	200980,	200990"))

World74 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("20,	2001,	200110,	200120,	200190,	2002,	200210,	200290,	2003,	200310,	200320,	2004,	200410,	200490,	2005,	200510,	200520,	200530,	200540,	200551,	200559,	200560,	200570,	200580,	200590,	2006,	200600,	2007,	200710,	200791,	200799,	2008,	200811,	200819,	200820,	200830,	200840,	200850,	200860,	200870,	200880,	200891,	200892,	200899,	2009,	200911,	200919,	200920,	200930,	200940,	200950,	200960,	200970,	200980,	200990"))

World75 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("20,	2001,	200110,	200120,	200190,	2002,	200210,	200290,	2003,	200310,	200320,	2004,	200410,	200490,	2005,	200510,	200520,	200530,	200540,	200551,	200559,	200560,	200570,	200580,	200590,	2006,	200600,	2007,	200710,	200791,	200799,	2008,	200811,	200819,	200820,	200830,	200840,	200850,	200860,	200870,	200880,	200891,	200892,	200899,	2009,	200911,	200919,	200920,	200930,	200940,	200950,	200960,	200970,	200980,	200990"))

World76 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("20,	2001,	200110,	200120,	200190,	2002,	200210,	200290,	2003,	200310,	200320,	2004,	200410,	200490,	2005,	200510,	200520,	200530,	200540,	200551,	200559,	200560,	200570,	200580,	200590,	2006,	200600,	2007,	200710,	200791,	200799,	2008,	200811,	200819,	200820,	200830,	200840,	200850,	200860,	200870,	200880,	200891,	200892,	200899,	2009,	200911,	200919,	200920,	200930,	200940,	200950,	200960,	200970,	200980,	200990"))

World73a <- World73$data
World74a <- World74$data
World75a <- World75$data
World76a <- World76$data

World <-  rbind(World73a,World74a,World75a,World76a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-1.134e-06, -1.611e-07, -1.200e-10,0,4.332e-08,  6.178e-05 ), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=85,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-1.13e-06,-1.61e-07]", "(-1.61e-07,-1.2e-10]", "(-1.2e-10,0]", "(0,4.33e-08]","(4.33e-08,6.18e-05]" ))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)




###################  Miscellaneous edible preparations

World77 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("21,	2101,	210110,	210120,	210130,	2102,	210210,	210220,	210230,	2103,	210310,	210320,	210330,	210390,	2104,	210410,	210420,	2105,	210500,	2106,	210610,	210690"))

World78 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("21,	2101,	210110,	210120,	210130,	2102,	210210,	210220,	210230,	2103,	210310,	210320,	210330,	210390,	2104,	210410,	210420,	2105,	210500,	2106,	210610,	210690"))

World79 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("21,	2101,	210110,	210120,	210130,	2102,	210210,	210220,	210230,	2103,	210310,	210320,	210330,	210390,	2104,	210410,	210420,	2105,	210500,	2106,	210610,	210690"))

World80 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("21,	2101,	210110,	210120,	210130,	2102,	210210,	210220,	210230,	2103,	210310,	210320,	210330,	210390,	2104,	210410,	210420,	2105,	210500,	2106,	210610,	210690"))

World77a <- World77$data
World78a <- World78$data
World79a <- World79$data
World80a <- World80$data

World <-  rbind(World77a,World78a,World79a,World80a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

#Guyana_World <- Guyana_World[-c(268,291,301),]

library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-2.826e-07, -3.528e-09,0,  1.919e-08  , 2.296e-07,  1.639e-05 ), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=34,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-2.83e-07,-3.53e-09]", "(-3.53e-09,0]" ,"(0,1.92e-08]", "(1.92e-08,2.3e-07]","(2.3e-07,1.64e-05]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)




###################  Beverages, spirits and vinegar

World81 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("22,	2201,	220110,	220190,	2202,	220210,	220290,	2203,	220300,	2204,	220410,	220421,	220429,	220430,	2205,	220510,	220590,	2206,	220600,	2207,	220710,	220720,	2208,	220810,	220820,	220830,	220840,	220850,	220890,	2209,	220900"))

World82 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("22,	2201,	220110,	220190,	2202,	220210,	220290,	2203,	220300,	2204,	220410,	220421,	220429,	220430,	2205,	220510,	220590,	2206,	220600,	2207,	220710,	220720,	2208,	220810,	220820,	220830,	220840,	220850,	220890,	2209,	220900"))

World83 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("22,	2201,	220110,	220190,	2202,	220210,	220290,	2203,	220300,	2204,	220410,	220421,	220429,	220430,	2205,	220510,	220590,	2206,	220600,	2207,	220710,	220720,	2208,	220810,	220820,	220830,	220840,	220850,	220890,	2209,	220900"))

World84 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("22,	2201,	220110,	220190,	2202,	220210,	220290,	2203,	220300,	2204,	220410,	220421,	220429,	220430,	2205,	220510,	220590,	2206,	220600,	2207,	220710,	220720,	2208,	220810,	220820,	220830,	220840,	220850,	220890,	2209,	220900"))

World81a <- World81$data
World82a <- World82$data
World83a <- World83$data
World84a <- World84$data

World <-  rbind(World81a,World82a,World83a,World84a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)

Guyana_World <- Guyana_World[-c(137,155),]


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-1.067e-05, -6.306e-07, -1.040e-09 ,0, 1.427e-07,  1.401e-04 ), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=45,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-1.07e-05,-6.31e-07]", "(-6.31e-07,-1.04e-09]" ,"(-1.04e-09,0]","(0,1.43e-07]","(1.43e-07,0.00014]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)



###################  Food Industries

World85 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("23,	2301,	230110,	230120,	2302,	230210,	230220,	230230,	230240,	230250,	2303,	230310,	230320,	230330,	2304,	230400,	2305,	230500,	2306,	230610,	230620,	230630,	230640,	230650,	230660,	230690,	2307,	230700,	2308,	230810,	230890,	2309,	230910,	230990"))

World86 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("23,	2301,	230110,	230120,	2302,	230210,	230220,	230230,	230240,	230250,	2303,	230310,	230320,	230330,	2304,	230400,	2305,	230500,	2306,	230610,	230620,	230630,	230640,	230650,	230660,	230690,	2307,	230700,	2308,	230810,	230890,	2309,	230910,	230990"))

World87 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("23,	2301,	230110,	230120,	2302,	230210,	230220,	230230,	230240,	230250,	2303,	230310,	230320,	230330,	2304,	230400,	2305,	230500,	2306,	230610,	230620,	230630,	230640,	230650,	230660,	230690,	2307,	230700,	2308,	230810,	230890,	2309,	230910,	230990"))

World88 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("23,	2301,	230110,	230120,	2302,	230210,	230220,	230230,	230240,	230250,	2303,	230310,	230320,	230330,	2304,	230400,	2305,	230500,	2306,	230610,	230620,	230630,	230640,	230650,	230660,	230690,	2307,	230700,	2308,	230810,	230890,	2309,	230910,	230990"))

World85a <- World85$data
World86a <- World86$data
World87a <- World87$data
World88a <- World88$data

World <-  rbind(World85a,World86a,World87a,World88a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)


Guyana_World <- Guyana_World[-c(38,39,42,43),]
library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:22)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-4.494e-07,0,2.371e-09,  1.170e-07, 3.849e-07 , 1.331e-05), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=31,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-4.49e-07,0]","(0,2.37e-09]","(2.37e-09,1.17e-07]","(1.17e-07,3.85e-07]","(3.85e-07,1.33e-05]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)



###################  Food Industries

World89 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("1997,1998,1999, 2000, 2001, 2002"),
                        cc = c("24,	2401,	240110,	240120,	240130,	2402,	240210,	240220,	240290,	2403,	240310,	240391,	240399"))

World90 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2003,2004,2005,2006,2007,2008"),
                        cc = c("24,	2401,	240110,	240120,	240130,	2402,	240210,	240220,	240290,	2403,	240310,	240391,	240399"))

World91 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2009,2010,2011,2012,2013,2014"),
                        cc = c("24,	2401,	240110,	240120,	240130,	2402,	240210,	240220,	240290,	2403,	240310,	240391,	240399"))

World92 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = c("2015,2016,2017"),
                        cc = c("24,	2401,	240110,	240120,	240130,	2402,	240210,	240220,	240290,	2403,	240310,	240391,	240399"))

World89a <- World89$data
World90a <- World90$data
World91a <- World91$data
World92a <- World92$data

World <-  rbind(World89a,World90a,World91a,World92a)

Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)



### Heatmap
#install.packages('devtools')
#devtools::install_github('talgalili/heatmaply')
library(heatmaply)
Guyana_World <- Guyana_World[,c(1,2,42)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:21)])
row.names(m) <- data_wide$Commodity


vals <- unique(scales::rescale(c(m)))
vals <- quantile(c(m), probs = seq(0,1, by= 0.1), na.rm = T)
vals <- c(1:10,40, NA)
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
#colz[1,1] <- 0, "34A4B4C")


p <- plot_ly(type = "heatmap",
             x = colnames(m),y = rownames(m) ,
             z = m ,colorscale = colz
)
p <- layout(p,title="Revealed Comparative advantage by Agricultural Commodities", titlefont = list(color ="rgb(49, 61, 183)",size = 26, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 12),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 13),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 20, cex = 20), yanchor = "center",  y = -0.24 ),margin = list(l = 230, r = 70, t = 80, b = 200))

p


require(RColorBrewer)
#mat.intervals <- cut(m,breaks=10)

#m[is.na(m)] <- 0


mat.intervals <- cut(m, breaks=c(-8.424e-07, -2.000e-10,0,  2.339e-08 ,1.690e-07,  5.835e-06), include.lowest=TRUE, right = TRUE)
#mat.intervals <- levels(addNA(mat.intervals))
interval.mat <- matrix(mat.intervals,nrow=14,ncol=20,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
interval.df$expr <- factor(interval.df$expr, levels = c("[-8.42e-07,-2e-10]", "[-8.42e-07,-2e-10]","(0,2.34e-08]","(2.34e-08,1.69e-07]","(1.69e-07,5.84e-06]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#D8E7F5",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#afaaaa", "#FF0000", "#A50F15","#F0E68C", "#2db66e")
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 500, r = -50, t = 50, b = 0))

hide_legend(p)



######################################################################################################################
####### Comparative Advantage Overall (not within groups)
######################################################################################################################



World <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World8a,
                World9a,World10a,World11a,World12a,World13a,World14a,World15a,World16a,
                World17a,World18a,World19a,World20a,World21a,World22a,World23a,World24a,
                World25a,World26a,World27a,World28a,World29a,World30a,World31a,World32a,
                World33a,World34a,World35a,World36a,World37a,World38a,World39a,World40a,
                World41a,World42a,World43a,World44a,World45a,World46a,World47a,World48a,
                World49a,World50a,World51a,World52a,World53a,World54a,World55a,World56a,
                World57a,World58a,World59a,World60a,World61a,World62a,World63a,World64a,
                World65a,World66a,World67a,World68a,World69a,World70a,World71a,World72a,
                World93a,World94a,World95a,World96a,
                World73a,World74a,World75a,World76a,World77a,World78a,World79a,World80a,
                World81a,World82a,World83a,World84a,World85a,World86a,World87a,World88a,
                World89a,World90a,World91a,World92a)


######## Comparative advantage at digit 2 level
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification/World")
World_Export <- read.csv("All_Export_to_World_1_24_All_digits.csv", header = T, sep= ",")


Guyana <- World_Export[which(World_Export$Reporter == "Guyana"),]
World <-aggregate(World_Export$Trade.Value..US.., by=list(Category=World_Export$Commodity, World_Export$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "2"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)
Guyana_World$Normalized_CA <- Guyana_World$Normalized_CA*100000


####
Guyana_World <- Guyana_World[,c(1,2,24,43)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)
Guyana_World$Commodity.Code <- as.numeric(Guyana_World$Commodity.Code)


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity


require(RColorBrewer)
summary(Guyana_World$Normalized_CA)



# to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=53,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("[-0.6,-0.2]", "(-0.2,-0.1]", "(-0.1,0]", "(0,2]" , "(2,5]" ,"(5,11]"))

#interval.cols <- c("#F0F0F0" , "#EDF4FC", "#E3EEF8" , "#E45250",  "#CFE1F2")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
interval.cols <- c("#ffd8b1", "#CCBFD1", "#ffe119","#f58231", "#e6194B","#1E7823")
#, "#2db66e"
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}

library(plotly)
p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 11),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 50, r = -50, t = 50, b = 0))

hide_legend(p)



###### 6 digits
######
###############################


#write.csv(World,"All_Export_to_World_1_24_All_digits.csv")
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification/World")
World <- read.csv("All_Export_to_World_1_24_All_digits.csv", header = T, sep= ",")


Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana <- Guyana[which(Guyana$Aggregate.Level == "6"),]
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)
Guyana_World$Normalized_CA <- Guyana_World$Normalized_CA*100000


####
Guyana_World <- Guyana_World[,c(1,2,24,43)]
Guyana_World$Year <- as.numeric(Guyana_World$Year)
Guyana_World$Commodity <- as.character(Guyana_World$Commodity)
Guyana_World$Normalized_CA <- as.numeric(Guyana_World$Normalized_CA)
Guyana_World$Commodity.Code <- as.numeric(Guyana_World$Commodity.Code)


library(tidyr)
data_wide <- spread(Guyana_World, Year, Normalized_CA)

m <- as.matrix(data_wide[,c(2:23)])
row.names(m) <- data_wide$Commodity


require(RColorBrewer)
summary(Guyana_World$Normalized_CA)


##############
#Live Animals and Meat and Edible Meat (01 and 02)
##############
meat <- Guyana_World[which(Guyana_World$Commodity.Code <= 021090),]
data_wide <- spread(meat, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:21)])
row.names(m) <- data_wide$Commodity


# to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=60,ncol=19,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.1,0]", "(0,2]"))
#interval.cols <- c("#ffd8b1", "#CCBFD1", "#ffe119","#f58231", "#e6194B","#1E7823")

interval.cols <- c("#ffe119", "#ffd8b1", "#CCBFD1","#e6194B","#1E7823",  "#f58231")
#, "#2db66e"
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 9),showgrid = FALSE, zeroline = FALSE), legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 ),margin = list(l = 50, r = -50, t = 50, b = 0))

hide_legend(p)


##############
#Fish  (03)
##############
fish <- Guyana_World[which(Guyana_World$Commodity.Code <= 30799 &  Guyana_World$Commodity.Code > 21090),]
data_wide <- spread(fish, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=167,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.1,0]" ,"(0,2]" ,   "(2,5]"))

#interval.cols <- c("#ffd8b1", "#CCBFD1", "#ffe119","#f58231", "#e6194B","#1E7823")
interval.cols <- c("#ffe119",
                   "#ffd8b1",
                   "#CCBFD1",
                   "#f58231",
                   "#1E7823",
                   "#e6194B")
                   
                   

#, "#2db66e"
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 7.2),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 50, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p



##############
# Dairy and animal originated Products (04 and 05)
##############
dairy_an <- Guyana_World[which(Guyana_World$Commodity.Code <= 51199 &  Guyana_World$Commodity.Code > 30799),]
data_wide <- spread(dairy_an, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=63,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.1,0]", "(0,2]"))

interval.cols <- c("#ffe119", "#ffd8b1", "#CCBFD1","#e6194B","#1E7823",  "#f58231")




names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 8),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 50, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p


##############
# Trees  (06)
##############
trees <- Guyana_World[which(Guyana_World$Commodity.Code <= 60499 &  Guyana_World$Commodity.Code > 51199),]
data_wide <- spread(trees, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=19,ncol=21,dimnames=list(rownames(m),colnames(m)))

require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.1,0]", "(0,2]"))

interval.cols <- c("#ffe119", "#ffd8b1", "#CCBFD1","#e6194B","#1E7823",  "#f58231")
#, "#08306B", "#808080"
#interval.cols <- brewer.pal(6,"Reds")
#interval.cols <- c("#ffd8b1", "#ffe119","#f58231", "#e6194B")
interval.cols <- c("#ffe119", "#ffd8b1", "#CCBFD1","#e6194B","#1E7823",  "#f58231")

#, "#2db66e"
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 8),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p

##############
# Vegetables (07)
##############
Veg <- Guyana_World[which(Guyana_World$Commodity.Code <= 71490 &  Guyana_World$Commodity.Code > 60499),]
data_wide <- spread(Veg, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=108,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.1,0]", "(0,2]"))

interval.cols <- c("#ffe119", "#ffd8b1", "#CCBFD1","#e6194B","#1E7823",  "#f58231")

names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 5),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p


##############
# Fruits and Nuts (08)
##############
Fruits <- Guyana_World[which(Guyana_World$Commodity.Code <= 81400 &  Guyana_World$Commodity.Code > 71490),]
data_wide <- spread(Fruits, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=75,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.2,-0.1]","(-0.1,0]","(0,2]"))

#interval.cols <- c("#ffd8b1", "#CCBFD1", "#ffe119","#f58231", "#e6194B","#1E7823")
interval.cols <- c(
 "#CCBFD1", "#ffd8b1",
"#e6194B","#ffe119","#1E7823",
"#f58231")
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 8),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p


##############
# Coffee, tea, mate and spices (09)
##############
Coffe <- Guyana_World[which(Guyana_World$Commodity.Code <= 91099 &  Guyana_World$Commodity.Code > 81400),]
data_wide <- spread(Coffe, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=55,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.2,-0.1]","(-0.1,0]","(0,2]"))

#interval.cols <- c("#ffd8b1", "#CCBFD1", "#ffe119","#f58231", "#e6194B","#1E7823")
interval.cols <- c(
  "#CCBFD1", "#ffd8b1",
  "#e6194B","#ffe119","#1E7823",
  "#f58231")

names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 10),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p


##############
# Cereals and products of the milling industry (10 - 11)
##############
Cereals_Mill <- Guyana_World[which(Guyana_World$Commodity.Code <= 110900 &  Guyana_World$Commodity.Code > 91099),]
data_wide <- spread(Cereals_Mill, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=75,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.2,-0.1]", "(-0.1,0]", "(0,2]" , "(2,5]" ,"(5,11]"))

#interval.cols <- c("#ffd8b1", "#CCBFD1", "#ffe119","#f58231", "#e6194B","#1E7823")

interval.cols <- c( "#CCBFD1","#ffe119","#ffd8b1","#f58231", "#e6194B","#1E7823")


names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 8),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p


##############
# Oil seeds Gum (12 - 13)
##############
Oil_Seeds_Gum <- Guyana_World[which(Guyana_World$Commodity.Code <= 130239 &  Guyana_World$Commodity.Code > 110900),]
data_wide <- spread(Oil_Seeds_Gum, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=59,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.2,-0.1]","(-0.1,0]","(0,2]"))

#interval.cols <- c("#ffd8b1", "#CCBFD1", "#ffe119","#f58231", "#e6194B","#1E7823")
interval.cols <- c(
  "#CCBFD1", "#ffd8b1",
  "#e6194B","#ffe119","#1E7823",
  "#f58231")
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 6.8),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p


##############
# Vegetable plaiting materials - Animal or vegetable fats (14 - 15)
##############
Veg_Fats <- Guyana_World[which(Guyana_World$Commodity.Code <= 152200 &  Guyana_World$Commodity.Code > 130239),]
data_wide <- spread(Veg_Fats, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=75,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, c("(-0.1,0]", "(0,2]"))

interval.cols <- c("#ffe119", "#ffd8b1", "#CCBFD1","#e6194B","#1E7823",  "#f58231")


names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 6),showgrid = FALSE, zeroline = FALSE),margin = list(l = 1050, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p



##############
# Meat Fish or Crustaceans
##############
Meat_F_C <- Guyana_World[which(Guyana_World$Commodity.Code <= 160590  &  Guyana_World$Commodity.Code > 152200),]
data_wide <- spread(Meat_F_C, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=37,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, c("(-0.1,0]", "(0,2]"))

interval.cols <- c("#ffe119", "#ffd8b1", "#CCBFD1","#e6194B","#1E7823",  "#f58231")
#, "#2db66e"
#"#F0E68C"
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 12),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p



##############
# Sugars and cocoa preparations
##############
Sugars_Coc <- Guyana_World[which(Guyana_World$Commodity.Code <= 180690  &  Guyana_World$Commodity.Code > 160590),]
data_wide <- spread(Sugars_Coc, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=48,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.1,0]" ,"(0,2]" ,   "(2,5]" ,   "(5,11]"))

#interval.cols <- c("#ffd8b1", "#CCBFD1", "#ffe119","#f58231", "#e6194B","#1E7823")
interval.cols <- c( "#ffe119","#ffd8b1", "#f58231","#e6194B","#CCBFD1","#1E7823")



names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 7),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p



##############
# preparation of cereals
##############
PrepCer <- Guyana_World[which(Guyana_World$Commodity.Code <= 190590  &  Guyana_World$Commodity.Code > 180690),]
data_wide <- spread(PrepCer, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=37,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels =  c("(-0.1,0]", "(0,2]"))

interval.cols <- c("#ffe119", "#ffd8b1", "#CCBFD1","#e6194B","#1E7823",  "#f58231")
names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 5),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p



##############
# preparation of vegetables
##############
PrepVeg <- Guyana_World[which(Guyana_World$Commodity.Code <= 200990  &  Guyana_World$Commodity.Code > 190590),]
data_wide <- spread(PrepVeg, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=85,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, evels =  c("(-0.1,0]", "(0,2]"))

interval.cols <- c("#ffe119", "#ffd8b1", "#CCBFD1","#e6194B","#1E7823",  "#f58231")

names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 7),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p



##############
# edible preparations
##############
EdiPrep <- Guyana_World[which(Guyana_World$Commodity.Code <= 210690  &  Guyana_World$Commodity.Code > 200990),]
data_wide <- spread(EdiPrep, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=34,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.2,-0.1]","(-0.1,0]","(0,2]"))

#interval.cols <- c("#ffd8b1", "#CCBFD1", "#ffe119","#f58231", "#e6194B","#1E7823")
interval.cols <- c(
  "#CCBFD1", "#ffd8b1",
  "#e6194B","#ffe119","#1E7823",
  "#f58231")

names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 12),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p




##############
# Beverages
##############
Beverages <- Guyana_World[which(Guyana_World$Commodity.Code <= 220900  &  Guyana_World$Commodity.Code > 210690),]
data_wide <- spread(Beverages, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=46,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.2,-0.1]","(-0.1,0]","(0,2]"))

#interval.cols <- c("#ffd8b1", "#CCBFD1", "#ffe119","#f58231", "#e6194B","#1E7823")
interval.cols <- c(
  "#CCBFD1", "#ffd8b1",
  "#e6194B","#ffe119","#1E7823",
  "#f58231")

names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 10),showgrid = FALSE, zeroline = FALSE),margin = list(l = 600, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p


##############
# Food Industries, Residues, wastes thereof and Tobacco
##############
Food_Ind_tobacco <- Guyana_World[which(Guyana_World$Commodity.Code <=  240399 &  Guyana_World$Commodity.Code > 220900),]
data_wide <- spread(Food_Ind_tobacco, Year, Normalized_CA)

#####
# to change
m <- as.matrix(data_wide[,c(3:23)])
row.names(m) <- data_wide$Commodity

# not to change
mat.intervals <- cut(m, breaks=c(-0.6, -0.2, -0.1, 0, 2,5,11), include.lowest=TRUE, right = TRUE)

# to change
interval.mat <- matrix(mat.intervals,nrow=47,ncol=21,dimnames=list(rownames(m),colnames(m)))
require(reshape2)
interval.df <- reshape2::melt(interval.mat,varnames=c("gene","sample"),value.name="expr")
# not to change
interval.df$expr <- factor(interval.df$expr, levels = c("(-0.2,-0.1]","(-0.1,0]","(0,2]"))

#interval.cols <- c("#ffd8b1", "#CCBFD1", "#ffe119","#f58231", "#e6194B","#1E7823")
interval.cols <- c(
  "#CCBFD1", "#ffd8b1",
  "#e6194B","#ffe119","#1E7823",
  "#f58231")

names(interval.cols) <- levels(mat.intervals)
interval.cols2 <- rep(interval.cols, each=ncol(m))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)

#
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}


p <- plot_ly(z=c(interval.df$expr),x=interval.df$sample,y=interval.df$gene,colors=interval.cols2,type="heatmap",colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:6),ticktext=names(interval.cols),len=0.2,outlinecolor="white",bordercolor="white",borderwidth=10,bgcolor="white"))

p <- layout(p, showlegend=FALSE,title="", titlefont = list(color ="rgb(49, 61, 183)",size = 36, cex = 30),
            xaxis = list(title = "", tickfont = list(size = 16),tickangle = 60,autotick = F, dtick = 1,ticks="outside"),
            yaxis = list(side = 'left', title = '', titlefont = list(size = 20),tickfont = list(size = 10),showgrid = FALSE, zeroline = FALSE),margin = list(l = 500, r = -50, t = 0, b = 0) )
#legend = list(orientation = 'h',font = list(size = 32, cex = 36), yanchor = "center",  y = -0.24 )
p


############################################################################################### 
##############################################################################################
#################################################################################################
#############################   Ranking of comparative advantage among agro-food products
##################################################################################################
####################################################################################################


setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification/World")
World_Export <- read.csv("All_Export_to_World_1_24_All_digits.csv", header = T, sep= ",")


Guyana <- World_Export[which(World_Export$Reporter == "Guyana"),]
World <-aggregate(World_Export$Trade.Value..US.., by=list(Category=World_Export$Commodity, World_Export$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")


# Guyana
Total_Trade_Guyana <- aggregate(Guyana$Trade.Value..US.., by=list(Category=Guyana$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana) <- c("Year","Tot_Export_Guyana")
Guyana <- merge(Guyana, Total_Trade_Guyana, by = "Year")
Guyana$share <- (Guyana$Trade.Value..US../Guyana$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World <- aggregate(World$World_Export, by=list(Category=World$Year), FUN=sum, na.rm = T)
names(Total_Trade_World) <- c("Year","Tot_Export_World")
World <- merge(World, Total_Trade_World, by = "Year")
World$Worldshare <- (World$World_Export/World$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World <- merge(Guyana, World, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World$Comparative_Advantage <- Guyana_World$share/Guyana_World$Worldshare 
Guyana_World$Normalized_CA <- (Guyana_World$Trade.Value..US../Guyana_World$Tot_Export_World) - ((Guyana_World$World_Export * Guyana_World$Tot_Export_Guyana)/(Guyana_World$Tot_Export_World)^2)
Guyana_World$Normalized_CA <- Guyana_World$Normalized_CA*100000


Guyana_World_6_digits <- Guyana_World[which(Guyana_World$Aggregate.Level == 6),]
Guyana_World_6_digits <- Guyana_World_6_digits[,c(1,2,24,43)]

Guyana_World_6_digits_2017 <- Guyana_World_6_digits[which(Guyana_World_6_digits$Year == 2017),]

Ranking2017 <- Guyana_World_6_digits_2017
Ranking2017 <- Ranking2017[order(-Ranking2017$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2017, "Ranking_CA_2017.csv")  

Guyana_World_6_digits_2012 <- Guyana_World_6_digits[which(Guyana_World_6_digits$Year == 2012),]
Ranking2012 <- Guyana_World_6_digits_2012
Ranking2012 <- Ranking2012[order(-Ranking2012$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2012, "Ranking_CA_2012.csv")  

Guyana_World_6_digits_2014 <- Guyana_World_6_digits[which(Guyana_World_6_digits$Year == 2014),]
Ranking2014 <- Guyana_World_6_digits_2014
Ranking2014 <- Ranking2014[order(-Ranking2014$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2014, "Ranking_CA_2014.csv")  

Guyana_World_6_digits_Export_Share <- Guyana_World[which(Guyana_World$Aggregate.Level == 6),]
Guyana_World_6_digits_Export_Share <- Guyana_World_6_digits_Export_Share[,c(1,2,24,38)]
write.csv(Guyana_World_6_digits_Export_Share, "Guyana_World_6_digits_Export_Share.csv")  


Guyana_World_6_digits_2000 <- Guyana_World_6_digits[which(Guyana_World_6_digits$Year == 2000),]
Ranking2000 <- Guyana_World_6_digits_2000
Ranking2000 <- Ranking2000[order(-Ranking2000$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2000, "Ranking_CA_2000.csv")  

Guyana_World_6_digits_2001 <- Guyana_World_6_digits[which(Guyana_World_6_digits$Year == 2001),]
Ranking2001 <- Guyana_World_6_digits_2001
Ranking2001 <- Ranking2001[order(-Ranking2001$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2001, "Ranking_CA_2001.csv")  

Guyana_World_6_digits_2005 <- Guyana_World_6_digits[which(Guyana_World_6_digits$Year == 2005),]
Ranking2005 <- Guyana_World_6_digits_2005
Ranking2005 <- Ranking2005[order(-Ranking2005$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2005, "Ranking_CA_2005.csv")  

first20Ranking2017 <- Ranking2017[c(1:20),]
Guyana_World_6_digits_Export_Share_2017 <- Guyana_World_6_digits_Export_Share[which(Guyana_World_6_digits_Export_Share$Year == 2017),]
first20_Export_Share2017 <- merge(Guyana_World_6_digits_Export_Share_2017 , first20Ranking2017, by = "Commodity.Code")
first20_Export_Share2017 <- first20_Export_Share2017[order(-first20_Export_Share2017$Normalized_CA),] 
write.csv(first20_Export_Share2017, "first20_Export_Share2017.csv")  

Guyana_World_6_digits_2004 <- Guyana_World_6_digits[which(Guyana_World_6_digits$Year == 2004),]
Ranking2004 <- Guyana_World_6_digits_2004
Ranking2004 <- Ranking2004[order(-Ranking2004$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2004, "Ranking_CA_2004.csv")  

Guyana_World_6_digits_2003 <- Guyana_World_6_digits[which(Guyana_World_6_digits$Year == 2003),]
Ranking2003 <- Guyana_World_6_digits_2003
Ranking2003 <- Ranking2003[order(-Ranking2003$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2003, "Ranking_CA_2003.csv")  

Guyana_World_6_digits_2007 <- Guyana_World_6_digits[which(Guyana_World_6_digits$Year == 2007),]
Ranking2007 <- Guyana_World_6_digits_2007
Ranking2007 <- Ranking2007[order(-Ranking2007$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2007, "Ranking_CA_2007.csv")  


first20Ranking2012 <- Ranking2012[c(1:20),]
Guyana_World_6_digits_Export_Share_2012 <- Guyana_World_6_digits_Export_Share[which(Guyana_World_6_digits_Export_Share$Year == 2012),]
first20_Export_Share2012 <- merge(Guyana_World_6_digits_Export_Share_2012 , first20Ranking2017, by = "Commodity.Code")
first20_Export_Share2012 <- first20_Export_Share2012[order(-first20_Export_Share2012$Normalized_CA),] 
write.csv(first20_Export_Share2012, "first20_Export_Share2012.csv")  

first20Ranking2014 <- Ranking2014[c(1:20),]
Guyana_World_6_digits_Export_Share_2014 <- Guyana_World_6_digits_Export_Share[which(Guyana_World_6_digits_Export_Share$Year == 2014),]
first20_Export_Share2014 <- merge(Guyana_World_6_digits_Export_Share_2014 , first20Ranking2017, by = "Commodity.Code")
first20_Export_Share2014 <- first20_Export_Share2014[order(-first20_Export_Share2014$Normalized_CA),] 
write.csv(first20_Export_Share2014, "first20_Export_Share2014.csv")  



first20Ranking2000 <- Ranking2000[c(1:20),]
Guyana_World_6_digits_Export_Share_2000 <- Guyana_World_6_digits_Export_Share[which(Guyana_World_6_digits_Export_Share$Year == 2000),]
first20_Export_Share2000 <- merge(Guyana_World_6_digits_Export_Share_2000 , first20Ranking2017, by = "Commodity.Code")
first20_Export_Share2000 <- first20_Export_Share2000[order(-first20_Export_Share2000$Normalized_CA),] 
write.csv(first20_Export_Share2000, "first20_Export_Share2000.csv")  


first20Ranking2001 <- Ranking2001[c(1:20),]
Guyana_World_6_digits_Export_Share_2001 <- Guyana_World_6_digits_Export_Share[which(Guyana_World_6_digits_Export_Share$Year == 2001),]
first20_Export_Share2001 <- merge(Guyana_World_6_digits_Export_Share_2001 , first20Ranking2017, by = "Commodity.Code")
first20_Export_Share2001 <- first20_Export_Share2001[order(-first20_Export_Share2001$Normalized_CA),] 
write.csv(first20_Export_Share2001, "first20_Export_Share2001.csv")  

first20Ranking2007 <- Ranking2007[c(1:20),]
Guyana_World_6_digits_Export_Share_2007 <- Guyana_World_6_digits_Export_Share[which(Guyana_World_6_digits_Export_Share$Year == 2007),]
first20_Export_Share2007 <- merge(Guyana_World_6_digits_Export_Share_2007 , first20Ranking2017, by = "Commodity.Code")
first20_Export_Share2007 <- first20_Export_Share2007[order(-first20_Export_Share2007$Normalized_CA),] 
write.csv(first20_Export_Share2007, "first20_Export_Share2007.csv")  


fish <- Guyana_World_6_digits[which(Guyana_World_6_digits$Commodity.Code == 30349),]


rice <- Guyana_World[which(Guyana_World$Commodity.Code == 100620),]


write.csv(Guyana_World, "Guyana_Agri_Food_Exports.csv")
Guyana_World_2000 <- Guyana_World[which(Guyana_World$Year == 2000 & Guyana_World$Aggregate.Level == 6),]
Guyana_World_2017 <- Guyana_World[which(Guyana_World$Year == 2017 & Guyana_World$Aggregate.Level == 6),]

Guyana2000 <- Guyana_World_2000[,c(1,2,24,38)]
Guyana2017 <- Guyana_World_2017[,c(1,2,24,38)]

names(Guyana2000)[4] <- "share_2000"

Guyana_share_diff <- merge(Guyana2017, Guyana2000, by = "Commodity.Code")
Guyana_share_diff$diff <- (Guyana_share_diff$share *100) - (Guyana_share_diff$share_2000 * 100)

Guyana_share_diff <- Guyana_share_diff[order(-Guyana_share_diff$dif),] 
write.csv(Guyana_share_diff, "Guyana_share_diff.csv")




############################################
#############################################   Comparative Advantage at 4 digit level
###############################################

setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
#World_Export <- read.csv("All_Export_to_World_1_24_All_digits.csv", header = T, sep= ",")


Guyana1 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	
                              
Guyana2 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

Guyana3 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

Guyana4 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

Guyana5 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

Guyana6 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

Guyana7 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

Guyana8 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	
Guyana9 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

Guyana10 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	

Guyana11 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	

Guyana12 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

Guyana13 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

Guyana14 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	

Guyana15 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	

Guyana16 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

Guyana17 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

Guyana18 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	

Guyana19 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

Guyana20 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	


Guyana21 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

Guyana22 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

Guyana23 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


Guyana24 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

Guyana25 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

Guyana26 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

Guyana27 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

Guyana28 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = "2017",
                         cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	

                       
    
Guyana1a <- Guyana1$data
Guyana2a <- Guyana2$data
Guyana3a <- Guyana3$data
Guyana4a <- Guyana4$data
Guyana5a <- Guyana5$data
Guyana6a <- Guyana6$data
Guyana7a <- Guyana7$data
Guyana8a <- Guyana8$data
Guyana9a <- Guyana9$data
Guyana10a <- Guyana10$data
Guyana11a <- Guyana11$data
Guyana12a <- Guyana12$data
Guyana13a <- Guyana13$data
Guyana14a <- Guyana14$data
Guyana15a <- Guyana15$data
Guyana16a <- Guyana16$data
Guyana17a <- Guyana17$data
Guyana18a <- Guyana18$data
Guyana19a <- Guyana19$data
Guyana20a <- Guyana20$data
Guyana21a <- Guyana21$data
Guyana22a <- Guyana22$data
Guyana23a <- Guyana23$data
Guyana24a <- Guyana24$data
Guyana25a <- Guyana25$data
Guyana26a <- Guyana26$data
Guyana27a <- Guyana27$data
Guyana28a <- Guyana28$data






Guyana_Export_4_digit <-  rbind(Guyana1a,Guyana2a,Guyana3a,Guyana4a,Guyana5a,Guyana6a,Guyana7a,Guyana8a,Guyana9a,
                                Guyana10a,Guyana11a,Guyana12a,Guyana13a,Guyana14a,Guyana15a,Guyana16a,Guyana17a,Guyana18a,
                                Guyana19a,Guyana20a,Guyana21a,Guyana22a,Guyana23a,Guyana24a,Guyana25a,Guyana26a,Guyana27a,Guyana28a)





World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2017",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data






World_Export_4_digit <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                               World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                               World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit, "All_Export_4_digits_All_Products.csv")


Guyana_Export_4_digit <- World_Export_4_digit[which(World_Export_4_digit$Reporter == "Guyana"),]
World_Export_4_digit <-aggregate( World_Export_4_digit$Trade.Value..US.., by=list(Category= World_Export_4_digit$Commodity,  World_Export_4_digit$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit ) <- c("Commodity", "Year", "World_Export")


### shares for 6 digits
# Guyana
Guyana_Export_4_digit  <- Guyana_Export_4_digit[which(Guyana_Export_4_digit$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit <- aggregate(Guyana_Export_4_digit$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit <- merge(Guyana_Export_4_digit, Total_Trade_Guyana_4_digit, by = "Year")
Guyana_Export_4_digit$share <- (Guyana_Export_4_digit$Trade.Value..US../Guyana_Export_4_digit$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit <- aggregate(World_Export_4_digit$World_Export, by=list(Category=World_Export_4_digit$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit) <- c("Year","Tot_Export_World")
World_Export_4_digit <- merge(World_Export_4_digit, Total_Trade_World_4_digit, by = "Year")
World_Export_4_digit$Worldshare <- (World_Export_4_digit$World_Export/World_Export_4_digit$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit <- merge(Guyana_Export_4_digit, World_Export_4_digit, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit$Comparative_Advantage <- Guyana_World_Export_4_digit$share/Guyana_World_Export_4_digit$Worldshare 
Guyana_World_Export_4_digit$Normalized_CA <- (Guyana_World_Export_4_digit$Trade.Value..US../Guyana_World_Export_4_digit$Tot_Export_World) - ((Guyana_World_Export_4_digit$World_Export * Guyana_World_Export_4_digit$Tot_Export_Guyana)/(Guyana_World_Export_4_digit$Tot_Export_World)^2)


Ranking2017 <- Guyana_World_Export_4_digit[order(-Guyana_World_Export_4_digit$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2017, "All_Products_Guyana_2017.csv")  




############################################ Other Years (2014)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2014",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data






World_Export_4_digit_2014 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                               World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                               World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2014, "All_Export_4_digits_All_Products_2014.csv")



Guyana_Export_4_digit_2014 <- World_Export_4_digit_2014[which(World_Export_4_digit_2014$Reporter == "Guyana"),]
World_Export_4_digit_2014 <-aggregate( World_Export_4_digit_2014$Trade.Value..US.., by=list(Category= World_Export_4_digit_2014$Commodity,  World_Export_4_digit_2014$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2014 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2014  <- Guyana_Export_4_digit_2014[which(Guyana_Export_4_digit_2014$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2014 <- aggregate(Guyana_Export_4_digit_2014$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2014$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2014) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2014 <- merge(Guyana_Export_4_digit_2014, Total_Trade_Guyana_4_digit_2014, by = "Year")
Guyana_Export_4_digit_2014$share <- (Guyana_Export_4_digit_2014$Trade.Value..US../Guyana_Export_4_digit_2014$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2014 <- aggregate(World_Export_4_digit_2014$World_Export, by=list(Category=World_Export_4_digit_2014$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2014) <- c("Year","Tot_Export_World")
World_Export_4_digit_2014 <- merge(World_Export_4_digit_2014, Total_Trade_World_4_digit_2014, by = "Year")
World_Export_4_digit_2014$Worldshare <- (World_Export_4_digit_2014$World_Export/World_Export_4_digit_2014$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2014 <- merge(Guyana_Export_4_digit_2014, World_Export_4_digit_2014, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2014$Comparative_Advantage <- Guyana_World_Export_4_digit_2014$share/Guyana_World_Export_4_digit_2014$Worldshare 
Guyana_World_Export_4_digit_2014$Normalized_CA <- (Guyana_World_Export_4_digit_2014$Trade.Value..US../Guyana_World_Export_4_digit_2014$Tot_Export_World) - ((Guyana_World_Export_4_digit_2014$World_Export * Guyana_World_Export_4_digit_2014$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2014$Tot_Export_World)^2)


Ranking2014 <- Guyana_World_Export_4_digit_2014[order(-Guyana_World_Export_4_digit_2014$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2014, "All_Products_Guyana_2014.csv")  







############################################ Other Years (2015)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2015",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2015 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2015, "All_Export_4_digits_All_Products_2015.csv")



Guyana_Export_4_digit_2015 <- World_Export_4_digit_2015[which(World_Export_4_digit_2015$Reporter == "Guyana"),]
World_Export_4_digit_2015 <-aggregate( World_Export_4_digit_2015$Trade.Value..US.., by=list(Category= World_Export_4_digit_2015$Commodity,  World_Export_4_digit_2015$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2015 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2015  <- Guyana_Export_4_digit_2015[which(Guyana_Export_4_digit_2015$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2015 <- aggregate(Guyana_Export_4_digit_2015$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2015$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2015) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2015 <- merge(Guyana_Export_4_digit_2015, Total_Trade_Guyana_4_digit_2015, by = "Year")
Guyana_Export_4_digit_2015$share <- (Guyana_Export_4_digit_2015$Trade.Value..US../Guyana_Export_4_digit_2015$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2015 <- aggregate(World_Export_4_digit_2015$World_Export, by=list(Category=World_Export_4_digit_2015$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2015) <- c("Year","Tot_Export_World")
World_Export_4_digit_2015 <- merge(World_Export_4_digit_2015, Total_Trade_World_4_digit_2015, by = "Year")
World_Export_4_digit_2015$Worldshare <- (World_Export_4_digit_2015$World_Export/World_Export_4_digit_2015$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2015 <- merge(Guyana_Export_4_digit_2015, World_Export_4_digit_2015, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2015$Comparative_Advantage <- Guyana_World_Export_4_digit_2015$share/Guyana_World_Export_4_digit_2015$Worldshare 
Guyana_World_Export_4_digit_2015$Normalized_CA <- (Guyana_World_Export_4_digit_2015$Trade.Value..US../Guyana_World_Export_4_digit_2015$Tot_Export_World) - ((Guyana_World_Export_4_digit_2015$World_Export * Guyana_World_Export_4_digit_2015$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2015$Tot_Export_World)^2)


Ranking2015 <- Guyana_World_Export_4_digit_2015[order(-Guyana_World_Export_4_digit_2015$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2015, "All_Products_Guyana_2015.csv")  




############################################ Other Years (2016)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2016",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2016 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2016, "All_Export_4_digits_All_Products_2016.csv")



Guyana_Export_4_digit_2016 <- World_Export_4_digit_2016[which(World_Export_4_digit_2016$Reporter == "Guyana"),]
World_Export_4_digit_2016 <-aggregate( World_Export_4_digit_2016$Trade.Value..US.., by=list(Category= World_Export_4_digit_2016$Commodity,  World_Export_4_digit_2016$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2016 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2016  <- Guyana_Export_4_digit_2016[which(Guyana_Export_4_digit_2016$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2016 <- aggregate(Guyana_Export_4_digit_2016$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2016$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2016) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2016 <- merge(Guyana_Export_4_digit_2016, Total_Trade_Guyana_4_digit_2016, by = "Year")
Guyana_Export_4_digit_2016$share <- (Guyana_Export_4_digit_2016$Trade.Value..US../Guyana_Export_4_digit_2016$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2016 <- aggregate(World_Export_4_digit_2016$World_Export, by=list(Category=World_Export_4_digit_2016$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2016) <- c("Year","Tot_Export_World")
World_Export_4_digit_2016 <- merge(World_Export_4_digit_2016, Total_Trade_World_4_digit_2016, by = "Year")
World_Export_4_digit_2016$Worldshare <- (World_Export_4_digit_2016$World_Export/World_Export_4_digit_2016$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2016 <- merge(Guyana_Export_4_digit_2016, World_Export_4_digit_2016, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2016$Comparative_Advantage <- Guyana_World_Export_4_digit_2016$share/Guyana_World_Export_4_digit_2016$Worldshare 
Guyana_World_Export_4_digit_2016$Normalized_CA <- (Guyana_World_Export_4_digit_2016$Trade.Value..US../Guyana_World_Export_4_digit_2016$Tot_Export_World) - ((Guyana_World_Export_4_digit_2016$World_Export * Guyana_World_Export_4_digit_2016$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2016$Tot_Export_World)^2)


Ranking2016 <- Guyana_World_Export_4_digit_2016[order(-Guyana_World_Export_4_digit_2016$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2016, "All_Products_Guyana_2016.csv")  





############################################ Other Years (2013)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2013",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2013 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2013, "All_Export_4_digits_All_Products_2013.csv")



Guyana_Export_4_digit_2013 <- World_Export_4_digit_2013[which(World_Export_4_digit_2013$Reporter == "Guyana"),]
World_Export_4_digit_2013 <-aggregate( World_Export_4_digit_2013$Trade.Value..US.., by=list(Category= World_Export_4_digit_2013$Commodity,  World_Export_4_digit_2013$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2013 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2013  <- Guyana_Export_4_digit_2013[which(Guyana_Export_4_digit_2013$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2013 <- aggregate(Guyana_Export_4_digit_2013$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2013$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2013) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2013 <- merge(Guyana_Export_4_digit_2013, Total_Trade_Guyana_4_digit_2013, by = "Year")
Guyana_Export_4_digit_2013$share <- (Guyana_Export_4_digit_2013$Trade.Value..US../Guyana_Export_4_digit_2013$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2013 <- aggregate(World_Export_4_digit_2013$World_Export, by=list(Category=World_Export_4_digit_2013$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2013) <- c("Year","Tot_Export_World")
World_Export_4_digit_2013 <- merge(World_Export_4_digit_2013, Total_Trade_World_4_digit_2013, by = "Year")
World_Export_4_digit_2013$Worldshare <- (World_Export_4_digit_2013$World_Export/World_Export_4_digit_2013$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2013 <- merge(Guyana_Export_4_digit_2013, World_Export_4_digit_2013, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2013$Comparative_Advantage <- Guyana_World_Export_4_digit_2013$share/Guyana_World_Export_4_digit_2013$Worldshare 
Guyana_World_Export_4_digit_2013$Normalized_CA <- (Guyana_World_Export_4_digit_2013$Trade.Value..US../Guyana_World_Export_4_digit_2013$Tot_Export_World) - ((Guyana_World_Export_4_digit_2013$World_Export * Guyana_World_Export_4_digit_2013$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2013$Tot_Export_World)^2)


Ranking2013 <- Guyana_World_Export_4_digit_2013[order(-Guyana_World_Export_4_digit_2013$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2013, "All_Products_Guyana_2013.csv")  






############################################ Other Years (2012)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2012",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2012 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2012, "All_Export_4_digits_All_Products_2012.csv")



Guyana_Export_4_digit_2012 <- World_Export_4_digit_2012[which(World_Export_4_digit_2012$Reporter == "Guyana"),]
World_Export_4_digit_2012 <-aggregate( World_Export_4_digit_2012$Trade.Value..US.., by=list(Category= World_Export_4_digit_2012$Commodity,  World_Export_4_digit_2012$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2012 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2012  <- Guyana_Export_4_digit_2012[which(Guyana_Export_4_digit_2012$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2012 <- aggregate(Guyana_Export_4_digit_2012$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2012$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2012) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2012 <- merge(Guyana_Export_4_digit_2012, Total_Trade_Guyana_4_digit_2012, by = "Year")
Guyana_Export_4_digit_2012$share <- (Guyana_Export_4_digit_2012$Trade.Value..US../Guyana_Export_4_digit_2012$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2012 <- aggregate(World_Export_4_digit_2012$World_Export, by=list(Category=World_Export_4_digit_2012$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2012) <- c("Year","Tot_Export_World")
World_Export_4_digit_2012 <- merge(World_Export_4_digit_2012, Total_Trade_World_4_digit_2012, by = "Year")
World_Export_4_digit_2012$Worldshare <- (World_Export_4_digit_2012$World_Export/World_Export_4_digit_2012$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2012 <- merge(Guyana_Export_4_digit_2012, World_Export_4_digit_2012, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2012$Comparative_Advantage <- Guyana_World_Export_4_digit_2012$share/Guyana_World_Export_4_digit_2012$Worldshare 
Guyana_World_Export_4_digit_2012$Normalized_CA <- (Guyana_World_Export_4_digit_2012$Trade.Value..US../Guyana_World_Export_4_digit_2012$Tot_Export_World) - ((Guyana_World_Export_4_digit_2012$World_Export * Guyana_World_Export_4_digit_2012$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2012$Tot_Export_World)^2)


Ranking2012 <- Guyana_World_Export_4_digit_2012[order(-Guyana_World_Export_4_digit_2012$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2012, "All_Products_Guyana_2012.csv")  




############################################ Other Years (2011)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2011",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2011 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2011, "All_Export_4_digits_All_Products_2011.csv")



Guyana_Export_4_digit_2011 <- World_Export_4_digit_2011[which(World_Export_4_digit_2011$Reporter == "Guyana"),]
World_Export_4_digit_2011 <-aggregate( World_Export_4_digit_2011$Trade.Value..US.., by=list(Category= World_Export_4_digit_2011$Commodity,  World_Export_4_digit_2011$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2011 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2011  <- Guyana_Export_4_digit_2011[which(Guyana_Export_4_digit_2011$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2011 <- aggregate(Guyana_Export_4_digit_2011$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2011$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2011) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2011 <- merge(Guyana_Export_4_digit_2011, Total_Trade_Guyana_4_digit_2011, by = "Year")
Guyana_Export_4_digit_2011$share <- (Guyana_Export_4_digit_2011$Trade.Value..US../Guyana_Export_4_digit_2011$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2011 <- aggregate(World_Export_4_digit_2011$World_Export, by=list(Category=World_Export_4_digit_2011$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2011) <- c("Year","Tot_Export_World")
World_Export_4_digit_2011 <- merge(World_Export_4_digit_2011, Total_Trade_World_4_digit_2011, by = "Year")
World_Export_4_digit_2011$Worldshare <- (World_Export_4_digit_2011$World_Export/World_Export_4_digit_2011$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2011 <- merge(Guyana_Export_4_digit_2011, World_Export_4_digit_2011, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2011$Comparative_Advantage <- Guyana_World_Export_4_digit_2011$share/Guyana_World_Export_4_digit_2011$Worldshare 
Guyana_World_Export_4_digit_2011$Normalized_CA <- (Guyana_World_Export_4_digit_2011$Trade.Value..US../Guyana_World_Export_4_digit_2011$Tot_Export_World) - ((Guyana_World_Export_4_digit_2011$World_Export * Guyana_World_Export_4_digit_2011$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2011$Tot_Export_World)^2)


Ranking2011 <- Guyana_World_Export_4_digit_2011[order(-Guyana_World_Export_4_digit_2011$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2011, "All_Products_Guyana_2011.csv")  




############################################ Other Years (2010)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2010",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2010 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2010, "All_Export_4_digits_All_Products_2010.csv")



Guyana_Export_4_digit_2010 <- World_Export_4_digit_2010[which(World_Export_4_digit_2010$Reporter == "Guyana"),]
World_Export_4_digit_2010 <-aggregate( World_Export_4_digit_2010$Trade.Value..US.., by=list(Category= World_Export_4_digit_2010$Commodity,  World_Export_4_digit_2010$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2010 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2010  <- Guyana_Export_4_digit_2010[which(Guyana_Export_4_digit_2010$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2010 <- aggregate(Guyana_Export_4_digit_2010$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2010$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2010) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2010 <- merge(Guyana_Export_4_digit_2010, Total_Trade_Guyana_4_digit_2010, by = "Year")
Guyana_Export_4_digit_2010$share <- (Guyana_Export_4_digit_2010$Trade.Value..US../Guyana_Export_4_digit_2010$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2010 <- aggregate(World_Export_4_digit_2010$World_Export, by=list(Category=World_Export_4_digit_2010$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2010) <- c("Year","Tot_Export_World")
World_Export_4_digit_2010 <- merge(World_Export_4_digit_2010, Total_Trade_World_4_digit_2010, by = "Year")
World_Export_4_digit_2010$Worldshare <- (World_Export_4_digit_2010$World_Export/World_Export_4_digit_2010$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2010 <- merge(Guyana_Export_4_digit_2010, World_Export_4_digit_2010, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2010$Comparative_Advantage <- Guyana_World_Export_4_digit_2010$share/Guyana_World_Export_4_digit_2010$Worldshare 
Guyana_World_Export_4_digit_2010$Normalized_CA <- (Guyana_World_Export_4_digit_2010$Trade.Value..US../Guyana_World_Export_4_digit_2010$Tot_Export_World) - ((Guyana_World_Export_4_digit_2010$World_Export * Guyana_World_Export_4_digit_2010$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2010$Tot_Export_World)^2)


Ranking2010 <- Guyana_World_Export_4_digit_2010[order(-Guyana_World_Export_4_digit_2010$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2010, "All_Products_Guyana_2010.csv")  




############################################ Other Years (2009)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2009",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2009 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2009, "All_Export_4_digits_All_Products_2009.csv")



Guyana_Export_4_digit_2009 <- World_Export_4_digit_2009[which(World_Export_4_digit_2009$Reporter == "Guyana"),]
World_Export_4_digit_2009 <-aggregate( World_Export_4_digit_2009$Trade.Value..US.., by=list(Category= World_Export_4_digit_2009$Commodity,  World_Export_4_digit_2009$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2009 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2009  <- Guyana_Export_4_digit_2009[which(Guyana_Export_4_digit_2009$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2009 <- aggregate(Guyana_Export_4_digit_2009$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2009$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2009) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2009 <- merge(Guyana_Export_4_digit_2009, Total_Trade_Guyana_4_digit_2009, by = "Year")
Guyana_Export_4_digit_2009$share <- (Guyana_Export_4_digit_2009$Trade.Value..US../Guyana_Export_4_digit_2009$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2009 <- aggregate(World_Export_4_digit_2009$World_Export, by=list(Category=World_Export_4_digit_2009$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2009) <- c("Year","Tot_Export_World")
World_Export_4_digit_2009 <- merge(World_Export_4_digit_2009, Total_Trade_World_4_digit_2009, by = "Year")
World_Export_4_digit_2009$Worldshare <- (World_Export_4_digit_2009$World_Export/World_Export_4_digit_2009$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2009 <- merge(Guyana_Export_4_digit_2009, World_Export_4_digit_2009, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2009$Comparative_Advantage <- Guyana_World_Export_4_digit_2009$share/Guyana_World_Export_4_digit_2009$Worldshare 
Guyana_World_Export_4_digit_2009$Normalized_CA <- (Guyana_World_Export_4_digit_2009$Trade.Value..US../Guyana_World_Export_4_digit_2009$Tot_Export_World) - ((Guyana_World_Export_4_digit_2009$World_Export * Guyana_World_Export_4_digit_2009$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2009$Tot_Export_World)^2)


Ranking2009 <- Guyana_World_Export_4_digit_2009[order(-Guyana_World_Export_4_digit_2009$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2009, "All_Products_Guyana_2009.csv")  


############################################ Other Years (2008)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2008",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2008 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2008, "All_Export_4_digits_All_Products_2008.csv")



Guyana_Export_4_digit_2008 <- World_Export_4_digit_2008[which(World_Export_4_digit_2008$Reporter == "Guyana"),]
World_Export_4_digit_2008 <-aggregate( World_Export_4_digit_2008$Trade.Value..US.., by=list(Category= World_Export_4_digit_2008$Commodity,  World_Export_4_digit_2008$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2008 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2008  <- Guyana_Export_4_digit_2008[which(Guyana_Export_4_digit_2008$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2008 <- aggregate(Guyana_Export_4_digit_2008$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2008$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2008) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2008 <- merge(Guyana_Export_4_digit_2008, Total_Trade_Guyana_4_digit_2008, by = "Year")
Guyana_Export_4_digit_2008$share <- (Guyana_Export_4_digit_2008$Trade.Value..US../Guyana_Export_4_digit_2008$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2008 <- aggregate(World_Export_4_digit_2008$World_Export, by=list(Category=World_Export_4_digit_2008$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2008) <- c("Year","Tot_Export_World")
World_Export_4_digit_2008 <- merge(World_Export_4_digit_2008, Total_Trade_World_4_digit_2008, by = "Year")
World_Export_4_digit_2008$Worldshare <- (World_Export_4_digit_2008$World_Export/World_Export_4_digit_2008$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2008 <- merge(Guyana_Export_4_digit_2008, World_Export_4_digit_2008, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2008$Comparative_Advantage <- Guyana_World_Export_4_digit_2008$share/Guyana_World_Export_4_digit_2008$Worldshare 
Guyana_World_Export_4_digit_2008$Normalized_CA <- (Guyana_World_Export_4_digit_2008$Trade.Value..US../Guyana_World_Export_4_digit_2008$Tot_Export_World) - ((Guyana_World_Export_4_digit_2008$World_Export * Guyana_World_Export_4_digit_2008$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2008$Tot_Export_World)^2)


Ranking2008 <- Guyana_World_Export_4_digit_2008[order(-Guyana_World_Export_4_digit_2008$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2008, "All_Products_Guyana_2008.csv")  




############################################ Other Years (2007)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2007",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2007 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2007, "All_Export_4_digits_All_Products_2007.csv")



Guyana_Export_4_digit_2007 <- World_Export_4_digit_2007[which(World_Export_4_digit_2007$Reporter == "Guyana"),]
World_Export_4_digit_2007 <-aggregate( World_Export_4_digit_2007$Trade.Value..US.., by=list(Category= World_Export_4_digit_2007$Commodity,  World_Export_4_digit_2007$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2007 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2007  <- Guyana_Export_4_digit_2007[which(Guyana_Export_4_digit_2007$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2007 <- aggregate(Guyana_Export_4_digit_2007$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2007$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2007) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2007 <- merge(Guyana_Export_4_digit_2007, Total_Trade_Guyana_4_digit_2007, by = "Year")
Guyana_Export_4_digit_2007$share <- (Guyana_Export_4_digit_2007$Trade.Value..US../Guyana_Export_4_digit_2007$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2007 <- aggregate(World_Export_4_digit_2007$World_Export, by=list(Category=World_Export_4_digit_2007$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2007) <- c("Year","Tot_Export_World")
World_Export_4_digit_2007 <- merge(World_Export_4_digit_2007, Total_Trade_World_4_digit_2007, by = "Year")
World_Export_4_digit_2007$Worldshare <- (World_Export_4_digit_2007$World_Export/World_Export_4_digit_2007$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2007 <- merge(Guyana_Export_4_digit_2007, World_Export_4_digit_2007, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2007$Comparative_Advantage <- Guyana_World_Export_4_digit_2007$share/Guyana_World_Export_4_digit_2007$Worldshare 
Guyana_World_Export_4_digit_2007$Normalized_CA <- (Guyana_World_Export_4_digit_2007$Trade.Value..US../Guyana_World_Export_4_digit_2007$Tot_Export_World) - ((Guyana_World_Export_4_digit_2007$World_Export * Guyana_World_Export_4_digit_2007$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2007$Tot_Export_World)^2)


Ranking2007 <- Guyana_World_Export_4_digit_2007[order(-Guyana_World_Export_4_digit_2007$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2007, "All_Products_Guyana_2007.csv")  


############################################ Other Years (2006)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2006",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2006 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2006, "All_Export_4_digits_All_Products_2006.csv")



Guyana_Export_4_digit_2006 <- World_Export_4_digit_2006[which(World_Export_4_digit_2006$Reporter == "Guyana"),]
World_Export_4_digit_2006 <-aggregate( World_Export_4_digit_2006$Trade.Value..US.., by=list(Category= World_Export_4_digit_2006$Commodity,  World_Export_4_digit_2006$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2006 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2006  <- Guyana_Export_4_digit_2006[which(Guyana_Export_4_digit_2006$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2006 <- aggregate(Guyana_Export_4_digit_2006$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2006$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2006) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2006 <- merge(Guyana_Export_4_digit_2006, Total_Trade_Guyana_4_digit_2006, by = "Year")
Guyana_Export_4_digit_2006$share <- (Guyana_Export_4_digit_2006$Trade.Value..US../Guyana_Export_4_digit_2006$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2006 <- aggregate(World_Export_4_digit_2006$World_Export, by=list(Category=World_Export_4_digit_2006$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2006) <- c("Year","Tot_Export_World")
World_Export_4_digit_2006 <- merge(World_Export_4_digit_2006, Total_Trade_World_4_digit_2006, by = "Year")
World_Export_4_digit_2006$Worldshare <- (World_Export_4_digit_2006$World_Export/World_Export_4_digit_2006$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2006 <- merge(Guyana_Export_4_digit_2006, World_Export_4_digit_2006, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2006$Comparative_Advantage <- Guyana_World_Export_4_digit_2006$share/Guyana_World_Export_4_digit_2006$Worldshare 
Guyana_World_Export_4_digit_2006$Normalized_CA <- (Guyana_World_Export_4_digit_2006$Trade.Value..US../Guyana_World_Export_4_digit_2006$Tot_Export_World) - ((Guyana_World_Export_4_digit_2006$World_Export * Guyana_World_Export_4_digit_2006$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2006$Tot_Export_World)^2)


Ranking2006 <- Guyana_World_Export_4_digit_2006[order(-Guyana_World_Export_4_digit_2006$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2006, "All_Products_Guyana_2006.csv")  



############################################ Other Years (2005)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2005",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2005 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2005, "All_Export_4_digits_All_Products_2005.csv")



Guyana_Export_4_digit_2005 <- World_Export_4_digit_2005[which(World_Export_4_digit_2005$Reporter == "Guyana"),]
World_Export_4_digit_2005 <-aggregate( World_Export_4_digit_2005$Trade.Value..US.., by=list(Category= World_Export_4_digit_2005$Commodity,  World_Export_4_digit_2005$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2005 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2005  <- Guyana_Export_4_digit_2005[which(Guyana_Export_4_digit_2005$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2005 <- aggregate(Guyana_Export_4_digit_2005$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2005$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2005) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2005 <- merge(Guyana_Export_4_digit_2005, Total_Trade_Guyana_4_digit_2005, by = "Year")
Guyana_Export_4_digit_2005$share <- (Guyana_Export_4_digit_2005$Trade.Value..US../Guyana_Export_4_digit_2005$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2005 <- aggregate(World_Export_4_digit_2005$World_Export, by=list(Category=World_Export_4_digit_2005$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2005) <- c("Year","Tot_Export_World")
World_Export_4_digit_2005 <- merge(World_Export_4_digit_2005, Total_Trade_World_4_digit_2005, by = "Year")
World_Export_4_digit_2005$Worldshare <- (World_Export_4_digit_2005$World_Export/World_Export_4_digit_2005$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2005 <- merge(Guyana_Export_4_digit_2005, World_Export_4_digit_2005, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2005$Comparative_Advantage <- Guyana_World_Export_4_digit_2005$share/Guyana_World_Export_4_digit_2005$Worldshare 
Guyana_World_Export_4_digit_2005$Normalized_CA <- (Guyana_World_Export_4_digit_2005$Trade.Value..US../Guyana_World_Export_4_digit_2005$Tot_Export_World) - ((Guyana_World_Export_4_digit_2005$World_Export * Guyana_World_Export_4_digit_2005$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2005$Tot_Export_World)^2)


Ranking2005 <- Guyana_World_Export_4_digit_2005[order(-Guyana_World_Export_4_digit_2005$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2005, "All_Products_Guyana_2005.csv")  


############################################ Other Years (2004)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2004",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2004 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2004, "All_Export_4_digits_All_Products_2004.csv")



Guyana_Export_4_digit_2004 <- World_Export_4_digit_2004[which(World_Export_4_digit_2004$Reporter == "Guyana"),]
World_Export_4_digit_2004 <-aggregate( World_Export_4_digit_2004$Trade.Value..US.., by=list(Category= World_Export_4_digit_2004$Commodity,  World_Export_4_digit_2004$Year), FUN=sum, na.rm = T)
names(World_Export_4_digit_2004 ) <- c("Commodity", "Year", "World_Export")



### shares for 6 digits
# Guyana
Guyana_Export_4_digit_2004  <- Guyana_Export_4_digit_2004[which(Guyana_Export_4_digit_2004$Aggregate.Level == "4"),]
Total_Trade_Guyana_4_digit_2004 <- aggregate(Guyana_Export_4_digit_2004$Trade.Value..US.., by=list(Category=Guyana_Export_4_digit_2004$Year), FUN=sum, na.rm = T)
names(Total_Trade_Guyana_4_digit_2004) <- c("Year","Tot_Export_Guyana")
Guyana_Export_4_digit_2004 <- merge(Guyana_Export_4_digit_2004, Total_Trade_Guyana_4_digit_2004, by = "Year")
Guyana_Export_4_digit_2004$share <- (Guyana_Export_4_digit_2004$Trade.Value..US../Guyana_Export_4_digit_2004$Tot_Export_Guyana)



# World
#World <- World[which(World$Aggregate.Level == "6"),]
Total_Trade_World_4_digit_2004 <- aggregate(World_Export_4_digit_2004$World_Export, by=list(Category=World_Export_4_digit_2004$Year), FUN=sum, na.rm = T)
names(Total_Trade_World_4_digit_2004) <- c("Year","Tot_Export_World")
World_Export_4_digit_2004 <- merge(World_Export_4_digit_2004, Total_Trade_World_4_digit_2004, by = "Year")
World_Export_4_digit_2004$Worldshare <- (World_Export_4_digit_2004$World_Export/World_Export_4_digit_2004$Tot_Export_World)


#Guyana <- Guyana$data
Guyana_World_Export_4_digit_2004 <- merge(Guyana_Export_4_digit_2004, World_Export_4_digit_2004, by = c("Year", "Commodity"))


## merge Guy and World share
Guyana_World_Export_4_digit_2004$Comparative_Advantage <- Guyana_World_Export_4_digit_2004$share/Guyana_World_Export_4_digit_2004$Worldshare 
Guyana_World_Export_4_digit_2004$Normalized_CA <- (Guyana_World_Export_4_digit_2004$Trade.Value..US../Guyana_World_Export_4_digit_2004$Tot_Export_World) - ((Guyana_World_Export_4_digit_2004$World_Export * Guyana_World_Export_4_digit_2004$Tot_Export_Guyana)/(Guyana_World_Export_4_digit_2004$Tot_Export_World)^2)


Ranking2004 <- Guyana_World_Export_4_digit_2004[order(-Guyana_World_Export_4_digit_2004$Normalized_CA),] 
setwd("C:/Users/BUBBICO.FAODOMAIN/Desktop/Guyana_project/data_analysis/Diversification")
write.csv(Ranking2004, "All_Products_Guyana_2004.csv")  





############################ shares_As_Averages_2004_2017
data_2013 <- read.csv("All_Products_Guyana_2013.csv", sep =",", header = T)
data_2012 <- read.csv("All_Products_Guyana_2012.csv", sep =",", header = T)
data_2011 <- read.csv("All_Products_Guyana_2011.csv", sep =",", header = T)
data_2010 <- read.csv("All_Products_Guyana_2010.csv", sep =",", header = T)
data_2009 <- read.csv("All_Products_Guyana_2009.csv", sep =",", header = T)
data_2008 <- read.csv("All_Products_Guyana_2008.csv", sep =",", header = T)
data_2007 <- read.csv("All_Products_Guyana_2007.csv", sep =",", header = T)
data_2006 <- read.csv("All_Products_Guyana_2006.csv", sep =",", header = T)
data_2005 <- read.csv("All_Products_Guyana_2005.csv", sep =",", header = T)
data_2004 <- read.csv("All_Products_Guyana_2004.csv", sep =",", header = T)
data_2014 <- read.csv("All_Products_Guyana_2014.csv", sep =",", header = T)
data_2015 <- read.csv("All_Products_Guyana_2015.csv", sep =",", header = T)
data_2016 <- read.csv("All_Products_Guyana_2016.csv", sep =",", header = T)
data_2017 <- read.csv("All_Products_Guyana_2017.csv", sep =",", header = T)

data_2004$share_2004 <- data_2004$share
data_2004$Normalized_CA_2004 <- data_2004$Normalized_CA
data_2004 <- data_2004[,c(3,24,44,45)]

data_2005$share_2005 <- data_2005$share
data_2005$Normalized_CA_2005 <- data_2005$Normalized_CA
data_2005 <- data_2005[,c(3,24,44,45)]

data_2006$share_2006 <- data_2006$share
data_2006$Normalized_CA_2006 <- data_2006$Normalized_CA
data_2006 <- data_2006[,c(3,24,44,45)]

data_2007$share_2007 <- data_2007$share
data_2007$Normalized_CA_2007 <- data_2007$Normalized_CA
data_2007 <- data_2007[,c(3,24,44,45)]

data_2008$share_2008 <- data_2008$share
data_2008$Normalized_CA_2008 <- data_2008$Normalized_CA
data_2008 <- data_2008[,c(3,24,44,45)]

data_2009$share_2009 <- data_2009$share
data_2009$Normalized_CA_2009 <- data_2009$Normalized_CA
data_2009 <- data_2009[,c(3,24,44,45)]

data_2010$share_2010 <- data_2010$share
data_2010$Normalized_CA_2010 <- data_2010$Normalized_CA
data_2010 <- data_2010[,c(3,24,44,45)]

data_2011$share_2011 <- data_2011$share
data_2011$Normalized_CA_2011 <- data_2011$Normalized_CA
data_2011 <- data_2011[,c(3,24,44,45)]

data_2012$share_2012 <- data_2012$share
data_2012$Normalized_CA_2012 <- data_2012$Normalized_CA
data_2012 <- data_2012[,c(3,24,44,45)]

data_2013$share_2013 <- data_2013$share
data_2013$Normalized_CA_2013 <- data_2013$Normalized_CA
data_2013 <- data_2013[,c(3,24,44,45)]

data_2014$share_2014 <- data_2014$share
data_2014$Normalized_CA_2014 <- data_2014$Normalized_CA
data_2014 <- data_2014[,c(3,24,44,45)]


data_2015$share_2015 <- data_2015$share
data_2015$Normalized_CA_2015 <- data_2015$Normalized_CA
data_2015 <- data_2015[,c(3,24,44,45)]


data_2016$share_2016 <- data_2016$share
data_2016$Normalized_CA_2016 <- data_2016$Normalized_CA
data_2016 <- data_2016[,c(3,24,44,45)]


data_2017$share_2017 <- data_2017$share
data_2017$Normalized_CA_2017 <- data_2017$Normalized_CA
data_2017 <- data_2017[,c(3,24,44,45)]


data_tot <- merge(data_2004, data_2005, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2006, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2007, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2008, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2009, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2010, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2011, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2012, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2013, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2014, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2015, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2016, by = "Commodity.Code", all = T)
data_tot <- merge(data_tot, data_2017, by = "Commodity.Code", all = T)



data_tot$Average_Share_2004_2017 <- rowMeans(data_tot[,c(3,6,9,12,15,18,21,24,27,30,33,36,39,42)], na.rm = T)
data_tot$Average_Norm_Camp_Advantage_2004_2017 <- rowMeans(data_tot[,c(4,7,10,13,16,19,22,25,28,31,34,37,40,43)], na.rm = T)
data_tot <- data_tot[, c(1,2,44,45)]
write.csv(data_tot, "data_export_share_comp_Adv_Aerage_2004_2017.csv")









################################################################################
##### Data Gaulier et al. (2013)


# I want Montly data

# Which products have a positive comparative advantage in 2017?
Guyana_World_2017 <- Guyana_World[which(Guyana_World$Year == 2017 & Guyana_World$Aggregate.Level == 6),]
Guyana_World_2017 <- Guyana_World_2017[which(Guyana_World_2017$Normalized_CA > 0),]
write.csv(Guyana_World_2017, "Guyana_products_comparative_adv2017.csv")




Guyana_Export_2016 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("201601, 201602, 201603, 201604, 201605, 201606, 201607, 201608, 201609, 201610, 201611, 201612"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_2016 <- Guyana_Export_2016$data

Guyana_Export_2015 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("201501, 201502, 201503, 201504, 201505, 201506, 201507, 201508, 201509, 201510, 201511, 201512"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))
Guyana_Export_2015 <- Guyana_Export_2015$data

Guyana_Export_2014 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("201401, 201402, 201403, 201404, 201405, 201406, 201407, 201408, 201409, 201410, 201411, 201412"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2014 <- Guyana_Export_2014$data


Guyana_Export_2013 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("201301, 201302, 201303, 201304, 201305, 201306, 201307, 201308, 201309, 201310, 201311, 201312"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2013 <- Guyana_Export_2013$data

Guyana_Export_2012 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("201201, 201202, 201203, 201204, 201205, 201206, 201207, 201208, 201209, 201210, 201211, 201212"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2012 <- Guyana_Export_2012$data

Guyana_Export_2011 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("201101, 201102, 201103, 201104, 201105, 201106, 201107, 201108, 201109, 201110, 201111, 201112"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2011 <- Guyana_Export_2011$data

Guyana_Export_2010 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("201001, 201002, 201003, 201004, 201005, 201006, 201007, 201008, 201009, 201010, 201011, 201012"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2010 <- Guyana_Export_2010$data

Guyana_Export_2009 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("200901, 200902, 200903, 200904, 200905, 200906, 200907, 200908, 200909, 200910, 200911, 200912"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2009 <- Guyana_Export_2009$data

Guyana_Export_2008 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("200801, 200802, 200803, 200804, 200805, 200806, 200807, 200808, 200809, 200810, 200811, 200812"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2008 <- Guyana_Export_2008$data

Guyana_Export_2007 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("200701, 200702, 200703, 200704, 200705, 200706, 200707, 200708, 200709, 200710, 200711, 200712"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2007 <- Guyana_Export_2007$data

Guyana_Export_2006 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("200601, 200602, 200603, 200604, 200605, 200606, 200607, 200608, 200609, 200610, 200611, 200612"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2006 <- Guyana_Export_2006$data


Guyana_Export_2005 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("200501, 200502, 200503, 200504, 200505, 200506, 200507, 200508, 200509, 200510, 200511, 200512"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2005 <- Guyana_Export_2005$data

Guyana_Export_2004 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("200401, 200402, 200403, 200404, 200405, 200406, 200407, 200408, 200409, 200410, 200411, 200412"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2004 <- Guyana_Export_2004$data

Guyana_Export_2003 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("200301, 200302, 200303, 200304, 200305, 200306, 200307, 200308, 200309, 200310, 200311, 200312"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2003 <- Guyana_Export_2003$data

Guyana_Export_2002 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("200201, 200202, 200203, 200204, 200205, 200206, 200207, 200208, 200209, 200210, 200211, 200212"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2002 <- Guyana_Export_2002$data

Guyana_Export_2001 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("200101, 200102, 200103, 200104, 200105, 200106, 200107, 200108, 200109, 200110, 200111, 200112"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2001 <- Guyana_Export_2001$data


Guyana_Export_2000 <- get.Comtrade(r="328", p="0", rg = "2",fmt="csv", ps = c("200001, 200002, 200003, 200004, 200005, 200006, 200007, 200008, 200009, 200010, 200011, 200012"), freq="M",
                                   cc = c("150600,	151610,	51199,	71490,	70920,	70930,	80300,	200559,	200551,	70820,	71022,	40700,	230240,	190590,	100640,	60120,	70490,	170111,	170199,	90830,	151530,	70410,	70940,	110290,	100890,	190490,	80920,	81210,	170410,	70529,	240220,	240290,	90620,	80590,	200791,	90700,	180400,	180500,	151311,	151319,	90122,	90121,	160510,	30614,	30624,	160540,	30629,	70700,	80410,	30265,	30375,	30559,	81340,	71290,	151790,	200899,	41000,	210120,	150430,	220600,	80420,	30379,	30530,	30270,	30380,	30569,	30269,	151590,	30229,	110620,	110610,	110630,	120890,	30510,	60499,	210690,	40610,	81090,	30619,	81190,	81290,	90420,	70960,	70320,	110430,	220850,	91010,	190520,	170230,	80540,	80610,	110319,	110311,	80450,	160241,	200710,	100620,	220190,	210500,	200980,	71333,	130190,	70390,	70890,	71029,	70519,	30110,	10392,	30191,	30520,	121299,	30264,	160415,	110220,	151529,	190190,	80520,	71410,	110814,	240399,	151710,	40299,	40291,	40229,	220110,	190120,	91091,	200892,	200990,	81350,	71090,	170390,	30799,	160590,	130239,	130232,	40900,	220290,	90810,	200819,	80290,	230650,	150990,	150910,	70310,	71220,	200911,	80510,	110429,	30710,	30541,	200891,	151329,	80720,	190230,	80820,	71310,	200540,	71021,	90412,	90411,	80430,	200820,	121190,	70190,	200520,	190110,	230990,	200799,	160290,	210230,	190410,	160250,	160249,	40630,	210610,	81120,	100610,	210130,	220840,	190540,	30329,	210390,	160100,	90910,	90920,	90930,	120999,	100630,	160520,	30613,	30549,	30760,	210410,	120100,	210310,	220410,	91099,	70970,	220820,	190220,	170490,	170290,	200580,	71420,	190300,	90220,	90210,	210320,	70200,	200290,	30211,	30321,	30349,	160414,	91030,	190211,	190219,	220890,	60210,	140490,	130219,	71190,	200190,	200600,	71080,	70990,	220900,	220210,	110900,	100190,	110100,	220429" ))

Guyana_Export_2000 <- Guyana_Export_2000$data


Guyana_Export <-  rbind(Guyana_Export_2000,Guyana_Export_2001,Guyana_Export_2002,Guyana_Export_2003,Guyana_Export_2004,Guyana_Export_2005,Guyana_Export_2006,Guyana_Export_2007,Guyana_Export_2008,Guyana_Export_2009,Guyana_Export_2010,Guyana_Export_2011,Guyana_Export_2012,Guyana_Export_2013,Guyana_Export_2014,Guyana_Export_2015,Guyana_Export_2016)


#montly data are available only from 2013. Then, we do 2013-2016 
Guyana_Export <- Guyana_Export[which(Guyana_Export$Year != 'NA'),]

#Guyana_Export <- Guyana_Export[,c(2,22,32)]
##### Equation 1
# I spli the dataset for each commodity

Guyana_Export_s <- split(Guyana_Export, list(Guyana_Export$Year, Guyana_Export$Commodity.Code))
g_ickt <-  sapply(Guyana_Export_s, function (x) diff(x[,"Trade.Value..US.."], lag = 1, differences = 1)/((1/2)*rollapply(x[,"Trade.Value..US.."], 2, sum)))
g_ickt <- unlist(g_ickt)
#### equation 2






Guyana <- World[which(World$Reporter == "Guyana"),]
World <-aggregate(World$Trade.Value..US.., by=list(Category=World$Commodity, World$Year), FUN=sum, na.rm = T)
names(World) <- c("Commodity", "Year", "World_Export")







############################################ Other Years (2003)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2003",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2003 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2003, "All_Export_4_digits_All_Products_2003.csv")


############################################ Other Years (2002)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2003,	2002,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2002",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2002 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2002, "All_Export_4_digits_All_Products_2002.csv")



############################################ Other Years (2001)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2001",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2001 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2001, "All_Export_4_digits_All_Products_2001.csv")


############################################ Other Years (2000)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "2000",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_2000 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_2000, "All_Export_4_digits_All_Products_2000.csv")


############################################ Other Years (1999)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1999",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_1999 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_1999, "All_Export_4_digits_All_Products_1999.csv")




############################################ Other Years (1998)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1998",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_1998 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_1998, "All_Export_4_digits_All_Products_1998.csv")



############################################ Other Years (1997)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1997",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_1997 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_1997, "All_Export_4_digits_All_Products_1997.csv")



############################################ Other Years (1996)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1996",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_1996 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_1996, "All_Export_4_digits_All_Products_1996.csv")


############################################ Other Years (1995)
###########################################
World1 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                       cc = c("0101,	0102,	0103,	0104,	0105,	0106,	0201,	0202,	0203,	0204,	0205,	0206,	0207,	0208,	0209,	0210"))	

World2 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                       cc = c("0301,	0302,	0303,	0304,	0305,	0306,	0307,0308,	0401,	0402,	0403,	0404,	0405,	0406,	0407,	0408,	0409,	0410,	0501,	0502,	0503,	0504,	0505,	0506,	0507,	0508,	0509,	0510,	0511"))	

World3 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                       cc = c("0601,	0602,	0603,	0604,	0701,	0702,	0703,	0704,	0705,	0706,	0707,	0708,	0709,	0710,	0711,	0712,	0713,	0714,	0801,	0802,	0803,	0804,	0805,	0806,	0807,	0808,	0809,	0810,	0811,	0812,	0813,	0814"))	

World4 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                       cc = c("0901,	0902,	0903,	0904,	0905,	0906,	0907,	0908,	0909,	0910,	1001,	1002,	1003,	1004,	1005,	1006,	1007,	1008,	1101,	1102,	1103,	1104,	1105,	1106,	1107,	1108,	1109"))	

World5 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                       cc = c("1201,	1202,	1203,	1204,	1205,	1206,	1207,	1208,	1209,	1210,	1211,	1212,	1213,	1214,	1301,	1302,	1401,	1402,	1403,	1404"))	

World6 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                       cc = c("1501,	1502,	1503,	1504,	1505,	1506,	1507,	1508,	1509,	1510,	1511,	1512,	1513,	1514,	1515,	1516,	1517,	1518,	1519,	1520,	1521,	1522,	1601,	1602,	1603,	1604,	1605"))	

World7 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                       cc = c("1701,	1702,	1703,	1704,	1801,	1802,	1803,	1804,	1805,	1806,	1901,	1902,	1903,	1904,	1905,	2001,	2002,	2003,	2004,	2005,	2006,	2007,	2008,	2009"))	

World9 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                       cc = c("2101,	2102,	2103,	2104,	2105,	2106,	2201,	2202,	2203,	2204,	2205,	2206,	2207,	2208,	2209,	2301,	2302,	2303,	2304,	2305,	2306,	2307,	2308,	2309,	2401,	2402,	2403"))	

World10 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("2501,	2502,	2503,	2504,	2505,	2506,	2507,	2508,	2509,	2510,	2511,	2512,	2513,	2514,	2515,	2516,	2517,	2518,	2519,	2520,	2521,	2522,	2523,	2524,	2525,	2526,	2527,	2528,	2529,	2530"))	


World12 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("2601,	2602,	2603,	2604,	2605,	2606,	2607,	2608,	2609,	2610,	2611,	2612,	2613,	2614,	2615,	2616,	2617,	2618,	2619,	2620,	2621,	2701,	2702,	2703,	2704,	2705,	2706,	2707,	2708,	2709,	2710,	2711,	2712,	2713,	2714,	2715,	2716"))	

World13 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("2801,	2802,	2803,	2804,	2805,	2806,	2807,	2808,	2809,	2810,	2811,	2812,	2813,	2814,	2815,	2816,	2817,	2818,	2819,	2820,	2821,	2822,	2823,	2824,	2825,	2826,	2827,	2828,	2829,	2830,	2831,	2832,	2833,	2834,	2835,	2836,	2837,	2838,	2839,	2840,	2841,	2842,	2843,	2844,	2845,	2846,	2847,	2848,	2849,	2850,	2851,	2852,2853"))	

World14 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("2901,	2902,	2903,	2904,	2905,	2906,	2907,	2908,	2909,	2910,	2911,	2912,	2913,	2914,	2915,	2916,	2917,	2918,	2919,	2920,	2921,	2922,	2923,	2924,	2925,	2926,	2927,	2928,	2929,	2930,	2931,	2932,	2933,	2934,	2935,	2936,	2937,	2938,	2939,	2940,	2941,	2942"))	


World16 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("3001,	3002,	3003,	3004,	3005,	3006,	3101,	3102,	3103,	3104,	3105,	3201,	3202,	3203,	3204,	3205,	3206,	3207,	3208,	3209,	3210,	3211,	3212,	3213,	3214,	3215"))	

World17 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("3301,	3302,	3303,	3304,	3305,	3306,	3307,	3401,	3402,	3403,	3404,	3405,	3406,	3407,	3501,	3502,	3503,	3504,	3505,	3506,	3507,	3601,	3602,	3603,	3604,	3605,	3606"))	

World18 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("3701,	3702,	3703,	3704,	3705,	3706,	3707,	3801,	3802,	3803,	3804,	3805,	3806,	3807,	3808,	3809,	3810,	3811,	3812,	3813,	3814,	3815,	3816,	3817,	3818,	3819,	3820,	3821,	3822,	3823,3824,3825,3826"))	



World19 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("3901,	3902,	3903,	3904,	3905,	3906,	3907,	3908,	3909,	3910,	3911,	3912,	3913,	3914,	3915,	3916,	3917,	3918,	3919,	3920,	3921,	3922,	3923,	3924,	3925,	3926,	4001,	4002,	4003,	4004,	4005,	4006,	4007,	4008,	4009,	4010,	4011,	4012,	4013,	4014,	4015,	4016,	4017"))	

World20 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("4101,	4102,	4103,	4104,	4105,	4106,	4107,	4108,	4109,	4110,	4111,4112,4113,4114,4115,	4201,	4202,	4203,	4204,	4205,	4206"))	

World21 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("4301,	4302,	4303,	4304,	4401,	4402,	4403,	4404,	4405,	4406,	4407,	4408,	4409,	4410,	4411,	4412,	4413,	4414,	4415,	4416,	4417,	4418,	4419,	4420,	4421,	4501,	4502,	4503,	4504"))	

World22 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("4601,	4602,	4701,	4702,	4703,	4704,	4705,	4706,	4707,	4801,	4802,	4803,	4804,	4805,	4806,	4807,	4808,	4809,	4810,	4811,	4812,	4813,	4814,	4815,	4816,	4817,	4818,	4819,	4820,	4821,	4822,	4823"))	

World23 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("4901,	4902,	4903,	4904,	4905,	4906,	4907,	4908,	4909,	4910,	4911,	5001,	5002,	5003,	5004,	5005,	5006,	5007,	5101,	5102,	5103,	5104,	5105,	5106,	5107,	5108,	5109,	5110,	5111,	5112,	5113,	5201,	5202,	5203,	5204,	5205,	5206,	5207,	5208,	5209,	5210,	5211,	5212,	5301,	5302,	5303,	5304,	5305,	5306,	5307,	5308,	5309,	5310,	5311,	5401,	5402,	5403,	5404,	5405,	5406,	5407,	5408,	5501,	5502,	5503,	5504,	5505,	5506,	5507,	5508,	5509,	5510,	5511,	5512,	5513,	5514,	5515,	5516,	5601,	5602,	5603,	5604,	5605,	5606,	5607,	5608,	5609"))	


World24 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("5701,	5702,	5703,	5704,	5705,	5801,	5802,	5803,	5804,	5805,	5806,	5807,	5808,	5809,	5810,	5811,	5901,	5902,	5903,	5904,	5905,	5906,	5907,	5908,	5909,	5910,	5911,	6001,	6002,6003,6004,6005,6006,	6101,	6102,	6103,	6104,	6105,	6106,	6107,	6108,	6109,	6110,	6111,	6112,	6113,	6114,	6115,	6116,	6117,	6201,	6202,	6203,	6204,	6205,	6206,	6207,	6208,	6209,	6210,	6211,	6212,	6213,	6214,	6215,	6216,	6217,	6301,	6302,	6303,	6304,	6305,	6306,	6307,	6308,	6309,	6310,	6401,	6402,	6403,	6404,	6405,	6406,	6501,	6502,	6503,	6504,	6505,	6506,	6507,	6601,	6602,	6603,	6701,	6702,	6703,	6704,	6801,	6802,	6803,	6804,	6805,	6806,	6807,	6808,	6809,	6810,	6811,	6812,	6813,	6814,	6815,	6901,	6902,	6903,	6904,	6905,	6906,	6907,	6908,	6909,	6910,	6911,	6912,	6913,	6914"))	

World25 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("7001,	7002,	7003,	7004,	7005,	7006,	7007,	7008,	7009,	7010,	7011,	7012,	7013,	7014,	7015,	7016,	7017,	7018,	7019,	7020,	7101,	7102,	7103,	7104,	7105,	7106,	7107,	7108,	7109,	7110,	7111,	7112,	7113,	7114,	7115,	7116,	7117,	7118,	7201,	7202,	7203,	7204,	7205,	7206,	7207,	7208,	7209,	7210,	7211,	7212,	7213,	7214,	7215,	7216,	7217,	7218,	7219,	7220,	7221,	7222,	7223,	7224,	7225,	7226,	7227,	7228,	7229,	7301,	7302,	7303,	7304,	7305,	7306,	7307,	7308,	7309,	7310,	7311,	7312,	7313,	7314,	7315,	7316,	7317,	7318,	7319,	7320,	7321,	7322,	7323,	7324,	7325,	7326,	7401,	7402,	7403,	7404,	7405,	7406,	7407,	7408,	7409,	7410,	7411,	7412,	7413,	7414,	7415,	7416,	7417,	7418,	7419,	7501,	7502,	7503,	7504,	7505,	7506,	7507,	7508,	7601,	7602,	7603,	7604,	7605,	7606,	7607,	7608,	7609,	7610,	7611,	7612,	7613,	7614,	7615,	7616"))	

World26 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("7801,	7802,	7803,	7804,	7805,	7806,	7901,	7902,	7903,	7904,	7905,	7906,	7907,	8001,	8002,	8003,	8004,	8005,	8006,	8007,	8101,	8102,	8103,	8104,	8105,	8106,	8107,	8108,	8109,	8110,	8111,	8112,	8113,	8201,	8202,	8203,	8204,	8205,	8206,	8207,	8208,	8209,	8210,	8211,	8212,	8213,	8214,	8215,	8301,	8302,	8303,	8304,	8305,	8306,	8307,	8308,	8309,	8310,	8311,	8401,	8402,	8403,	8404,	8405,	8406,	8407"))	

World27 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("8408,	8409,	8410,	8411,	8412,	8413,	8414,	8415,	8416,	8417,	8418,	8419,	8420,	8421,	8422,	8423,	8424,	8425,	8426,	8427,	8428,	8429,	8430,	8431,	8432,	8433,	8434,	8435,	8436,	8437,	8438,	8439,	8440,	8441,	8442,	8443,	8444,	8445,	8446,	8447,	8448,	8449,	8450,	8451,	8452,	8453,	8454,	8455,	8456,	8457,	8458,	8459,	8460,	8461,	8462,	8463,	8464,	8465,	8466,	8467,	8468,	8469,	8470,	8471,	8472,	8473,	8474,	8475,	8476,	8477,	8478,	8479,	8480,	8481,	8482,	8483,	8484,	8485,8486,8487,	8501,	8502,	8503,	8504,	8505,	8506,	8507,	8508,	8509,	8510,	8511,	8512,	8513,	8514,	8515,	8516,	8517,	8518,	8519,	8520,	8521,	8522,	8523,	8524,	8525,	8526,	8527,	8528,	8529,	8530,	8531,	8532,	8533,	8534,	8535,	8536,	8537,	8538,	8539,	8540,	8541,	8542,	8543,	8544,	8545,	8546,	8547,	8548,	8601,	8602,	8603,	8604,	8605,	8606,	8607,	8608,	8609"))	

World28 <- get.Comtrade(r="all", p="0", rg = "2",fmt="csv", ps = "1995",
                        cc = c("8701,	8702,	8703,	8704,	8705,	8706,	8707,	8708,	8709,	8710,	8711,	8712,	8713,	8714,	8715,	8716,	8801,	8802,	8803,	8804,	8805,	8901,	8902,	8903,	8904,	8905,	8906,	8907,	8908,	9001,	9002,	9003,	9004,	9005,	9006,	9007,	9008,	9009,	9010,	9011,	9012,	9013,	9014,	9015,	9016,	9017,	9018,	9019,	9020,	9021,	9022,	9023,	9024,	9025,	9026,	9027,	9028,	9029,	9030,	9031,	9032,	9033,	9101,	9102,	9103,	9104,	9105,	9106,	9107,	9108,	9109,	9110,	9111,	9112,	9113,	9114,	9201,	9202,	9203,	9204,	9205,	9206,	9207,	9208,	9209,	9301,	9302,	9303,	9304,	9305,	9306,	9307,	9401,	9402,	9403,	9404,	9405,	9406,	9501,	9502,	9503,	9504,	9505,	9506,	9507,	9508,	9601,	9602,	9603,	9604,	9605,	9606,	9607,	9608,	9609,	9610,	9611,	9612,	9613,	9614,	9615,	9616,	9617,	9618,9619,9620,	9701,	9702,	9703,	9704,	9705,	9706,	9999"))	




World1a <- World1$data
World2a <- World2$data
World3a <- World3$data
World4a <- World4$data
World5a <- World5$data
World6a <- World6$data
World7a <- World7$data
World9a <- World9$data
World10a <- World10$data
World12a <- World12$data
World13a <- World13$data
World14a <- World14$data
World16a <- World16$data
World17a <- World17$data
World18a <- World18$data
World19a <- World19$data
World20a <- World20$data
World21a <- World21$data
World22a <- World22$data
World23a <- World23$data
World24a <- World24$data
World25a <- World25$data
World26a <- World26$data
World27a <- World27$data
World28a <- World28$data







World_Export_4_digit_1995 <-  rbind(World1a,World2a,World3a,World4a,World5a,World6a,World7a,World9a,
                                    World10a,World12a,World13a,World14a,World16a,World17a,World18a,
                                    World19a,World20a,World21a,World22a,World23a,World24a,World25a,World26a,World27a,World28a)


write.csv(World_Export_4_digit_1995, "All_Export_4_digits_All_Products_1995.csv")


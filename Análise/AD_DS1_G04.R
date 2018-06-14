###  Loading dataset  ###
sales <- read.csv("~/Desktop/SalesKaggle3.csv")



###  Get a first sense of the data  ###
dim(sales)
colnames(sales)
head(sales)



###  Tidy the column names (remove "_" symbol)  ###
colnames(sales) <- gsub("_","", colnames(sales))
colnames(sales)



###  Get a more detailed sense of the data. Initial exploration of atributes considered relevant at first glance - FileType, SoldFlag, MarketingType  ###
str(sales)
summary(sales)

#sales$SoldFlag <- as.factor(sales$SoldFlag)
#sales$NewReleaseFlag <- as.factor(sales$NewReleaseFlag)
#str(sales)
#summary(sales)
#sales$ReleaseYear <- as.factor(sales$ReleaseYear)

table(sales$FileType)                                       # QUANTIDADE de registos do tipo "Historical" e "Active"
prop.table(table(sales$FileType))*100                       # PERCENTAGEM de registos do tipo "Historical" e "Active"
table(sales$FileType, sales$SoldFlag)                       # QUANTIDADE de registos do tipo "Historical" e "Active" que foram vendidos(1) e não vendidos(2). Note-se que os "Active" estão a 0 uma vez que não possuem informação acerca se foram ou não vendidos.
prop.table(table(sales$FileType, sales$SoldFlag))*100       # PERCENTAGEM de produtos vendidos(1) e não vendidos(2) para cada tipo de registo ("Historical", "Active"). 
# Em alternativa à função "prop.table" utilizada acima podia-se efetuar o calculo de forma manual. E.g.: ((sum(sales$FileType == "Historical" & sales$SoldFlag == "0")/sum(sales$FileType == "Historical")))*100.
table(sales$MarketingType)                                  # QUANTIDADE de produtos em cada tipo de marketing. Atenção que inclui os produtos "Active" que não possuem informação acerca dos que foram vendidos e da quantidade vendida.
table(sales$MarketingType, sales$FileType == "Historical")  # QUANTIDADE de produtos em cada tipo de marketing para cada tipo de registo ("TRUE -> Historical", "FALSE -> Active")



###  Split the sales data into "Historical" and "Active"  ###
historical_sales <- subset(sales, FileType == "Historical")
active_sales <- subset(sales, FileType == "Active")



###  Check the correlation between attributes  ###
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(historical_sales[,c(-1,-2,-3,-6,-7)], lower.panel = panel.cor, upper.panel = panel.smooth)

# Alternative visualization where only the correlated values are shown as "heatmap"
corMatrix<-cor(historical_sales[,c(-1,-2,-3,-6,-7)])
heatmap(corMatrix)
heatmap(corMatrix, col = topo.colors(16))

# Inprove the visualization of the heatmap and save to a PDF
library(gplots)
pdf("Correlation_Heatmap_3.pdf",w=12, h=12)
par(oma=c(10,0,0,10))
heatmap.2(corMatrix, col = redblue(16),trace="none")
dev.off()


#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################

###  Plot all attributes in a histogram (numerical attributes)  ###
# par(mfrow=c(1,1))
# par(mfrow=c(5,3))                                           # Total 14 atributos -> grelha de 5x3 é suficiente
# for (att in colnames(sales)[c(-2,-4,-5,-6)]){               # Atributos não numéricos são retirados
#   hist(sales[,att], main=att, col="red", xlab="value")
# }

# hist(sales$SoldFlag, col = "orange")
# barplot(table(sales$SoldFlag),las=2, col="orange")
# hist(sales$SoldCount, col = "orange")
# barplot(table(sales$SoldCount),las=2, col="orange")
# barplot(table(sales$MarketingType),las=2, col="orange")
# barplot(table(sales$NewReleaseFlag), las=1, col = "orange")
# barplot(table(sales$StrengthFactor), las=2, col = "orange")
# hist(sales$StrengthFactor, col = "orange")
# hist(sales$ReleaseYear, col = "orange")
# barplot(table(sales$ReleaseYear), las=1, col = "orange")






#table(sales$MarketingType, sales$SoldFlag)
#table(sales$FileType, sales$MarketingType)


library(reshape2)
storm <- read.csv("repdata_data_StormData.csv")
head(storm)

event <- c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
data <- storm[event]

factor(unique(data$PROPDMGEXP))

data$PROPEXP[data$PROPDMGEXP == "K"] <- 1000
data$PROPEXP[data$PROPDMGEXP == "M"] <- 1e+06
data$PROPEXP[data$PROPDMGEXP == ""] <- 1
data$PROPEXP[data$PROPDMGEXP == "B"] <- 1e+09
data$PROPEXP[data$PROPDMGEXP == "m"] <- 1e+06
data$PROPEXP[data$PROPDMGEXP == "0"] <- 1
data$PROPEXP[data$PROPDMGEXP == "5"] <- 1e+05
data$PROPEXP[data$PROPDMGEXP == "6"] <- 1e+06
data$PROPEXP[data$PROPDMGEXP == "4"] <- 10000
data$PROPEXP[data$PROPDMGEXP == "2"] <- 100
data$PROPEXP[data$PROPDMGEXP == "3"] <- 1000
data$PROPEXP[data$PROPDMGEXP == "h"] <- 100
data$PROPEXP[data$PROPDMGEXP == "7"] <- 1e+07
data$PROPEXP[data$PROPDMGEXP == "H"] <- 100
data$PROPEXP[data$PROPDMGEXP == "1"] <- 10
data$PROPEXP[data$PROPDMGEXP == "8"] <- 1e+08
data$PROPEXP[data$PROPDMGEXP == "+"] <- 0
data$PROPEXP[data$PROPDMGEXP == "-"] <- 0
data$PROPEXP[data$PROPDMGEXP == "?"] <- 0
data$PROPDMGVAL <- data$PROPDMG * data$PROPEXP

factor(unique(data$CROPDMGEXP))

data$CROPEXP[data$CROPDMGEXP == "M"] <- 1e+06
data$CROPEXP[data$CROPDMGEXP == "K"] <- 1000
data$CROPEXP[data$CROPDMGEXP == "m"] <- 1e+06
data$CROPEXP[data$CROPDMGEXP == "B"] <- 1e+09
data$CROPEXP[data$CROPDMGEXP == "0"] <- 1
data$CROPEXP[data$CROPDMGEXP == "k"] <- 1000
data$CROPEXP[data$CROPDMGEXP == "2"] <- 100
data$CROPEXP[data$CROPDMGEXP == ""] <- 1
data$CROPEXP[data$CROPDMGEXP == "?"] <- 0
data$CROPDMGVAL <- data$CROPDMG * data$CROPEXP

fatal <- aggregate(data$FATALITIES~data$EVTYPE, FUN = sum)
colnames(fatal) <- c("EVTYPE", "FATALITIES")
injury <- aggregate(data$INJURIES~data$EVTYPE, FUN = sum)
colnames(injury) <- c("EVTYPE", "INJURIES")
fatal8 <- fatal[order(-fatal$FATALITIES), ][1:10, ]
injury8 <- injury[order(-injury$INJURIES), ][1:10, ]
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(fatal8$FATALITIES, las = 3, names.arg = fatal8$EVTYPE, main = "Events with Highest Fatalities", 
        ylab = "Number of fatalities", col = "light blue")
barplot(injury8$INJURIES, las = 3, names.arg = injury8$EVTYPE, main = "Events with Highest Injuries", 
        ylab = "Number of injuries", col = "orange")

propdmg <- aggregate(PROPDMGVAL ~ EVTYPE, data, FUN = sum)
colnames(propdmg) <- c("EVTYPE","PROPDMGVAL")
cropdmg <- aggregate(CROPDMGVAL ~ EVTYPE, data, FUN = sum)
colnames(cropdmg) <- c("EVTYPE","CROPDMGVAL")
propdmg8 <- propdmg[order(-propdmg$PROPDMGVAL), ][1:10, ]
cropdmg8 <- cropdmg[order(-cropdmg$CROPDMGVAL), ][1:10, ]
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(propdmg8$PROPDMGVAL/(10^9), las = 3, names.arg = propdmg8$EVTYPE, 
        main = "Events with Highest Property Damages", ylab = "Damage Cost ($ billions)", 
        col = "lightblue")
barplot(cropdmg8$CROPDMGVAL/(10^9), las = 3, names.arg = cropdmg8$EVTYPE, 
        main = "Events With Highest Crop Damages", ylab = "Damage Cost ($ billions)", 
        col = "orange")
##RESULTS on multi-panel plot

mergeFI <- merge(fatal8, injury8, by = "EVTYPE")
mergeFI$total <- rowSums(mergeFI[c("FATALITIES", "INJURIES")])
keep <- mergeFI[order(-mergeFI$total), ]
melted <- melt(keep, id.vars ="EVTYPE", measure.vars = c("FATALITIES","INJURIES","total"), variable.names = "variable")

mergeCP <- merge(propdmg8, cropdmg8, by = "EVTYPE")
mergeCP$total <- rowSums(mergeCP[c("PROPDMGVAL", "CROPDMGVAL")])
more <- mergeCP[order(-mergeCP$total), ]
allmelt <- melt(more, id.vars = "EVTYPE", measure.vars = c("PROPDMGVAL", "CROPDMGVAL", "total"),variable.names = "variable1")

library(ggplot2)
library(ggpubr)
p1 <- ggplot(melted, aes(x=reorder(EVTYPE, -value),y = value, fill = variable))+ 
geom_bar(stat = "identity", position = "dodge") + labs(title = " Total Health Effect") + xlab("event type") + ylab("frequency count") +
theme(axis.text.x = element_text(angle = 90, hjust = 0.3))

p2 <- ggplot(allmelt, aes(x=reorder(EVTYPE, -value),y = value, fill = variable))+ 
geom_bar(stat = "identity", position = "dodge") + labs(title = " Total Economy Effect") + xlab("event type") + ylab("Damage costs") +
theme(axis.text.x = element_text(angle = 90, hjust = 0.3))
figure <- ggarrange(p1 + font("x.text", size = 8), p2 + font("x.text", size = 8),
                    ncol = 2, nrow = 1)
annotate_figure(figure,
                top = text_grob("VISUALISING TOTAL EFFECTS", color = "red", face = "bold", size = 14),
                left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                right = "I'm done, thanks :-)!",
                fig.lab = "Figure 1", fig.lab.face = "bold"
)



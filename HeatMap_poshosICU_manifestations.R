
setwd("R:\\Andrew\\R")

library(reshape2)
library(readxl)
library(knitr) 
library(tidyverse, warn.conflict=F)
library(Hmisc)
library(patchwork)
library(cowplot)
library(grDevices)
library(Cairo)
library(stringr)

#bias >1 puts more colors at high values
#colors <- colorRampPalette(c("white", "orange", "red","purple","blue", "black"),bias=2.5)(n=100)
colors <- c("#FFF8BC", "#FAD364", "#F7A032", "#F73232", "#E20E62", "#B90F6E", "#B60FB9", "#732788", "#542788", "#41008A")

######## -------  Positive cohort --------###########

data <- read_excel("R:\\Covid\\Long COVID\\burden result 0403\\subgroup weighted cohort\\Part B_manifestation_POS subgroup olwt.xlsx", sheet=1)
data <- data.frame(data)

names(data)[4] <- "Age \n\u226460"
names(data)[5] <- "Age \n60 - 70"
names(data)[6] <- "Age \n>70"
names(data)[7] <- "Black"
names(data)[8] <- "White"
names(data)[9] <- "Male"
names(data)[10] <- "Female"
names(data)[11] <- "No \ncomorbidities"
names(data)[12] <- "1 - 3 \ncomorbidities"
names(data)[13] <- ">3 \ncomorbidities"

data[,2] <- str_to_sentence(data[,2])

data[1,2] <- "Acute\nkidney injury"
data[4,2] <- "Chest\npain"
data[5,2] <- "Chronic\nkidney disease"
data[10,2] <- "Diabetes\nmellitus"
data[12,2] <- "GERD"
data[13,2] <- "Hair\nloss"
data[16,2] <- "Heart\nfailure"
data[19,2] <- "Joint\npain"
data[20,2] <- "Memory\nproblems"
data[21,2] <- "Acute coronary\ndisease"
data[22,2] <- "Mood\ndisorder"
data[23,2] <- "Muscle\nweakness"
data[26,2] <- "Shortness\nof breath"
data[27,2] <- "Skin\nrash"
data[28,2] <- "Sleep\ndisorders"
data[29,2] <- "Smell\nproblems"
data[31,2] <- "Substance\nabuse"

#remove parentheses
for (i in 3:ncol(data)) {
  data[,i] <- sub(" \\(.*", "", data[,i])
}

#change to numeric
for (i in 3:ncol(data)) {
  data[,i] <- as.numeric(data[,i])
}

data <- merge(data, order, by="Manifestation", all.x=TRUE)
data <- data[order(data$order),]

datalong <- data[,!(names(data) %in% c("Organ","order"))]
datalong <- melt(datalong, id.vars="Manifestation")
datalong <- data.frame(datalong)
datalong$outcome <- datalong$Manifestation
datalong$Burden <- datalong$value

a <- datalong[1:33,]
b <- datalong[34:132,]
c <- datalong[133:198,]
d <- datalong[199:264,]
e <- datalong[265:363,]

a$order <- c(1:33)
b$order <- c(34:132)
c$order <- c(133:198)
d$order <- c(199:264)
e$order <- c(265:363)

# make legend

alegend <- a %>%
  ggplot(aes(variable, outcome, col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden per 1000 
    person-years", breaks=c(0,10,20,30,40), labels=c("0","10","20","30","40"), limits=c(-5,40), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="bottom", 
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.key.height=unit(.1,'in'),
        legend.key.width=unit(.7,'in'),
        legend.key = element_blank()) +
  guides(size=guide_legend(order=1, keywidth=unit(.2,'in')),
         color= guide_colorbar(order=2)) +
  labs(fill="Burden per 1000 
    person-years")

legend <- get_legend(alegend)

ap <- a %>%
  ggplot(aes(variable, reorder(outcome, order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden",  limits=c(-5,40), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",
        plot.margin=margin(0,0.1,0,0,unit="cm"))  

bp <- b %>%
  ggplot(aes(variable, reorder(outcome, order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", limits=c(-5,40), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm"))  

cp <- c %>%
  ggplot(aes(variable, reorder(outcome, order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden",  limits=c(-5,40), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm")) 

dp <- d %>%
  ggplot(aes(variable, reorder(outcome, order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", limits=c(-5,40), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm")) 

ep <- e %>%
  ggplot(aes(variable, reorder(outcome, order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden",  limits=c(-5,40), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0,0,0,unit="cm"))  

layout <- "
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
FFFFFFFFFFF
"
ap + bp + cp + dp + ep + legend + plot_layout(design=layout)

pall <- ap + bp + cp + dp + ep + legend + plot_layout(design=layout)
ggsave("R:\\Covid\\Long COVID\\Figures\\Heat Maps\\pos_manifestations.pdf", pall, device=cairo_pdf, units = 'in', width = 8.5, height = 11)



######## -------  Hospitalized cohort --------###########


data <- read_excel("R:\\Covid\\Long COVID\\burden result 0403\\subgroup weighted cohort\\Part B_manifestation_HOS subgroup olwt.xlsx", sheet=1)
data <- data.frame(data)

names(data)[4] <- "Age \n\u226460"
names(data)[5] <- "Age \n60 - 70"
names(data)[6] <- "Age \n>70"
names(data)[7] <- "Black"
names(data)[8] <- "White"
names(data)[9] <- "Male"
names(data)[10] <- "Female"
names(data)[11] <- "No \ncomorbidities"
names(data)[12] <- "1 - 3 \ncomorbidities"
names(data)[13] <- ">3 \ncomorbidities"

data[,2] <- str_to_sentence(data[,2])

data[1,2] <- "Acute\nkidney injury"
data[4,2] <- "Chest\npain"
data[5,2] <- "Chronic\nkidney disease"
data[10,2] <- "Diabetes\nmellitus"
data[12,2] <- "GERD"
data[13,2] <- "Hair\nloss"
data[16,2] <- "Heart\nfailure"
data[19,2] <- "Joint\npain"
data[20,2] <- "Memory\nproblems"
data[21,2] <- "Acute coronary\ndisease"
data[22,2] <- "Mood\ndisorder"
data[23,2] <- "Muscle\nweakness"
data[26,2] <- "Shortness\nof breath"
data[27,2] <- "Skin\nrash"
data[28,2] <- "Sleep\ndisorders"
data[29,2] <- "Smell\nproblems"
data[31,2] <- "Substance\nabuse"

#remove parentheses
for (i in 3:ncol(data)) {
  data[,i] <- sub(" \\(.*", "", data[,i])
}

#change to numeric
for (i in 3:ncol(data)) {
  data[,i] <- as.numeric(data[,i])
}

data <- merge(data, order, by="Manifestation", all.x=TRUE)
data <- data[order(data$order),]

datalong <- data[,!(names(data) %in% c("Organ","order"))]
datalong <- melt(datalong, id.vars="Manifestation")
datalong <- data.frame(datalong)
datalong$outcome <- datalong$Manifestation
datalong$Burden <- datalong$value

a <- datalong[1:33,]
b <- datalong[34:132,]
c <- datalong[133:198,]
d <- datalong[199:264,]
e <- datalong[265:363,]

a$order <- c(1:33)
b$order <- c(34:132)
c$order <- c(133:198)
d$order <- c(199:264)
e$order <- c(265:363)

# make legend

alegend <- a %>%
  ggplot(aes(variable, outcome)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden per 1000 
    person-years", breaks=c(0,20,40,60,80), labels=c("0","20","40","60","80"), limits=c(-5,81), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="bottom", 
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.key.height=unit(.1,'in'),
        legend.key.width=unit(.7,'in'),
        legend.key = element_blank()) +
  guides(size=guide_legend(order=1, keywidth=unit(.2,'in')),
         color= guide_colorbar(order=2)) +
  labs(fill="Burden per 1000 
    person-years") 

legend <- get_legend(alegend)

ap <- a %>%
  ggplot(aes(variable, reorder(outcome, order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", limits=c(-5,81), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",
        plot.margin=margin(0,0.1,0,0,unit="cm"))  

bp <- b %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden",  limits=c(-5,81), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm"))  

cp <- c %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", limits=c(-5,81), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm")) 

dp <- d %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden",  limits=c(-5,81), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm")) 

ep <- e %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden",  limits=c(-5,81), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0,0,0,unit="cm"))  

layout <- "
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
FFFFFFFFFFF
"

ap + bp + cp + dp + ep + legend + plot_layout(design=layout)

pall <- ap + bp + cp + dp + ep + legend + plot_layout(design=layout)
ggsave("R:\\Covid\\Long COVID\\Figures\\Heat Maps\\hos_manifestations.pdf", pall, device=cairo_pdf, units = 'in', width = 8.5, height = 11)



######## -------  ICU cohort --------###########

data <- read_excel("R:\\Covid\\Long COVID\\burden result 0403\\subgroup weighted cohort\\Part B_manifestation_ICU subgroup olwt.xlsx", sheet=1)
data <- data.frame(data)

names(data)[4] <- "Age \n\u226460"
names(data)[5] <- "Age \n60 - 70"
names(data)[6] <- "Age \n>70"
names(data)[7] <- "Black"
names(data)[8] <- "White"
names(data)[9] <- "Male"
names(data)[10] <- "Female"
names(data)[11] <- "No \ncomorbidities"
names(data)[12] <- "1 - 3 \ncomorbidities"
names(data)[13] <- ">3 \ncomorbidities"

data[,2] <- str_to_sentence(data[,2])

data[1,2] <- "Acute\nkidney injury"
data[4,2] <- "Chest\npain"
data[5,2] <- "Chronic\nkidney disease"
data[10,2] <- "Diabetes\nmellitus"
data[12,2] <- "GERD"
data[13,2] <- "Hair\nloss"
data[16,2] <- "Heart\nfailure"
data[19,2] <- "Joint\npain"
data[20,2] <- "Memory\nproblems"
data[21,2] <- "Acute coronary\ndisease"
data[22,2] <- "Mood\ndisorder"
data[23,2] <- "Muscle\nweakness"
data[26,2] <- "Shortness\nof breath"
data[27,2] <- "Skin\nrash"
data[28,2] <- "Sleep\ndisorders"
data[29,2] <- "Smell\nproblems"
data[31,2] <- "Substance\nabuse"

#remove parentheses
for (i in 3:ncol(data)) {
  data[,i] <- sub(" \\(.*", "", data[,i])
}

#change to numeric
for (i in 3:ncol(data)) {
  data[,i] <- as.numeric(data[,i])
}

data <- merge(data, order, by="Manifestation", all.x=TRUE)
data <- data[order(data$order),]

datalong <- data[,!(names(data) %in% c("Organ","order"))]
datalong <- melt(datalong, id.vars="Manifestation")
datalong <- data.frame(datalong)
datalong$outcome <- datalong$Manifestation
datalong$Burden <- datalong$value

a <- datalong[1:33,]
b <- datalong[34:132,]
c <- datalong[133:198,]
d <- datalong[199:264,]
e <- datalong[265:363,]

a$order <- c(1:33)
b$order <- c(34:132)
c$order <- c(133:198)
d$order <- c(199:264)
e$order <- c(265:363)

# make legend

alegend <- a %>%
  ggplot(aes(variable, outcome, col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden per 1000 
    person-years", breaks=c(0,40,80,120,160), labels=c("0","40","80","120","160"), limits=c(-5,160), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="bottom", 
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.key.height=unit(.1,'in'),
        legend.key.width=unit(.7,'in'),
        legend.key = element_blank()) +
  guides(size=guide_legend(order=1, keywidth=unit(.2,'in')),
         color= guide_colorbar(order=2))  +
  labs(fill="Burden per 1000 
    person-years")

legend <- get_legend(alegend)

ap <- a %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0.05,40,80,120,160), labels=c("0.05","40","80","120","160"), limits=c(-5,160), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",
        plot.margin=margin(0,0.1,0,0,unit="cm"))  

bp <- b %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0.05,40,80,120,160), labels=c("0.05","40","80","120","160"), limits=c(-5,160), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm"))  

cp <- c %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0.05,40,80,120,160), labels=c("0.05","40","80","120","160"), limits=c(-5,160), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm")) 

dp <- d %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0.05,40,80,120,160), labels=c("0.05","40","80","120","160"), limits=c(-5,160), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm")) 

ep <- e %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-3.39,162.18)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0.05,40,80,120,160), labels=c("0.05","40","80","120","160"), limits=c(-5,160), range=c(1,9), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0,0,0,unit="cm"))  

layout <- "
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
ABBBCCDDEEE
FFFFFFFFFFF
"

ap + bp + cp + dp + ep + legend + plot_layout(design=layout)

pall <- ap + bp + cp + dp + ep + legend + plot_layout(design=layout)
ggsave("R:\\Covid\\Long COVID\\Figures\\Heat Maps\\ICU_manifestations.pdf", pall, device=cairo_pdf, units = 'in', width = 8.5, height = 11)

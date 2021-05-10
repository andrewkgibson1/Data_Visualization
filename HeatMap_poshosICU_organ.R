
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

#bias >1 puts more colors at high values
#colors <- colorRampPalette(c("white", "orange", "red","purple","blue", "black"),bias=2.5)(n=100)
colors <- c("#FFF8BC", "#FAD364", "#F7A032", "#F73232", "#E20E62", "#B90F6E", "#B60FB9", "#732788", "#542788", "#41008A")

######## -------  Positive cohort --------###########

data <- read_excel("R:\\Covid\\Long COVID\\burden result 0403\\subgroup weighted cohort\\Part B_organ_POS subgroup olwt.xlsx", sheet=1)
data <- data.frame(data)

names(data)[3] <- "Age \n\u226460"
names(data)[4] <- "Age \n60 - 70"
names(data)[5] <- "Age \n>70"
names(data)[6] <- "Black"
names(data)[7] <- "White"
names(data)[8] <- "Male"
names(data)[9] <- "Female"
names(data)[10] <- "No \ncomorbidities"
names(data)[11] <- "1 - 3 \ncomorbidities"
names(data)[12] <- ">3 \ncomorbidities"

data[7,1] <- "Musculoskeletal"
data[9,1] <- "Pulmonary"

#remove parentheses
for (i in 2:ncol(data)) {
  data[,i] <- sub(" \\(.*", "", data[,i])
}

#change to numeric
for (i in 2:ncol(data)) {
  data[,i] <- as.numeric(data[,i])
}

data <- merge(data, order, by="Outcome", all.x=TRUE)
data <- data[order(data$order),]

datalong <- melt(data, id.vars="Outcome")
datalong <- data.frame(datalong)
datalong$outcome <- with(datalong, reorder(Outcome, -value))
datalong$Burden <- datalong$value

a <- datalong[1:11,]
b <- datalong[12:44,]
c <- datalong[45:66,]
d <- datalong[67:88,]
e <- datalong[89:121,]

a$order <- c(1:11)
b$order <- c(12:44)
c$order <- c(45:66)
d$order <- c(67:88)
e$order <- c(89:121)

# make legend

alegend <- a %>%
  ggplot(aes(variable, outcome, col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden per 1000 
    person-years", breaks=c(0,10,20,30,40), labels=c("0","10","20","30","40"), limits=c(-12,44.56), range=c(1,20), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="bottom", 
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.key.height=unit(.1,'in'),
        legend.key.width=unit(0.8,'in'),
        legend.key = element_blank(),
        legend.box.just = "left") +
  guides(size=guide_legend(order=1, keywidth=unit(.2,'in')),
         color= guide_colorbar(order=2))  +
  labs(fill="Burden per 1000 
    person-years")

legend <- get_legend(alegend)

ap <- a %>%
  ggplot(aes(variable, reorder(outcome, order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden",  limits=c(-12,44.56), range=c(1,20), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",
        plot.margin=margin(0,0.1,0,0,unit="cm"))  

bp <- b %>%
  ggplot(aes(variable, reorder(outcome, order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden",  limits=c(-12,44.56), range=c(1,20), guide="legend") +
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
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden",  limits=c(-12,44.56), range=c(1,20), guide="legend") +
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
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden",  limits=c(-12,44.56), range=c(1,20), guide="legend") +
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
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden",  limits=c(-12,44.56), range=c(1,20), guide="legend") +
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
FFFFFFFFFFF
"
ap + bp + cp + dp + ep + legend + plot_layout(design=layout)

pall <- ap + bp + cp + dp + ep + legend + plot_layout(design=layout)
#ggsave("R:\\Andrew\\R\\test_pos_organ.jpg", pall, units = 'in', width = 11, height = 8.5)
ggsave("R:\\Covid\\Long COVID\\Figures\\Heat Maps\\pos_organ.pdf", pall, device=cairo_pdf, units = 'in', width = 11, height = 8.5)


######## -------  Hospitalized cohort --------###########


data <- read_excel("R:\\Covid\\Long COVID\\burden result 0403\\subgroup weighted cohort\\Part B_organ_HOS subgroup olwt.xlsx", sheet=1)
data <- data.frame(data)

names(data)[3] <- "Age \n\u226460"
names(data)[4] <- "Age \n60 - 70"
names(data)[5] <- "Age \n>70"
names(data)[6] <- "Black"
names(data)[7] <- "White"
names(data)[8] <- "Male"
names(data)[9] <- "Female"
names(data)[10] <- "No \ncomorbidities"
names(data)[11] <- "1 - 3 \ncomorbidities"
names(data)[12] <- ">3 \ncomorbidities"

data[7,1] <- "Musculoskeletal"
data[9,1] <- "Pulmonary"

#remove parentheses
for (i in 2:ncol(data)) {
  data[,i] <- sub(" \\(.*", "", data[,i])
}

#change to numeric
for (i in 2:ncol(data)) {
  data[,i] <- as.numeric(data[,i])
}

data <- merge(data, order, by="Outcome", all.x=TRUE)
data <- data[order(data$order),]

datalong <- melt(data, id.vars="Outcome")
datalong <- data.frame(datalong)
datalong$outcome <- with(datalong, reorder(Outcome, -value))
datalong$Burden <- datalong$value

a <- datalong[1:11,]
b <- datalong[12:44,]
c <- datalong[45:66,]
d <- datalong[67:88,]
e <- datalong[89:121,]

a$order <- c(1:11)
b$order <- c(12:44)
c$order <- c(45:66)
d$order <- c(67:88)
e$order <- c(89:121)

# make legend

alegend <- a %>%
  ggplot(aes(variable, outcome)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden per 1000 
    person-years", breaks=c(0,30,60,90,120), labels=c("0","30","60","90","120"), limits=c(-5,122.91), range=c(1,20), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="bottom", 
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.key.height=unit(.1,'in'),
        legend.key.width=unit(0.8,'in'),
        legend.key = element_blank(),
        legend.box.just = "left") +
  guides(size=guide_legend(order=1, keywidth=unit(.2,'in'), keyheight=unit(.1,'in')),
         color= guide_colorbar(order=2))  +
  labs(fill="Burden per 1000 
    person-years")

legend <- get_legend(alegend)

ap <- a %>%
  ggplot(aes(variable, reorder(outcome, order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0,30,60,90,120), labels=c("0","30","60","90","120"), limits=c(-5,122.91), range=c(1,20), guide="legend") +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",
        plot.margin=margin(0,0.1,0,0,unit="cm"))  

bp <- b %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0,30,60,90,120), labels=c("0","30","60","90","120"), limits=c(-5,122.91), range=c(1,20), guide="legend") +
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
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0,30,60,90,120), labels=c("0","30","60","90","120"), limits=c(-5,122.91), range=c(1,20), guide="legend") +
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
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0,30,60,90,120), labels=c("0","30","60","90","120"), limits=c(-5,122.91), range=c(1,20), guide="legend") +
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
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0,30,60,90,120), labels=c("0","30","60","90","120"), limits=c(-5,122.91), range=c(1,20), guide="legend") +
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
FFFFFFFFFFF
FFFFFFFFFFF
"
ap + bp + cp + dp + ep + legend + plot_layout(design=layout)

pall <- ap + bp + cp + dp + ep + legend + plot_layout(design=layout)
#ggsave("R:\\Andrew\\R\\test_hos_organ.jpg", pall, units = 'in', width = 11, height = 8.5)
ggsave("R:\\Covid\\Long COVID\\Figures\\Heat Maps\\hos_organ.pdf", pall, device=cairo_pdf, units = 'in', width = 11, height = 8.5)



######## -------  ICU cohort --------###########

data <- read_excel("R:\\Covid\\Long COVID\\burden result 0403\\subgroup weighted cohort\\Part B_organ_ICU subgroup olwt.xlsx", sheet=1)
data <- data.frame(data)

names(data)[3] <- "Age \n\u226460"
names(data)[4] <- "Age \n60 - 70"
names(data)[5] <- "Age \n>70"
names(data)[6] <- "Black"
names(data)[7] <- "White"
names(data)[8] <- "Male"
names(data)[9] <- "Female"
names(data)[10] <- "No \ncomorbidities"
names(data)[11] <- "1 - 3 \ncomorbidities"
names(data)[12] <- ">3 \ncomorbidities"

data[7,1] <- "Musculoskeletal"
data[9,1] <- "Pulmonary"

#remove parentheses
for (i in 2:ncol(data)) {
  data[,i] <- sub(" \\(.*", "", data[,i])
}

#change to numeric
for (i in 2:ncol(data)) {
  data[,i] <- as.numeric(data[,i])
}

data <- merge(data, order, by="Outcome", all.x=TRUE)
data <- data[order(data$order),]

datalong <- melt(data, id.vars="Outcome")
datalong <- data.frame(datalong)
datalong$outcome <- with(datalong, reorder(Outcome, -value))
datalong$Burden <- datalong$value

a <- datalong[1:11,]
b <- datalong[12:44,]
c <- datalong[45:66,]
d <- datalong[67:88,]
e <- datalong[89:121,]

a$order <- c(1:11)
b$order <- c(12:44)
c$order <- c(45:66)
d$order <- c(67:88)
e$order <- c(89:121)

# make legend

alegend <- a %>%
  ggplot(aes(variable, outcome, col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden per 1000 
    person-years", breaks=c(0,60,120,180,240), labels=c("0","60","120","180","240"), limits=c(-5,266.54), range=c(1,20)) +
  theme(text=element_text(size=9,color='black'),
        legend.position="bottom", 
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.key.height=unit(.1,'in'),
        legend.key.width=unit(0.8,'in'),
        legend.key = element_blank(),
        legend.box.just = "left") +
  guides(size=guide_legend(order=1, keywidth=unit(.2,'in')),
         color= guide_colorbar(order=2, vjust=0))  +
  labs(fill="Burden per 1000 
    person-years")

legend <- get_legend(alegend)

ap <- a %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0.05,60,120,180,240), labels=c("0.05","60","120","180","240"), limits=c(-5,266.54), range=c(1,20)) +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",
        plot.margin=margin(0,0.1,0,0,unit="cm"))  

bp <- b %>%
  ggplot(aes(variable, reorder(outcome,order), col=Burden)) +
  geom_tile(col="black", fill="white") +
  geom_point(aes(size = Burden, fill=Burden), shape=22, pch=21, color="Black") + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0.05,60,120,180,240), labels=c("0.05","60","120","180","240"), limits=c(-5,266.54), range=c(1,20)) +
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
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0.05,60,120,180,240), labels=c("0.05","60","120","180","240"), limits=c(-5,266.54), range=c(1,20)) +
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
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0.05,60,120,180,240), labels=c("0.05","60","120","180","240"), limits=c(-5,266.54), range=c(1,20)) +
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
  scale_fill_gradientn(colors=colors, limits=c(-11.09,266.54)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  scale_size("Burden", breaks=c(0.05,60,120,180,240), labels=c("0.05","60","120","180","240"), limits=c(-5,266.54), range=c(1,20)) +
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
FFFFFFFFFFF
"
ap + bp + cp + dp + ep + legend + plot_layout(design=layout)

pall <- ap + bp + cp + dp + ep + legend + plot_layout(design=layout)
#ggsave("R:\\Andrew\\R\\test_icu_organ.jpg", pall, units = 'in', width = 11, height = 8.5)
ggsave("R:\\Covid\\Long COVID\\Figures\\Heat Maps\\icu_organ.pdf", pall, device=cairo_pdf, units = 'in', width = 11, height = 8.5)



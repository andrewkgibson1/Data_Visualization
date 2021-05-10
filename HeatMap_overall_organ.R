
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

data <- read_excel("R:\\Covid\\Long COVID\\burden result 0403\\subgroup weighted cohort\\Part B_organ_overall subgroup olwt.xlsx", sheet=1)
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

data <- data[order(-data$Overall),]

# get order for care setting plot
data$order <- rep(1,11)
for (i in 1:nrow(data)) {
  data[i,13] <- i
}

order <- data.frame(Outcome=data$Outcome,
                    order=data$order)

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


##############---- color spaced USE THIS!  ----#############

# to add borders to cells: geom_tile(col="black")

# make a legend

alegend <- a %>%
  ggplot(aes(variable, reorder(outcome, order), fill=Burden)) +
  geom_tile(col="black") +
  geom_text(aes(label=round(Burden, 2), color=ifelse(Burden>25,"white","black"))) +
  scale_color_manual(values=c("white"="white", "black"="black")) +
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(0,66)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  theme(text=element_text(size=9,color='black'),
        legend.position="bottom", 
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.key.height=unit(.1,'in'),
        legend.key.width=unit(1.9,'in')) +
  guides(color= guide_colorbar(order=2)) +
  labs(fill="Burden per 1000 
    person-years")

legend <- get_legend(alegend)

ap <- a %>%
  ggplot(aes(variable, reorder(outcome, order), fill=Burden)) +
  geom_tile(col="black") +
  geom_text(aes(label=format(round(Burden, 2),nsmall=2), color=ifelse(Burden>25,"white","black"))) +
  scale_color_manual(values=c("white"="white", "black"="black")) +
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(0,66)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",
        plot.margin=margin(0,0.1,0,0,unit="cm")) +
  guides(color=FALSE)  

bp <- b %>%
  ggplot(aes(variable, reorder(outcome, order), fill=Burden)) +
  geom_tile(col="black") +
  geom_text(aes(label=format(round(Burden, 2),nsmall=2), color=ifelse(Burden>25,"white","black"))) +
  scale_color_manual(values=c("white"="white", "black"="black")) +
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(0,66)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm")) +
  guides(color=FALSE)

cp <- c %>%
  ggplot(aes(variable, reorder(outcome, order), fill=Burden)) +
  geom_tile(col="black") +
  geom_text(aes(label=format(round(Burden, 2),nsmall=2), color=ifelse(Burden>25,"white","black"))) +
  scale_color_manual(values=c("white"="white", "black"="black")) +
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(0,66)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm")) +
  guides(color=FALSE)

dp <- d %>%
  ggplot(aes(variable, reorder(outcome, order), fill=Burden)) +
  geom_tile(col="black") +
  geom_text(aes(label=format(round(Burden, 2),nsmall=2), color=ifelse(Burden>25,"white","black"))) +
  scale_color_manual(values=c("white"="white", "black"="black")) +
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(0,66)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0.1,0,0,unit="cm")) +
  guides(color=FALSE)

ep <- e %>%
  ggplot(aes(variable, reorder(outcome, order), fill=Burden)) +
  geom_tile(col="black") +
  geom_text(aes(label=format(round(Burden, 2),nsmall=2), color=ifelse(Burden>25,"white","black"))) +
  scale_color_manual(values=c("white"="white", "black"="black")) +
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colors=colors, limits=c(0,66)) +
  scale_x_discrete(position="top", expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=c(0,0))  +
  theme(text=element_text(size=9,color='black'),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0,0,0,unit="cm")) +
  guides(color=FALSE)

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
#ggsave("R:\\Andrew\\R\\test_all_organ.jpg", pall, units = 'in', width = 11, height = 8.5)
ggsave("R:\\Covid\\Long COVID\\Figures\\Heat Maps\\all_organ.pdf", pall, device=cairo_pdf, units = 'in', width = 11, height = 8.5)
































# 
# ######### -----------  size and color ---------#######
# 
# 
# ap <- a %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="white", mid="#e7e1ef", high="#ff0000", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none")
# 
# bp <- b %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="white", mid="#e7e1ef", high="#ff0000", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none",
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# cp <- c %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="white", mid="#e7e1ef", high="#ff0000", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         legend.position="bottom",
#         legend.title = element_text(size=8),
#         legend.text = element_text(size=8),
#         legend.key.height=unit(.1,'in'),
#         legend.key.width=unit(.5,'in'))
# 
# dp <- d %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="white", mid="#e7e1ef", high="#ff0000", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none",
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# ep <- e %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="white", mid="#e7e1ef", high="#ff0000", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none",
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# 
# pall = ap + bp + cp + dp + ep + plot_layout(widths = c(1, 2, 3, 3, 2))
# ggsave("R:\\Andrew\\R\\test_overall.jpg", pall, units = 'in', width = 8.5, height = 11)
# 
# 
# 
# ###### color all in one ##########
# 
# all <- datalong %>%
#   ggplot(aes(variable, outcome, fill=Burden)) +
#   # adds cells
#   geom_tile() +
#   geom_text(aes(label=round(Burden, 2), color=ifelse(Burden>27,"white","black"))) +
#   scale_color_manual(values=c("white"="white", "black"="black")) +
#   labs(x = NULL, y = NULL) + 
#   scale_fill_gradientn(colors=colors) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="bottom",
#         legend.title = element_text(size=8),
#         legend.text = element_text(size=8),
#         legend.key.height=unit(.1,'in'),
#         legend.key.width=unit(1.25,'in')) +
#   guides(color=FALSE)
# 
# # to put in guides
# # fill=guide_legend(title="Burden",label.position="bottom")
# ggsave("R:\\Andrew\\R\\test_overall_nospace.jpg", all, units = 'in', width = 8.5, height = 11)
# 
# 
# 
# 
# 
# 
# 
# # with borders
# all2 <- datalong %>%
#   ggplot(aes(variable, outcome, fill=Burden)) +
#   geom_tile(col="black") + 
#   geom_text(aes(label=round(Burden, 2))) +
#   labs(x = NULL, y = NULL) + 
#   scale_fill_gradient(low="#ffeda0", high="#f03b20", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="bottom",
#         legend.title = element_text(size=8),
#         legend.text = element_text(size=8),
#         legend.key.height=unit(.1,'in'),
#         legend.key.width=unit(1.25,'in'))
# 
# ggsave("R:\\Andrew\\R\\test_overall_nospace_border.jpg", all2, units = 'in', width = 8.5, height = 11)
# 
# 
# ########## size all in one ###########
# 
# p <- datalong %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="#f03b20", mid="#f03b20", high="#f03b20", limits=c(0,42)) +
#   scale_size_continuous(name="Burden") +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(legend.position="bottom",
#         text=element_text(size=9,color='black'),
#         legend.title = element_text(size=8),
#         legend.text = element_text(size=8),
#         legend.key.height=unit(.1,'in'),
#         legend.key.width=unit(1.25,'in'))
# 
# ggsave("R:\\Andrew\\R\\test_overall_size.jpg", p, units = 'in', width = 8.5, height = 11)
# 
# 
# ##### size with space  #########
# 
# 
# ap <- a %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="#f03b20", mid="#f03b20", high="#f03b20", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none")
# 
# bp <- b %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="#f03b20", mid="#f03b20", high="#f03b20", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none",
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# cp <- c %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="#f03b20", mid="#f03b20", high="#f03b20", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         legend.position="bottom",
#         legend.title = element_text(size=8),
#         legend.text = element_text(size=8),
#         legend.key.height=unit(.1,'in'),
#         legend.key.width=unit(.5,'in'))
# 
# dp <- d %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="#f03b20", mid="#f03b20", high="#f03b20", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none",
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# ep <- e %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="#f03b20", mid="#f03b20", high="#f03b20", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none",
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# pall = ap + bp + cp + dp + ep + plot_layout(widths = c(1, 2, 3, 3, 2))
# ggsave("R:\\Andrew\\R\\test_overall_size_space.jpg", pall, units = 'in', width = 8.5, height = 11)
# 
# 
# #########
# 
# # try a bunch of patchworks
# 
# ap <- datalong[26,] %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="#f03b20", mid="#f03b20", high="#f03b20", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none",
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# bp <- datalong[c(59,92),] %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="#f03b20", mid="#f03b20", high="#f03b20", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none",
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# cp <- datalong[24,] %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="#f03b20", mid="#f03b20", high="#f03b20", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none",
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# dp <- datalong[c(57,90),] %>%
#   ggplot(aes(variable, outcome, col=Burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(Burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   scale_color_gradient2(low="#f03b20", mid="#f03b20", high="#f03b20", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,10), guide=NULL) +
#   theme(text=element_text(size=9,color='black'),
#         legend.position="none",
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# ap + bp / cp + dp 
# 
# pall = ap + bp +cp / dp + ep + fp 
# ggsave("R:\\Andrew\\R\\test_overall_size_space.jpg", pall, units = 'in', width = 8.5, height = 11)
# 
# 
# 
# 
# 
# 
# 
# 
# #########
# 
# 
# 
# 
# 
# ####-------- other colors ---------------#########
# 
# 
# 
# # blue
# datalong %>%
#   ggplot(aes(variable, outcome, col=burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   theme_classic() +
#   scale_color_gradient2(low="#edf8b1", mid="#7fcdbb", high="#2c7fb8", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,11), guide=NULL)
# 
# # dull orange
# datalong %>%
#   ggplot(aes(variable, outcome, col=value)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(value)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   theme_classic() +
#   scale_color_gradient2(low="white", mid="#fff7bc", high="#de2d26", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,11), guide=NULL)
# 
# # dull red
# datalong %>%
#   ggplot(aes(variable, outcome, col=burden)) +
#   geom_tile(col="black", fill="white") + 
#   geom_point(aes(size = abs(burden)), shape=15) + 
#   labs(x = NULL, y = NULL) + 
#   theme_classic() +
#   scale_color_gradient2(mid="#FBFEF9",low="white",high="#A63446", limits=c(0,42)) +
#   scale_x_discrete(position="top", expand=c(0,0)) +
#   scale_y_discrete(limits=rev, expand=c(0,0))  +
#   scale_size(range=c(1,11), guide=NULL)
# 
# 
# install.packages("plotly")
# library(plotly)
# ggplotly(p, tooltip="text")
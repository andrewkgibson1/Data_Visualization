library(ggplot2)
library(patchwork)
library(readxl)
library(extrafont)
loadfonts(device = "win")

exceldata = read_excel("C:\\Users\\Evan\\Desktop\\R\\Part A_manifestation burden in overall.xlsx")
data = data.frame(exceldata)

data[1,2] <- "Cardiovascular"
data[2,2] <- "    Acute coronary disease"
data[3,2] <- "    Arrythmias"
data[4,2] <- "    Bradycardia"
data[5,2] <- "    Chest pain"
data[6,2] <- "    Heart failure"
data[7,2] <- "    Myocarditis"
data[8,2] <- "    Tachycardia"
data[9,2] <- "Coagulation"
data[10,2] <- "    Thromboembolism"
data[11,2] <- "Dermatologic"
data[12,2] <- "    Hair loss"
data[13,2] <- "    Skin rash"
data[14,2] <- "Endocrine"
data[15,2] <- "    Diabetes mellitus"
data[16,2] <- "    Hyperlipidemia"
data[17,2] <- "    Obesity"
data[18,2] <- "Gastrointestinal"
data[19,2] <- "    Constipation"
data[20,2] <- "    Diarrhea"
data[21,2] <- "    GERD"
data[22,2] <- "General"
data[23,2] <- "    Fatigue"
data[24,2] <- "Kidney"
data[25,2] <- "    Acute kidney injury"
data[26,2] <- "    Chronic kidney disease"
data[27,2] <- "Mental Health"
data[28,2] <- "    Anxiety"
data[29,2] <- "    Depression"
data[30,2] <- "    Mood disorder"
data[31,2] <- "    Sleep disorders"
data[32,2] <- "    Substance abuse"
data[33,2] <- "Musculoskeletal"
data[34,2] <- "    Joint pain"
data[35,2] <- "    Muscle weakness"
data[36,2] <- "Neurologic"
data[37,2] <- "    Headache"
data[38,2] <- "    Memory problems"
data[39,2] <- "    Smell problems"
data[40,2] <- "    Stroke"
data[41,2] <- "Pulmonary"
data[42,2] <- "    Cough"
data[43,2] <- "    Hypoxemia"
data[44,2] <- "    Shortness of breath"

data$Manifestation <- factor(data$Manifestation, levels = data$Manifestation)


bold.Manifestation <- c("Cardiovascular", "Coagulation", "Dermatologic", "Endocrine", "Gastrointestinal", "General",
                        "Kidney", "Mental Health", "Musculoskeletal", "Neurologic", "Pulmonary")

bold.labels <- ifelse(levels(data$Manifestation) %in% bold.Manifestation, yes = "bold", no = "plain")

p1 =  
  ggplot(data=data, aes(x = D, y = Manifestation, color = ToHighlight)) +
  geom_point(aes(size = Point)) +
  scale_size(range = c(1.25,3.5)) +
  scale_color_manual(values = c( "yes"="#998ec3", "no"="#f1a340" ), guide = FALSE ) +
  theme_bw() +
  geom_errorbarh(aes(xmin=HR_L, xmax=HR_U), colour="black", height=data$Size) +
  scale_x_continuous(trans='log10',
                     limits = c(0.8, 20),
                     expand = c(0, 0),
                     breaks = c(0.8,1,2,3,5,20),
                     labels = function(x) ifelse(x == 0, "0", x)) +
  geom_vline(xintercept=1, color="black", linetype="dashed")+
  scale_y_discrete(limits=rev, expand = c(0,0), labels=c("Cardiovascular"=expression(bold(Cardiovascular)), 
                                                         "Coagulation"=expression(bold(Coagulation)),
                                                         "Dermatologic"=expression(bold(Dermatologic)),
                                                         "Endocrine"=expression(bold(Endocrine)),
                                                         "Gastrointestinal"=expression(bold(Gastrointestinal)),
                                                         "General"=expression(bold(General)),
                                                         "Kidney"=expression(bold(Kidney)),
                                                         "Mental Health"=expression(bold("Mental Health")),
                                                         "Musculoskeletal"=expression(bold(Musculoskeletal)),
                                                         "Neurologic"=expression(bold(Neurologic)),
                                                         "Pulmonary"=expression(bold(Pulmonary)),
                                                         parse=TRUE)) +
  labs(x = 'HR (95% CI)', y = NULL) +
  coord_cartesian(clip="off") +
  theme_test(base_size = base_size_init) +
  theme(plot.margin = unit(c(0,6,0,0),"pt")) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(axis.text.y = element_text(size = 9, family="sans", color = "black")) + 
  labs(subtitle="Organ System 
& Manifestation")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size=9,color='black'),
    axis.text.x = element_text(size=9,color='black'),
    text=element_text(family="sans", color = "black"),
    legend.position= "none")


p1

p2 = 
  ggplot(data=data, aes(y = Manifestation, x = B, fill = ToHighlight, width = Width/100)) +
  labs(x = 'Burden per 1000 person-years', y = NULL)+
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = c( "yes"="#998ec3", "no"="#f1a340" ), guide = FALSE ) +
  geom_errorbar(aes(xmin = B_L, xmax = B_U)) +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(0, 55),
                     breaks = c(0, 1, 5, 10, 20, 55))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.y=element_blank())+
  theme(axis.text.y=element_blank(), axis.ticks.y = element_blank())+
  theme(plot.margin = unit(c(0,0,0,0),"pt"))+
  theme(text = element_text(size = 8))+
  theme(text=element_text(family="sans", color = "black")) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title.x = element_text( size = 9))

p2

pall = 
  p1 + p2 + plot_layout(widths = c(5000, 5000))

ggsave('C:\\Users\\Evan\\Desktop\\R\\FullColor11x8.5.pdf', pall, device=cairo_pdf, width = 11, height = 8.5)
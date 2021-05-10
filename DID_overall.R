
setwd("R:\\Andrew\\R")

# this is the same as 
pacman::p_load(data.table, dplyr, tidyr, readxl, stringr, ggplot2, patchwork, Cairo)

dropEndingZero <- function(x){
  str_replace(x, '(\\.)0|(\\.)00', '')
}

base_size_init = 15
expand_size = c(0.15, 0.15)

a = read_excel("R:\\Covid\\Long COVID\\Figures\\DID\\Archive\\DID_overall.xlsx")

a[2,1] <- "    Acute coronary disease"
a[3,1] <- "    Arrythmias"
a[4,1] <- "    Bradycardia"
a[5,1] <- "    Chest pain"
a[6,1] <- "    Heart failure"
a[7,1] <- "    Myocarditis"
a[8,1] <- "    Tachycardia"

a[10,1] <- "    Thromboembolism"

a[12,1] <- "    Hair loss"
a[13,1] <- "    Skin rash"

a[15,1] <- "    Diabetes mellitus"
a[16,1] <- "    Hyperlipidemia"
a[17,1] <- "    Obesity"

a[19,1] <- "    Constipation"
a[20,1] <- "    Diarrhea"
a[21,1] <- "    GERD"

a[23,1] <- "    Fatigue"

a[25,1] <- "    Acute kidney injury"
a[26,1] <- "    Chronic kidney disease"

a[28,1] <- "    Anxiety"
a[29,1] <- "    Depression"
a[30,1] <- "    Mood disorder"
a[31,1] <- "    Sleep disorders"
a[32,1] <- "    Substance abuse"

a[34,1] <- "    Joint pain"
a[35,1] <- "    Muscle weakness"

a[37,1] <- "    Headache"
a[38,1] <- "    Memory problems"
a[39,1] <- "    Smell problems"
a[40,1] <- "    Stroke"

a[42,1] <- "    Cough"
a[43,1] <- "    Hypoxemia"
a[44,1] <- "    Shortness of breath"

factor_level = a$Outcome
a$Outcome <- factor(a$Outcome, levels = a$Outcome)


bold.organ <- c("Cardiovascular", "Coagulation", "Dermatologic", "Endocrine", "Gastrointestinal", "General",
                        "Kidney", "Mental health", "Musculoskeletal", "Neurologic", "Pulmonary")

bold.labels <- ifelse(levels(a$Outcome) %in% bold.organ, yes = "bold", no = "plain")
width1 <- ifelse(levels(a$Outcome) %in% bold.organ, yes = 0.8, no = 0.4)

#fill_color = '#ae017e'
fill_color = c('#1b9e77',rep("#d95f02",7),
               '#1b9e77',"#d95f02",
               '#1b9e77',rep("#d95f02",2),
               '#1b9e77',rep("#d95f02",3),
               '#1b9e77',rep("#d95f02",3),
               '#1b9e77',"#d95f02",
               '#1b9e77',rep("#d95f02",2),
               '#1b9e77',rep("#d95f02",5),
               '#1b9e77',rep("#d95f02",2),
               '#1b9e77',rep("#d95f02",4),
               '#1b9e77',rep("#d95f02",3))

p1 = ggplot(data=a, aes(x = Age, y = Outcome)) +
  geom_bar(stat = 'identity', width = width1, alpha = 0.9, fill=fill_color) +
  geom_vline(xintercept=0) +
  labs(x ="\U2190 Age \U2264 60  |  Age > 70 \U2192", y = NULL, subtitle="Organ System 
& Manifestation") +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-32, 32),
                     breaks = c(-30,-15,0,15,30),
                     labels = dropEndingZero) +
  scale_y_discrete(limits=rev, expand = c(0,0)) +
  theme_test(base_size = base_size_init) +
  theme(legend.position = c(0.75, 0.97),
        strip.placement = 'none',
        axis.text.y = element_text(face=c(rep("plain",3), "bold", rep("plain",4), "bold", rep("plain",2), "bold",
                                          rep("plain",5), "bold", rep("plain",2), "bold", rep("plain",1), "bold",
                                          rep("plain",3), "bold", rep("plain",3), "bold",rep("plain",2), "bold",
                                          rep("plain",1), "bold",rep("plain",7), "bold"), size=9, hjust=0, color="black",family="sans"),
        #axis.ticks.y = element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 10, face = "bold",family="sans"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_text(size=9,color='black',family="sans"),
        axis.text.x = element_text(size=9,color='black',family="sans"),
        strip.background = element_blank(),
        plot.margin=margin(0,0.2,0,0,unit="cm"))

p2 = ggplot(data=a, aes(x = Race, y = Outcome)) +
  geom_bar(stat = 'identity', width = width1, alpha = 0.9, fill=fill_color) +
  geom_vline(xintercept=0) +
  labs(x = "\U2190 White  |  Black \U2192 ", y = NULL) +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-32, 32),
                     breaks = c(-30,-15,0,15,30),
                     labels = dropEndingZero) +
  scale_y_discrete(limits=rev, expand = c(0,0)) +
  theme_test(base_size = base_size_init) +
  theme(legend.position = c(0.75, 0.97),
        strip.placement = 'none',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_text(size=9,color='black',family="sans"),
        axis.text.x = element_text(size=9,color='black',family="sans"),
        strip.background = element_blank(),
        plot.margin=margin(0,0.2,0,0,unit="cm"))

p3 = ggplot(data=a, aes(x = Sex, y = Outcome)) +
  geom_bar(stat = 'identity', width = width1, alpha = 0.9, fill=fill_color) +
  geom_vline(xintercept=0) +
  labs(x = "\U2190 Female  |  Male \U2192    ", y = NULL) +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-32, 32),
                     breaks = c(-30,-15,0,15,30),
                     labels = dropEndingZero) +
  scale_y_discrete(limits=rev, expand = c(0,0)) +
  theme_test(base_size = base_size_init) +
  theme(legend.position = c(0.75, 0.97),
        strip.placement = 'none',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_text(size=9,color='black',family="sans"),
        axis.text.x = element_text(size=9,color='black',family="sans"),
        strip.background = element_blank(),
        plot.margin=margin(0,0.2,0,0,unit="cm"))

p4 = ggplot(data=a, aes(x = Comorbidity, y = Outcome)) +
  geom_bar(stat = 'identity', width = width1, alpha = 0.9, fill=fill_color) +
  geom_vline(xintercept=0) +
  labs(x = " \U2190 Low comorbidity  |  High comorbidity \U2192", y = NULL) +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-32, 32),
                     breaks = c(-30,-15,0,15,30),
                     labels = dropEndingZero) +
  scale_y_discrete(limits=rev, expand = c(0,0)) +
  theme_test(base_size = base_size_init) +
  theme(legend.position = c(0.75, 0.97),
        strip.placement = 'none',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_text(size=9,color='black',family="sans"),
        axis.text.x = element_text(size=9,color='black',family="sans"),
        strip.background = element_blank(),
        plot.margin=margin(0,0,0,0,unit="cm"))

layout <- "
ABCD
"

p1 + p2 + p3 + p4 + plot_layout(design=layout)

pall <- p1 + p2 + p3 + p4 + plot_layout(design=layout)
#ggsave("R:\\Andrew\\R\\test.jpg", pall, units = 'in', width = 11, height = 8.5)
ggsave("R:\\Covid\\Long COVID\\Figures\\DID\\overall.pdf", pall, device=cairo_pdf, units = 'in', width = 11, height = 8.5)

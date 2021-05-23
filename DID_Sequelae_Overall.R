

pacman::p_load(data.table, dplyr, tidyr, readxl, stringr, ggplot2, patchwork, Cairo, gridExtra)

dropEndingZero <- function(x){
  str_replace(x, '(\\.)0|(\\.)00', '')
}

base_size_init = 15
expand_size = c(0.15, 0.15)

bc = read_excel("R:\\Covid\\Long COVID\\Result 0409\\Part B burden by age race gender CMRB\\DID\\Code\\DID_Sequelae_Overall.xlsx")
data = data.frame(bc)

# data[1,1] <- "Acute coronary\ndisease"
# data[2,1] <- "Acute\nkidney injury"
# data[6,1] <- "Chest\npain"
# data[7,1] <- "Chronic\nkidney disease"
# data[11,1] <- "Diabetes\nmellitus"
# data[15,1] <- "Hair\nloss"
# data[17,1] <- "Heart\nfailure"
# data[20,1] <- "Joint\npain"
# data[21,1] <- "Memory\nproblems"
# data[22,1] <- "Mood\ndisorder"
# data[23,1] <- "Muscle\nweakness"
# data[26,1] <- "Shortness\nof breath"
# data[27,1] <- "Skin\nrash"
# data[28,1] <- "Sleep\ndisorders"
# data[29,1] <- "Smell\nproblems"
# data[31,1] <- "Substance\nabuse"

b <- data

factor_level = b$Outcome
b$Outcome <- factor(b$Outcome, levels = b$Outcome)

width1 = 0.4

p1 = 
  
  ggplot(data=b, aes(x = Age, y = reorder(Outcome, -Age), fill=ifelse(Age_H>0 & Age_L<0,"not-significant","significant"))) + 
  scale_fill_manual(values = c("gray","#d95f02")) +
  geom_bar(stat = 'identity', width = width1, alpha = 0.9) +
  geom_vline(xintercept=0, size=0.25) +
  geom_errorbar(aes(xmin = Age_L, xmax = Age_H), width = width1/2, alpha = 0.9, size=0.25) +
  labs(x ="\U2190 Age \U2264 60  |  Age > 70 \U2192", y = NULL, caption = "Burden per 1000 persons") +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-20, 42),
                     breaks = c(-15,0,15,30),
                     labels = dropEndingZero) +
  scale_y_discrete(limits=rev) +
  theme_test(base_size = base_size_init) +
  theme(legend.position = c(.7,.039),
        legend.key.size=unit(0.3,"cm"),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7,face="bold"),
        strip.placement = 'none',
        plot.title.position = "plot",
        plot.caption = element_text(size=7, hjust = 0),
        axis.text.y = element_text(size=7, color = 'black', hjust=1, family = "sans"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_text(size=7,color='black',family="sans", hjust=0),
        axis.text.x = element_text(size=7,color='black',family="sans", hjust=0.5),
        axis.line = element_line(color='black', size=0.25),
        axis.ticks = element_line(color="black",size=0.25),
        plot.margin=margin(0,0.1,0,0,unit="cm"),
        strip.background = element_blank()) +
  guides(fill=guide_legend(title="Legend",reverse=TRUE))

p1


p2 = 
  
  ggplot(data=b, aes(x = Race, y = reorder(Outcome, -Race), fill=ifelse(Race_H>0 & Race_L<0,"not-significant","significant"))) + 
  scale_fill_manual(values = c("gray","#d95f02")) +
  geom_bar(stat = 'identity', width = width1, alpha = 0.9) +
  geom_vline(xintercept=0, size=0.25) +
  geom_errorbar(aes(xmin = Race_L, xmax = Race_H), width = width1/2, alpha = 0.9, size=0.25) +
  labs(x ="\U2190 White  |  Black \U2192 ", y = NULL, caption = "Burden per 1000 persons") +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-20, 42),
                     breaks = c(-15,0,15,30),
                     labels = dropEndingZero) +
  scale_y_discrete(limits=rev) +
  theme_test(base_size = base_size_init) +
  theme(legend.position = c(.7,.039),
        legend.key.size=unit(0.3,"cm"),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7,face="bold"),
        strip.placement = 'none',
        plot.title.position = "plot",
        plot.caption = element_text(size=7, hjust = 0),
        axis.text.y = element_text(size=7, color = 'black', hjust=1, family = "sans"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_text(size=7,color='black',family="sans", hjust=0.08),
        axis.text.x = element_text(size=7,color='black',family="sans", hjust=0.5),
        axis.line = element_line(color='black', size=0.25),
        axis.ticks = element_line(color="black",size=0.25),
        plot.margin=margin(0,0.1,0,0,unit="cm"),
        strip.background = element_blank()) +
  guides(fill=guide_legend(title="Legend",reverse=TRUE))

p2


p3 = 
  
  ggplot(data=b, aes(x = Sex, y = reorder(Outcome, -Sex), fill=ifelse(Sex_H>0 & Sex_L<0,"not-significant","significant"))) + 
  scale_fill_manual(values = c("gray","#d95f02")) +
  geom_bar(stat = 'identity', width = width1, alpha = 0.9) +
  geom_vline(xintercept=0, size=0.25) +
  geom_errorbar(aes(xmin = Sex_L, xmax = Sex_H), width = width1/2, alpha = 0.9, size=0.25) +
  labs(x ="\U2190 Female  |  Male \U2192    ", y = NULL, caption = "Burden per 1000 persons") +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-20, 42),
                     breaks = c(-15,0,15,30),
                     labels = dropEndingZero) +
  scale_y_discrete(limits=rev) +
  theme_test(base_size = base_size_init) +
  theme(legend.position = c(.7,.039),
        legend.key.size=unit(0.3,"cm"),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7,face="bold"),
        strip.placement = 'none',
        plot.title.position = "plot",
        plot.caption = element_text(size=7, hjust = 0),
        axis.text.y = element_text(size=7, color = 'black', hjust=1, family = "sans"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_text(size=7,color='black',family="sans", hjust=0),
        axis.text.x = element_text(size=7,color='black',family="sans", hjust=0.5),
        axis.line = element_line(color='black', size=0.25),
        axis.ticks = element_line(color="black",size=0.25),        
        plot.margin=margin(0,0.1,0,0,unit="cm"),
        strip.background = element_blank()) +
  guides(fill=guide_legend(title="Legend",reverse=TRUE))

p3

p4 = 
  
  ggplot(data=b, aes(x = Comorbidity, y = reorder(Outcome, -Comorbidity), fill=ifelse(Comorbidity_H>0 & Comorbidity_L<0,"not-significant","significant"))) + 
  scale_fill_manual(values = c("gray","#d95f02")) +
  geom_bar(stat = 'identity', width = width1, alpha = 0.9) +
  geom_vline(xintercept=0, size=0.25) +
  geom_errorbar(aes(xmin = Comorbidity_L, xmax = Comorbidity_H), width = width1/2, alpha = 0.9, size=0.25) +
  labs(x ="\U2190 Low comorbidity  |  High comorbidity \U2192", y = NULL, caption = "Burden per 1000 persons") +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-20, 42),
                     breaks = c(-15,0,15,30),
                     labels = dropEndingZero) +
  scale_y_discrete(limits=rev) +
  theme_test(base_size = base_size_init) +
  theme(legend.position = c(.7,.039),
        legend.key.size=unit(0.3,"cm"),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7,face="bold"),
        strip.placement = 'none',
        plot.title.position = "plot",
        plot.caption = element_text(size=7, hjust = 0.25),
        axis.text.y = element_text(size=7, color = 'black', hjust=1, family = "sans"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_text(size=7,color='black',family="sans", hjust=0),
        axis.text.x = element_text(size=7,color='black',family="sans", hjust=0.5),
        axis.line = element_line(color='black', size=0.25),
        axis.ticks = element_line(color="black",size=0.25),        
        plot.margin=margin(0,0.7,0,0,unit="cm"),
        strip.background = element_blank()) +
  guides(fill=guide_legend(title="Legend",reverse=TRUE))

p4



layout <- "
ABCD
"

p1 + p2 + p3 + p4 + plot_layout(design=layout)

pall <- p1 + p2 + p3 + p4 + plot_layout(design=layout)
#ggsave("R:\\Andrew\\R\\test.jpg", pall, units = 'in', width = 11, height = 8.5)
ggsave("R:\\Covid\\Long COVID\\Result 0409\\Part B burden by age race gender CMRB\\DID\\Sequelae_Overall.pdf", 
       pall, device=cairo_pdf, units = 'in', width = 11, height = 8.5)


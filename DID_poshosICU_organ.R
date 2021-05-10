
setwd("R:\\Andrew\\R")

# this is the same as 
pacman::p_load(data.table, dplyr, tidyr, readxl, stringr, ggplot2, patchwork, Cairo)

dropEndingZero <- function(x){
  str_replace(x, '(\\.)0|(\\.)00', '')
}

base_size_init = 15
expand_size = c(0.15, 0.15)

a = read_excel("R:\\Covid\\Long COVID\\Figures\\DID\\Archive\\DID_organ.xlsx")

p1 = ggplot(data=a, aes(x = Age, y = Organ, fill=factor(cohort, levels=c("ICU", "Hospitalized", "Positive")))) +
  geom_bar(position="dodge", stat = 'identity') +
  geom_vline(xintercept=0) +
  labs(x ="\U2190 Age \U2264 60  |  Age > 70 \U2192", y=NULL, subtitle="Organ System") +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-161, 161),
                     breaks = c(-150,-75,0,75,150),
                     labels = dropEndingZero) +
  #facet_grid(Organ ~ ., space = 'free', scales = 'free', switch = 'y') +
  scale_y_discrete(limits=rev, expand = c(0, 0)) +
  scale_fill_manual(values = rev(c('#1b9e77', '#d95f02', '#7570b3'))) + 
  theme_test(base_size = base_size_init) +
  theme(legend.position = "none",
        legend.title = element_text(),
        #strip.placement = 'none',
        #strip.text.x = element_blank(),
        #strip.text.y = element_text(angle=0),
        #strip.background = element_blank(),
        #axis.ticks.y = element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 10, face = "bold",family="sans"),
        axis.title.x = element_text(size=9,color='black',family="sans"),
        axis.text.x = element_text(size=9,color='black',family="sans"),
        axis.text.y = element_text(size=9,color='black',family="sans"),
        plot.margin=margin(0,0.2,0,0,unit="cm")
        ) 


p2 = ggplot(data=a, aes(x = Race, y = Organ, fill=factor(cohort, levels=c("ICU", "Hospitalized", "Positive")))) +
  geom_bar(position="dodge", stat = 'identity') +
  geom_vline(xintercept=0) +
  labs(x ="\U2190 White  |  Black \U2192 ", y=NULL) +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-161, 161),
                     breaks = c(-150,-75,0,75,150),
                     labels = dropEndingZero) +
  #facet_grid(Organ ~ ., space = 'free', scales = 'free', switch = 'y') +
  scale_y_discrete(limits=rev, expand = c(0, 0)) +
  scale_fill_manual(values = rev(c('#1b9e77', '#d95f02', '#7570b3'))) + 
  theme_test(base_size = base_size_init) +
  theme(legend.position = 'none',
        #legend.title = element_text(),
        #strip.placement = 'none',
        #strip.background = element_blank(),
        #strip.text.x = element_blank(),
        #plot.title.position = "plot",
        #plot.subtitle = element_text(size = 10, face = "bold",family="sans"),
        #strip.text.y = element_text(angle=0),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=9,color='black',family="sans"),
        axis.text.x = element_text(size=9,color='black',family="sans"),
        axis.text.y = element_blank(),

        plot.margin=margin(0,0.2,0,0,unit="cm")
        ) 


p3 = ggplot(data=a, aes(x = Sex, y = Organ, fill=factor(cohort, levels=c("ICU", "Hospitalized", "Positive")))) +
  geom_bar(position="dodge", stat = 'identity') +
  geom_vline(xintercept=0) +
  labs(x ="\U2190 Female  |  Male \U2192    ", y=NULL) +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-161, 161),
                     breaks = c(-150,-75,0,75,150),
                     labels = dropEndingZero) +
  #facet_grid(Organ ~ ., space = 'free', scales = 'free', switch = 'y') +
  scale_y_discrete(limits=rev, expand = c(0, 0)) +
  scale_fill_manual(values = rev(c('#1b9e77', '#d95f02', '#7570b3'))) + 
  theme_test(base_size = base_size_init) +
  theme(legend.position = 'none',
        #legend.title = element_text(),
        #strip.placement = 'none',
        #strip.background = element_blank(),
        #strip.text.x = element_blank(),
        #plot.title.position = "plot",
        #plot.subtitle = element_text(size = 10, face = "bold",family="sans"),
        #strip.text.y = element_text(angle=0),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=9,color='black',family="sans"),
        axis.text.x = element_text(size=9,color='black',family="sans"),
        axis.text.y = element_blank(),
        
        plot.margin=margin(0,0.2,0,0,unit="cm")
  ) 

p4 = ggplot(data=a, aes(x = Comorbidity, y = Organ, fill=factor(cohort, levels=c("ICU", "Hospitalized", "Positive")))) +
  geom_bar(position="dodge", stat = 'identity') +
  geom_vline(xintercept=0) +
  labs(x =" \U2190 Low comorbidity  |  High comorbidity \U2192", y=NULL) +
  scale_x_continuous(expand = c(0, 0),
                     #trans='log10',
                     limits = c(-161, 161),
                     breaks = c(-150,-75,0,75,150),
                     labels = dropEndingZero) +
  #facet_grid(Organ ~ ., space = 'free', scales = 'free', switch = 'y') +
  scale_y_discrete(limits=rev, expand = c(0, 0)) +
  scale_fill_manual(values = rev(c('#1b9e77', '#d95f02', '#7570b3'))) + 
  theme_test(base_size = base_size_init) +
  theme(legend.position = c(0.22,.953),
        legend.key.size=unit(0.4,"cm"),
        legend.text=element_text(size=9),
        legend.title=element_text(size=9,face="bold"),
        #legend.title = element_text(),
        #strip.placement = 'none',
        #strip.background = element_blank(),
        #strip.text.x = element_blank(),
        #plot.title.position = "plot",
        #plot.subtitle = element_text(size = 10, face = "bold",family="sans"),
        #strip.text.y = element_text(angle=0),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=9,color='black',family="sans"),
        axis.text.x = element_text(size=9,color='black',family="sans"),
        axis.text.y = element_blank(),
        
        plot.margin=margin(0,0.2,0,0,unit="cm")
  ) +
  guides(fill=guide_legend(title="Legend",reverse=TRUE))
  

layout <- "
ABCD
"

p1 + p2 + p3 + p4 + plot_layout(design=layout)

pall <- p1 + p2 + p3 + p4 + plot_layout(design=layout)
#ggsave("R:\\Andrew\\R\\test.jpg", pall, units = 'in', width = 11, height = 8.5)
ggsave("R:\\Covid\\Long COVID\\Figures\\DID\\poshosICU_organ.pdf", pall, device=cairo_pdf, units = 'in', width = 11, height = 8.5)

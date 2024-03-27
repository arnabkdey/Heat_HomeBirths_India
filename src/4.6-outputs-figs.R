################################################################################
### Project:
### Heatwaves and Home Births: Impact of Extreme Heat on Delivery Choices in India
################################################################################
rm(list =ls())
options(scipen=999)
options(digits=5)
library(tidyverse)
library(ggpubr)
library(ggplot2)
#install.packages("sjPlot")
library(sjPlot)
library(readxl)
library(hrbrthemes)


setwd("C:/Users/annak/Dropbox/Projects/2023_San Diego/Arnab_Heat and healthcare utilization/results")

##First: get all sheet names
caste_sheets_to_read <- readxl::excel_sheets("./multicomp-outputs/caste.xlsx")
##Second: read all sheets, add tabname, and then bind rows
caste <- bind_rows(lapply(1:length(caste_sheets_to_read),
                      function(i)readxl::read_excel("./multicomp-outputs/caste.xlsx",
                                                    sheet = caste_sheets_to_read[i]) %>%
                        mutate(tabname = caste_sheets_to_read[i]))) %>% 
  mutate(effect_modifier="Caste")

religion_sheets_to_read <- readxl::excel_sheets("./multicomp-outputs/religion.xlsx")
religion <- bind_rows(lapply(1:length(religion_sheets_to_read),
                          function(i)readxl::read_excel("./multicomp-outputs/religion.xlsx",
                                                        sheet = religion_sheets_to_read[i]) %>%
                            mutate(tabname = religion_sheets_to_read[i]))) %>% 
  mutate(effect_modifier="Religion")

residence_sheets_to_read <- readxl::excel_sheets("./multicomp-outputs/rural.xlsx")
residence <- bind_rows(lapply(1:length(residence_sheets_to_read),
                             function(i)readxl::read_excel("./multicomp-outputs/rural.xlsx",
                                                           sheet = residence_sheets_to_read[i]) %>%
                               mutate(tabname = residence_sheets_to_read[i]))) %>% 
  mutate(effect_modifier="Residence")

wealth_sheets_to_read <- readxl::excel_sheets("./multicomp-outputs/wealth.xlsx")
wealth <- bind_rows(lapply(1:length(wealth_sheets_to_read),
                              function(i)readxl::read_excel("./multicomp-outputs/wealth.xlsx",
                                                            sheet = wealth_sheets_to_read[i]) %>%
                                mutate(tabname = wealth_sheets_to_read[i]))) %>% 
  mutate(effect_modifier="Wealth")

lt_tmax_sheets_to_read <- readxl::excel_sheets("./multicomp-outputs/lt_tmax.xlsx")
lt_tmax <- bind_rows(lapply(1:length(lt_tmax_sheets_to_read),
                           function(i)readxl::read_excel("./multicomp-outputs/lt_tmax.xlsx",
                                                         sheet = lt_tmax_sheets_to_read[i]) %>%
                             mutate(tabname = lt_tmax_sheets_to_read[i]))) %>% 
  mutate(effect_modifier="Long-term temperature tertile")


data <- rbind(caste, religion, residence, wealth, lt_tmax)
data <- data %>% 
  filter(contrast %in% c("OBC", "SC", "ST", "Other", 
                         "Hindu", "Not-Hindu",
                         "Rural", "Urban",
                         "Poorest", "Poorer", "Middle", "Richer", "Richest",
                         "Lowest_Tertile", "Medium_Tertile", "High_Tertile")) %>% 
  mutate(OR = estimate,
         CILow  = OR - 1.96*std.error,
         CIHigh = OR + 1.96*std.error) %>% 
  mutate(OR = exp(OR),
         CILow = exp(CILow),
         CIHigh = exp(CIHigh)) %>% 
  rename(exposure = tabname) %>% 
  dplyr::select(effect_modifier, contrast, OR, CILow, CIHigh, exposure) 


data <- data %>% 
  mutate(OR_sign = OR) %>% 
  dplyr::mutate(help = 1 >= CILow & 1 <= CIHigh) %>%                 #Determine if CI contains 1 (non-sign results)
  dplyr::mutate(OR_sign=replace(OR_sign, help=="TRUE", NA))          #Change insignificant results to NA
  

data <- data %>% 
  dplyr::mutate(tmp_threshold = str_split(exposure, "_", simplify=T)[ , 3]) %>% 
  dplyr::mutate(duration = str_split(exposure, "_", simplify=T)[ , 5]) 

unique(data$duration)

data$duration[data$duration == "hh" | data$duration == "rural" | data$duration == "lt"] <- ""
data$duration[data$duration == "2d"] <- "for 2+ days"
data$duration[data$duration == "3d"] <- "for 3+ days"
data$duration[data$duration == "5d"] <- "for 5+ days"

data$tmp_threshold[data$tmp_threshold == "28"] <- "28°C"
data$tmp_threshold[data$tmp_threshold == "30"] <- "30°C"
data$tmp_threshold[data$tmp_threshold == "32"] <- "32°C"
data$tmp_threshold[data$tmp_threshold == "90"] <- "90th %ile"
data$tmp_threshold[data$tmp_threshold == "95"] <- "95th %ile"
data$tmp_threshold[data$tmp_threshold == "97"] <- "97th %ile"

data <- data %>% 
  mutate(exposure = paste("Daily temperature≥", tmp_threshold, " ", duration, sep=""))


unique(data$exposure)

ord <- c("Daily temperature≥28°C ",  "Daily temperature≥28°C for 2+ days", "Daily temperature≥28°C for 3+ days" , "Daily temperature≥28°C for 5+ days",
         "Daily temperature≥30°C ", "Daily temperature≥30°C for 2+ days", "Daily temperature≥30°C for 3+ days", "Daily temperature≥30°C for 5+ days",
         "Daily temperature≥32°C ", "Daily temperature≥32°C for 2+ days", "Daily temperature≥32°C for 3+ days", "Daily temperature≥32°C for 5+ days",
         "Daily temperature≥90th %ile ",  "Daily temperature≥90th %ile for 2+ days", "Daily temperature≥90th %ile for 3+ days", "Daily temperature≥90th %ile for 5+ days",
         "Daily temperature≥95th %ile ",  "Daily temperature≥95th %ile for 2+ days", "Daily temperature≥95th %ile for 3+ days", "Daily temperature≥95th %ile for 5+ days",
         "Daily temperature≥97th %ile ",  "Daily temperature≥97th %ile for 2+ days", "Daily temperature≥97th %ile for 3+ days", "Daily temperature≥97th %ile for 5+ days")

data$exposure <- factor(data$exposure,levels=(ord))
data <- data %>% mutate(exposure = fct_reorder(exposure, desc(exposure))) 

unique(data$contrast)

data$contrast[data$contrast == "Lowest_Tertile"] <- "Cooler"
data$contrast[data$contrast == "Medium_Tertile"] <- "Medium"
data$contrast[data$contrast == "High_Tertile"] <- "Warmer"

ord2 <- c("ST", "SC", "OBC", "Other",
          "Hindu", "Not-Hindu",
          "Rural", "Urban",
          "Poorest", "Poorer", "Middle", "Richer", "Richest", 
          "Cooler", "Medium", "Warmer")


data$contrast <- factor(data$contrast,levels=rev(ord2))
data <- data %>% mutate(contrast = fct_reorder(contrast, desc(contrast))) 

ord3 <- c("Long-term temperature tertile", "Caste", "Religion", "Residence", "Wealth")
data$effect_modifier <- factor(data$effect_modifier,levels=rev(ord3))
data <- data %>% mutate(effect_modifier = fct_reorder(effect_modifier, desc(effect_modifier))) 

min(data$OR)
max(data$OR)

data$OR[data$OR>1.5] <- 1.5

plot_1 <- data %>% 
  filter(exposure %in% c("Daily temperature≥28°C ", "Daily temperature≥28°C for 2+ days", "Daily temperature≥28°C for 3+ days", "Daily temperature≥28°C for 5+ days",
                         "Daily temperature≥30°C ", "Daily temperature≥30°C for 2+ days", "Daily temperature≥30°C for 3+ days", "Daily temperature≥30°C for 5+ days",
                         "Daily temperature≥32°C ", "Daily temperature≥32°C for 2+ days", "Daily temperature≥32°C for 3+ days", "Daily temperature≥32°C for 5+ days")) %>% 
  ggplot(aes(contrast, exposure, fill= OR_sign)) + 
  facet_wrap(vars(effect_modifier), nrow = 1, scales = "free_x") +
  geom_tile(aes(width=0.95, height=0.95)) +
  scale_fill_gradient2(low="#08306b", high="#a50f15", midpoint=1,
                       labels = c("0.75", "1", "1.25", expression("">=1.5)),
                       breaks = c(0.75, 1, 1.25, 1.5),
                       limits=c(0.75,1.5)) +
  theme_ipsum() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5)) 

plot_1

plot_2 <- data %>% 
  filter(exposure %in% c("Daily temperature≥90th %ile ",  "Daily temperature≥90th %ile for 2+ days", "Daily temperature≥90th %ile for 3+ days", "Daily temperature≥90th %ile for 5+ days",
                         "Daily temperature≥95th %ile ",  "Daily temperature≥95th %ile for 2+ days", "Daily temperature≥95th %ile for 3+ days", "Daily temperature≥95th %ile for 5+ days",
                         "Daily temperature≥97th %ile ",  "Daily temperature≥97th %ile for 2+ days", "Daily temperature≥97th %ile for 3+ days", "Daily temperature≥97th %ile for 5+ days")) %>% 
  ggplot(aes(contrast, exposure, fill= OR_sign)) + 
  geom_tile(aes(width=0.95, height=0.95)) +
  facet_wrap(vars(effect_modifier), nrow = 1, scales = "free_x") +
  scale_fill_gradient2(low="#08306b", high="#a50f15", midpoint=1,
                       labels = c("0.75", "1", "1.25", expression("">=1.5)),
                       breaks = c(0.75, 1, 1.25, 1.5),
                       limits=c(0.75,1.5)) +
  theme_ipsum() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5)) 


ggarrange(plot_1, plot_2, align = "v", ncol=1, nrow=2, heights = c(1, 1)) #labels = c("a", "b", "c")

save_plot("Plot_effect_modifiers_absoluteTmp.svg", fig = plot_1, width=42, height=10)
save_plot("Plot_effect_modifiers_relativeTmp.svg", fig = plot_2, width=42, height=10)




comparisons <- rbind(caste, religion, residence, wealth, lt_tmax) %>% 
  subset(!(contrast %in% c("OBC", "SC", "ST", "Other", 
                           "Hindu", "Not-Hindu",
                           "Rural", "Urban",
                           "Poorest", "Poorer", "Middle", "Richer", "Richest",
                           "Lowest_Tertile", "Medium_Tertile", "High_Tertile"))) %>% 
  dplyr::select(effect_modifier, contrast, adj.p.value, tabname) %>% 
  filter(adj.p.value<=0.05)


################################################################################


data2 <- data %>% 
  filter(exposure %in% c("Daily temperature≥30°C ", "Daily temperature≥30°C for 2+ days", "Daily temperature≥30°C for 3+ days", "Daily temperature≥30°C for 5+ days")) %>% 
  mutate(exposure = as.character(exposure)) %>% 
  mutate(exposure = ifelse(exposure=="Daily temperature≥30°C ", "Daily temperature≥30°C on the delivery date", exposure))

data3 <- data %>% 
  filter(exposure %in% c("Daily temperature≥95th %ile ",  "Daily temperature≥95th %ile for 2+ days", "Daily temperature≥95th %ile for 3+ days", "Daily temperature≥95th %ile for 5+ days")) %>% 
  mutate(exposure = as.character(exposure)) %>% 
  mutate(exposure = ifelse(exposure=="Daily temperature≥95th %ile ", "Daily temperature≥95th %ile on the delivery date", exposure))


ord4 <- c("Daily temperature≥30°C on the delivery date", "Daily temperature≥30°C for 2+ days", "Daily temperature≥30°C for 3+ days", "Daily temperature≥30°C for 5+ days")
data2$exposure <- factor(data2$exposure,levels=rev(ord4))
data2 <- data2 %>% mutate(exposure = fct_reorder(exposure, desc(exposure))) 
 
ord5 <- c("Daily temperature≥95th %ile on the delivery date",  "Daily temperature≥95th %ile for 2+ days", "Daily temperature≥95th %ile for 3+ days", "Daily temperature≥95th %ile for 5+ days")
data3$exposure <- factor(data3$exposure,levels=rev(ord5))
data3 <- data3 %>% mutate(exposure = fct_reorder(exposure, desc(exposure))) 

 
plot_caste_abs <- ggplot(data2 %>% filter(effect_modifier == "Caste"), aes(y=contrast, x=OR)) +
  facet_wrap(~exposure, labeller = label_wrap_gen(width=22), nrow = 1) + #, scales = "free_y"
  geom_vline(xintercept=1, colour="darkgray") +  
  geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65)) +  # Ranges for each coefficient
  labs(y="Caste", x="aOR") +  # , title = "Child's age at measurement" Labels , just="right"
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
  theme_classic() +
  #scale_x_continuous(limits=c(0.5, 2.23), breaks=c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2),
  #                   labels=c("0.6", "0.8", "1", "1.2", "", "1.6","", "2")) +
  theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
  theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
  #theme(strip.text.y = element_text(hjust = 0, vjust = 0.75))+
  theme(text = element_text(size = 13),  axis.ticks = element_blank(), panel.border = element_blank()) +           
  #theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) + #angle = 90, 
  theme(legend.position="none") +
  theme(panel.spacing=unit(0, "cm", data=NULL)) +
  theme(plot.title = element_text(size=13)) + 
  scale_x_continuous(limits = c(0.55, 1.6)) +
  coord_flip()


plot_religion_abs <- ggplot(data2 %>% filter(effect_modifier == "Religion"), aes(y=contrast, x=OR)) +
  facet_wrap(~exposure, labeller = label_wrap_gen(width=22), nrow = 1) + #, scales = "free_y"
  geom_vline(xintercept=1, colour="darkgray") +  
  geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65)) +  # Ranges for each coefficient
  labs(y="Religion", x="aOR") +  # , title = "Child's age at measurement" Labels , just="right"
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
  theme_classic() +
  #scale_x_continuous(limits=c(0.5, 2.23), breaks=c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2),
  #                   labels=c("0.6", "0.8", "1", "1.2", "", "1.6","", "2")) +
  theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
  theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
  #theme(strip.text.y = element_text(hjust = 0, vjust = 0.75))+
  theme(text = element_text(size = 13),  axis.ticks = element_blank(), panel.border = element_blank()) +           
  #theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) + #angle = 90, 
  theme(legend.position="none") +
  theme(panel.spacing=unit(0, "cm", data=NULL)) +
  theme(plot.title = element_text(size=13)) + 
  scale_x_continuous(limits = c(0.55, 1.6)) +
  coord_flip()

plot_residence_abs <- ggplot(data2 %>% filter(effect_modifier == "Residence"), aes(y=contrast, x=OR)) +
  facet_wrap(~exposure, labeller = label_wrap_gen(width=22), nrow = 1) + #, scales = "free_y"
  geom_vline(xintercept=1, colour="darkgray") +  
  geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65)) +  # Ranges for each coefficient
  labs(y="Residence", x="aOR") +  # , title = "Child's age at measurement" Labels , just="right"
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
  theme_classic() +
  #scale_x_continuous(limits=c(0.5, 2.23), breaks=c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2),
  #                   labels=c("0.6", "0.8", "1", "1.2", "", "1.6","", "2")) +
  theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
  theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
  #theme(strip.text.y = element_text(hjust = 0, vjust = 0.75))+
  theme(text = element_text(size = 13),  axis.ticks = element_blank(), panel.border = element_blank()) +           
  #theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) + #angle = 90, 
  theme(legend.position="none") +
  theme(panel.spacing=unit(0, "cm", data=NULL)) +
  theme(plot.title = element_text(size=13)) + 
  scale_x_continuous(limits = c(0.55, 1.6)) +
  coord_flip()

plot_wealth_abs <- ggplot(data2 %>% filter(effect_modifier == "Wealth"), aes(y=contrast, x=OR)) +
  facet_wrap(~exposure, labeller = label_wrap_gen(width=22), nrow = 1) + #, scales = "free_y"
  geom_vline(xintercept=1, colour="darkgray") +  
  geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65)) +  # Ranges for each coefficient
  labs(y="Wealth quintile", x="aOR") +  # , title = "Child's age at measurement" Labels , just="right"
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
  theme_classic() +
  #scale_x_continuous(limits=c(0.5, 2.23), breaks=c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2),
  #                   labels=c("0.6", "0.8", "1", "1.2", "", "1.6","", "2")) +
  theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
  theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
  #theme(strip.text.y = element_text(hjust = 0, vjust = 0.75))+
  theme(text = element_text(size = 13),  axis.ticks = element_blank(), panel.border = element_blank()) +           
  #theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) + #angle = 90, 
  theme(legend.position="none") +
  theme(panel.spacing=unit(0, "cm", data=NULL)) +
  theme(plot.title = element_text(size=13)) + 
  scale_x_continuous(limits = c(0.55, 1.6)) +
  coord_flip()

plot_lt_temp_abs <- ggplot(data2 %>% filter(effect_modifier == "Long-term temperature tertile"), aes(y=contrast, x=OR)) +
  facet_wrap(~exposure, labeller = label_wrap_gen(width=22), nrow = 1) + #, scales = "free_y"
  geom_vline(xintercept=1, colour="darkgray") +  
  geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65)) +  # Ranges for each coefficient
  labs(y="Long-term temperature tertile", x="aOR") +  # , title = "Child's age at measurement" Labels , just="right"
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
  theme_classic() +
  #scale_x_continuous(limits=c(0.5, 2.23), breaks=c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2),
  #                   labels=c("0.6", "0.8", "1", "1.2", "", "1.6","", "2")) +
  theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
  theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
  #theme(strip.text.y = element_text(hjust = 0, vjust = 0.75))+
  theme(text = element_text(size = 13),  axis.ticks = element_blank(), panel.border = element_blank()) +           
  #theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) + #angle = 90, 
  theme(legend.position="none") +
  theme(panel.spacing=unit(0, "cm", data=NULL)) +
  theme(plot.title = element_text(size=13)) + 
  scale_x_continuous(limits = c(0.55, 1.6)) +
  coord_flip()

plot_effMod_absTmp <- ggarrange(plot_caste_abs, plot_religion_abs, plot_residence_abs, plot_wealth_abs, plot_lt_temp_abs, align = "h", ncol = 1, nrow=5, labels = c("a", "b", "c", "d", "e")) 



plot_caste_rel <- ggplot(data3 %>% filter(effect_modifier == "Caste"), aes(y=contrast, x=OR)) +
  facet_wrap(~exposure, labeller = label_wrap_gen(width=22), nrow = 1) + #, scales = "free_y"
  geom_vline(xintercept=1, colour="darkgray") +  
  geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65)) +  # Ranges for each coefficient
  labs(y="Caste", x="aOR") +  # , title = "Child's age at measurement" Labels , just="right"
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
  theme_classic() +
  #scale_x_continuous(limits=c(0.5, 2.23), breaks=c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2),
  #                   labels=c("0.6", "0.8", "1", "1.2", "", "1.6","", "2")) +
  theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
  theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
  #theme(strip.text.y = element_text(hjust = 0, vjust = 0.75))+
  theme(text = element_text(size = 13),  axis.ticks = element_blank(), panel.border = element_blank()) +           
  #theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) + #angle = 90, 
  theme(legend.position="none") +
  theme(panel.spacing=unit(0, "cm", data=NULL)) +
  theme(plot.title = element_text(size=13)) + 
  scale_x_continuous(limits = c(0.55, 1.6)) +
  coord_flip()


plot_religion_rel <- ggplot(data3 %>% filter(effect_modifier == "Religion"), aes(y=contrast, x=OR)) +
  facet_wrap(~exposure, labeller = label_wrap_gen(width=22), nrow = 1) + #, scales = "free_y"
  geom_vline(xintercept=1, colour="darkgray") +  
  geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65)) +  # Ranges for each coefficient
  labs(y="Religion", x="aOR") +  # , title = "Child's age at measurement" Labels , just="right"
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
  theme_classic() +
  #scale_x_continuous(limits=c(0.5, 2.23), breaks=c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2),
  #                   labels=c("0.6", "0.8", "1", "1.2", "", "1.6","", "2")) +
  theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
  theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
  #theme(strip.text.y = element_text(hjust = 0, vjust = 0.75))+
  theme(text = element_text(size = 13),  axis.ticks = element_blank(), panel.border = element_blank()) +           
  #theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) + #angle = 90, 
  theme(legend.position="none") +
  theme(panel.spacing=unit(0, "cm", data=NULL)) +
  theme(plot.title = element_text(size=13)) + 
  scale_x_continuous(limits = c(0.55, 1.6)) +
  coord_flip()

plot_residence_rel <- ggplot(data3 %>% filter(effect_modifier == "Residence"), aes(y=contrast, x=OR)) +
  facet_wrap(~exposure, labeller = label_wrap_gen(width=22), nrow = 1) + #, scales = "free_y"
  geom_vline(xintercept=1, colour="darkgray") +  
  geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65)) +  # Ranges for each coefficient
  labs(y="Residence", x="aOR") +  # , title = "Child's age at measurement" Labels , just="right"
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
  theme_classic() +
  #scale_x_continuous(limits=c(0.5, 2.23), breaks=c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2),
  #                   labels=c("0.6", "0.8", "1", "1.2", "", "1.6","", "2")) +
  theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
  theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
  #theme(strip.text.y = element_text(hjust = 0, vjust = 0.75))+
  theme(text = element_text(size = 13),  axis.ticks = element_blank(), panel.border = element_blank()) +           
  #theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) + #angle = 90, 
  theme(legend.position="none") +
  theme(panel.spacing=unit(0, "cm", data=NULL)) +
  theme(plot.title = element_text(size=13)) + 
  scale_x_continuous(limits = c(0.55, 1.6)) +
  coord_flip()

plot_wealth_rel <- ggplot(data3 %>% filter(effect_modifier == "Wealth"), aes(y=contrast, x=OR)) +
  facet_wrap(~exposure, labeller = label_wrap_gen(width=22), nrow = 1) + #, scales = "free_y"
  geom_vline(xintercept=1, colour="darkgray") +  
  geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65)) +  # Ranges for each coefficient
  labs(y="Wealth quintile", x="aOR") +  # , title = "Child's age at measurement" Labels , just="right"
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
  theme_classic() +
  #scale_x_continuous(limits=c(0.5, 2.23), breaks=c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2),
  #                   labels=c("0.6", "0.8", "1", "1.2", "", "1.6","", "2")) +
  theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
  theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
  #theme(strip.text.y = element_text(hjust = 0, vjust = 0.75))+
  theme(text = element_text(size = 13),  axis.ticks = element_blank(), panel.border = element_blank()) +           
  #theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) + #angle = 90, 
  theme(legend.position="none") +
  theme(panel.spacing=unit(0, "cm", data=NULL)) +
  theme(plot.title = element_text(size=13)) + 
  scale_x_continuous(limits = c(0.55, 1.6)) +
  coord_flip()

plot_lt_temp_rel <- ggplot(data3 %>% filter(effect_modifier == "Long-term temperature tertile"), aes(y=contrast, x=OR)) +
  facet_wrap(~exposure, labeller = label_wrap_gen(width=22), nrow = 1) + #, scales = "free_y"
  geom_vline(xintercept=1, colour="darkgray") +  
  geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65)) +  # Ranges for each coefficient
  labs(y="Long-term temperature tertile", x="aOR") +  # , title = "Child's age at measurement" Labels , just="right"
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
  theme_classic() +
  #scale_x_continuous(limits=c(0.5, 2.23), breaks=c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2),
  #                   labels=c("0.6", "0.8", "1", "1.2", "", "1.6","", "2")) +
  theme(panel.grid.major = element_line(size=0.25), panel.grid.minor.x = element_line(size=0.15)) +  #linetype="dotted"
  theme(strip.background = element_blank(),  strip.placement = "outside",  strip.text.y.left = element_text(angle = 0)) +
  #theme(strip.text.y = element_text(hjust = 0, vjust = 0.75))+
  theme(text = element_text(size = 13),  axis.ticks = element_blank(), panel.border = element_blank()) +           
  #theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) + #angle = 90, 
  theme(legend.position="none") +
  theme(panel.spacing=unit(0, "cm", data=NULL)) +
  theme(plot.title = element_text(size=13)) + 
  scale_x_continuous(limits = c(0.55, 1.6)) +
  coord_flip()

plot_effMod_relTmp <- ggarrange(plot_caste_rel, plot_religion_rel, plot_residence_rel, plot_wealth_rel, plot_lt_temp_rel, align = "h", ncol = 1, nrow=5, labels = c("f", "g", "h", "i", "j")) 

plot_effectMod_all <- ggarrange(plot_effMod_absTmp, plot_effMod_relTmp, align = "h", ncol = 2, nrow=1)

save_plot("./Plot_EffectMod_combined.png", fig = plot_effectMod_all, width=38, height=40)
save_plot("./Plot_EffectMod_combined.svg", fig = plot_effectMod_all, width=38, height=40)

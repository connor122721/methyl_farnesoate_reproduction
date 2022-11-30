# Calcualte the effect size and analyze Methyl farnesoate experiment
# Connor Murray
# 11.30.2022

# Libraries
library(data.table)
library(lattice)
library(tidyr)
library(tidyverse)
library(merTools)
library(patchwork)

# Working directory
setwd("C:/Users/Conno/Desktop/UndergradProjects_2022/Methyl_farn/Data/")

# Load data file
dt <- data.table(fread("Daphnia_Research_Aim1.csv") %>% 
        mutate(sc=case_when(Clone=="D8179" ~ "A",
                            Clone=="D8222" ~ "C",
                            Clone %like% "H" ~ "Hybrids")))

# Pooled Standard deviation
pooled_sd <- dt %>% 
  summarize(sd=sd(Prop_Male, na.rm = T))

# Cohen's D: effect Sizes
coh <- dt %>% 
  group_by(Treatment, Clone) %>% 
  summarize(mean=sum(Male_neonates, na.rm=T)/sum(Total_neonates, na.rm=T)) %>% 
  pivot_wider(names_from = c(Treatment), values_from=c(mean)) %>% 
  group_by(Clone) %>% 
  summarize(eff=((MF-C)/pooled_sd$sd))

# Average Effect size across all clones
mean(coh$eff, na.rm=T)

lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

# Plot results - avg diff bet trmts
out1 <- {dt[Total_neonates>0] %>%
    mutate(male=Male_neonates/Total_neonates) %>% 
    group_by(Treatment, Clone, sc) %>% 
    summarise(male_tot = mean(male, na.rm = TRUE),
              ssd = sd(male, na.rm = TRUE),
              n = n()) %>%
    mutate(se = ssd / sqrt(n),
           lci.male = lower_ci(male_tot, se, n),
           uci.male = upper_ci(male_tot, se, n)) %>% 
  ggplot(.,aes(x=as.factor(Treatment), 
             ymin=lci.male, 
             ymax=uci.male,
             y=male_tot,
             color=sc)) +
  geom_pointrange(position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5),
                  size=2) +
  theme_bw() +
  labs(x = "Treatment", 
       color = "Clone",
       fill = "Clone",
       y = "Proportion of Males") +
  scale_color_manual(values = c("C"="Purple", "A"="Blue", "Hybrids"="orange")) +
  #scale_fill_manual(values = c("C"="Purple", "MF"="steelblue4")) +
  theme(strip.text = element_text(face="bold.italic", size=12),
        legend.text = element_text(size=16),
        legend.title = element_text(face="bold", size=18),
        axis.text.x = element_text(face="bold", size=18),
        axis.text.y = element_text(face="bold", size=18),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18),
        axis.title = element_text(face="bold", size=20))}

png(filename = "../Data/mf_exposure_mean_CI.png")
out1
dev.off()

# Plot - avg male prod over time
out2 <- {dt[Total_neonates>0] %>% 
    pivot_longer(cols = c(Prop_Male, Prop_Female, 
                          Male_neonates, Female_neonates)) %>% 
    ggplot(.,aes(x=value, 
                 fill=Treatment)) +
    facet_wrap(~name, nrow = 2, scales = "free_x") +
    geom_histogram(color="black", size=1) +
    theme_bw() +
    labs(x = "Number or Prop.",
         fill = "Treatment",
         y = "Number of Clutches") +
    scale_fill_manual(values = c("C"="lightgreen", "MF"="steelblue4")) +
    theme(strip.text = element_text(face="bold", size=20),
          legend.text = element_text(face="bold", size=20),
          legend.title = element_text(face="bold", size=20),
          axis.text.x = element_text(face="bold", size=18),
          axis.text.y = element_text(face="bold", size=18),
          axis.title.x = element_text(face="bold", size=18),
          axis.title.y = element_text(face="bold", size=18),
          axis.title = element_text(face="bold", size=20))}

pdf(file = "../Data/mf_exposure_tot.pdf")
out2
dev.off()

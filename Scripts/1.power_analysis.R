# Power analysis for Daphnia undergrad project - Caroline Roda
# 11.30.2022
# ijob -c 1 --mem=50G -p standard -A berglandlab
# module load goolf/7.1.0_3.1.4 R/4.0.3; module load gdal geos proj; R

# Libraries
library(data.table)
library(foreach)
library(tidyverse)
library(pwr)
library(viridis)

# Working directory
setwd("C:/Users/Conno/Desktop/UndergradProjects_2022/Methyl_farn/Data/")

# Parameters
num <- seq(from = 5, to = 50, by = 1) # Sample sizes to test
eff <- seq(from = 1, to = 1.7, by = 0.1) # Effect sizes to test
pval=0.05 # P-value 

# Test variable combinations of effect size and sample size
test <- data.table(expand.grid(num, eff))
colnames(test) <- c("sample.size", "effect.size")

# Go through each effect size
p <- foreach(i=1:dim(test)[1], .combine = "rbind", .errorhandling = "remove") %do% {

  # Start
  print(i)
  print(paste("Sample size:", test[i]$sample.size, sep=" "))
  print(paste("Effect size:", test[i]$effect.size, sep=" "))
  
  # Power analysis
  jj<- pwr.t.test(n=test[i]$sample.size,
                  d=test[i]$effect.size, 
                  sig.level=pval, 
                  power=NULL, 
                  type="two.sample",
                  alternative="two.sided")
  
  # Get data
  j <- data.table(n=jj$n,
                  d=jj$d,
                  sig.level=jj$sig.level,
                  power=jj$power,
                  i=i)
  
  # Finish
  return(j)
}

out <- {p %>% 
  ggplot(.) +
  geom_line(aes(x=power, y=n, color=d, group=d), size=2) +
  scale_color_viridis(option = "plasma") +
  geom_vline(xintercept = 0.9, size=2, linetype=2) +
  theme_bw() +
  labs(x = "Power", 
       color = "Effect size (Cohen's D)",
       y = "Sample size") +
  theme(strip.text = element_text(face="bold.italic", size=12),
        legend.text = element_text(size=16),
        legend.title = element_text(face="bold", size=18),
        axis.text.x = element_text(face="bold", size=18),
        axis.text.y = element_text(face="bold", size=18),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18),
        axis.title = element_text(face="bold", size=20))}

png(filename = "power_curve_highd.png")
out
dev.off()

p[d==1.5][power>=0.9]

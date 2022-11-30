# Calcualte the effect size of the methyl farnesoate experiment
# Connor Murray
# 11.30.2022
# Data from: https://doi.org/10.1093/molbev/msac121

# libraries
library(data.table)
library(lattice)
library(tidyr)
library(tidyverse)
library(merTools)

setwd("C:/Users/Conno/Desktop/UndergradProjects_2022/Methyl_farn/Data/")

### Load data file
neo <- fread("FinalNeoNotAdj.csv")

### Alan's version
neopretrmt <- neo[, c("Clone", "MomJar", "Neonate", "Neo2ndGen", "Isolated", "Clutch1", "Male1", "Female1", "Trmt"), with=FALSE]
neopretrmtMC <- neopretrmt[Trmt=="MF" | Trmt=="C"][Clone!="SW4" & Clone!="AxoMorse"]
neopretrmtMC$propmale <- neopretrmtMC$Male1/(neopretrmtMC$Male1+neopretrmtMC$Female1)
neopretrmtMC$N <- (neopretrmtMC$Male1+neopretrmtMC$Female1)
neopretrmtMC[,PrePost:=("PreTrmt")]
neoposttrmt <- neo[, c("Clone", "MomJar", "Neonate", "Neo2ndGen", "Isolated", "Clutch1", "Week3PlusMales", "Week3PlusFemales", "Trmt"), with=FALSE]
neoposttrmtMC <- neoposttrmt[Trmt=="MF" | Trmt=="C"][Clone!="SW4" & Clone!="AxoMorse"]
neoposttrmtMC$propmale <- neoposttrmtMC$Week3PlusMales/(neoposttrmtMC$Week3PlusMales+neoposttrmtMC$Week3PlusFemales)
neoposttrmtMC$N <- (neoposttrmtMC$Week3PlusMales+neoposttrmtMC$Week3PlusFemales)
neoposttrmtMC[,PrePost:=("PostTrmt")]
dat <- rbind(neopretrmtMC[,c("Clone", "MomJar", "Trmt", "PrePost", "propmale", "N"), with=F],
             neoposttrmtMC[,c("Clone", "MomJar", "Trmt", "PrePost", "propmale", "N"), with=F])
dat[grepl("D8515|D8222", Clone), SC:="C"]
dat[!grepl("D8515|D8222", Clone), SC:="A"]

# Pooled Standard deviation
pooled_sd <- dat %>% 
  filter(PrePost=="PostTrmt") %>% 
  summarize(sd=sd(propmale, na.rm = T))

# Cohen's D: effect Sizes
coh <-dat %>% 
  group_by(Trmt, PrePost, SC, Clone) %>% 
  summarize(mean=sum(propmale*N, na.rm=T)/sum(N, na.rm=T)) %>% 
  pivot_wider(names_from = c(Trmt), values_from=c(mean)) %>% 
  filter(PrePost=="PostTrmt") %>%
  group_by(SC, Clone) %>% 
  summarize(eff=((MF-C)/pooled_sd$sd))

mean(coh$eff)

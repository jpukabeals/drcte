
# Jesse PB
# 17Jun2022

# This code provides outputs of p-values among all treatment comparisons using
# the drcte package

# for data manipulation at the end
library(tidyverse)

# prevent scientific notation so p-values are clearer
options(scipen = 999)

# read in datafile
read.csv("LQ2.csv") -> dat

# treatment vector must be defined as a factor for combinations function
dat$trt <- as.factor(dat$trt)

# generate all possible treatment comparisons
pairs <- arrangements::combinations(levels(dat$trt), 2)

# create an empty list to fill information on each treatment comparison
pval_list <- vector("list",length(pairs)/2)

# execute a for loop for each of the 15 possible treatment comparisons

# for each comparison...

# we first subset from the pairs dataframe which treatments are being compared

# then we create a new dataset where only these two treatments are present 

# then we create a model using this subsetted data

# then we make a comparison within this model and extract the pvalue (not
# totally sure how this compCDF works)

# then we fill our empty list with information about this iteration of the for
# loop before beginning the next loop

for(i in 1:15){
  subs <- pairs[i,]
  dat.sub <- filter(dat, trt==subs[1] | trt== subs[2])
  mod <- drmte(nSeeds ~ timeBef + timeAf, fct = LL.3(),
               curveid = trt, data = dat.sub)
  pvalu <- compCDF(mod, units = dat.sub$rep)$pval
  pvalu_pretty <- paste("p-value is",pvalu)
  trt.comparison <- paste("comaring treatments",
                          subs[1],
                          "&",
                          subs[2])
  pval_list[[i]] <- list(trt.comparison,
                         pvalu_pretty,
                         pvalu)
  print(pval_list)
}

sink("output_17Jun2022.txt")
print(pval_list)
sink()


for(i in 1:15){
  subs <- pairs[i,]
  dat.sub <- filter(dat, trt==subs[1] | trt== subs[2])
  mod <- drmte(nSeeds ~ timeBef + timeAf, fct = LL.3(),
               curveid = trt, data = dat.sub)
  pvalu <- compCDF(mod, units = dat.sub$rep)$pval
  trt.comparison <- paste(
    # "comaring treatments",
                          subs[1],
                          "--",
                          subs[2])
  pval_list[[i]] <- list(trt.comparison,
                         pvalu)
  print(pval_list)
}

do.call(rbind.data.frame, pval_list) -> ugly.combined

ugly.combined %>% 
  rename(trt.comparison = colnames(ugly.combined)[1],
         p.value = colnames(ugly.combined)[2]) %>% 
  mutate(p.value = round(p.value,digits = 5)) %>% 
  write.csv("17Jun2022-model-outputs.csv",
            row.names = F)



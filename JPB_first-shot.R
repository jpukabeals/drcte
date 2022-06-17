
# Greta's objective -------------------------------------------------------

# Weibull function has a biological meaning
# 3 parameter

# run an anova on each parameter estimate

# consider the time between these seeds germinating as an interval unit

# you are fitting a curve to each treatment

# how do you compute pairwise comparisons among treatment parameters


# thorough code -----------------------------------------------------------

# Installing the package (only at first instance)

install.packages("devtools")
devtools::install_github("OnofriAndreaPG/drcte")

install.packages("drcte")
library(drcte)




# Loading the package
library(drcte)


# Example 1
# The first line (highlighted in blue) downloads the example data from a website.
# But when you use your own data, you need to reference the path to the file on your
# computer. The data need to be formatted and labeled exactly like the example,
# or you need to change the code. The Excel file should be saved as a CSV. 
filePath <- https://www.casaonofri.it/_datasets/alfalfa3.csv
#OR
filePath <- C:\Users\greta.gramig\Documents\Projects\example_1.csv

dataset <- read.csv(filePath, header = T)
head(dataset)

mod <- drmte(nSeeds ~ timeBef + timeAf, fct = W1.3(),
             data = dataset)
coef(mod)

# Example 2
rm(list = ls())
filePath <- "https://www.casaonofri.it/_datasets/TwoFlushes.csv"
dataset <- read.csv(filePath, header = T)
head(dataset)

mod <- drmte(nEmerg ~ timeBef + timeAf, data = dataset,
             fct = NPMLE())

plot(mod, ylim = c(0, 1), xlim = c(0, 35),
     xlab = "Time (d)",
     ylab = "Cumulative proportion of emerged seedlings")

mod2 <- drmte(nEmerg ~ timeBef + timeAf, fct = KDE(),
              data = dataset)
mod3 <- drmte(nEmerg ~ timeBef + timeAf, data = dataset,
              fct = KDE(bw = "boot"))
mod4 <- drmte(nEmerg ~ timeBef + timeAf, data = dataset,
              fct = loglogistic())

plot(mod2, ylim = c(0, 1), xlim = c(0, 35),
     xlab = "Time (d)", ylab = "Cumulative proportion 
     of emerged seedlings")
points(mod3$curve[[1]][[1]](1:30) ~ c(1:30),
       type = "l", lty = 2)
points(as.numeric(mod4$curve[[1]](1:30)) ~ c(1:30),
       type = "l", lty = 3)

# Example 3
rm(list = ls())
data(verbascum)
head(verbascum)
modVerb <- drmte(nSeeds ~ timeBef + timeAf, curveid = Species,
                 fct = NPMLE(), data = verbascum)

plot(modVerb, legendPos = c(12, 0.6), legendText = c("V. arcturus", "V. blattaria",
                                                     "V. creticum"),
     ylab = "Cumulative proportion of germinated seeds",
     xlab = "Time (d)")
test <- compCDF(modVerb, units = verbascum$Dish)
test

tab <- predict(modVerb, newdata = c(5, 10), se.fit = T, units = verbascum$Dish)
tab

probsList <- c(0.1, 0.5)
GR <- lapply(probsList, function(x) quantile(modVerb,
                                             probs = x, restricted = F, rate = T,
                                             interval = "boot", units = verbascum$Dish,
                                             display = F))
GR <- do.call(rbind, GR)
GR

# Example 4
rm(list = ls())
data(rape)
head(rape, 20)
modTE <- drmte(nSeeds ~ timeBef + timeAf + Psi,
               data = rape, fct = HTLL())
# modNLS <- drm(propCum ~ timeAf + Psi,
#             data = rape, fct = HTLL())
coef(modTE)
summary(modTE)
summary(modTE, units = rape$Dish)

# Example 5
rm(list = ls())
path <- "https://www.casaonofri.it/_datasets/Lactuca.csv"
lactuca <- read.csv(path, header = T)
head(lactuca)
mod <- drmte(nSeeds ~ timeBef + timeAf,
             data = lactuca,
             fct = lognormal())
plot(mod)
tab1 <- summary(mod)
tab1
tab2 <- summary(mod, units = lactuca$dish)
tab2

First, install R, version 4.2.0:
  https://cran.r-project.org/bin/windows/base/
  
  The installation of drcte package fails because of a missing dependency: kedd. This package is not available for the latest version of R. To install it from the archives, do the following:
  Install the devtools package, from R
Install R Tools https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html
Install kedd from archives:
  
  
  Run this code in R: 
  
  require(devtools)
install_version("kedd", version = "1.0.3", repos = "http://cran.us.r-project.org")





# specific chunk ----------------------------------------------------------


library(drcte)

?verbascum
??verbascum

drtce::ver
data("verbascum")
head(verbascum)

pairs <- arrangements::combinations(levels(verbascum$Species), 2)
pairComp <- function(i){
  subs <- verbascum[(verbascum$Species == i[1] | 
                       verbascum$Species == i[2]), ]
  mod <- drmte(nSeeds ~ timeBef + timeAf, fct = LL.3(),
               curveid = Species, data = subs)
  pval <- compCDF(mod, units = subs$Dish)$pval
  pval
}
pvals <- apply(pairs, 1, function(i) pairComp(i))
names(pvals) <- c("A - B", "A - C", "B - C")
pvals 



# Greta's data ------------------------------------------------------------


# species = trt
# dish = rep


read.csv("LQ2.csv") -> dat

dat %>% 
  glimpse()

install.packages("arrangements")

pairs <- arrangements::combinations(levels(dat$trt), 2)
str(dat)
dat %>% 
  mutate(trt.factor = as.factor(trt)) -> dat
pairs <- arrangements::combinations(levels(dat$trt.factor), 2)
# note that trt must be defined as a factor

# pairs %>% 
  # glimpse()
  # View()
# Note that pairs is a 2 column matrix with all potential combinations of treatments


pairComp <- function(i){
  subs <- dat[(dat$trt == i[1] | 
                 dat$trt == i[2]), ]
  mod <- drmte(nSeeds ~ timeBef + timeAf, fct = LL.3(),
               curveid = trt, data = subs)
  pval <- compCDF(mod, units = subs$rep)$pval
  pval
}

## START TROUBLESHOOT
dat %>% 
  glimpse()

drmte(nSeeds ~ timeBef + timeAf, fct = LL.3(),
      curveid = trt, data = dat)
# we can get function to work

drmte(nSeeds ~ timeBef + timeAf, fct = LL.3(),
      curveid = trt, data = dat) -> test.mod
compCDF(test.mod, units = dat$rep)$pval
# we can get compCDF to work


# try replacing apply() with a for loop

# creating empty dataframe
output <- data.frame(rowname=1:length(pairs),
                     trt_comparison=999,
                     p_value=999)

output <- list()
output[1]
output[2]
output[3]

pval_list <- vector("list",length(pairs)/2)

for(i in 1:15){
  subs <- pairs[i,]
  dat.sub <- filter(dat, trt.factor==subs[1] | trt.factor== subs[2])
  mod <- drmte(nSeeds ~ timeBef + timeAf, fct = LL.3(),
               curveid = trt.factor, data = dat.sub)
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



pairs[1,]
pairs[2,]
pairs[3,]

output



print(output)

compCDF(mod, units = dat.sub$rep) %>% 
  .$pval %>% 
  str()


output <- data.frame(1:length(pairs),0)


str(compCDF(mod, units = dat.sub$rep)$pval)

## END TROUBLESHOOT



pvals <- apply(pairs, 1, function(i) pairComp(i))

pvals <- apply(
  pairs, #the array of treatment combinations
  1, # the margin arg, this tells the function to be applied to across each row of pairs 
  function(i) pairComp(i))


pvals <- apply(
  pairs, #the array of treatment combinations
  1, # the margin arg, this tells the function to be applied to across each row of pairs 
  function(i) pairComp(i) #confused what this is/means
  )


library(drmte)
install.packages("drmte")


names(pvals) <- c("A - B", "A - C", "B - C")
pvals 









####################### THESIS SCRIPT #######################
# By Henrik Hung Haram
# Supervisor: Johan Braeken
#############################################################

### Part I: packages used in the script ###

library('mirt')
library('corrplot')
library('ggplot2')
library('dplyr')
library('xtable')
library('tidyr')
library('mirtCAT')
library('jtools')
library('tibble')
library("parallel")


### Part II: cleaning and re-scoring of the data ###


##### The part here that is commmented out was only done one time to clean up the data
# DATA = readRDS("B:/thesis_data/U5sample.rds")
# str(DATA)
# summary(DATA)
# # Removing all test-takers with missing values
# DATA = DATA[complete.cases(DATA),]
# 
# #Excluding all test-takers who took the test prior to 2014
# DATA = DATA[DATA$U5.Testyear>=2014,]
# summary(DATA)
# dim(DATA)
# nrItems = 36
# items = paste0("U5.",1:nrItems)
# # Removing all unecessary variables
# data <- DATA[,c(items,'U4.Rawscore','U6.Rawscore')]
# saveRDS(data, "B:/thesis_data/data.rds")

# Loading the cleaned data set
setwd('C:/Users/henri/OneDrive/MAE/MAE4090/Thesis/')
full.data <- readRDS("B:/thesis_data/data.rds")

# Re-scoring the data set
data <- full.data[,1:36]
nrItems = 36
items = paste0("U5.",1:nrItems)
rescored <- data
for (i in 1:(nrItems-1)){
  index = apply(data[,i:nrItems], 1, na.rm = T, sum)==0
  rescored[index,(i+1):nrItems]= NA
}

# Checking differences in how many test-takers answered correctly and comparing this to the re-scored data set
round(100*apply(data,2,table,useNA="always")/nrow(data),1)
round(100*apply(rescored,2,table,useNA="always")/nrow(data),1)


### Part III: Model Selection ###


# Fitting the 1PL, 2PL, and 3PL models to the rescored data set
rescored.1PL = mirt(data=rescored,model=1,itemtype="Rasch")
rescored.2PL = mirt(data=rescored,model=1,itemtype="2PL")
rescored.3PL = mirt(data=rescored,model=1,itemtype="3PL",technical = list(NCYCLES = 10000)) # allowing for 10 000 iterations

# Investigating whether model parameter estimates are reasonable (crazy values, switch in sign discrimination parameter, ...)
itempar <- round(cbind(
  coef(rescored.1PL,IRTpars=TRUE,simplify=TRUE)$items,
  coef(rescored.2PL,IRTpars=TRUE,simplify=TRUE)$items,
  coef(rescored.3PL,IRTpars=TRUE,simplify=TRUE)$items
),2)

itempar

# Model comparison
anova(rescored.1PL,rescored.2PL)
anova(rescored.2PL,rescored.3PL)

model.comparison <- rbind(
  anova(rescored.1PL),
  anova(rescored.2PL),
  anova(rescored.3PL)
)

# Calculating the weighted AIC
aic <- model.comparison$AIC
daic <- exp(-.5 * (aic-min(aic)))
wAIC <- daic/sum(daic)

# Calculating the weighted BIC
bic <- model.comparison$BIC
dbic <- exp(-.5*(bic-min(bic)))
wBIC <- dbic/sum(dbic)

# Comparing the AIC, BIC, wAIC, and wBIC for the 1PL, 2PL, and 3PL models
mod.comp <- model.comparison[c('AIC', 'BIC','logLik')]
mod.comp <- cbind(mod.comp,wAIC, wBIC)
mod.comp <- mod.comp[c('AIC','wAIC','BIC','wBIC','logLik')]

mod.comp

### Part IV: Item selection ###
# Investigating for misfit with Bock's chi-squared method
itemfit <- itemfit(rescored.3PL, 'X2')		#Simple fit tests

itemfit

# Correcting the p-value with the Bonferroni correction
which(itemfit$p.X2 < 0.05/36) #dividing by the number of significance tests
length(which(itemfit$p.X2 < 0.05/36))

# Graphical analysis
# Plotting ICC vs. approximate empirical ICC for a worse fitting item for the items that were flagged for misfit with the Bonferroni correction criteria
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 2, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 4, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 11, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 12, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 14, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 15, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 18, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 19, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 20, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 21, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 22, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 23, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 24, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 25, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 26, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 27, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 28, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 29, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 30, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 31, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 33, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 34, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 35, method = 'ML')
itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = 36, method = 'ML')

# Analysis of local dependency with the Q3 statistic
Q3 = residuals(rescored.3PL,type="Q3")
corrplot(Q3)


### Part V: Simulation Study ###


######################################
## Seting up for the simulation study
######################################
# New data set without the removed items
item.bank <- rescored[,c(1:33,35)]

# Creating a new mirt.object/model without the removed items, e.g. the item bank
pars <- coef(rescored.3PL,simplify=TRUE)$items #using the item parameters from the 3PL model
pars <- pars[c(1:33,35),1:3] # only choosing the items that were included in the item bank and only the item parameters of interest
model <- generate.mirt_object(parameters = pars, itemtype = '3PL') # new model with just the items in the item bank

# Checking the properties of the item bank
plot(model,type='infoSE')
plot(model,type='rxx')
plot(model,type='score')

# Creating the new test-takers
thetas <- as.matrix(rep(seq(-3,3,.1), each = 100))

# Generating answers to the item bank for the new test-takers
set.seed(0) # setting seed so that the results will be the same every time this is run
pat <- generate_pattern(model, Theta = thetas) # using the item parameters from the item bank and the theta values for the new test-takers
colnames(pat) <- colnames(item.bank)

########################
## Simulating the CAT
########################
I = ncol(item.bank) # max number of items in the item bank
## Termination Criterion for the CAT, when these criteria are reached the CAT stops
design.CAT <- list(min_SEM = .632, # The CAT stops when the SE is reached for the estimated ability of a test-taker
                   delta_thetas = 0.05, # The CAT stops when the change in the estimated ability is lower than .05 when an item is administered
                   min_items = 5, max_items = I) # At least 5 items have to be administered to each test-taker and the maximum items administered is all the items in the item bank

## Termination criterion for the item bank
design.baseline <- list(min_items = I, max_items = I) # This basically administers all the items

## Simulating the test-takers answering the CAT and the item bank
cl <- makeCluster(detectCores()-1) # Using all the cores except for one that the computer has for a faster simulation
out.CAT <- mirtCAT(mo = model, # These are the calibrated item parameters 
                   criteria="MI", # How subsequent items are chosen, here they are chosen to maximize information
                   local_pattern = pat, # The test-takers' answers to the full item bank
                   start_item = "random", # Which item the CAT starts with, here it chosen randomly
                   design=design.CAT, # Giving the CAT the termination criterion
                   cl=cl)
out.baseline <- mirtCAT(mo = model, # These are the calibrated item parameters
                        criteria = 'seq', # Items are administered sequentially
                        local_pattern = pat, # The test-takers' answers to the full item bank
                        start_item = 1, # This test starts with the first item
                        design=design.baseline, # Termination criterion
                        cl=cl)
stopCluster(cl) # Stops R from using all the cores on your computer

##############################
## Setup for the short forms
##############################
## It is necessary to change the order of the model and the answers of the test-takers used earlier for simulation to work with the mirtCAT package
# Function that calculates the item-total correlation based on the remainder score for each item
item_total <- function(items){
  items <- na.exclude(items)
  CTT <- c()
  for(i in 1:ncol(items)){
    CTT[i] <- cor(items[,i],rowMeans(items[,-i]))
  }
  return(CTT)
}
CTT <- item_total(rescored) # Using the function to calculate the item-total correlation for the rescored data set
CTT <- sort(CTT, decreasing = T, index = T)$ix # Sorting the items from highest to lowest item-total correlation
CTT <- CTT[!CTT %in% c(34,36)] # Removing the two items that were removed
pars.CTT <- coef(rescored.3PL,simplify=TRUE)$items[CTT, 1:3] # Extracting the item parameters in the order of item-total correlation
model.CTT <- generate.mirt_object(parameters = pars.CTT, itemtype = '3PL') # Creating new model with the items ordered according to item-total correlation

# Need results from the CAT to find the most exposed items 
iexp <- sapply(out.CAT,function(y){x=!is.na(match(1:I,y$items_answered))}) # This checks which items were administered for each test-taker
iexp.order <-  sort(colMeans(t(iexp)),index = T, decreasing = T)$ix # Sorting the items according to highest item exposure
pars.iexp <- coef(rescored.3PL,simplify=TRUE)$items[iexp.order, 1:3] # Extracting item parameters in the order of item exposure
model.iexp <- generate.mirt_object(parameters = pars.iexp, itemtype = '3PL') # Creating new model with items ordered according to item exposure

#################################
## Simulating the short forms
#################################
## Setting termination criterion for the short forms, which is that the short forms should be as long as the median CAT
nr_items_answered <- median(sapply(out.CAT,function(y)n=length(y$items_answered))) # Finding the median CAT length from the results of the CAT
design.fixed.length <- list(max_items = round(nr_items_answered)) 

## Simulating the test-takers answering the short forms
cl <- makeCluster(detectCores()-1)
# Random short form
out.random <- mirtCAT(mo = model, # Using the same item parameters as earlier for this short form
                      criteria = 'random', # Subsequent items are chosen at random
                      local_pattern = pat, # The test-takers' answers to the full item bank
                      start_item = 'random', # First item chosen at random
                      design = design.fixed.length, # Number of items administered
                      cl=cl)
# Item-total correlation short form
out.CTT <- mirtCAT(mo = model.CTT, # Using the item parameters in order of item-total correlation
                   criteria = 'seq', # Items are administered sequentially
                   local_pattern = pat[,(replace(CTT, CTT==35,34))], # Reordering test-takers' answers to the full item bank
                   start_item = 1, # First item administered is the first item in model.CTT
                   design = design.fixed.length, 
                   cl=cl)
# Item exposure short form
out.iexp <- mirtCAT(mo = model.iexp, # Using the item parameters in order of item exposure
                    criteria='seq', # Items are administered sequentially
                    local_pattern = pat[,iexp.order], # Reordering test-takers' answers to the full item bank
                    start_item = 1, # First item administered is the first item in model.iexp
                    design=design.fixed.length, 
                    cl=cl)
stopCluster(cl)

################################################
## Getting the results from the simulations
################################################
## Results from the CAT
results.CAT = as.data.frame(t(sapply(out.CAT,function(y){
  c(n=length(y$items_answered), # The length of the CAT
    theta=y$thetas, # The estimated ability level
    SE=y$SE_thetas, # Standard error of the estimated ability
    start=y$items_answered[1]) # Which item the test started with
})))
results.CAT$BIAS.f = results.CAT$theta-thetas # Estimation bias found from the estimated ability minus the true ability
summary(results.CAT)

## Results from the full item bank
results.baseline = as.data.frame(t(sapply(out.baseline,function(y){
  c(n=length(y$items_answered),theta=y$thetas,SE=y$SE_thetas,start=y$items_answered[1])
})))
results.baseline$BIAS.f = results.baseline$theta-thetas
summary(results.baseline)

## Results from the random short form
results.random = as.data.frame(t(sapply(out.random,function(y){
  c(n=length(y$items_answered),theta=y$thetas,SE=y$SE_thetas,start=y$items_answered[1])
})))
results.random$BIAS.f = results.random$theta-thetas
summary(results.random)

## Results from the item-total correlation short form
results.CTT = as.data.frame(t(sapply(out.CTT,function(y){
  c(n=length(y$items_answered),theta=y$thetas,SE=y$SE_thetas,start=y$items_answered[1])
})))
results.CTT$BIAS.f = results.CTT$theta-thetas
summary(results.CTT)

## Results for the item exposure short form
results.iexp = as.data.frame(t(sapply(out.iexp,function(y){
  c(n=length(y$items_answered),theta=y$thetas,SE=y$SE_thetas,start=y$items_answered[1])
})))
results.iexp$BIAS.f = results.iexp$theta-thetas
summary(results.iexp)

## Combining the results to one larger data set
# Changing the names of the results, so they don't have the same names
colnames(results.CAT) <- paste(colnames(results.CAT), "_CAT", sep="")
colnames(results.baseline) <- paste(colnames(results.baseline), "_baseline", sep="")
colnames(results.iexp) <- paste(colnames(results.iexp), "_iexp", sep = "")
colnames(results.CTT) <- paste(colnames(results.CTT), "_CTT", sep = "")
colnames(results.random) <- paste(colnames(results.random), "_random", sep = "")

results <- cbind(results.baseline, results.CAT, results.iexp,results.CTT,results.random)
results$truetheta <- thetas
results$id <- 1:nrow(results)



#########################################
## Efficiency and information per item
#########################################
## CAT vs the item bank
# Efficiency
efficiency_baseline = results$SE_baseline^2
results$efficiency_CAT <- efficiency_baseline/(results$SE_CAT^2)
summary(results$efficiency_CAT)
## Information per item
InfoPrItem_baseline <-  results$n_baseline*results$SE_baseline^2
results$InfoPrItem_CAT <- InfoPrItem_baseline/(results$n_CAT*results$SE_CAT^2)
summary(results$InfoPrItem_CAT)

## For the short forms vs the CAT
# Efficiency
efficiency_CAT <- results$SE_CAT^2
results$efficiency_CTT <- efficiency_CAT/(results$SE_CTT^2)
summary(results$efficiency_CTT)
results$efficiency_iexp <- efficiency_CAT/(results$SE_iexp^2)
summary(results$efficiency_iexp)
results$efficiency_random <- efficiency_CAT/(results$SE_random^2)
summary(results$efficiency_random)
# Information per item
InfoPrItem_CAT <-  results$n_CAT*results$SE_CAT^2
results$InfoPrItem_CTT <- InfoPrItem_CAT/(results$n_CTT*results$SE_CTT^2)
summary(results$InfoPrItem_CTT)
results$InfoPrItem_iexp <- InfoPrItem_CAT/(results$n_iexp*results$SE_iexp^2)
summary(results$InfoPrItem_iexp)
results$InfoPrItem_random <- InfoPrItem_CAT/(results$n_random*results$SE_random^2)
summary(results$InfoPrItem_random)


#######################################
### Paired t-tests and Cohen's d
#######################################
## Item bank vs CAT
# Standard error
t.test(results$SE_baseline, results$SE_CAT, paired = T) # Paired t-test
mean(results$SE_baseline-results$SE_CAT)/sd(results$SE_baseline-results$SE_CAT) # Cohen's d
# Estimation bias
t.test(results$BIAS.f_baseline, results$BIAS.f_CAT, paired = T) # Paired t-test
mean(results$BIAS.f_baseline-results$BIAS.f_CAT)/sd(results$BIAS.f_baseline-results$BIAS.f_CAT) # Cohen's d

### CAT vs Short forms
## CAT vs item-total information short form
# Standard error
t.test(results$SE_CAT, results$SE_CTT, paired = T)
(mean(results$SE_CAT-results$SE_CTT))/sd(results$SE_CAT-results$SE_CTT)
# Estimation bias
t.test(results$BIAS.f_CAT, results$BIAS.f_CTT, paired = T)
(mean(results$BIAS.f_CAT-results$BIAS.f_CTT))/sd(results$BIAS.f_CAT-results$BIAS.f_CTT)

## CAT vs item exposure short form
# Standard error
t.test(results$SE_CAT, results$SE_iexp, paired = T)
(mean(results$SE_CAT-results$SE_iexp))/sd(results$SE_CAT-results$SE_iexp)
# Estimation bias
t.test(results$BIAS.f_CAT, results$BIAS.f_iexp, paired = T)
(mean(results$BIAS.f_CAT-results$BIAS.f_iexp))/sd(results$BIAS.f_CAT-results$BIAS.f_iexp)

## CAT vs random short form
# Standard error
t.test(results$SE_CAT, results$SE_random, paired = T)
(mean(results$SE_CAT-results$SE_random))/sd(results$SE_CAT-results$SE_random)
# Estimation bias
t.test(results$BIAS.f_CAT, results$BIAS.f_random, paired = T)
(mean(results$BIAS.f_CAT-results$BIAS.f_random))/sd(results$BIAS.f_CAT-results$BIAS.f_random)


### Part VI: Tables and figures ###


###############
## Tables
###############
### The outputs are formatted for being put in to latex

## Table with the item parameters of the 1PL, 2PL, and 3PL models
itempar <- round(cbind( # Getting the item parameters from the models
  coef(rescored.1PL,IRTpars=TRUE,simplify=TRUE)$items,
  coef(rescored.2PL,IRTpars=TRUE,simplify=TRUE)$items,
  coef(rescored.3PL,IRTpars=TRUE,simplify=TRUE)$items
),2)

itempar.table <- as.data.frame(itempar[,c(2,5,6,9,10,11)]) # Removing the item parameters that are the same for all the items
colnames(itempar.table) <- c('gamma','alpha','gamma','alpha','gamma','chi') 
rownames(itempar.table) <- c(1:36)
write.table(itempar.table, 'figures/app_itempar.txt', quote = F, sep = '&')

## Table with the percentage reached, RMSEA, and the 3PL item parameters
it.nr <- 1:36
percent.rescored <- round(100-100*colMeans(is.na(rescored)),0)
parameters <- coef(rescored.3PL,IRTpars=TRUE,simplify=TRUE)$items
parameters <- parameters[,1:3]
fitstat <- itemfit$RMSEA.X2
table <- as.data.frame(cbind(it.nr,percent.rescored,parameters,fitstat))
colnames(table) <- c('Item', '% reached', 'alpha', 'gamma', 'chi', 'RMSEA')
write.table(table %>% mutate_if(is.numeric, ~round(.,2)), # Rounding all the numbers to 2 decimals
            'figures/table1.txt', quote = F, sep = '&', row.names = F)

## Table with the AIC, BIC, wAIC, wBIC, and loglikelihood
model.comparison <- rbind(
  anova(rescored.1PL),
  anova(rescored.2PL),
  anova(rescored.3PL)
)
mod.comp <- model.comparison[c('AIC', 'BIC','logLik')] 
mod.comp <- cbind(mod.comp,wAIC, wBIC) # The wAIC and wBIC were calculated earlier
mod.comp <- mod.comp[c('AIC','wAIC','BIC','wBIC','logLik')]

write.table(mod.comp %>% mutate_if(is.numeric, ~round(.,0)), # Rounding the numbers
            'figures/model.comparison.txt', quote = F, sep = '&')


###################
#### Figures
###################

## Figures from the graphical analysis
for(i in which(itemfit$p.X2 < 0.05/36)){ # For loop to automatically save all the ICCs
  i <- as.numeric(i)
  png(paste("figures/ICC_", i, ".png"), width = 400, height = 400)
  print(itemfit(rescored.3PL, 'X2', group.bins=15, empirical.plot = i, method = 'ML'))
  dev.off()
}

## Figure with the Q3 statistic for the analysis of local dependency
Q3 = residuals(rescored.3PL,type="Q3")
colnames(Q3) <- rownames(Q3) <- 1:36
corrplot(Q3, method = 'color',type = 'upper', diag = F)

### The figure with the test information and standard error was created with the commented out script below.
### The script was created using a script by Aidan Loe (https://aidenloe.github.io/irtplots.html#test_information_plot)
# library('grid')
# library('gtable')
# tinfo <- testinfo(model, Theta=matrix(seq(-4,4,0.01))) %>% as.data.frame
# se <- 1/(sqrt(tinfo)) #get the standard error
# tinfo <- cbind.data.frame(tinfo, seq(-4,4,length.out=801), se) #make sure they are the same length
# colnames(tinfo) <- c("information", "xAxis","se")
# p1 <- ggplot(tinfo, aes(x=xAxis, y=information, colour="black")) + geom_line()  + theme_bw() +
#   scale_colour_identity(name="", guide="legend", labels=c("Information")) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_blank(),
#         axis.text=element_text(size=25),
#         axis.title=element_text(size=25),
#         legend.text=element_text(size=25),
#         legend.title=element_text(size=25),
#         plot.title = element_text(hjust = 0.5, size=25, face="bold"),
#         legend.position="bottom") +
#   labs(x="\nTheta", y = "Information\n") +
#   scale_x_continuous(breaks=c(-4,-3,-2,-1,0,1,2,3,4))
# p2 <- ggplot(tinfo, aes(x=xAxis, y=se, colour="red"))  + geom_line(linetype = "dashed") + theme_bw() +
#   scale_colour_identity(name="", guide="legend", labels=c("SE")) +
#   geom_hline(yintercept = .632) + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_blank(),
#         axis.text=element_text(size=25),
#         axis.title=element_text(size=25),
#         axis.text.y = element_text(colour = "red"),
#         legend.text=element_text(size=25),
#         legend.title=element_text(size=25),
#         legend.position="bottom")+
#   scale_x_continuous(breaks=c(-4,-3,-2,-1,0,1,2,3,4))
# 
# # extract gtable
# g1 <- ggplot_gtable(ggplot_build(p1))
# g2 <- ggplot_gtable(ggplot_build(p2))
# 
# # overlap the panel of 2nd plot on that of 1st plot
# pp <- c(subset(g1$layout, name == "panel", se = t:r))
# g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
#                      pp$l, pp$b, pp$l)
# 
# # axis tweaks
# ia <- which(g$layout$name == "ylab")
# ia <- which(g2$layout$name == "axis-l")
# ga <- g2$grobs[[ia]]
# ax <- ga$children[[2]]
# ax$widths <- rev(ax$widths)
# ax$grobs <- rev(ax$grobs)
# ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.08, "cm")
# g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
# g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
# 
# # extract legend
# leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
# leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]
# 
# g$grobs[[which(g$layout$name == "guide-box")]] <-
#   gtable:::cbind_gtable(leg1, leg2, "first")
# 
# # draw it
# grid.draw(g)
# 
# ggsave('tinfo_SE.png', width = 15, height = 9, plot = g)


################################
### Landscape plots for SE
################################
## Item bank vs CAT
ggplot(results, aes(SE_baseline, SE_CAT, color = truetheta))+ geom_point() + 
  geom_abline(slope = 1, intercept = 0, size=.75) + 
  geom_hline(yintercept=median(results$SE_CAT), size=.75)+
  geom_vline(xintercept = median(results$SE_baseline), size=.75)+ labs(color='True \U03B8') +
  xlab(expression(paste(SE(theta[p]), ' for the item bank')))+
  ylab(expression(paste(SE(theta[p]), ' for the CAT')))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=22, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('landscape_baseline.png', width = 15, height = 9)

## CAT vs item-total correlation short form
ggplot(results, aes(SE_CAT, SE_CTT, color = truetheta))+ geom_point() + 
  geom_abline(slope = 1, intercept = 0, size=.75) + 
  geom_hline(yintercept=median(results$SE_CTT), size=.75)+
  geom_vline(xintercept = median(results$SE_CAT), size=.75)+
  ylim(.5,.85)+
  xlab(expression(paste(SE(theta[p]), ' for the CAT')))+
  ylab(expression(paste(SE(theta[p]), ' for the item-total correlation short form')))+
  labs(color='True \U03B8')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=22, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('landscape_CTT.png', width = 15, height = 9)

## CAT vs random short form
ggplot(results, aes(SE_CAT, SE_random, color = truetheta))+ geom_point() + 
  geom_abline(slope = 1, intercept = 0, size=.75) + 
  geom_hline(yintercept=median(results$SE_random), size=.75)+
  geom_vline(xintercept = median(results$SE_CAT), size=.75)+
  xlab(expression(paste(SE(theta[p]), ' for the CAT')))+
  ylab(expression(paste(SE(theta[p]), ' for the random short form')))+
  labs(color='True \U03B8')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=22, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('landscape_random.png', width = 15, height = 9)

## CAT vs item exposure short form
ggplot(results, aes(SE_CAT, SE_iexp, color = truetheta))+ geom_point() + 
  geom_abline(slope = 1, intercept = 0, size=.75) + 
  geom_hline(yintercept=median(results$SE_iexp), size=.75)+
  geom_vline(xintercept = median(results$SE_CAT), size=.75)+
  ylim(.53,.85)+
  xlab(expression(paste(SE(theta[p]), ' for the CAT')))+
  ylab(expression(paste(SE(theta[p]), ' for the item exposure short form')))+
  labs(color='True \U03B8')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=22, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('landscape_iexp.png', width = 15, height = 9)

#############################################
### Landscape plots for the estimation bias
#############################################
## Item bank vs CAT
ggplot(results, aes(BIAS.f_baseline, BIAS.f_CAT, color = truetheta))+ geom_point() +
  geom_abline(slope = 1, intercept = 0, size = .75) + 
  geom_hline(yintercept=median(results$BIAS.f_CAT), size = .75) +
  geom_vline(xintercept = median(results$BIAS.f_baseline), size = .75)+ labs(color='True \U03B8') +
  scale_x_continuous(expression(paste(Bias(theta[p]), ' for the item bank')), labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  scale_y_continuous(expression(paste(Bias(theta[p]), ' for the CAT')), labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=22, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('landscape_bias_baseline.png', width = 15, height = 9)

## CAT vs item-total correlation short form
ggplot(results, aes(BIAS.f_CAT, BIAS.f_CTT, color = truetheta))+ geom_point() + 
  geom_abline(slope = 1, intercept = 0, size = .75) + 
  geom_hline(yintercept=median(results$BIAS.f_CTT), size = .75) +
  geom_vline(xintercept = median(results$BIAS.f_CAT), size = .75)+ labs(color='True \U03B8') +
  scale_x_continuous(expression(paste(Bias(theta[p]), ' for the CAT')), labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  scale_y_continuous(expression(paste(Bias(theta[p]), ' for the item-correlation short form')), labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=22, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('landscape_bias_CTT.png', width = 15, height = 9)

## CAT vs random short form
ggplot(results, aes(BIAS.f_CAT, BIAS.f_random, color = truetheta))+ geom_point() + 
  geom_abline(slope = 1, intercept = 0, size = .75) + 
  geom_hline(yintercept=median(results$BIAS.f_random), size = .75) +
  geom_vline(xintercept = median(results$BIAS.f_CAT), size = .75)+ labs(color='True \U03B8') +
  scale_x_continuous(expression(paste(Bias(theta[p]), ' for the CAT')), labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  scale_y_continuous(expression(paste(Bias(theta[p]), ' for the random short form')), labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=22, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('landscape_bias_random.png', width = 15, height = 9)

## CAT vs item exposure short form
ggplot(results, aes(BIAS.f_CAT, BIAS.f_iexp, color = truetheta))+ geom_point() + 
  geom_abline(slope = 1, intercept = 0, size = .75) + 
  geom_hline(yintercept=median(results$BIAS.f_iexp), size = .75) +
  geom_vline(xintercept = median(results$BIAS.f_CAT), size = .75)+ labs(color='True \U03B8') +
  scale_x_continuous(expression(paste(Bias(theta[p]), ' for the CAT')), labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  scale_y_continuous(expression(paste(Bias(theta[p]), ' for the item exposure short form')), labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=22, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('landscape_bias_iexp.png', width = 15, height = 9)


## Histogram of the estimation bias for the CAT and the short forms
results %>% select(BIAS.f_CAT, BIAS.f_iexp, BIAS.f_CTT, BIAS.f_random,id) %>%  # Choosing the varialbes to plotted
  pivot_longer(., cols = c(BIAS.f_CAT, BIAS.f_iexp, # Pivoting the data from wide to long
                           BIAS.f_CTT, BIAS.f_random), names_to = 'Test', values_to = 'Val') %>%
  mutate(Test = recode(Test, 'BIAS.f_CAT' = 'CAT', 'BIAS.f_iexp' = 'Item exposure',  # Changing the names of the tests
                       'BIAS.f_CTT' = 'Item-total correlation', 'BIAS.f_random' = 'Random')) %>%
  mutate(Test = factor(Test, levels = c('CAT','Item exposure','Item-total correlation','Random'))) %>% # Setting factor levels so that the histograms are in the right order
  ggplot(aes(x = Val)) + geom_histogram(bins=20) +ylab('')+  # Plotting figure
  facet_grid(Test~.)+ 
  scale_x_continuous(expression(paste(Bias(theta[p]))), labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        strip.background = element_rect(fill = 'white'),
        strip.text.y = element_text(size=15),
        axis.text.x=element_text(size=25),
        axis.title=element_text(size=25),
        axis.text.y=element_text(size=15),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=22, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('hist_bias.png', width = 15, height=9)


## Plot of the length of the CAT against the true ability
ggplot(results, aes(x= truetheta, y = n_CAT)) + geom_jitter()+
  geom_smooth(method = 'loess', color = 'red', size=3)+
  scale_x_continuous('True \U03B8', labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  scale_y_continuous('Test length', labels=as.character(seq(5,10,1)), breaks=seq(5,10,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        legend.key.size = unit(1, 'cm'))
ggsave('jitter.png', width = 15, height = 9)

#################################
### Plots of the efficiency
#################################
## Item bank vs CAT
ggplot(results,aes(x=truetheta, y=efficiency_CAT)) + geom_point() + 
  geom_smooth(method = 'loess', color = 'red', size=3)+
  scale_y_continuous('Efficiency',labels = scales::number_format(accuracy = 0.1), breaks = seq(.3,1.2,.1))+ 
  scale_x_continuous('True \U03B8', labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('efficiency_baseline.png', width = 15, height = 9)

## CAT vs item-total correlation short form
ggplot(results,aes(x=truetheta, y=efficiency_CTT)) + geom_point() + 
  geom_smooth(method = 'loess', color = 'red', size=3)+
  scale_y_continuous('Efficiency',labels = scales::number_format(accuracy = 0.1), breaks = seq(.3,1.2,.1))+ 
  scale_x_continuous('True \U03B8', labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('efficiency_CTT.png', width = 15, height = 9)

## CAT vs item exposure short form
ggplot(results,aes(x=truetheta, y=efficiency_iexp)) + geom_point() + 
  geom_smooth(method = 'loess', color = 'red', size=3)+
  scale_y_continuous('Efficiency',labels = scales::number_format(accuracy = 0.1), breaks = seq(.3,1.2,.1))+ 
  scale_x_continuous('True \U03B8', labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('efficiency_iexp.png', width = 15, height = 9)

## CAT vs random short form
ggplot(results,aes(x=truetheta, y=efficiency_random)) + geom_point() + 
  geom_smooth(method = 'loess', color = 'red', size=3)+
  scale_y_continuous('Relative efficiency',labels = scales::number_format(accuracy = 0.1), breaks = seq(.3,1.2,.1))+ 
  scale_x_continuous('True \U03B8', labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('efficiency_random.png', width = 15, height = 9)


###################################
### Plots of information per item
###################################
## Item bank vs CAT
ggplot(results,aes(x=truetheta, y=InfoPrItem_CAT)) + geom_point() + 
  geom_smooth(method = 'loess', color = 'red', size=3)+
  scale_x_continuous('True \U03B8', labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  ylab('Information per item')+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('infopritem_baseline.png', width = 15, height = 9)

## CAT vs item-total correlation short form
ggplot(results,aes(x=truetheta, y=InfoPrItem_CTT)) + geom_point() + 
  geom_smooth(method = 'loess', color = 'red', size=3)+
  scale_x_continuous('True \U03B8', labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  ylab('Information per item')+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('infopritem_CTT.png', width = 15, height = 9)

## CAT vs item exposure short form
ggplot(results,aes(x=truetheta, y=InfoPrItem_iexp)) + geom_point() + 
  geom_smooth(method = 'loess', color = 'red', size=3)+
  scale_x_continuous('True \U03B8', labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  ylab('Information per item')+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('infopritem_iexp.png', width = 15, height = 9)

## CAT vs random short form
ggplot(results,aes(x=truetheta, y=InfoPrItem_random)) + geom_point() + 
  geom_smooth(method = 'loess', color = 'red', size=3)+
  scale_x_continuous('True \U03B8', labels = as.character(seq(-3,3,1)), breaks = seq(-3,3,1))+
  ylab('Information per item')+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.key.size = unit(1, 'cm'))
ggsave('infopritem_random.png', width = 15, height = 9)

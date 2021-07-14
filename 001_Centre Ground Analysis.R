library(tidyverse)
library(survey)
library(srvyr)
library(gridExtra)
library(ggthemes)


###########################

## Hypotheticals

normcore <- tibble(measure = 1:19,
                   proportion = c(0,0,0,0.01,0.08,0.21,0.35,0.6,0.85,0.99,
                             0.85,0.6,0.35,0.21,0.08,0.01,0,0,0))
bimodal <- tibble(measure = 1:19,
                  proportion = c(0.41,0.79,0.99,0.81,0.39,0.1,0.08,
                            0.098,0.1,0.09,0.11,0.09,0.11,0.1,0.45,0.77,0.99,0.81,0.39))
uniform <- tibble(measure = 1:19,
                  proportion = c(0.51,0.509,0.50,0.49,0.50,0.511,0.505,0.578,0.51,0.509,
                            0.50,0.49,0.50,0.505,0.573,0.51,0.51,0.509,0.50))
skewed <- tibble(measure = 1:19,
                 proportion = c(0.05,0.76,0.99,0.85,0.67,0.511,0.50,0.35,0.20,0.09,
                           0.06,0.03,0.01,0,0,0,0,0,0))

greyplot2 <- function(DF, CAPTION) {
  ggplot(DF, aes(x = measure, y = proportion)) +
    geom_col(colour = 'black', fill = 'grey80') +
    scale_y_continuous(limits = c(0,1), breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    labs(caption = CAPTION) +
    theme_tufte(base_family = 'Times') + 
    theme(plot.caption = element_text(hjust = 0.5, colour = 'black', size = 12, margin = margin(t = 7)),
          axis.text = element_text(colour = 'black', size = 10),
          axis.title = element_blank())
}

plots <- vector("list", length = 4)
greyplot2(normcore, "(i) Normal") -> plots[[1]]
greyplot2(bimodal, "(ii) Bimodal")  -> plots[[2]]
greyplot2(uniform, "(iii) Uniform") -> plots[[3]]
greyplot2(skewed, "(iv) Skewed") -> plots[[4]]

grid.arrange(grobs= lapply(plots, "+", theme(plot.margin=margin(10,10,10,10))),
             nrow=1) -> hypotheticals


###########################

## British Election Studies Analysis
## Process variables

BES <- read_csv("BES2020.csv") # https://www.britishelectionstudy.com/data-object/wave-20-of-the-2014-2023-british-election-study-internet-panel/

# BES <- BES[BES$polAttention %in% c('5','6','7','8','9','Pay a great deal of attention'),] # Limit to those paying some attention to politics

BES$mt_LR_scale <- ifelse(BES$lr_scale == 'Left', 0, ifelse(BES$lr_scale == 'Right', 10, BES$lr_scale))
BES$mt_al_scale <- ifelse(BES$al_scale == 'Left', 0, ifelse(BES$al_scale == 'Right', 10, BES$al_scale))
BES$mt_al_scale <- (as.numeric(BES$mt_al_scale)-10)*-1
BES$mt_LRself <- ifelse(BES$leftRight == 'Left', 0, ifelse(BES$leftRight == 'Right', 10, BES$leftRight))
BES$mt_taxspend <- ifelse(BES$taxSpendSelf == "Government should cut taxes a lot and spend much less on health and social services", 0,
                          ifelse(BES$taxSpendSelf == "Government should increase taxes a lot and spend much more on health and social services", 10, BES$taxSpendSelf))
BES$mt_taxspend <- (as.numeric(BES$mt_taxspend)-10)*-1
BES$mt_redistSelf <- ifelse(BES$redistSelf == "Government should try to make incomes equal", 0,
                            ifelse(BES$redistSelf == "Government should be less concerned about equal incomes", 10, BES$redistSelf))
BES$mt_redistSelf <- as.numeric(BES$mt_redistSelf)
BES$mt_deficitReduce <- recode(BES$deficitReduce, "It is completely necessary" = 4,
                               "It is important but not absolutely  necessary" = 3,
                               "It is not necessary but it would be desirable" = 2,
                               "It is completely unnecessary" = 1,
                               "Don't know" = 999)
BES$mt_deficitReduce <- na_if(BES$mt_deficitReduce, 999)
BES$mt_immigSelf <- ifelse(BES$immigSelf == "Allow many fewer", 0,
                           ifelse(BES$immigSelf == "Allow many more", 10, BES$immigSelf))
BES$mt_immigSelf <- (as.numeric(BES$mt_immigSelf)-10)*-1
BES$mt_immigCultural <- ifelse(BES$immigCultural == "Undermines cultural life", 1,
                               ifelse(BES$immigCultural == "Enriches cultural life", 7, BES$immigCultural))
BES$mt_immigCultural <- (as.numeric(BES$mt_immigCultural)-7)*-1
BES$mt_immigEcon <- ifelse(BES$immigEcon == "Bad for economy", 1,
                           ifelse(BES$immigEcon == "Good for economy", 7, BES$immigEcon))
BES$mt_immigEcon <- (as.numeric(BES$mt_immigEcon)-7)*-1

BES %>%
  mutate(across(.cols = c("lr1", "lr2", "lr3", "lr4", "lr5"),
                .fns = ~recode(.,
                               "Strongly agree" = 0,
                               "Agree" = 1,
                               "Neither agree nor disagree" = 2,
                               "Disagree" = 3,
                               "Strongly disagree" = 4,
                               "Don't know" = 55),
                .names = "mt_{col}")) %>%
  mutate(across(.cols = c("mt_lr1", "mt_lr2", "mt_lr3", "mt_lr4", "mt_lr5"),
                .fns = ~na_if(., 55))) -> BES

BES %>%
  mutate(across(.cols = c("al1", "al2", "al3", "al4", "al5"),
                .fns = ~recode(.,
                               "Strongly agree" = 0,
                               "Agree" = 1,
                               "Neither agree nor disagree" = 2,
                               "Disagree" = 3,
                               "Strongly disagree" = 4,
                               "Don't know" = 55),
                .names = "mt_{col}")) %>%
  mutate(across(.cols = c("mt_al1", "mt_al2", "mt_al3", "mt_al4", "mt_al5"),
                .fns = ~na_if(., 55))) -> BES

BES %>%
  select(wt, starts_with("mt_")) %>%
  as_survey_design(weights = wt) -> BES_wt


###########################

## Graph functions

groupedsummary <- function(varName) {
  BES_wt %>%
    group_by_(varName) %>%
    summarize(proportion=survey_mean(),
              total=survey_total()) %>%
    rename(measure = 1) %>%
    mutate(measure = as.numeric(measure))
}

greyplot <- function(DF, CAPTION, YLIMITS = NULL, XBREAKS = NULL, XLABS = NULL) {
  ggplot(DF, aes(x = measure, y = proportion)) +
    geom_col(colour = 'black', fill = 'grey80') +
    scale_y_continuous(limits = YLIMITS, breaks = NULL) +
    scale_x_continuous(breaks = XBREAKS, labels = XLABS) +
    labs(caption = CAPTION) +
    geom_rangeframe(data=data.frame(x = XBREAKS, y = c(0,0)), aes(x, y), sides = 'b') +
    theme_tufte(base_family = 'Times') + 
    theme(plot.caption = element_text(hjust = 0.5, colour = 'black', size = 12, margin = margin(t = 7)),
          axis.text = element_text(colour = 'black', size = 10),
          axis.title = element_blank())
}

## Graphs

groupedsummary("as.integer(mt_LR_scale)") -> LRSCALE
groupedsummary("mt_LRself") -> LRSELF
plots <- vector("list", length = 2)
greyplot(LRSCALE, "(i) Left-Right index", c(0,0.25), c(0,10), c('Left','Right')) -> plots[[1]]
greyplot(LRSELF, "(ii) Left-Right self placement", c(0,0.25), c(0,10), c('Left','Right')) -> plots[[2]]
grid.arrange(grobs= lapply(plots, "+", theme(plot.margin=margin(10,10,10,10))),
             ncol=2) -> LR

NROW(BES$mt_LRself[BES$mt_LRself == "Don't know"]) # 6499 said don't know, 21%
NROW(BES$mt_LR_scale[is.na(BES$mt_LR_scale)]) # 3596 are NA, i.e. 11% (and that is cumulative across 5 items)


groupvar <- c("mt_lr1", "mt_lr2", "mt_lr3", "mt_lr4", "mt_lr5")
Output <- lapply(groupvar, groupedsummary)
plots <- vector("list", length = 5)
labz <- c("(i) Government should redistribute income", "(ii) Big business takes advantage of ordinary people",
          "(iii) Ordinary people don't get their fair share", "(iv) There is one law for the rich and one for the poor",
          "(v) Management will always try to get the better of employees")
for (i in 1:5){
  Output[[i]] %>%
    greyplot(., labz[i], c(0,0.45), c(0,4), c('Agree','Disagree')) -> plots[[i]]
}
grid.arrange(grobs= lapply(plots, "+", theme(plot.margin=margin(10,10,10,10))),
             nrow=3, ncol=2) -> LRQS


groupvar <- c("mt_al1", "mt_al3", "mt_al4", "mt_al5", "mt_al2")
Output <- lapply(groupvar, groupedsummary)
plots <- vector("list", length = 5)
labz <- c("(i) Young people do not respect traditional British values",
          "(ii) Schools should teach children to obey authority", "(iii) Censorship is necessary to uphold moral standards",
          "(iv) Criminals should be given stiffer sentences", "(v) For some crimes the death penalty is appropriate")
for (i in 1:5){
  Output[[i]] %>%
    greyplot(., labz[i], c(0,0.45), c(0,4), c('Agree','Disagree')) -> plots[[i]]
}
grid.arrange(grobs= lapply(plots, "+", theme(plot.margin=margin(10,10,10,10))),
             nrow=3, ncol=2) -> ALQS


groupedsummary("mt_taxspend") -> TAXSPEND
groupedsummary("mt_redistSelf") -> REDIST
groupedsummary("mt_deficitReduce") -> DEFICIT
plots <- vector("list", length = 3)
greyplot(TAXSPEND, "(i) Government should increase taxes and spending", c(0,0.25), c(0,10), c('Agree','Disagree')) -> plots[[1]]
greyplot(REDIST, "(ii) Government should try to make incomes equal", c(0,0.25), c(0,10), c('Agree','Disagree')) -> plots[[2]]
greyplot(DEFICIT, "(iii) Reducing government deficits is...", XBREAKS = c(1,4), XLABS = c('Unnecessary','Necessary')) -> plots[[3]]
grid.arrange(grobs= lapply(plots, "+", theme(plot.margin=margin(10,10,10,10))),
             ncol=2) -> SPEND


groupedsummary("mt_immigSelf") -> IMMIGRATION
groupedsummary("mt_immigCultural") -> IMMICULTURE
groupedsummary("mt_immigEcon") -> IMMIECON
plots <- vector("list", length = 3)
greyplot(IMMICULTURE, "(i) Immigration enriches cultural life", c(0,0.25), c(0,6), c('Agree','Disagree')) -> plots[[1]]
greyplot(IMMIECON, "(ii) Immigration enriches the economy", c(0,0.25), c(0,6), c('Agree','Disagree')) -> plots[[2]]
greyplot(IMMIGRATION, "(iii) Immigration should be...", XBREAKS = c(0,10), XLABS = c('Increased','Reduced')) -> plots[[3]]
grid.arrange(grobs= lapply(plots, "+", theme(plot.margin=margin(10,10,10,10))),
             ncol=2) -> IMMI


###########################

##### Moderation

BES %>%
  mutate(across(.cols = c('mt_lr1','mt_lr2','mt_lr3','mt_lr4','mt_lr5'),
                .fns = ~recode(.,
                               `0` = 0L,
                               `1` = 1L,
                               `2` = 2L,
                               `3` = 1L,
                               `4` = 0L),
                .names = "abs_{col}")) -> BES

BES %>%
  select(wt, mt_LRself, starts_with("abs_")) %>%
  as_survey_design(weights = wt) -> BES_wt2

varlist <- c("abs_mt_lr1","abs_mt_lr2","abs_mt_lr3","abs_mt_lr4","abs_mt_lr5")

for (i in 1:5) {
  BES_wt2 %>%
    filter(mt_LRself != 5) %>%
    group_by_(varlist[i]) %>%
    summarize(proportion=survey_mean(),
              total=survey_total()) %>% print()
  BES_wt2 %>%
    filter(mt_LRself == 5) %>%
    group_by_(varlist[i]) %>%
    summarize(proportion=survey_mean(),
              total=survey_total()) %>% print()
}

mean(c(22.8,15.5,19.2,14.1,19.7))
mean(c(30.7,17.4,23.5,16.2,21.7))

# Average percent of (weighted) respondents giving the most extreme answers to the 5 individual L-R questions
# is 25.92% for people who self-identified as at the middle (5), vs 27.98% for everyone else

for (i in 1:5) {
  BES_wt2 %>%
    group_by_(varlist[i]) %>%
    summarize(proportion=survey_mean(),
              total=survey_total()) %>% print()
}

# Across issues, the average of number people choosing either of the most extreme answers is 27.56%, 
# compared to only 18.9% choosing the middle value


###########################

#### Constraint

BES %>%
  mutate(LRanswers = as.factor(paste0(mt_lr1,mt_lr2,mt_lr3,mt_lr4,mt_lr5))) %>%
  mutate(LRInconstant = ifelse((grepl("0", LRanswers) & grepl("4", LRanswers)), 1, 0)) %>%
  select(wt, LRInconstant) %>%
  as_survey_design(weights = wt) %>%
  summarise(proportion=survey_mean(LRInconstant)) # 3% of people agree strongly and disagree strongly

BES %>%
  mutate(LRanswers = as.factor(paste0(mt_lr1,mt_lr2,mt_lr3,mt_lr4,mt_lr5))) %>%
  mutate(LRInconstant_weak = ifelse((grepl("0|1", LRanswers) & grepl("3|4", LRanswers)), 1, 0)) %>%
  select(wt, LRInconstant_weak) %>%
  as_survey_design(weights = wt) %>%
  summarise(proportion=survey_mean(LRInconstant_weak)) # 27% agree and disagree

# Inter-item correlation
jtools::svycor(~ mt_lr1 + mt_lr2 + mt_lr3 + mt_lr4+ mt_lr5, design = BES_wt, na.rm = TRUE, digits = 7)
# All statistically significant
# Range from 0.36 to 0.65

# Chronbach alpha = 0.92, c = 0.47
mean(.4717809,0.6462653,0.5461447,0.3639732,0.5716939,0.5909460,0.5702302,0.6262279,0.4595475,0.4778243) -> c
mean(svyvar(~ mt_lr1 + mt_lr2 + mt_lr3 + mt_lr4+ mt_lr5, design = BES_wt, na.rm = TRUE)) -> v
(5*c)/(v+(4*c))

# PCA
summary(prcomp(~ mt_lr1 + mt_lr2 + mt_lr3 + mt_lr4 + mt_lr5, data = BES, center = TRUE, scale = TRUE)) # singular value decomposition
summary(princomp(~ mt_lr1 + mt_lr2 + mt_lr3 + mt_lr4 + mt_lr5, data = BES, center = TRUE, scale = TRUE)) # eigendecomposition of correlation matrix
# 63% of variance explained by one underlying dimension

# Can't check intertemporal stability because left-right values are only asked once of each person!



###########################

## YouGov

FAVRUSSIA <- tibble(measure = c(1,2,3,4,NA),
                 proportion = c(1,9,34,41,16)) #https://docs.cdn.yougov.com/3ce71typvy/Eurotrack_May21_Topline_Favourability_Israel.pdf
FAVCHINA <- tibble(measure = c(1,2,3,4,NA),
                    proportion = c(1,11,31,43,14)) #https://docs.cdn.yougov.com/3ce71typvy/Eurotrack_May21_Topline_Favourability_Israel.pdf
FAVIRAN <- tibble(measure = c(1,2,3,4,NA),
                    proportion = c(1,6,30,42,21)) #https://docs.cdn.yougov.com/3ce71typvy/Eurotrack_May21_Topline_Favourability_Israel.pdf
plots <- vector("list", length = 3)
greyplot(FAVRUSSIA, "(i) Views of Russia", c(0,44), c(1,4), c("Favourable","Unfavourable")) -> plots[[1]]
greyplot(FAVCHINA, "(ii) Views of China", c(0,44), c(1,4), c("Favourable","Unfavourable")) -> plots[[2]]
greyplot(FAVIRAN, "(iii) Views of Iran", c(0,44), c(1,4), c("Favourable","Unfavourable")) -> plots[[3]]
grid.arrange(grobs= lapply(plots, "+", theme(plot.margin=margin(10,10,10,10))),
             ncol=2) -> FAV


NATOSAFE <- tibble(measure = c(1,2,3,NA),
                  proportion = c(57,16,3,24)) #https://yougov.co.uk/topics/politics/survey-results/daily/2019/12/04/15d13/3
NATOWEST <- tibble(measure = c(1,2,3,NA),
                  proportion = c(66,4,6,24)) #https://yougov.co.uk/topics/politics/survey-results/daily/2019/12/04/15d13/2
SUPNATO <- tibble(measure = c(1,2,3,4,5,NA),
                  proportion = c(37,28,12,2,1,20)) #https://yougov.co.uk/topics/international/survey-results/daily/2019/12/04/15d13/1
plots <- vector("list", length = 3)
greyplot(NATOSAFE, "(i) NATO's role in defending the West is...", c(0,67), c(1,3), c("Important","Unimportant")) -> plots[[1]]
greyplot(NATOWEST, "(ii) Does NATO make Britain...", c(0,67), c(1,3), c("More safe","Less safe")) -> plots[[2]]
greyplot(SUPNATO, "(iii) Britain's membership of NATO", c(0,44), c(1,5), c("Support","Oppose")) -> plots[[3]]
grid.arrange(grobs= lapply(plots, "+", theme(plot.margin=margin(10,10,10,10))),
             ncol=2) -> NATO

library(lme4)
library(multcomp)
library(dplyr)

AP <- read.csv("0701AlkPhenTrialsFINAL.csv")
MAP <- read.csv("0728ModifiedAlkPhenTrialsFINAL.csv")


################ AP fruit trials
  
  #1) HISTOGRAM
  hist(AP$No..of.Ants)
#"poisson" distribution observed

#2) ANOVA
f1 <- glmer(No..of.Ants ~ Treatment*Habitat.Type + (1|Trial.No), data=AP, family=poisson)
summary(f1)

f2 <- glmer(No..of.Ants ~ Treatment + Habitat.Type + (1|Trial.No), data=AP, family=poisson)
f3 <- glmer(No..of.Ants ~ Habitat.Type + (1|Trial.No), data=AP, family=poisson)
f4 <- glmer(No..of.Ants ~ Treatment + (1|Trial.No), data=AP, family=poisson)

anova(f1, f2)
# int. of Treatment and Habitat is no sig: p = 0.343
anova(f2, f3)
# effect of Treatment is no sig: p = 0.1962
anova(f2, f4)
# Habitat is no sig: p = 0.0728

#3) TUKEY
letf4 <- glht(f4, linfct=mcp(Treatment="Tukey"))
summary(letf4)
cld(letf4, level = 0.05)
# given letters: alkenylphenol = a, control = a

######## Modified AP trials

#1) HISTOGRAM
hist(MAP$No.Ants)
#observed: poisson distribution

#2) ANOVA
f1 <- glmer(No.Ants ~ Treatment*Habitat.Type + (1|Trial.ID), data=MAP, family = poisson)
summary(f1)

f2 <- glmer(No.Ants ~ Treatment + Habitat.Type + (1|Trial.ID), data=MAP, family = poisson)
f3 <- glmer(No.Ants ~ Habitat.Type + (1|Trial.ID), data=MAP, family=poisson)
f4 <- glmer(No.Ants ~ Treatment + (1|Trial.ID), data=MAP, family = poisson)

anova(f1,f2)
# interaction of Treatment and Habitat no sig: p = 0.305
anova(f2,f3)
# effect of Treatment no sig: p = 0.9319
anova(f2,f4)
# effect of Habitat no sig: p = 0.3024

#3) TUKEY
# treatment
letf4 <- glht(f4, linfct=mcp(Treatment="Tukey"))
summary(letf4)
cld(letf4, level = 0.05)
# given letters: alkenylphenol = a, treatment = a


#### Visualization

tiff(filename = "fig6_alkenyl.tiff", width=6.42, height=3.56, units="in", res=500)
par(mfrow = c(1,2), 
    oma = c(4,3.2,0,0) + 0.1,
    mar = c(0,2,2,1) + 0.1)

plot(No..of.Ants ~ droplevels(Treatment),
     main = "(A)",
     ylab = " ",
     xlab = "",
     ylim = c(0,15),
     col = (c("firebrick2", "goldenrod3")),
     data=AP)
plot(No.Ants ~ droplevels(Treatment),
     main = "(B)",
     ylab = "",
     xlab = "",
     ylim = c(0,15),
     col = (c("firebrick2", "goldenrod3")),
     data=MAP)

title(ylab = "No. of Ants",
      xlab = "Treatment",
      outer = TRUE, line = 2)
dev.off()

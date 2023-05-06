### 7 August 2018
  # Ant Choice - 5 Trial Analysis


#### Opening Packages ####
library(lme4)
library(multcomp)
library(dplyr)
library(vegan)
library(colourlovers)
library(ggplot2)

#### Cleaning up Data ####
ants <- read.csv("0919FiveChoiceTrialswSpecies.csv")
ants <- ants[1:200,]
ants2 <- ants[, -c(5:10)]

##fixing variable names for ant species
ants$Species.No..1 <- as.character(ants$Species.No..1)
ants$Species.No..2 <- as.character(ants$Species.No..2)
ants$Species.No..3 <- as.character(ants$Species.No..3)

ants$X..Species.No..1 <- as.character(ants$X..Species.No..1)
ants$X..Species.No.2 <- as.character(ants$X..Species.No.2)
ants$X..Species.No..3 <- as.character(ants$X..Species.No..3)

ants$Species.No..1[which(ants$Species.No..1=="No sample")] <- "Unidentified"
ants$Species.No..1[which(ants$Species.No..1=="No specimen")] <- "Unidentified"
ants$Species.No..1[which(ants$Species.No..1=="No Specimen")] <- "Unidentified"
ants$Species.No..1[which(ants$Species.No..1=="Unidentifiable")] <- "Unidentified"
ants$Species.No..1[which(ants$Species.No..1=="Ectatoma edendatum")] <- "Ectatomma edendatum"

ants$Species.No..2[which(ants$Species.No..2=="No sample")] <- "Unidentified"
ants$Species.No..2[which(ants$Species.No..2=="No specimen")] <- "Unidentified"
ants$Species.No..2[which(ants$Species.No..2=="Pheidole spp. (nigricula, nebulosa, u1)")] <- "Pheidole sp."
ants$Species.No..2[which(ants$Species.No..2=="NA")] <- "Unidentified"

ants$X..Species.No..1[which(ants$X..Species.No..1=="?")] <- NA
ants$X..Species.No..1[which(ants$X..Species.No..1=="?, majority ")] <- 20
ants$X..Species.No.2[which(ants$X..Species.No.2=="?")] <- NA
ants$X..Species.No..3[which(ants$X..Species.No..3=="?")] <- NA

ants$X..Species.No..1 <- as.numeric(ants$X..Species.No..1)
ants$X..Species.No.2 <- as.numeric(ants$X..Species.No.2)
ants$X..Species.No..3 <- as.numeric(ants$X..Species.No..3)

##Also a few weird ones with unidentified Pheidole where I will just fix manually

ants$Species.No..1[17] <- "Pheidole sp."
ants$X..Species.No..1[17] <- 6
ants$Species.No..2[17] <- "None"
ants$X..Species.No.2[17] <- 0

ants$X..Species.No..1[30] <- 1

ants$Species.No..1[93] <- "Pheidole sp."
ants$X..Species.No..1[93] <- 4
ants$Species.No..2[93] <- "None"
ants$X..Species.No.2[93] <- 0

ants$X..Species.No..1[109] <- 1

ants$X..Species.No..3[117] <- 2
ants$X..Species.No.2[117] <- 3
ants$X..Species.No..3[118] <- 1
ants$X..Species.No.2[118] <- 2

ants$Species.No..1[142] <- "Unidentified"

ants$Species.No..1[162] <- "Pheidole sp."
ants$Species.No..2[162] <- "None"
ants$Species.No..2[162] <- "None"
ants$X..Species.No..1[163] <- 20

ants$Species.No..1[168] <- "Pheidole sp."
ants$X..Species.No..1[168] <- 11
ants$Species.No..2[168] <- "None"
ants$X..Species.No.2[168] <- 0

ants$Species.No..2[192] <- "Pheidole sp."
ants$X..Species.No.2[192] <- 4
ants$Species.No..3[192] <- "None"
ants$X..Species.No..3[192] <- 0

ants$X..Species.No..1[142] <- 3


antsp <- unique(c(as.character(ants$Species.No..1), as.character(ants$Species.No..2), as.character(ants$Species.No..3)))
antsp <- antsp[order(antsp)]
antsp <- antsp[-7]  #getting rid of "none"

ants2[ , antsp] <- NA



##Now pulling counts from original data frame and adding to new species columns

for (i in 16:ncol(ants2)){
  for (j in 1:nrow(ants2)){
    if(ants[j,5]==colnames(ants2)[i]){
      ants2[j,i] <- ants[j,6]
    }
    if (ants[j,7]==colnames(ants2)[i]){
      ants2[j,i] <- ants[j,8]
    }
    if (ants[j,9]==colnames(ants2)[i]){
      ants2[j,i] <- ants[j,10]
    }
  }
}

ants2$total <- rowSums(ants2[,16:35], na.rm=TRUE)

ants2$No.Ants==ants2$total

#### Summary Tables for Data ####
#Having a look at some summary tables
sums <- ants2 %>%
  group_by(Habitat.Type) %>%
  summarise_at (c(4, 16:35), sum, na.rm=T)

counts <- ants2 %>%
  group_by(Habitat.Type) %>%
  summarise_at (c(4,16:35), funs(sum(!is.na(.))))

#Having a look at some summary tables
sums <- ants2 %>%
  group_by(Treatment) %>%
  summarise_at (c(4, 16:35), sum, na.rm=T)

counts <- ants2 %>%
  group_by(Treatment) %>%
  summarise_at (c(4, 16:35), funs(sum(!is.na(.))))


#### Question 1: Differences in Communities and Preferences? ####

####This is the final NMDS analysis that I think makes sense to present in the paper
##Have tried running this with dataset simplified in various ways by grouping species, dropping unidentified 
##species, using presence/absence data. No matter what there is no separation by groups

##In this analysis we group everything that occurs in fewer than 5 samples, creating the following new
  ##variables
  ##Pheidole spp = all uncommon or unidentified Pheidole grouped
  ##Ectatomma spp = all uncommon or unidentified Ectatomma grouped
  ##Other = all other uncommon or unidentfied species that occurred in fewer than 5 samples 


ants10 <- ants2
ants10$Pheidole.spp <- rowSums(ants10[,25:32], na.rm=TRUE)
ants10 <- ants10[,-c(25:32)]
ants10$Ectatomma.spp <- rowSums(ants10[,c(18,21)], na.rm=TRUE)
ants10 <- ants10[,-c(18,21)]
ants10$Other <- rowSums(ants10[,c(16,17, 20, 24)], na.rm=TRUE)
ants10 <- ants10[,-c(16,17, 20, 24)]


###will need to get rid of lines with no ants observed
ants10$total <- rowSums(ants10[,c(16:21, 23:25)], na.rm=TRUE)
ants10 <- ants10[which(ants10$total !=0),]

##and replace na with 0
ants10[is.na(ants10)] <- 0

#### NMDS Analysis (Q1) ####

#the matrices for the NMDS
sp <- ants10[,c(16:21, 23:25)]
expl <- ants10[,c(2, 15)]
trt <- ants10[,c(2)]
hab <- ants10[,c(15)]

permanova <- adonis(sp ~ trt*hab, method = "bray", perm = 999)
print(permanova)

#The NMDS
mds1 <- metaMDS(sp, k=2, trymax=200, autotransform=T)  #run the NMDS
mds1 #print the NMDS results
cor(vegdist(sp), dist(mds1$points))^2  #calculate R^2 for NMDS

#get colors
plot(clpalettes('top'))

getpal <- swatch(clpalette('1473'))[[1]]
plot.clpalette('1473', type='png')

  # WORK with me once you have access to more palettes. 

#pal <- c("#962136", "#A238A5", "#D86C6F", "#23CE6B", "#452EB5")
#pal <- c("#D7191C","#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6" ) 
pal <- c("#DA5526", "#F6893D", "#FEBC38", "#23CE6B", "#697F98")
#pal <- c("#F6EFF7", "#BDC9E1", "#67A9CF", "#1C9099", "#016C59")
#to see colors
y <- c(5,5,5,5,5)
barplot(y, col=pal)
#play with these a bit more, I am not really happy with the colors


##Plot color-coded by seed state (Fig. 2a)

#setting up fig file for export--DON't RUN if you just want to see the plots in RStudio
tiff("Fig2_NMDS.tiff", width = 84, height = 100, units = 'mm', res = 800, compression="lzw")
par(mfrow=c(2,1))
par(mar = c(4, 4, 0, 0), oma = c(2, 0.5, 2, 0.5))
par(cex=0.6)


#Set up colors
expl$Treatment <- droplevels(expl$Treatment)
cols <- as.numeric(expl$Treatment)
cols[which(cols==2)] <- pal[5]
cols[which(cols=="3")] <- pal[4]
cols[which(cols=="4")] <- pal[2]
cols[which(cols=="5")] <- pal[3]
cols[which(cols=="1")] <- pal[1]

pal.r <- c(pal[5], pal[4], pal[3], pal[2], pal[1])
symb <- as.numeric(expl$Treatment) +20


#the plot
plot(jitter(mds1$points[,1], amount=0.15), jitter(mds1$points[,2], amount=0.15), pch=symb,
     col="black", bg=cols, cex=1, xlab="", ylab="NMDS 2", xlim=c(-2.1, 3))  
ordiellipse(mds1, expl$Treatment, display = "sites", kind = "sd", label = F, lwd=1.2,
            col = pal.r)
legend('bottomright', legend = unique(expl$Treatment), cex = .8, pch = unique(symb), pt.bg = pal.r)
text(x=-2.5, y=2.5, "(A)", xpd=NA)


#plot color-coded by forest type (Fig. 2b)
expl$Habitat.Type <- droplevels(expl$Habitat.Type)
cols <- as.numeric(expl$Habitat.Type)
cols[which(cols==1)] <- pal[1]
cols[which(cols=="2")] <- pal[5]

symb <- as.numeric(expl$Habitat.Type) +20


plot(jitter(mds1$points[,1], amount=0.1), jitter(mds1$points[,2], amount=0.1), pch=symb,
     bg=cols, cex=1, xlab="NMDS 1", ylab="NMDS 2", xlim=c(-2.1, 3))  
ordiellipse(mds1, expl$Habitat.Type, display = "sites", kind = "sd", label = F, lwd=1.2,
            col = unique(cols))
legend('bottomright', legend = unique(expl$Habitat.Type), cex = .8, pch = unique(symb), pt.bg = unique(cols))
text(x=-2.5, y=2.5, "(B)", xpd=NA)



dev.off()







#### Q2: Differences in recruitment to seed states ####

###first looking at whether total ant recruitment differs among treatments

hist(ants2$No.Ants)  #should be poisson

m1 <- glmer(No.Ants ~ Treatment * Habitat.Type + (1|Trial), family=poisson, data=ants2)
#throwing errors...I think this is because there are no datapoints for feces samples that have
##ants in the edge sites
m2 <- glmer(No.Ants ~ Treatment + Habitat.Type + (1|Trial), family=poisson, data=ants2)
anova(m1, m2) #strong interaction but I don't this is real due to errors and no apparent interaction in plots

#test for interaction without feces data
m1b <- glmer(No.Ants ~ Treatment * Habitat.Type + (1|Trial), family=poisson, data=ants2[which(ants2$Treatment!="feces"),])
m2b <- glmer(No.Ants ~ Treatment + Habitat.Type + (1|Trial), family=poisson, data=ants2[which(ants2$Treatment!="feces"),])
anova(m1b, m2b)  ##no interaction here. I think we will stick with the no-interaction model 2


m3 <- glmer(No.Ants ~ Treatment + (1|Trial), family=poisson, data=ants2)
m4 <- glmer(No.Ants ~ Habitat.Type + (1|Trial), family=poisson, data=ants2)
m5 <- glmer(No.Ants ~ (1|Trial), family=poisson, data=ants2)

anova(m1b, m2b)  #no interaction between habitat x tx
anova(m2, m3) #no effect of habitat type
anova(m3, m5)  ##strong effect of treatment
tuk1 = (glht(m3, linfct=mcp(Treatment="Tukey")))
cld(tuk1, level = 0.05)



###Now looking at effects for individual ant species, we can Ectatomma, Pheidole, and Trachymyrmex

#first checking ant counts by species
ants12 <- ants2 
ants12$Pheidole <- rowSums(ants12[,23:32], na.rm=TRUE)
ants12 <- ants12[,-c(23:32)]
ants12$Ectatomma <- rowSums(ants12[,18:21], na.rm=TRUE)
ants12 <- ants12[,-c(18:21)]

#For each dataset, will remove trials where there were no ants of that species
trials <- ants12[,c(1,19,21,23,24)] %>%
  group_by(Trial) %>%
  summarise_all(sum, na.rm=TRUE)


#Ectatomma
t <- trials$Trial[which(trials$Ectatomma != 0)]
ants.E <- ants12[ants12$Trial %in% t,]

ants.E %>%
  group_by(Treatment) %>%
  summarise(sum = sum(Ectatomma))   

##no data for feces or seeds, will remove for analyses
ants.E <- filter(ants.E, Treatment!="feces" & Treatment!="seeds")


m.E.1 <- glmer(Ectatomma ~ Treatment + (1|Trial), family=poisson, data=ants.E)
m.E.2 <- glmer(Ectatomma ~ (1|Trial), family=poisson, data=ants.E)
anova(m.E.1, m.E.2)
tuk <- glht(m.E.1, linfct=mcp(Treatment="Tukey"))
summary(tuk)
cld(tuk, level = 0.05) 

#Pheidole
t <- trials$Trial[which(trials$Pheidole != 0)]
ants.P <- ants12[ants12$Trial %in% t,]

ants.P %>%
  group_by(Treatment) %>%
  summarise(sum = sum(Pheidole))  

m.P.1 <- glmer(Pheidole ~ Treatment + (1|Trial), family=poisson, data=ants.P)
m.P.2 <- glmer(Pheidole ~ (1|Trial), family=poisson, data=ants.P)
anova(m.P.1, m.P.2)
tuk <- glht(m.P.1, linfct=mcp(Treatment="Tukey"))
summary(tuk)
cld(tuk, level = 0.05) 



#Trachymyrmex
t <- trials$Trial[which(trials$`Trachymyrmex sp.` != 0)]
ants.T <- ants12[ants12$Trial %in% t,]
colnames(ants.T)[19] <- "Trachymyrmex"


ants.T %>%
  group_by(Treatment) %>%
  summarise(sum = sum(Trachymyrmex, na.rm=T)) 

ants.T$Trachymyrmex[is.na(ants.T$Trachymyrmex)] <- 0

m.T.1 <- glmer(Trachymyrmex ~ Treatment + (1|Trial), family=poisson, data=ants.T)
m.T.2 <- glmer(Trachymyrmex ~ (1|Trial), family=poisson, data=ants.T)
anova(m.T.1, m.T.2)
tuk <- glht(m.T.1, linfct=mcp(Treatment="Tukey"))
summary(tuk)
cld(tuk, level = 0.05) 

# now looking at fruit removal 

hist(ants2$Adj.Per.Rem)  #needs a transformation
hist(asin(sqrt(ants2$Adj.Per.Rem/100))) #better (arcsin transformation!)


ants2$Adj.Per.Rem.tr <- asin(sqrt(ants2$Adj.Per.Rem/100))

m.R.1 <- lmer(Adj.Per.Rem.tr ~ Treatment * Habitat.Type + (1|Trial), data=ants2)
m.R.2 <- lmer(Adj.Per.Rem.tr ~ Treatment + Habitat.Type + (1|Trial), data=ants2)
m.R.3 <- lmer(Adj.Per.Rem.tr ~ Treatment + (1|Trial), data=ants2)
m.R.4 <- lmer(Adj.Per.Rem.tr ~ Habitat.Type + (1|Trial), data=ants2)
m.R.5 <- lmer(Adj.Per.Rem.tr ~ (1|Trial), data=ants2)

anova(m.R.1, m.R.2) #no interaction
anova(m.R.2, m.R.3) #no effect of habitat
anova(m.R.3, m.R.5) #strong effect of treatment

tuk <- glht(m.R.3, linfct=mcp(Treatment="Tukey"))
summary(tuk)
cld(tuk, level = 0.05) 


#### Visualization ####
# Basic Plot (no ggplot)

op <- par(mfrow = c(2,2),
          oma = c(3,2,0,0) + 0.1,
          mar = c(2,2,2,2) + 0.1)
  
Treatment  = factor(ants2$Treatment, levels = c("unripe", "ripe", "overripe",
                                                "feces", "seeds" ))
No.Ants = ants2$No.Ants
plot(No.Ants ~ Treatment, 
     main = "(A)",
     ylab =  "",
     xlab = "",
     xlim = c(0.1,5.6),
     ylim = c(0,20),
     col = (c("#FEBC38", "#23CE6B", "#697F98", "#DA5526", "#F6893D" )))
   

antect = ants.E$Ectatomma
treatment1 =  factor(ants.E$Treatment, levels = c("unripe", "ripe", "overripe",
                                                             "feces", "seeds" ))
plot(antect ~ treatment1,
     main = "(B)",
     ylab =  "",
     xlab = "",
     xlim = c(0.1,5.6),
     ylim = c(0,20),
     col = (c("#FEBC38", "#23CE6B", "#697F98", "#DA5526", "#F6893D")))

antphi = ants.P$Pheidole
treatment2 =  factor(ants.P$Treatment, levels = c("unripe", "ripe", "overripe",
                                                  "feces", "seeds" ))
plot(antphi ~ treatment2, main = "(C)",
     ylab = "",
     xlab = "",
     xlim = c(0.1,5.6),
     ylim = c(0,20),
     col = (c("#FEBC38", "#23CE6B", "#697F98", "#DA5526", "#F6893D")))
anttra = ants.T$Trachymyrmex
treatment3 = factor(ants.T$Treatment, levels = c("unripe", "ripe", "overripe",
                                                 "feces", "seeds" ))

plot(anttra ~ treatment3, main = "(D)", 
     xlab = "",
     ylab = "",
     xlim = c(0.1,5.6),
     ylim = c(0,20),
     col = (c("#FEBC38", "#23CE6B", "#697F98", "#DA5526", "#F6893D")))

title(xlab = "Seed/Fruit State",
      ylab = "No. of Ants",
      outer = TRUE, line = 1)
dev.off()

adjperrem = ants2$Adj.Per.Rem
plot(adjperrem ~ Treatment,
     main = "Percent Removal of Seed/Fruit States",
     ylab = "% Mass Removed by Ants",
     xlab = "Fruit/Seed State",
     col = (c("#FEBC38", "#23CE6B", "#697F98", "#DA5526", "#F6893D")))




# ggplots
ants3 <- data.frame(ants2$Treatment, ants2$No.Ants, ants12$Ectatomma, 
                    ants12$Pheidole, ants12$`Trachymyrmex sp.`)
names(ants3) <- c("Treatment", "All", "Ect", "Phe", "Tra")
library(reshape2)
ants3a <- melt(ants3, id.var = "Treatment")
ants3b <- na.omit(ants3a)


p2 <- ggplot(data = ants3b, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=Treatment))
p2 + facet_wrap( ~ variable, scales="free")


ap <- ggplot(ants2, aes(x = Treatment, y = No.Ants, fill = Treatment)) + geom_boxplot() +
  guides(fill = FALSE)
ep <- ggplot(ants.E, aes(x = Treatment, y = No.Ants, fill = Treatment)) + geom_boxplot() +
  guides(fill = FALSE)
pp <- ggplot(ants.P, aes(x = Treatment, y = No.Ants, fill = Treatment)) + geom_boxplot() +
  guides(fill = FALSE)
tp <- ggplot(ants.T, aes(x = Treatment, y = No.Ants, fill = Treatment)) + geom_boxplot() +
  guides(fill = FALSE)

qap <- ap + scale_fill_manual(values=c("#DA5526", "#697F98", "#23CE6B", "#F6893D", "#FEBC38"))
qep <- ep + scale_fill_manual(values=c("#DA5526", "#697F98", "#23CE6B", "#F6893D", "#FEBC38"))
qpp <- pp + scale_fill_manual(values=c("#DA5526", "#697F98", "#23CE6B", "#F6893D", "#FEBC38"))
qtp <- tp + scale_fill_manual(values=c("#DA5526", "#697F98", "#23CE6B", "#F6893D", "#FEBC38"))

grid.arrange(
  grobs = gl,
  widths = c(2, 1, 1),
  layout_matrix = rbind(c(1, 2),
                        c(3, 4))
)

### ------------------------------ PROTOTYPE DO NOT USE!!!!!! --------------------------------

#OLD CODE

#will need to get rid of unidentified ants
ants3=ants2[, -c(26, 34, 36)]
ants3$total <- rowSums(ants3[,16:33], na.rm=TRUE)

###will need to get rid of lines with no ants observed
ants4 <- ants3[which(ants3$total !=0),]

#Get matrices for NMDS
sp <- ants4[,16:33]
for(i in 1:ncol(sp) ){
  sp[,i][which(is.na(sp[,i]))] <- 0
}
expl <- ants4[,c(2, 15)]

#run the NMDS
mds1 <- metaMDS(sp, k=2)  #run the NMDS
mds1 #print the NMDS results
cor(vegdist(sp), dist(mds1$points))^2  #calculate R^2 for NMDS

#plot
plot(jitter(mds1$points[,1], amount=0.1), jitter(mds1$points[,2], amount=0.1), pch=as.numeric(expl$Treatment),
     col=as.numeric(expl$Treatment))  
text(mds1, cex = 0.8, col = "darkcyan")
legend('topright', legend = unique(expl$Treatment), cex = .95, 
       pch = unique(as.numeric(expl$Treatment)),
       col = unique(as.numeric(expl$Treatment)))
ordiellipse(mds1, expl$Treatment, display = "sites", kind = "sd", label = F, lwd=2,
            col = unique(as.numeric(expl$Treatment)))

##Looks super odd with some major outliers and we are getting some warnings. I think we have too many 
#species that are rare and random probably better to simplify some by grouping the 
#Pheidole at least and maybe the Ectatomma


ants5 <- ants2 
ants5$Pheidole <- rowSums(ants5[,23:32], na.rm=TRUE)
ants5 <- ants5[,-c(23:32)]
ants5$total <- rowSums(ants5[,c(16:25, 27)], na.rm=TRUE)

###will need to get rid of lines with no ants observed
ants5 <- ants5[which(ants5$total !=0),]

#the matrices for the NMDS
sp <- ants5[,c(16:25, 27)]
for(i in 1:ncol(sp) ){
  sp[,i][which(is.na(sp[,i]))] <- 0
}
expl <- ants5[,c(2, 15)]


#The NMDS
mds1 <- metaMDS(sp, autotransform = FALSE, k=2, trymax=200)  #run the NMDS
mds1 #print the NMDS results
cor(vegdist(sp), dist(mds1$points))^2  #calculate R^2 for NMDS


#get colors
plot(clpalettes('top'))

pal <- swatch(clpalette('1930'))[[1]]
pal[4] <- 'gold'
pal[2] <- 'turquoise4'
cols <- as.numeric(expl$Treatment)
cols[which(cols==2)] <- pal[1]
cols[which(cols=="3")] <- pal[5]
cols[which(cols=="4")] <- pal[4]
cols[which(cols=="5")] <- pal[2]
cols[which(cols=="6")] <- pal[3]

pal.r <- c(pal[5], pal[4], pal[3], pal[2], pal[1])
symb <- as.numeric(expl$Treatment) +19

#plot
plot(jitter(mds1$points[,1], amount=0.1), jitter(mds1$points[,2], amount=0.1), pch=symb,
     col="black", bg=cols, cex=1.2)  
legend('topleft', legend = unique(expl$Treatment), cex = .95, pch = unique(symb), pt.bg = pal.r)
#text(mds1, cex = 0.8, col = "darkcyan")
ordiellipse(mds1, expl$Treatment, display = "sites", kind = "sd", label = F, lwd=2,
            col = pal.r)



#can also try plot based on forest type
plot(jitter(mds1$points[,1], amount=0.1), jitter(mds1$points[,2], amount=0.1), pch=as.numeric(expl$Habitat.Type),
     col=as.numeric(expl$Habitat.Type))  
legend('topright', legend = unique(expl$Habitat.Type), cex = .95, 
       pch = unique(as.numeric(expl$Habitat.Type)),
       col = unique(as.numeric(expl$Habitat.Type)))
ordiellipse(mds1, expl$Habitat.Type, display = "sites", kind = "sd", label = F, lwd=2,
            col = unique(as.numeric(expl$Habitat.Type)))




###Will try one more with Ectatomma merged also

ants6 <- ants2 
ants6$Pheidole <- rowSums(ants6[,23:32], na.rm=TRUE)
ants6 <- ants6[,-c(23:32)]
ants6$Ectatomma <- rowSums(ants6[,18:21], na.rm=TRUE)
ants6 <- ants6[,-c(18:21)]

#remove the unidentified
ants6 <- ants6[, -20]

ants6$total <- rowSums(ants6[,c(16:20, 22:23)], na.rm=TRUE)

###will need to get rid of lines with no ants observed
ants6 <- ants6[which(ants6$total !=0),]

#the matrices for the NMDS
sp <- ants6[,c(16:20, 22:23)]
for(i in 1:ncol(sp) ){
  sp[,i][which(is.na(sp[,i]))] <- 0
}
expl <- ants6[,c(2, 15)]


#The NMDS
mds1 <- metaMDS(sp, k=4)  #run the NMDS
mds1 #print the NMDS results
cor(vegdist(sp), dist(mds1$points))^2  #calculate R^2 for NMDS

#plot
plot(jitter(mds1$points[,1], amount=0.1), jitter(mds1$points[,2], amount=0.1), pch=as.numeric(expl$Treatment),
     col=as.numeric(expl$Treatment))  
legend('topright', legend = unique(expl$Treatment), cex = .95, 
       pch = unique(as.numeric(expl$Treatment)),
       col = unique(as.numeric(expl$Treatment)))
ordiellipse(mds1, expl$Treatment, display = "sites", kind = "sd", label = F, lwd=2,
            col = unique(as.numeric(expl$Treatment)))





###and finally one more but without Atta (only one instance)

ants7 <- ants6 
ants7 <- ants7[,-c(16:18)]
ants7$total <- rowSums(ants7[,c(16:17, 19:20)], na.rm=TRUE)

###will need to get rid of lines with no ants observed
ants7 <- ants7[which(ants7$total !=0),]

#the matrices for the NMDS
sp <- ants7[,c(16:17, 19:20)]
for(i in 1:ncol(sp) ){
  sp[,i][which(is.na(sp[,i]))] <- 0
}
expl <- ants6[,c(2, 15)]


#The NMDS
mds1 <- metaMDS(sp, k=2)  #run the NMDS
mds1 #print the NMDS results
cor(vegdist(sp), dist(mds1$points))^2  #calculate R^2 for NMDS

#plot
plot(jitter(mds1$points[,1], amount=0.1), jitter(mds1$points[,2], amount=0.1), pch=as.numeric(expl$Treatment),
     col=as.numeric(expl$Treatment))  
legend('topright', legend = unique(expl$Treatment), cex = .95, 
       pch = unique(as.numeric(expl$Treatment)),
       col = unique(as.numeric(expl$Treatment)))
ordihull(mds1, expl$Treatment, display = "sites", kind = "sd", label = F, lwd=2,
         col = unique(as.numeric(expl$Treatment)))

##Can't get the ellipses to print here for some reaon (maybe not enough sample size for some factor levels?
##but anyway it seems obvious there is no clear separation)





####Finally, one more but with all uncommon ants grouped as "Other"

ants9 <- ants2
ants9$Pheidole <- rowSums(ants9[,23:32], na.rm=TRUE)
ants9 <- ants9[,-c(23:32)]
ants9$Ectatomma <- rowSums(ants9[,18:21], na.rm=TRUE)
ants9 <- ants9[,-c(18:21)]
ants9$Other <- rowSums(ants9[,c(16:18, 20)], na.rm=TRUE)
ants9 <- ants9[,-c(16:18, 20)]



###will need to get rid of lines with no ants observed
ants9$total <- rowSums(ants9[,c(16:17, 19:21)], na.rm=TRUE)
ants9 <- ants9[which(ants9$total !=0),]

#the matrices for the NMDS
sp <- ants9[,c(16:17, 19:21)]
for(i in 1:ncol(sp) ){
  sp[,i][which(is.na(sp[,i]))] <- 0
}
expl <- ants9[,c(2, 15)]


#The NMDS
mds1 <- metaMDS(sp, k=2, trymax=200, autotransform=T)  #run the NMDS
mds1 #print the NMDS results
cor(vegdist(sp), dist(mds1$points))^2  #calculate R^2 for NMDS

#plot
plot(jitter(mds1$points[,1], amount=0.1), jitter(mds1$points[,2], amount=0.1), pch=as.numeric(expl$Treatment),
     col=as.numeric(expl$Treatment))  
legend('topright', legend = unique(expl$Treatment), cex = .95, 
       pch = unique(as.numeric(expl$Treatment)),
       col = unique(as.numeric(expl$Treatment)))
ordiellipse(mds1, expl$Treatment, display = "sites", kind = "sd", label = F, lwd=2,
            col = unique(as.numeric(expl$Treatment)))




#####Okay, one more! Trying to group everything that occurs in fewer than 5 samples


ants10 <- ants2
ants10$Pheidole.spp <- rowSums(ants10[,25:32], na.rm=TRUE)
ants10 <- ants10[,-c(25:32)]
ants10$Ectatomma.spp <- rowSums(ants10[,c(18,21)], na.rm=TRUE)
ants10 <- ants10[,-c(18,21)]
ants10$Other <- rowSums(ants10[,c(16,17, 20, 24)], na.rm=TRUE)
ants10 <- ants10[,-c(16,17, 20, 24)]



###will need to get rid of lines with no ants observed
ants10$total <- rowSums(ants10[,c(16:21, 23:25)], na.rm=TRUE)
ants10 <- ants10[which(ants10$total !=0),]

##and replace na with 0
ants10[is.na(ants10)] <- 0


#the matrices for the NMDS
sp <- ants10[,c(16:21, 23:25)]
expl <- ants10[,c(2, 15)]


#The NMDS
mds1 <- metaMDS(sp, k=2, trymax=200, autotransform=T)  #run the NMDS
mds1 #print the NMDS results
cor(vegdist(sp), dist(mds1$points))^2  #calculate R^2 for NMDS

#get colors
#plot(clpalettes('top'))

pal <- swatch(clpalette('1930'))[[1]]
pal[4] <- 'gold'
pal[2] <- 'turquoise4'
cols <- as.numeric(expl$Treatment)
cols[which(cols==2)] <- pal[1]
cols[which(cols=="3")] <- pal[5]
cols[which(cols=="4")] <- pal[4]
cols[which(cols=="5")] <- pal[2]
cols[which(cols=="6")] <- pal[3]

pal.r <- c(pal[5], pal[4], pal[3], pal[2], pal[1])
symb <- as.numeric(expl$Treatment) +19

#plot
plot(jitter(mds1$points[,1], amount=0.1), jitter(mds1$points[,2], amount=0.1), pch=symb,
     col="black", bg=cols, cex=1.2, xlab="NMDS 1", ylab="NMDS 2")  
legend('bottomright', legend = unique(expl$Treatment), cex = .95, pch = unique(symb), pt.bg = pal.r)
#text(mds1, cex = 0.8, col = "darkcyan")
ordiellipse(mds1, expl$Treatment, display = "sites", kind = "sd", label = F, lwd=2,
            col = pal.r)





#can also try plot based on forest type

cols <- as.numeric(expl$Habitat.Type)
cols[which(cols==2)] <- pal[2]
cols[which(cols=="3")] <- pal[4]

symb <- as.numeric(expl$Habitat.Type) +19


plot(jitter(mds1$points[,1], amount=0.1), jitter(mds1$points[,2], amount=0.1), pch=symb,
     bg=cols, cex=1.2)  
legend('bottomright', legend = unique(expl$Habitat.Type), cex = .95, pch = unique(symb), pt.bg = unique(cols))
ordiellipse(mds1, expl$Habitat.Type, display = "sites", kind = "sd", label = F, lwd=2,
            col = unique(cols))




#####Okay, this could be the real last one! Working with presence absence data.

#the matrices for the NMDS
sp <- ants10[,c(16:21, 23:25)]
sp[is.na(sp)] <- 0
sp[sp > 0] <- 1
expl <- ants10[,c(2, 15)]


#The NMDS
mds1 <- metaMDS(sp, k=2, trymax=200, autotransform=T)  #run the NMDS
mds1 #print the NMDS results
cor(vegdist(sp), dist(mds1$points))^2  #calculate R^2 for NMDS

#get colors
#plot(clpalettes('top'))

pal <- swatch(clpalette('1930'))[[1]]
pal[4] <- 'gold'
pal[2] <- 'turquoise4'
cols <- as.numeric(expl$Treatment)
cols[which(cols==2)] <- pal[1]
cols[which(cols=="3")] <- pal[5]
cols[which(cols=="4")] <- pal[4]
cols[which(cols=="5")] <- pal[2]
cols[which(cols=="6")] <- pal[3]

pal.r <- c(pal[5], pal[4], pal[3], pal[2], pal[1])
symb <- as.numeric(expl$Treatment) +19

#plot
plot(jitter(mds1$points[,1], amount=0.1), jitter(mds1$points[,2], amount=0.1), pch=symb,
     col="black", bg=cols, cex=1.2)  
legend('bottomright', legend = unique(expl$Treatment), cex = .95, pch = unique(symb), pt.bg = pal.r)
#text(mds1, cex = 0.8, col = "darkcyan")
ordiellipse(mds1, expl$Treatment, display = "sites", kind = "sd", label = F, lwd=2,
            col = pal.r)





#can also try plot based on forest type

cols <- as.numeric(expl$Habitat.Type)
cols[which(cols==2)] <- pal[2]
cols[which(cols=="3")] <- pal[4]

symb <- as.numeric(expl$Habitat.Type) +19


plot(jitter(mds1$points[,1], amount=0.1), jitter(mds1$points[,2], amount=0.1), pch=symb,
     bg=cols, cex=1.2)  
legend('bottomright', legend = unique(expl$Habitat.Type), cex = .95, pch = unique(symb), pt.bg = unique(cols))
ordiellipse(mds1, expl$Habitat.Type, display = "sites", kind = "sd", label = F, lwd=2,
            col = unique(cols))



## Q2

###checking whether total ant recruitment differs among treatments

##will go back to original data because we want to have the 0s in here

hist(ants2$No.Ants)  #should be poisson



m1 <- glmer(No.Ants ~ Treatment * Habitat.Type + (1|Trial), family=poisson, data=ants2)
#throwing errors...I think this is because there are no datapoints for feces samples that have
##ants in the edge sites
m2 <- glmer(No.Ants ~ Treatment + Habitat.Type + (1|Trial), family=poisson, data=ants2)
m3 <- glmer(No.Ants ~ Treatment + (1|Trial), family=poisson, data=ants2)
m4 <- glmer(No.Ants ~ Habitat.Type + (1|Trial), family=poisson, data=ants2)
m5 <- glmer(No.Ants ~ (1|Trial), family=poisson, data=ants2)

anova(m1b, m2b)  #interaction between habitat x tx--not sure on this since there are the errors
anova(m2, m3) #no effect of habitat type
anova(m2, m4) #strong effect of treatment
anova(m3, m5)  ##strong effect of treatment, use this p-value!
summary(glht(m3, linfct=mcp(Treatment="Tukey")))
plot(No.Ants~Treatment, data=ants2)


#test for interaction without feces data
m1b <- glmer(No.Ants ~ Treatment * Habitat.Type + (1|Trial), family=poisson, data=ants2[which(ants2$Treatment!="feces"),])
m2b <- glmer(No.Ants ~ Treatment + Habitat.Type + (1|Trial), family=poisson, data=ants2[which(ants2$Treatment!="feces"),])
anova(m1b, m2b)  ##no interaction here. I think we will stick with the no-interaction model 2



##Splitting data by habitat type
ants.e <- ants2[which(ants2$Habitat.Type=="Forest Edge"),]
ants.s <- ants2[which(ants2$Habitat.Type=="Secondary Forest"),]


m.e.1 <- glmer(No.Ants ~ Treatment + (1|Trial), family=poisson, data=ants.e)
m.e.2 <- glmer(No.Ants ~ (1|Trial), family=poisson, data=ants.e)
anova(m.e.1, m.e.2)
summary(glht(m.e.1, linfct=mcp(Treatment="Tukey")))

m.s.1 <- glmer(No.Ants ~ Treatment + (1|Trial), family=poisson, data=ants.s)
m.s.2 <- glmer(No.Ants ~ (1|Trial), family=poisson, data=ants.s)
anova(m.s.1, m.s.2)
summary(glht(m.s.1, linfct=mcp(Treatment="Tukey")))

plot(No.Ants ~ Treatment, data=ants.e)
plot(No.Ants ~ Treatment, data=ants.s)




###Now checking ant counts by species


ants12 <- ants2 
ants12$Pheidole <- rowSums(ants12[,23:32], na.rm=TRUE)
ants12 <- ants12[,-c(23:32)]
ants12$Ectatomma <- rowSums(ants12[,18:21], na.rm=TRUE)
ants12 <- ants12[,-c(18:21)]

#For each dataset, will remove trials where there were no ants of that species
trials <- ants12[,c(1,19,21,23,24)] %>%
  group_by(Trial) %>%
  summarise_all(sum, na.rm=TRUE)


#Ectatomma
t <- trials$Trial[which(trials$Ectatomma != 0)]
ants.E <- ants12[ants12$Trial %in% t,]

ants.E %>%
  group_by(Treatment) %>%
  summarise(sum = sum(Ectatomma))   

##no data for feces or seeds, will remove for analyses
ants.E <- filter(ants.E, Treatment!="feces" & Treatment!="seeds")


m.E.1 <- glmer(Ectatomma ~ Treatment + (1|Trial), family=poisson, data=ants.E)
m.E.2 <- glmer(Ectatomma ~ (1|Trial), family=poisson, data=ants.E)
anova(m.E.1, m.E.2)
summary(glht(m.E.1, linfct=mcp(Treatment="Tukey")))
plot(Ectatomma ~ Treatment, data=ants.E)

#Pheidole
t <- trials$Trial[which(trials$Pheidole != 0)]
ants.P <- ants12[ants12$Trial %in% t,]

ants.P %>%
  group_by(Treatment) %>%
  summarise(sum = sum(Pheidole))  

m.P.1 <- glmer(Pheidole ~ Treatment + (1|Trial), family=poisson, data=ants.P)
m.P.2 <- glmer(Pheidole ~ (1|Trial), family=poisson, data=ants.P)
anova(m.P.1, m.P.2)
summary(glht(m.P.1, linfct=mcp(Treatment="Tukey")))
plot(Pheidole ~ Treatment, data=ants.P)


#Trachymyrmex
t <- trials$Trial[which(trials$`Trachymyrmex sp.` != 0)]
ants.T <- ants12[ants12$Trial %in% t,]
colnames(ants.T)[19] <- "Trachymyrmex"


ants.T %>%
  group_by(Treatment) %>%
  summarise(sum = sum(Trachymyrmex, na.rm=T)) 

ants.T$Trachymyrmex[is.na(ants.T$Trachymyrmex)] <- 0

m.T.1 <- glmer(Trachymyrmex ~ Treatment + (1|Trial), family=poisson, data=ants.T)
m.T.2 <- glmer(Trachymyrmex ~ (1|Trial), family=poisson, data=ants.T)
anova(m.T.1, m.T.2)
summary(glht(m.T.1, linfct=mcp(Treatment="Tukey")))
plot(Trachymyrmex ~ Treatment, data=ants.T)


#Wasmannia
t <- trials$Trial[which(trials$`Wasmannia auropunctata` != 0)]
ants.W <- ants8[ants8$Trial %in% t,]

colnames(ants.W)[21] <- "W.a"
m.W.1 <- glmer(W.a ~ Treatment + (1|Trial), family=poisson, data=ants.W)
m.W.2 <- glmer(W.a ~ (1|Trial), family=poisson, data=ants.W)
anova(m.W.1, m.W.2)
plot(W.a ~ Treatment, data=ants.W)

#Only 4 trials for Wasmania, probably should leave out. 



# Looking at fruit removal 

hist(ants2$Adj.Per.Rem)
hist(asin(sqrt(ants2$Adj.Per.Rem/100)))


ants2$Adj.Per.Rem.tr <- asin(sqrt(ants2$Adj.Per.Rem/100))

m.R.1 <- lmer(Adj.Per.Rem.tr ~ Treatment * Habitat.Type + (1|Trial), data=ants2)
m.R.2 <- lmer(Adj.Per.Rem.tr ~ Treatment + Habitat.Type + (1|Trial), data=ants2)
m.R.3 <- lmer(Adj.Per.Rem.tr ~ Treatment + (1|Trial), data=ants2)
m.R.4 <- lmer(Adj.Per.Rem.tr ~ Habitat.Type + (1|Trial), data=ants2)
m.R.5 <- lmer(Adj.Per.Rem.tr ~ (1|Trial), data=ants2)

anova(m.R.1, m.R.2)
anova(m.R.2, m.R.3)
anova(m.R.2, m.R.4) #strong effect of treatment
anova(m.R.3, m.R.5)

summary(glht(m.R.3, linfct=mcp(Treatment="Tukey")))

plot(Adj.Per.Rem ~ Treatment, data=ants2)

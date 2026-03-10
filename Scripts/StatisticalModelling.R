#This script uses the phytoplankton_phenology_clean and zooplankton_phenology_clean data frames created in the phenology scripts.
# It performs statistical modeling to analyze the relationships between phytoplankton and zooplankton phenology, as well as their responses to environmental variables. 
#The script includes linear regression models, generalized additive models (GAMs), and mixed-effects models to account for potential random effects due to spatial or temporal variability. 
#The results of the models are visualized using ggplot2, and the script also includes diagnostic plots to assess model fit and assumptions.

# Importing datasets & libraries ----
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(lme4)

norm_chl <- read_csv("Data/norm_chl.csv") #phyto
zoop_daily <- read_csv("Data/zoop_daily.csv") #zooplankton
temperature <- read_csv("Data/BATS_temp_FINAL.csv") #SST
phytoplankton_phenology_clean <- read_csv("Data/phytoplankton_phenology_clean.csv") #phytoplankton phenology indices
zooplankton_phenology_clean <- read_csv("Data/zooplankton_phenology_clean.csv") #zooplankton phenology indices


################# ############# ############# ############# ############# ############# ############# ############
########################## ############# ######  Statistical Analyses -----############# #################### 

# Model 1: Temporal Trends in Phenology Indices ----
summary(glm(BloomStartDay~Year, data=phytoplankton_phenology_clean))
summary(glm(BloomPeakDay~Year,family=Gamma, data=phytoplankton_phenology_clean))
summary(glm(BloomDuration~Year, data=phytoplankton_phenology_clean))
summary(glm(BloomMagnitude~Year,data=phytoplankton_phenology_clean))
summary(glm(BloomAmplitude~Year,data=phytoplankton_phenology_clean))

#shapiro for normality but n<30 
#shapiro.test(lm(BloomStartDay~Year, data=phytoplankton_phenology_clean)$residuals)
#shapiro.test(lm(BloomPeakDay~Year, data=phytoplankton_phenology_clean)$residuals) # non normal
#shapiro.test(lm(BloomDuration~Year, data=phytoplankton_phenology_clean)$residuals)
#t.test(phytoplankton_phenology_clean$BloomStartDay, mu=mean(phytoplankton_phenology_clean$BloomStartDay))

summary(glm(BloomStartDay~Year, data=zooplankton_phenology_clean))
summary(glm(BloomPeakDay~Year,data=zooplankton_phenology_clean))
summary(glm(BloomDuration~Year, data=zooplankton_phenology_clean))
summary(glm(BloomMagnitude~Year,data=zooplankton_phenology_clean))
summary(glm(BloomAmplitude~Year,data=zooplankton_phenology_clean))

#shapiro.test(lm(BloomStartDay~Year, data=zooplankton_phenology_clean)$residuals)
#shapiro.test(lm(BloomPeakDay~Year, data=zooplankton_phenology_clean)$residuals) # non normal
#shapiro.test(lm(BloomDuration~Year, data=zooplankton_phenology_clean)$residuals) 


# Model 2: Temporal Trends in Environmental Drivers ----
summary(lm(MeanTemp~Year, data=zooplankton_phenology_clean))
summary(lm(MeanStratification~Year, data=zooplankton_phenology_clean))

plot(lm(MeanStratification~MeanTemp, data=zooplankton_phenology_clean))
AIC(lm(MeanTemp~MeanStratification, data=zooplankton_phenology_clean))
AIC(lm(MeanTemp~Year+MeanStratification, data=zooplankton_phenology_clean)) #testing if temp changes over time and whether strat is associated with temp i.e., if MeanTemp is explained by Year and MeanStratification.
AIC(lm(MeanStratification~Year+MeanTemp, data=zooplankton_phenology_clean)) #testing if temp changes over time and whether strat is associated with temp i.e., if MeanTemp is explained by Year and MeanStratification.

cor(zooplankton_phenology_clean$MeanTemp, zooplankton_phenology_clean$MeanStratification) #checking correlation between temp and stratification to check for multicollinearity before including both in the same model.

#mann kendall test for temp and stratification trends over time - wont include this i dont think
#mk.test(zooplankton_phenology_clean$MeanTemp)
#mk.test(zooplankton_phenology_clean$MeanStratification)

#checking plot of stratification over time
ggplot(zooplankton_phenology_clean, aes(x=Year, y=MeanStratification)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Mean Stratification over Time", x="Year", y="Mean Stratification") +
  theme_classic()

# Model 3: Bloom Characteristic and Environment - Direct Effects  ----

### 3.1 - Phenology & Stratification ----

##### Bloom Start - P 
AIC(glm(BloomStartDay~MeanStratification, data=phytoplankton_phenology_clean))
AIC(glm(BloomStartDay~MeanStratification+Year, data=phytoplankton_phenology_clean)) 
summary(glm(BloomStartDay~MeanStratification*Year, data=phytoplankton_phenology_clean))# best AIC

##### Bloom Peak - P 
AIC(glm(BloomPeakDay~MeanStratification, data=phytoplankton_phenology_clean))
AIC(glm(BloomPeakDay~MeanStratification+Year, data=phytoplankton_phenology_clean))
summary(glm(BloomPeakDay~MeanStratification*Year, data=phytoplankton_phenology_clean))# best AIC

##### Bloom Duration - P
AIC(glm(BloomDuration~MeanStratification, data=phytoplankton_phenology_clean))
summary(glm(BloomDuration~MeanStratification+Year, data=phytoplankton_phenology_clean)) #best AIC
AIC(glm(BloomDuration~MeanStratification*Year, data=phytoplankton_phenology_clean))

##### Bloom Magnitude - P
summary(glm(BloomMagnitude~MeanStratification, data=phytoplankton_phenology_clean)) #best AIC
AIC(glm(BloomMagnitude~MeanStratification+Year, data=phytoplankton_phenology_clean)) 
AIC(glm(BloomMagnitude~MeanStratification*Year, data=phytoplankton_phenology_clean))

##### Bloom Amplitude - P
AIC(glm(BloomAmplitude~MeanStratification, data=phytoplankton_phenology_clean)) 
AIC(glm(BloomAmplitude~MeanStratification+Year, data=phytoplankton_phenology_clean)) 
summary(glm(BloomAmplitude~MeanStratification*Year, data=phytoplankton_phenology_clean))#best AIC


#### Bloom Start- Z
AIC(glm(BloomStartDay~MeanStratification, data=zooplankton_phenology_clean))
AIC(glm(BloomStartDay~MeanStratification+Year, data=zooplankton_phenology_clean))
summary(glm(BloomStartDay~MeanStratification*Year, data=zooplankton_phenology_clean)) #best AIC

#### Bloom Peak - Z
summary(glm(BloomPeakDay~MeanStratification, data=zooplankton_phenology_clean)) #best AIC
AIC(glm(BloomPeakDay~MeanStratification+Year, data=zooplankton_phenology_clean))
AIC(glm(BloomPeakDay~MeanStratification*Year, data=zooplankton_phenology_clean))

#### Bloom Duration - Z
AIC(glm(BloomDuration~MeanStratification, data=zooplankton_phenology_clean))
summary(glm(BloomDuration~MeanStratification+Year, data=zooplankton_phenology_clean)) #best AIC
AIC(glm(BloomDuration~MeanStratification*Year, data=zooplankton_phenology_clean))

##### Bloom Magnitude - Z
summary(glm(BloomMagnitude~MeanStratification, data=zooplankton_phenology_clean))#best AIC
AIC(glm(BloomMagnitude~MeanStratification+Year, data=zooplankton_phenology_clean)) 
AIC(glm(BloomMagnitude~MeanStratification*Year, data=zooplankton_phenology_clean))

##### Bloom Amplitude - Z
summary(glm(BloomAmplitude~MeanStratification, data=zooplankton_phenology_clean)) #best AIC
AIC(glm(BloomAmplitude~MeanStratification+Year, data=zooplankton_phenology_clean)) 
AIC(glm(BloomAmplitude~MeanStratification*Year, data=zooplankton_phenology_clean))


### 3.2 - Phenology & Temperature ----

##### Bloom Start - P 
summary(glm(BloomStartDay~MeanTemp, data=phytoplankton_phenology_clean))# best AIC
AIC(glm(BloomStartDay~MeanTemp+Year, data=phytoplankton_phenology_clean)) 
AIC(glm(BloomStartDay~MeanTemp*Year, data=phytoplankton_phenology_clean))

##### Bloom Peak - P 
summary(glm(BloomPeakDay~MeanTemp, data=phytoplankton_phenology_clean))# best AIC
AIC(glm(BloomPeakDay~MeanTemp+Year, data=phytoplankton_phenology_clean))
AIC(glm(BloomPeakDay~MeanTemp*Year, data=phytoplankton_phenology_clean))

##### Bloom Duration - P
summary(glm(BloomDuration~MeanTemp, data=phytoplankton_phenology_clean))#best AIC
AIC(glm(BloomDuration~MeanTemp+Year, data=phytoplankton_phenology_clean)) 
AIC(glm(BloomDuration~MeanTemp*Year, data=phytoplankton_phenology_clean))

##### Bloom Magnitude - P
AIC(glm(BloomMagnitude~MeanTemp, data=phytoplankton_phenology_clean)) 
summary(glm(BloomMagnitude~MeanTemp+Year, data=phytoplankton_phenology_clean)) #best AIC
AIC(glm(BloomMagnitude~MeanTemp*Year, data=phytoplankton_phenology_clean))

##### Bloom Amplitude - P
summary(glm(BloomAmplitude~MeanTemp, data=phytoplankton_phenology_clean)) #best AIC
AIC(glm(BloomAmplitude~MeanTemp+Year, data=phytoplankton_phenology_clean)) 
AIC(glm(BloomAmplitude~MeanTemp*Year, data=phytoplankton_phenology_clean))


#### Bloom Start- Z
summary(glm(BloomStartDay~MeanTemp, data=zooplankton_phenology_clean))#best AIC
AIC(glm(BloomStartDay~MeanTemp+Year, data=zooplankton_phenology_clean))
AIC(glm(BloomStartDay~MeanTemp*Year, data=zooplankton_phenology_clean)) 

#### Bloom Peak - Z
summary(glm(BloomPeakDay~MeanTemp, data=zooplankton_phenology_clean)) #best AIC
AIC(glm(BloomPeakDay~MeanTemp+Year, data=zooplankton_phenology_clean))
AIC(glm(BloomPeakDay~MeanTemp*Year, data=zooplankton_phenology_clean))

#### Bloom Duration - Z
summary(glm(BloomDuration~MeanTemp, data=zooplankton_phenology_clean))#best AIC
AIC(glm(BloomDuration~MeanTemp+Year, data=zooplankton_phenology_clean)) 
AIC(glm(BloomDuration~MeanTemp*Year, data=zooplankton_phenology_clean))

##### Bloom Magnitude - Z
summary(glm(BloomMagnitude~MeanTemp, data=zooplankton_phenology_clean))#best AIC
AIC(glm(BloomMagnitude~MeanTemp+Year, data=zooplankton_phenology_clean)) 
AIC(glm(BloomMagnitude~MeanTemp*Year, data=zooplankton_phenology_clean))

##### Bloom Amplitude - Z
summary(glm(BloomAmplitude~MeanTemp, data=zooplankton_phenology_clean)) #best AIC
AIC(glm(BloomAmplitude~MeanTemp+Year, data=zooplankton_phenology_clean)) 
AIC(glm(BloomAmplitude~MeanTemp*Year, data=zooplankton_phenology_clean))



# Model 4: Bloom Characteristic and Environment - Combined Effects  ----
summary(glm(BloomStartDay~MeanTemp + MeanStratification, data=phytoplankton_phenology_clean))
summary(lm(BloomPeakDay~MeanTemp + MeanStratification, data=phytoplankton_phenology_clean))
summary(lm(BloomDuration~MeanTemp + MeanStratification, data=phytoplankton_phenology_clean))

summary(lm(BloomStartDay~MeanTemp + MeanStratification, data=zooplankton_phenology_clean))
summary(lm(BloomPeakDay~MeanTemp + MeanStratification, data=zooplankton_phenology_clean))
summary(lm(BloomDuration~MeanTemp + MeanStratification, data=zooplankton_phenology_clean))

### 4.1 - Adding year ----
summary(glm(BloomStartDay~MeanTemp + MeanStratification + Year, data=phytoplankton_phenology_clean))
summary(lm(BloomPeakDay~MeanTemp + MeanStratification + Year, data=phytoplankton_phenology_clean))
summary(lm(BloomDuration~MeanTemp + MeanStratification + Year, data=phytoplankton_phenology_clean))

summary(lm(BloomStartDay~MeanTemp + MeanStratification + Year, data=zooplankton_phenology_clean))
summary(lm(BloomPeakDay~MeanTemp + MeanStratification + Year, data=zooplankton_phenology_clean))
summary(lm(BloomDuration~MeanTemp + MeanStratification + Year, data=zooplankton_phenology_clean))



# Model 5: Bloom Characteristic and Environment - Interactive Effects  ----
### 5.1 - Testing whether the effect of temp depends on strat ----
summary(glm(BloomStartDay~MeanTemp*MeanStratification, data=phytoplankton_phenology_clean))
summary(lm(BloomPeakDay~MeanTemp*MeanStratification, data=phytoplankton_phenology_clean))
summary(lm(BloomDuration~MeanTemp*MeanStratification, data=phytoplankton_phenology_clean))

summary(lm(BloomStartDay~MeanTemp*MeanStratification, data=zooplankton_phenology_clean))
summary(lm(BloomPeakDay~MeanTemp*MeanStratification, data=zooplankton_phenology_clean))
summary(lm(BloomDuration~MeanTemp*MeanStratification, data=zooplankton_phenology_clean))


#glm
summary(glm(BloomStartDay~MeanTemp*MeanStratification, data=phytoplankton_phenology_clean))

###5.2 - Adding year ----
summary(glm(BloomStartDay~MeanTemp*MeanStratification + Year, data=phytoplankton_phenology_clean))
summary(lm(BloomPeakDay~MeanTemp*MeanStratification + Year, data=phytoplankton_phenology_clean))
summary(lm(BloomDuration~MeanTemp*MeanStratification + Year, data=phytoplankton_phenology_clean))

summary(lm(BloomStartDay~MeanTemp*MeanStratification + Year, data=zooplankton_phenology_clean))
summary(lm(BloomPeakDay~MeanTemp*MeanStratification + Year, data=zooplankton_phenology_clean))
summary(lm(BloomDuration~MeanTemp*MeanStratification + Year, data=zooplankton_phenology_clean))


## TESTING INDIVIDUAL MODELS FOR EACH INDICIE AND COMPARING AIC TO SELECT BEST MODEL ----
#### Bloom Start - P
AIC(glm(BloomStartDay~MeanStratification, data=phytoplankton_phenology_clean))
AIC(glm(BloomStartDay~MeanTemp, data=phytoplankton_phenology_clean))
AIC(glm(BloomStartDay~MeanStratification+MeanTemp, data=phytoplankton_phenology_clean))
AIC(glm(BloomStartDay~MeanStratification+MeanTemp + Year, data=phytoplankton_phenology_clean))

AIC(glm(BloomStartDay~MeanStratification:MeanTemp, data=phytoplankton_phenology_clean))
AIC(glm(BloomStartDay~MeanStratification*MeanTemp*Year, data=phytoplankton_phenology_clean))
AIC(glm(BloomStartDay~MeanStratification+MeanTemp*Year, data=phytoplankton_phenology_clean))

# final model
summary(glm(BloomStartDay~MeanStratification*MeanTemp*Year, data=phytoplankton_phenology_clean))

#### Bloom Peak - P
AIC(glm(BloomPeakDay~MeanStratification, data=phytoplankton_phenology_clean))
AIC(glm(BloomPeakDay~MeanTemp, data=phytoplankton_phenology_clean))
AIC(glm(BloomPeakDay~MeanStratification+MeanTemp, data=phytoplankton_phenology_clean))
AIC(glm(BloomPeakDay~MeanStratification+MeanTemp + Year, data=phytoplankton_phenology_clean))

AIC(glm(BloomPeakDay~MeanStratification:MeanTemp, data=phytoplankton_phenology_clean))
AIC(glm(BloomPeakDay~MeanStratification*MeanTemp*Year, data=phytoplankton_phenology_clean))
AIC(glm(BloomPeakDay~MeanStratification+MeanTemp*Year, data=phytoplankton_phenology_clean))

# final model
summary(glm(BloomPeakDay~MeanStratification*MeanTemp*Year, data=phytoplankton_phenology_clean))


#### Bloom Duration - P
AIC(glm(BloomDuration~MeanStratification, data=phytoplankton_phenology_clean))
AIC(glm(BloomDuration~MeanTemp, data=phytoplankton_phenology_clean))
AIC(glm(BloomDuration~MeanStratification+MeanTemp, data=phytoplankton_phenology_clean))
AIC(glm(BloomDuration~MeanStratification+MeanTemp + Year, data=phytoplankton_phenology_clean))

AIC(glm(BloomDuration~MeanStratification:MeanTemp, data=phytoplankton_phenology_clean))
AIC(glm(BloomDuration~MeanStratification*MeanTemp*Year, data=phytoplankton_phenology_clean))
AIC(glm(BloomDuration~MeanStratification+MeanTemp*Year, data=phytoplankton_phenology_clean))


#cor.test for stratification, temperature and year
cor.test(phytoplankton_phenology_clean$MeanStratification)
cor.test(phytoplankton_phenology_clean$MeanTemp, phytoplankton_phenology_clean$Year)


# Trying ANOVA to look at interannual variability ----
# looking at discrete comparisons between decades 

#PHYTOPLANKTON
# Making decades the categories 
phytoplankton_phenology_clean_ANOVA <- phytoplankton_phenology_clean %>%
  mutate(Decade = case_when(
    Year >= 1990 & Year < 2000 ~ "1990s",
    Year >= 2000 & Year < 2010 ~ "2000s",
    Year >= 2010 & Year < 2020 ~ "2010s",
    TRUE ~ NA_character_))

summary(aov(BloomStartDay ~ Decade, data=phytoplankton_phenology_clean_ANOVA))
summary(aov(BloomPeakDay ~ Decade, data=phytoplankton_phenology_clean_ANOVA))
summary(aov(BloomDuration ~ Decade, data=phytoplankton_phenology_clean_ANOVA))

ggplot(phytoplankton_phenology_clean_ANOVA, aes(x = Decade, y = BloomPeakDay)) +
  geom_boxplot(fill="cornflowerblue")+
  geom_jitter(width = 0.2, alpha = 0.5) +
   labs(title="Phytoplankton Bloom Peak Day by Decade", x="Decade", y="Bloom Peak Day") +
   theme_classic()
ggplot(phytoplankton_phenology_clean_ANOVA, aes(x = Decade, y = BloomStartDay)) +
  geom_boxplot(fill="cornflowerblue")+
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title="Phytoplankton Bloom Peak Day by Decade", x="Decade", y="Bloom Start Day") +
  theme_classic()
ggplot(phytoplankton_phenology_clean_ANOVA, aes(x = Decade, y = BloomDuration)) +
  geom_boxplot(fill="cornflowerblue")+
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title="Phytoplankton Bloom Peak Day by Decade", x="Decade", y="Bloom Duration") +
  theme_classic()

# ZOOPLANKTON 

zooplankton_phenology_clean_ANOVA <- zooplankton_phenology_clean %>%
  mutate(Decade = case_when(
    Year >= 1990 & Year < 2000 ~ "1990s",
    Year >= 2000 & Year < 2010 ~ "2000s",
    Year >= 2010 & Year < 2020 ~ "2010s",
    TRUE ~ NA_character_))

kruskal.test(BloomStartDay ~ Decade, data=phytoplankton_phenology_clean_ANOVA)
kruskal.test(BloomPeakDay ~ Decade, data=phytoplankton_phenology_clean_ANOVA)
kruskal.test(BloomDuration ~ Decade, data=phytoplankton_phenology_clean_ANOVA)

kruskal.test(BloomStartDay ~ Decade, data=zooplankton_phenology_clean_ANOVA)
kruskal.test(BloomPeakDay ~ Decade, data=zooplankton_phenology_clean_ANOVA)
kruskal.test(BloomDuration ~ Decade, data=zooplankton_phenology_clean_ANOVA)


#merge zooplankton_phenology_clean_ANOVA with phytoplankton_phenology_clean_ANOVA to make a combined dataset for boxplotting
phytoplankton_phenology_clean_ANOVA <- phytoplankton_phenology_clean_ANOVA %>%
  mutate(Taxon = "Phytoplankton")
zooplankton_phenology_clean_ANOVA <- zooplankton_phenology_clean_ANOVA %>%
  mutate(Taxon = "Zooplankton")
phenology_ANOVA_combined <- bind_rows(phytoplankton_phenology_clean_ANOVA, zooplankton_phenology_clean_ANOVA)

par(mfrow=c(1,3))

ggplot(phenology_ANOVA_combined, aes(x = Decade, y = BloomStartDay, fill=Taxon)) +
  geom_boxplot()+
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title="Bloom Start Day by Decade and Taxon", x="Decade", y="Bloom Start Day") +
  scale_fill_manual(values=c("Phytoplankton"= "cornflowerblue",
                             "Zooplankton"= "darkorchid")) +
  theme_classic()

ggplot(phenology_ANOVA_combined, aes(x = Decade, y = BloomPeakDay, fill=Taxon)) +
  geom_boxplot()+
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title="Bloom Peak Day by Decade and Taxon", x="Decade", y="Bloom Peak Day") +
  scale_fill_manual(values=c("Phytoplankton"= "cornflowerblue",
                             "Zooplankton"= "darkorchid")) +
  theme_classic()

ggplot(phenology_ANOVA_combined, aes(x = Decade, y = BloomDuration, fill=Taxon)) +
  geom_boxplot()+
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title="Bloom Duration by Decade and Taxon", x="Decade", y="Bloom Duration") +
  scale_fill_manual(values=c("Phytoplankton"= "cornflowerblue",
                             "Zooplankton"= "darkorchid")) +
  theme_classic()




# Mann Kendall Test
install.packages("trend")
library(trend)

mk.test(lag_peak_df$LagPeakDays)

#mk test for phytoplankton bloom start day and year 
mk.test(phytoplankton_phenology_clean$BloomStartDay)
mk.test(phytoplankton_phenology_clean$BloomPeakDay)
mk.test(phytoplankton_phenology_clean$BloomDuration)

















# Model Test: Linear models for Phenology Trends vs Year ----
lm_phytoplankton_start <- lm(BloomStartDay  ~ Year, data=bloom_start_phytoplankton)
lm_zooplankton_start <-lm(BloomStartDay  ~ Year, data=bloom_start_zooplankton)

anova(lm_phytoplankton_start)
anova(lm_zooplankton_start)

shapiro.test(resid(lm_phytoplankton_start))
shapiro.test(resid(lm_zooplankton_start))


lm_phytoplankton_peak <-lm(BloomPeakDay ~ Year, data=bloom_peak_phytoplankton)
lm_zooplankton_peak <-lm(BloomPeakDay ~ Year, data=bloom_peak_zooplankton)

lm_phytoplankton_duration <-lm(BloomDuration ~ Year, data=bloom_duration_phytoplankton)
lm_zooplankton_duration <-lm(BloomDuration ~ Year, data=bloom_duration_zooplankton)

### Visualizing linear models and checking assumptions ----
par(mfrow = c(2,2))
plot(lm_phytoplankton_start)
plot(lm_zooplankton_start)

plot(lm_phytoplankton_peak)
plot(lm_zooplankton_peak)

plot(lm_phytoplankton_duration)
plot(lm_zooplankton_duration)

hist(bloom_start_phytoplankton$BloomStartDay,col="skyblue",breaks= 10, xlab="BloomStartDay")
hist(bloom_start_zooplankton$BloomStartDay,col="skyblue",breaks= 10,xlab="BloomStartDay")
shapiro.test(bloom_start_phytoplankton$BloomStartDay)
shapiro.test(bloom_start_zooplankton$BloomStartDay)


hist(bloom_peak_phytoplankton$BloomPeakDay,col="skyblue",breaks= 10,xlab="BloomPeakDay")
hist(bloom_peak_zooplankton$BloomPeakDay,col="skyblue",breaks= 10,xlab="BloomPeakDay")
shapiro.test(bloom_peak_phytoplankton$BloomPeakDay)
shapiro.test(bloom_peak_zooplankton$BloomPeakDay)

hist(bloom_duration_phytoplankton$BloomDuration,col="skyblue",breaks= 10,xlab="BloomDuration")
hist(bloom_duration_zooplankton$BloomDuration,col="skyblue",breaks= 10,xlab="BloomDuration")
shapiro.test(bloom_duration_phytoplankton$BloomDuration)
shapiro.test(bloom_duration_zooplankton$BloomDuration)


# Model 1: Linear models for Phenology vs Temp -----
### Phytoplankton ----
lm_phyto_bloomstart_temp <- lm(BloomStartDay ~ MeanTemp, data=phytoplankton_phenology_clean)
lm_phyto_bloompeak_temp <-lm(BloomPeakDay ~ MeanTemp, data=phytoplankton_phenology_clean)
lm_phyto_bloomduration_temp <-lm(BloomDuration ~ MeanTemp, data=phytoplankton_phenology_clean)

summary(lm_phyto_bloomstart_temp)
summary(lm_phyto_bloompeak_temp)
summary(lm_phyto_bloomduration_temp)

#testing assumptions 
par(mfrow = c(2,2))
plot(lm_phyto_bloomstart_temp)
plot(lm_phyto_bloompeak_temp)
plot(lm_phyto_bloomduration_temp )

shapiro.test(resid(lm_phyto_bloomstart_temp)) #normality test
shapiro.test(resid(lm_phyto_bloompeak_temp))
shapiro.test(resid(lm_phyto_bloomduration_temp ))

hist(residuals(lm_phyto_bloompeak_temp),
     breaks = 5,
     main = "Residuals histogram",
     xlab = "Residuals")


library(lmtest) #heteroscedasticity test
bptest(lm_phyto_bloomstart_temp)
bptest(lm_phyto_bloompeak_temp)
bptest(lm_phyto_bloomduration_temp )


### Zooplankton ----
lm_zoop_bloomstart_temp <- lm(BloomStartDay ~ MeanTemp, data=zooplankton_phenology_clean)
lm_zoop_bloompeak_temp <- lm(BloomPeakDay ~ MeanTemp, data=zooplankton_phenology_clean)
lm_zoop_bloomduration_temp <- lm(BloomDuration ~ MeanTemp, data=zooplankton_phenology_clean)

#testing assumptions 
par(mfrow = c(2,2))
plot(lm_zoop_bloomstart_temp)
plot(lm_zoop_bloompeak_temp)
plot(lm_zoop_bloomduration_temp )

shapiro.test(resid(lm_zoop_bloomstart_temp))
shapiro.test(resid(lm_zoop_bloompeak_temp))
shapiro.test(resid(lm_zoop_bloomduration_temp ))

bptest(lm_zoop_bloomstart_temp)
bptest(lm_zoop_bloompeak_temp)
bptest(lm_zoop_bloomduration_temp)


# Model 2: Linear models for Phenology vs Temp * Interactive Term (Year)  -----
### Phytoplankton ----
lm_phyto_bloomstart_temp_year <- lm(BloomStartDay ~ MeanTemp * Year, data=phytoplankton_phenology_clean)
lm_phyto_bloompeak_temp_year <-lm(BloomPeakDay ~ MeanTemp * Year, data=phytoplankton_phenology_clean)
lm_phyto_bloomduration_temp_year <-lm(BloomDuration ~ MeanTemp * Year, data=phytoplankton_phenology_clean)

#testing assumptions 
par(mfrow = c(2,2))
plot(lm_phyto_bloomstart_temp_year)
plot(lm_phyto_bloompeak_temp_year)
plot(lm_phyto_bloomduration_temp_year )

shapiro.test(resid(lm_phyto_bloomstart_temp)) #normality test
shapiro.test(resid(lm_phyto_bloompeak_temp))
shapiro.test(resid(lm_phyto_bloomduration_temp ))

library(lmtest) #heteroscedasticity test
bptest(lm_phyto_bloomstart_temp_year)
bptest(lm_phyto_bloompeak_temp_year)
bptest(lm_phyto_bloomduration_temp_year )


### Zooplankton ----
lm_zoop_bloomstart_temp_year <- lm(BloomStartDay ~ MeanTemp*Year, data=zooplankton_phenology_clean)
lm_zoop_bloompeak_temp_year <- lm(BloomPeakDay ~ MeanTemp*Year, data=zooplankton_phenology_clean)
lm_zoop_bloomduration_temp_year <- lm(BloomDuration ~ MeanTemp*Year, data=zooplankton_phenology_clean)

#testing assumptions
par(mfrow = c(2,2))
plot(lm_zoop_bloomstart_temp_year)
plot(lm_zoop_bloompeak_temp_year)
plot(lm_zoop_bloomduration_temp_year )

shapiro.test(resid(lm_zoop_bloomstart_temp_year))
shapiro.test(resid(lm_zoop_bloompeak_temp_year))
shapiro.test(resid(lm_zoop_bloomduration_temp_year ))

#heteroscedasticity test
bptest(lm_zoop_bloomstart_temp_year)
bptest(lm_zoop_bloompeak_temp_year)
bptest(lm_zoop_bloomduration_temp_year )

#AIC comparison with model 1
AIC(lm_zoop_bloomstart_temp, lm_zoop_bloomstart_temp_year)
AIC(lm_zoop_bloompeak_temp, lm_zoop_bloompeak_temp_year)
AIC(lm_zoop_bloomduration_temp, lm_zoop_bloomduration_temp_year)



# Model 3: Linear models for Phenology vs Temp + Main effect (Year)  -----
### Phytoplankton ----
lm_phyto_bloomstart_temp_MAINyear <- lm(BloomStartDay ~ MeanTemp + Year, data=phytoplankton_phenology_clean)
lm_phyto_bloompeak_temp_MAINyear <-lm(BloomPeakDay ~ MeanTemp + Year, data=phytoplankton_phenology_clean)
lm_phyto_bloomduration_temp_MAINyear <-lm(BloomDuration ~ MeanTemp + Year, data=phytoplankton_phenology_clean)

#testing assumptions 
par(mfrow = c(2,2))
plot(lm_phyto_bloomstart_temp_MAINyear)
plot(lm_phyto_bloompeak_temp_MAINyear)
plot(lm_phyto_bloomduration_temp_MAINyear )

shapiro.test(resid(lm_phyto_bloomstart_temp_MAINyear)) #normality test
shapiro.test(resid(lm_phyto_bloompeak_temp_MAINyear))
shapiro.test(resid(lm_phyto_bloomduration_temp_MAINyear ))

library(lmtest) #heteroscedasticity test
bptest(lm_phyto_bloomstart_temp_MAINyear)
bptest(lm_phyto_bloompeak_temp_MAINyear)
bptest(lm_phyto_bloomduration_temp_year )


#AIC comparison with model 1&2 
AIC(lm_phyto_bloomstart_temp, lm_phyto_bloomstart_temp_year,lm_phyto_bloomstart_temp_MAINyear)
AIC(lm_phyto_bloompeak_temp, lm_phyto_bloompeak_temp_year,lm_phyto_bloompeak_temp_MAINyear)
AIC(lm_phyto_bloomduration_temp, lm_phyto_bloomduration_temp_year,lm_phyto_bloomduration_temp_MAINyear)



### Zooplankton ----
lm_zoop_bloomstart_temp_MAINyear <- lm(BloomStartDay ~ MeanTemp+Year, data=zooplankton_phenology_clean)
lm_zoop_bloompeak_temp_MAINyear <- lm(BloomPeakDay ~ MeanTemp+Year, data=zooplankton_phenology_clean)
lm_zoop_bloomduration_MAINtemp_year <- lm(BloomDuration ~ MeanTemp+Year, data=zooplankton_phenology_clean)

#testing assumptions
par(mfrow = c(2,2))
plot(lm_zoop_bloomstart_temp_MAINyear)
plot(lm_zoop_bloompeak_temp_MAINyear)
plot(lm_zoop_bloomduration_MAINtemp_year )

shapiro.test(resid(lm_zoop_bloomstart_temp_MAINyear))
shapiro.test(resid(lm_zoop_bloompeak_temp_MAINyear))
shapiro.test(resid(lm_zoop_bloomduration_MAINtemp_year ))

#heteroscedasticity test
bptest(lm_zoop_bloomstart_temp_MAINyear)
bptest(lm_zoop_bloompeak_temp_MAINyear)
bptest(lm_zoop_bloomduration_MAINtemp_year )


#AIC comparison with model 1&2 
AIC(lm_zoop_bloomstart_temp, lm_zoop_bloomstart_temp_year,lm_zoop_bloomstart_temp_MAINyear)
AIC(lm_zoop_bloompeak_temp, lm_zoop_bloompeak_temp_year,lm_zoop_bloompeak_temp_MAINyear)
AIC(lm_zoop_bloomduration_temp, lm_zoop_bloomduration_temp_year,lm_zoop_bloomduration_MAINtemp_year)



# Model 4: Linear models for Phenology vs Temp + Year + Strat  -----
### Phytoplankton ----
lm_phyto_bloomstart_temp_MAINyearStrat <- lm(BloomStartDay ~ MeanTemp + Year + MeanStratification, data=phytoplankton_phenology_clean)
lm_phyto_bloompeak_temp_MAINyearStrat <-lm(BloomPeakDay ~ MeanTemp + Year + MeanStratification, data=phytoplankton_phenology_clean)
lm_phyto_bloomduration_temp_MAINyearStrat <-lm(BloomDuration ~ MeanTemp + Year + MeanStratification, data=phytoplankton_phenology_clean)

#testing assumptions 
par(mfrow = c(2,2))
plot(lm_phyto_bloomstart_temp_MAINyearStrat)
plot(lm_phyto_bloompeak_temp_MAINyearStrat)
plot(lm_phyto_bloomduration_temp_MAINyearStrat )

shapiro.test(resid(lm_phyto_bloomstart_temp_MAINyearStrat)) #normality test
shapiro.test(resid(lm_phyto_bloompeak_temp_MAINyearStrat))
shapiro.test(resid(lm_phyto_bloomduration_temp_MAINyearStrat ))

library(lmtest) #heteroscedasticity test
bptest(lm_phyto_bloomstart_temp_MAINyearStrat)
bptest(lm_phyto_bloompeak_temp_MAINyearStrat)
bptest(lm_phyto_bloomduration_temp_MAINyearStrat)

summary(lm_phyto_bloomstart_temp_MAINyearStrat)
summary(lm_phyto_bloompeak_temp_MAINyearStrat)
summary(lm_phyto_bloomduration_temp_MAINyearStrat)


# Mismatch 
summary(lm(mismatch ~ Year, data=mismatch_phenology_clean))








#Checking residual autocorrelation ----
acf(residuals(lm_phytoplankton_start)) #Blue dashed lines = 95% confidence limits, Bars outside those lines = significant autocorrelation
#Residuals are mostly independent, with weak, short-memory autocorrelation.
pacf(residuals(lm_phytoplankton_start)) 
#There is some short-lag structure, but it is weak and not persistent.

lmtest::dwtest(lm_phytoplankton_start)
lmtest::dwtest(lm_zooplankton_start)
lmtest::dwtest(lm_phytoplankton_peak)
lmtest::dwtest(lm_zooplankton_peak) #problem DW<2 (positive autocorrelation)
lmtest::dwtest(lm_phytoplankton_duration) #problem DW<2 (positive autocorrelation)
lmtest::dwtest(lm_phytoplankton_duration) #problem DW<2 (positive autocorrelation)


#“Autocorrelation in model residuals was assessed using autocorrelation (ACF) and partial autocorrelation (PACF) functions. 
# Residuals from ordinary least squares models exhibited weak short-lag autocorrelation, particularly at lag 2, indicating partial violation of independence assumptions. 
#To account for temporal dependence, all subsequent analyses were conducted using generalized least squares models with a first-order autoregressive [AR(1)] error structure.”

# Multiple linear regression - temp and stratification vs indice
lm_phyto_bloomstart_temp_strat <- lm(BloomStartDay ~ MeanTemp + MeanStratification, data=phytoplankton_phenology_clean)
lm_zoop_bloomstart_temp_strat <- lm(BloomStartDay ~ MeanTemp + MeanStratification, data=zooplankton_phenology_clean)
summary(lm_phyto_bloomstart_temp_strat)
summary(lm_zoop_bloomstart_temp_strat)

library(car)
vif(lm_phyto_bloomstart_temp_strat)




#-----
library(ggplot2)
library(dplyr)

# create predictions from  model
newdata <- expand.grid(
  MeanTemp = seq(min(phytoplankton_phenology_clean$MeanTemp),
                 max(phytoplankton_phenology_clean$MeanTemp), length.out = 30),
  MeanStratification = seq(min(phytoplankton_phenology_clean$MeanStratification),
                           max(phytoplankton_phenology_clean$MeanStratification), length.out = 30),
  Year = c(1995, 2005, 2015)
)

newdata$PredictedBloom <- predict(model_1, newdata = newdata, type = "response")

ggplot(newdata, aes(x = MeanTemp, y = MeanStratification, fill = PredictedBloom)) +
  geom_tile() +
  facet_wrap(~Year) +
  scale_fill_viridis_c(name = "BloomStartDay") +
  labs(x = "Mean Temperature", y = "Mean Stratification") +
  theme_classic()



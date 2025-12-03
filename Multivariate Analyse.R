library(sjPlot)
library(glmulti)
library(flextable)
library(tidyverse)
library(readxl)
daten <- read_excel("Path zum File") #ladet die daten, "weissnicht" wird zu "Missing value" 

# Modellauswahl
h_model <- glmulti(AR~Stilldauer+Passives_Rauchen+Genetische_PräAR+Anzahl_ÄG+
                     Schwangerschaftsdauer, #Kanditaten (Achtung!: Anzahl_ÄG nur bei AR und Geschwisteranzahl nur bei Asthma verwenden, sonst Warunung)
                   data = daten,
                   crit = aic,              # Bewertung anhand des AICs
                   level=1,                 # eine Interaktion
                   method= "h",             # exhaustive Screening algotithmus
                   family = binomial,       # binäre Zufallsgrösse
                   fitfunction= glm,        # generalized linear Models
                   confsetsize=100)
weightable(h_model)[1:3,] %>% # schaut die Modelle an, die weniger als 2 ICs vom Besten Modelll weg sind.
                              # das "3" muss entsprechend angepasst werden
  regulartable() %>% 
  autofit()                   # Spaltenbreiten werden automatisch angepasst
plot(h_model, type = "s") # Plot der relativen Wichttigkeit von Prädiktoren (Welche am meisten bei "guten" Modellen vorkommen)
m<- glm(formula=AR ~ Passive_Rauchen+Schwangerschaftsdauer+Genetische_PräAR+Anzahl_ÄG #die durch "exhaustive Screening" ausgesuchten Variablen werden in einer multiplen logistischen Regression untersucht.
          ,
        data = daten,
        family=binomial)
library(gtsummary)
fancy_table <- tbl_regression(
  m,
  exponentiate = TRUE,                               # Log-Odds zu Odds
  add_pairwise_contrasts= TRUE,                      # Eine Tabelle wird erstellt, die alle paarweise Vergleiche zwischen Kategorien anschaulich macht
  contrasts_adjust="none",
  pvalue_fun= ~style_pvalue(.x, digits =3)) %>%      # p-Wert mit 3 Nachkommastellen
  add_significance_stars(hide_p = F, hide_se=T,
                         hide_ci= F) %>%
  bold_p() # Signifikanz "fett" setzen

fancy_table


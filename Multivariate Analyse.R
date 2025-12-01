library(sjPlot)
library(glmulti)
library(flextable)
library(tidyverse)
library(readxl)
daten <- read_excel("C:/Users/rsour/Downloads/MA_Datenanalyse2.xlsx", na = "weissnicht") #ladet die daten, 
                                                                                         #weissnicht wird zu "Missing value" 
h_model <- glmulti(AR~Stilldauer+Geburtsart+Passives_Rauchen+Genetische_PräAR+Anzahl_ÄG+
                     Schwangerschaftsdauer, #Variablenselektion 
                   data = daten,
                   crit = aic,              # Bewertung anhand des AICs
                   level=1,
                   method= "h",             # exhaustive Screening algotith
                   family = binomial,       # binäre Zufallsgrösse
                   fitfunction= glm,        # generalized linear Models
                   confsetsize=100)
weightable(h_model)[1:3,] %>% # schaut die Modelle an, die weniger als 2 ICs vom Besten Modelll weg sind.
  regulartable() %>% 
  autofit()
plot(h_model, type = "s") #Schaut die Gewichtungen jeder Variable an. Falls eine Variable "wichtiger" als die andere, aber trotzdem nicht in das beste Modell aufgenommen wird, kann das identfiziert und geändert werden (Nur wenn das geänderte Modell) 2 ICs entfernt ist vom besten Modell.
m<- glm(formula=Asthma ~ Stilldauer:Geburtsart+Genetische_PräAs+ #die durch "exhaustive Screening" ausgesuchten Variablen werden in einer multiplen logistischen Regression untersucht.
          Geschwisteranzahl,
        data = daten,
        family=binomial)
library(gtsummary)
fancy_table <- tbl_regression(
  m,
  exponentiate = TRUE,
  add_pairwise_contrasts= TRUE,                      # Eine Tabelle wird erstellt, die alle paarweise Vergleiche zwischen Kategorien anschaulich macht
  contrasts_adjust="none",
  pvalue_fun= ~style_pvalue(.x, digits =3)) %>% 
  add_significance_stars(hide_p = F, hide_se=T,
                         hide_ci= F) %>%
  bold_p()
fancy_table
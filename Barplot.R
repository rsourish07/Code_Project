library(tidyverse)
theme_set(theme_bw())
library(readxl)
daten <- read_excel("C:/Users/rsour/Downloads/MA_Datenanalyse2.xlsx", na = "weissnicht")
library(ggstatsplot)
library(ggplot2)
daten$Asthma <- factor(daten$Asthma, levels = c(0, 1), labels = c("Nein", "Ja")) # 1 und 0 werden als Ja und Nein codiert
daten$AR <- factor(daten$AR, levels = c(0, 1), labels = c("Nein", "Ja"))

plot <- ggbarstats(
  data            = Daten,
  x               = Asthma,
  y               = Stilldauer,         # Erstellen eines Stackedbarplots
  label           = "both",             # counts and percentage
  proportion.test = FALSE,
  digits          = 3,
  sample.size.label.args = list(size = 3, color = "grey40")
)

slot<-plot + theme(
  axis.text.x = element_text(size=12, face = "plain")) +
  theme(
    plot.caption = element_text(size = 10, face = "plain"),   # unnötigen Labels werden verkleiner
    legend.text = element_text(size = 12)
  )

splot<-slot + labs(subtitle = plot@labels$subtitle |>
                     as.list() |>                                         # Alle Statistiken aus die an der 3ten und 7ten Stelle werden gelöscht
                     ( \(.) c(as.name('list'), .[[3]], .[[7]]) )() |>
                     as.call() ,caption=NULL)

glot<-splot + scale_fill_manual(values = c("Nein" = "#7393B3", "Ja" = "tomato"))    #Farben
glot



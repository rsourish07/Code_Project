library(readxl)
library(vcd)
daten <- read_excel("Path zum File")
daten$Asthma <- factor(daten$Asthma, levels = c(0, 1), labels = c("Nein", "Ja"))  

daten$AR <- factor(daten$AR, levels = c(0, 1), labels = c("Nein", "Ja"))


struct <- structable(~ Stilldauer
                     + Asthma, data = daten)  # Unabhänge + Abhängige Variable
struct
tab <- as.table(struct)    # In "table" umwandeln, Zeilensummen und Zeilenprozente berechnen
row_sums <- rowSums(tab)
perc_row <- round(100 * sweep(tab, 1, row_sums, "/"), 1) 
tab_label <- matrix(
  paste0("n=",tab, "; ", perc_row, "%"),                     #Labels, counts und percentages
  nrow = nrow(tab),
  dimnames = dimnames(tab)
)

mosaic(
  struct, 
  data = daten,
  highlighting = "Asthma", 
  highlighting_fill = c("#90BBD8", "tomato"),      # Mosaikplot erzeugen, gefärbt nach Asthma
  direction = "v",
  pop = FALSE,
  expand.plot = F
)

labeling_cells(text = tab_label, margin = 0)(as.table(struct)) # Labels in den Mosaik-"Tiles" einfügen


tbl <- as.table(struct)


chi_test <- chisq.test(tbl, correct = FALSE) # Chi-Quadrat test

fisher_test<- fisher.test(tbl)           #Fisher-Exakt-Test
p_value <- chi_test$p.value              #p-Wert auc Chi

p_value1 <- fisher_test$p.value          # p-Wert aus Fisher

p_text <- paste0("p = ", format.pval(p_value, digits = 2)) 

grid::grid.text(label = p_text, x = 0.1, y = 0.05, gp = grid::gpar(fontface="bold"))
nobs<- paste0("nobs= ",sum(as.table(struct)))                                         # Manuellen Einfügen des p-Werts und nobs auf dem Plot unten links
grid::grid.text(label= nobs, x=0.25, y=0.05, gp=grid::gpar(fontface="bold"))



library(grid)

 # Manuelles Einfügen von einer Legende

grid.text("Asthma",
          x = 0.96,  # weit rechts
          y = 0.75,  # ungefähr mittig
          just = "left",
          gp = gpar(fontface = "bold"))

# Kästchen + Text "Ja"                                              
grid.rect(x = 0.98, y = 0.69, width = 0.03, height = 0.06,
          gp = gpar(fill = "tomato", col = "black"))
grid.text("Ja", x = 1, y = 0.69, just = "left")

# Kästchen + Text "Nein"
grid.rect(x = 0.98, y = 0.60, width = 0.03, height = 0.06,
          gp = gpar(fill = "#90BBD8", col = "black"))
grid.text("Nein", x = 1, y = 0.60, just = "left")





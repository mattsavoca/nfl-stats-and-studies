library(easypackages)
packages(
  "tidyverse", 
  "tidymodels",
  "devtools",
  "plotly",
  "ggthemes", 
  "nflplotR", 
  "nflreadr",
  "nflfastR",
  "ggwaffle", 
  "ggcorrplot", 
  "zoo", 
  prompt = F
)


# My Personal GGplot Theme ------
theme_savoca = function (base_size = 12, base_family = "sans"){
  (theme_foundation(base_size = base_size, base_family = base_family) + 
     theme(axis.line.x = element_line(colour = "black", linewidth = 0.5, linetype = "solid"), 
           axis.line.y = element_line(colour = "black", linewidth = 0.5, linetype = "solid"), 
           axis.text = element_text(size = ceiling(base_size * 0.7), colour = "black"), 
           axis.title = element_text(size = ceiling(base_size * .95), face = "bold"), 
           panel.grid.minor = element_blank(), 
           panel.grid.major.y = element_line(colour = "white",linetype = "dotted"), 
           panel.grid.major.x = element_blank(), 
           panel.background = element_blank(), 
           panel.border = element_blank(), 
           strip.background = element_rect(linetype = 0), 
           strip.text = element_text(), 
           strip.text.x = element_text(vjust = 0.5), 
           strip.text.y = element_text(angle = -90), 
           legend.text = element_text(size = ceiling(base_size * 0.9), family = "sans"), 
           legend.title = element_text(size = base_size, face = "bold", family = "sans"), 
           legend.position = "right", 
           legend.key = element_rect(fill = "white", colour = NA), 
           legend.background = element_rect(colour = "black"), 
           plot.background = element_rect(colour = "black"), 
           plot.title = element_text(size = ceiling(base_size * 1.6), face = "bold"), 
           plot.subtitle = element_text(size = ceiling(base_size * 1.05))))
}

theme_set(theme_savoca())
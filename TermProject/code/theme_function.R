theme_custom <- function( base_size = 11, base_family = "") {
  
  theme_bw() %+replace% 
    theme(
      
      plot.background   = element_rect(fill = "grey92", colour = "grey92") ,
      
      panel.grid.major  = element_line(color = "white"),
      
      panel.background  = element_rect(fill = "grey92", colour = "grey92"),
      
      panel.border      = element_rect(linetype = "solid", fill = NA), 
      
      axis.line         = element_line(color = "black", size = 1),
      
      axis.text         = element_text(color = "black"),
      
      axis.ticks        = element_line(color = "royalblue4", size = 1),
      
      axis.ticks.length.y = unit(.25, "cm"),
      
      axis.ticks.length.x = unit(.25, "cm"),
      
      axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
      
    )
}

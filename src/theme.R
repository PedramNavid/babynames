theme_clean <- function(text_angle=0, base_size = 12, base_family = "sans",
                        flip=FALSE ){
  if(flip==TRUE) {
    ygrid = element_blank()
  } else {
    ygrid = element_line(colour="grey80", linetype = 3)
  }
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.title = element_text(size = rel(.9)),
      axis.title.y =  element_text(angle = 90, margin=margin(0,10,0,0)),
      axis.title.x =  element_text(angle = 0, margin=margin(10,0,0,0)),
      axis.text = element_text(size = rel(.9)),
      axis.text.x = element_text(angle = text_angle),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      
      panel.background = element_rect(fill="#FAF7F2"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = ygrid,
      panel.grid.minor.y = element_blank(),
      panel.margin = unit(1, "lines"),
      
      plot.title = element_text(margin=margin(5,0,5,0), face="bold", size = rel(1.0)),
      plot.background = element_rect(fill="#f6f1eb"),
      plot.margin = unit(c(.5,.5,.5,.5), "lines"),
      
      strip.background = element_rect(fill="#FAF7F2", colour = NA),
      strip.text.x = element_text(size = rel(1.0), margin=margin(0,0,5,0)),
      strip.text.y = element_text(size = rel(1.0),  margin=margin(0,5,0,5)),
      
      legend.background = element_blank(),
      legend.margin = unit(0.2, "cm"),
      legend.key =  element_blank(),
      legend.key.size = unit(.5, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = rel(.9)),
      legend.text.align = NULL,
      legend.title = element_text(size = rel(.9), hjust = 0),
      legend.title.align = NULL,
      legend.position = "bottom",
      legend.direction = NULL,
      legend.justification = "center",
      legend.box = NULL
    )
}
theme_set(theme_clean())

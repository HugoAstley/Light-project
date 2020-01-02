library(GGally)
library(R.utils)

n <- data2[sample(nrow(data2), 10000), ]


ggpairs(n,
        columns = c(62,63,71,74,75,77,80,82),
        columnLabels = c("Pollution", "Time of solar noon", "Insolation", "Pressure", "Humidity", "T2M", "Wind speed", "Sunlight duration"),
        ggplot2::aes(colour=REGION),
        lower = list(continuous = "smooth",
                     theme(panel.grid.major= element_line("Black"))),
        upper= list(theme(axis.ticks=element_blank(), 
                axis.line=element_blank(),
                panel.grid.major= element_blank(),
                panel.grid.minor= element_blank(),
                panel.border = element_rect(linetype = "dashed", colour = "black", fill = NA),
                strip.background = element_rect(fill = "white"),
                panel.background = element_rect(fill = "white")
                ))
        )+
  theme(
    strip.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black"))



###Plotting the representatives from each group together


#override the ggaly_cor function for the upper panels

cor_fun <- function(data, mapping, method="pearson", ndp=2, sz=25, stars=TRUE, ...){
  
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor.test(x, y, method=method)
  est <- corr$estimate
  lb.size <- sz* abs(est)
  
  if(lb.size < 7){lb.size <- 7}
  
  if(stars){
    stars <- c("***", "**", "*", "")[findInterval(corr$p.value, c(0, 0.001, 0.01, 0.05, 1))]
    lbl <- paste0(round(est, ndp), stars)
  }else{
    lbl <- round(est, ndp)
  }
  
  ggplot(data=data, mapping=mapping) + 
    annotate("text", x=mean(x, na.rm=TRUE), y=mean(y, na.rm=TRUE), label=lbl, size=lb.size,...)+
    theme(panel.grid = element_blank(),
          panel.background = element_blank())
}

#lm with corr function
lm_with_cor <- function(data, mapping, ..., method = "pearson") {
  x <- eval(as.numeric(mapping$x), data)
  y <- eval(as.numeric(mapping$y), data)
  cor <- cor(x, y, method = method)
  ggally_smooth_lm(data, mapping, ...) +
    ggplot2::geom_label(
      data = data.frame(
        x = min(x, na.rm = TRUE),
        y = max(y, na.rm = TRUE),
        lab = round(cor, digits = 3)
      ),
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      hjust = 0, vjust = 1,
      size = 5, fontface = "bold",
      inherit.aes = FALSE # do not inherit anything from the ...
    )
}


#Plot comparison
ggpairs(data2.short,
        columns = c(81,61,73, 72, 78),
        columnLabels = c("Sunlight duration", "Pollution",  "Pressure", "Precipitation", "Wet bulb temperature"),
        mapping = aes(colour=as.factor(REGION), alpha = 0.7),
        lower = list(continuous = function(data, mapping, ...) {
          ggally_smooth_loess(data = data, mapping = mapping) +
            theme(panel.background = element_blank(),
                  panel.grid.minor = element_line(colour="black", size=0.25),
                  panel.grid.major = element_line(colour="black", size=0.5))}
        ),
        upper= list(continuous = cor_fun),
        diag = list(continuous = function(data, mapping, ...) {
          ggally_densityDiag(data = data, mapping = mapping) + 
            theme(panel.background = element_blank(),
                  )}
        ))+
  theme(strip.background = element_rect(
    fill = "transparent", color = "grey80"))



#Plot light duration against timing of the solar noon
ggpairs(data2.short,
        columns = c(80,81,62,63),
        columnLabels = c("Sunlight duration", "S.D. of sunlight duration",  "Mean Solar Noon", "S.D. of Mean Solar Noon"),
        mapping = aes(colour=as.factor(REGION), alpha = 0.7),
        lower = list(continuous = function(data, mapping, ...) {
          ggally_smooth_loess(data = data, mapping = mapping) +
            theme(panel.background = element_blank(),
                  panel.grid.minor = element_line(colour="black", size=0.25),
                  panel.grid.major = element_line(colour="black", size=0.5))}
        ),
        upper= list(continuous = cor_fun),
        diag = list(continuous = function(data, mapping, ...) {
          ggally_densityDiag(data = data, mapping = mapping) + 
            theme(panel.background = element_blank(),
            )}
        ))+
  theme(strip.background = element_rect(
    fill = "transparent", color = "grey80"))


#Plot light duration against radiation
ggpairs(data2.short,
        columns = c(81,68,69,70,71),
        columnLabels = c("Sunlight duration", "Longwave insolation",  "Shortwave insolation", "Top of atmosphere insolation", "Clearness index"),
        mapping = aes(colour=as.factor(REGION), alpha = 0.7),
        lower = list(continuous = function(data, mapping, ...) {
          ggally_smooth_loess(data = data, mapping = mapping) +
            theme(panel.background = element_blank(),
                  panel.grid.minor = element_line(colour="black", size=0.25),
                  panel.grid.major = element_line(colour="black", size=0.5))}
        ),
        upper= list(continuous = cor_fun),
        diag = list(continuous = function(data, mapping, ...) {
          ggally_densityDiag(data = data, mapping = mapping) + 
            theme(panel.background = element_blank(),
            )}
        ))+
  theme(strip.background = element_rect(
    fill = "transparent", color = "grey80"))


















###Within sunlight there are these factors



ggpairs(mtcars, 
              upper = list(continuous = wrap(cor_func,
                                             method = 'spearman', symbol = expression('\u03C1 ='))),
              lower = list(continuous = function(data, mapping, ...) {
                ggally_smooth_lm(data = data, mapping = mapping) +
                  theme(panel.background = element_blank())}),
              diag = list(continuous = function(data, mapping, ...) {
                ggally_densityDiag(data = data, mapping = mapping) + 
                  theme(panel.background = element_blank())}
              ))















####Not used#####
#custom function to make the distributions transparent
my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_density(..., alpha = 0.7, color = NA) 
}



#63
#71
"Time of solar noon"
"Insolation",
legend.position = "none"



  xAxisLabels = c("A", "B", "C", "A", "B", "C", "A", "B")


theme()

n$REGION <- as.factor(n$REGION)
        
#ggplot2::aes(colour=species)


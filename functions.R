# customized axis 2 and gridlines
custom_plot <- function(){
  axis(2, las = 2)
  grid(NA, ny = NULL, col = "lightgrey", lty = "dashed")
}

# percentage
percent <- function(x) prop.table(table(x))*100

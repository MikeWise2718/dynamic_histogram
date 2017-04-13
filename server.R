library(shiny)
library(plotly)
library(moments)

server <- function(input, output) {
  
  # Read and transform data
  alltogether <- data.frame(read.csv(file = "D:/Users/acibis/Desktop/dynamic_histogram/dji.csv", sep = ",", header = TRUE))
  
  #change column names to simplify
  colnames(alltogether) <- c("daty","stopy","ceny")
  
  # Generate statistic magic values
  for (i in 3000:650){
    n = i-649
    alltogether$means[i] <- mean(alltogether$ceny[n:i])
    alltogether$variance[i] <- var(alltogether$ceny[n:i])
    alltogether$skewness[i] <- skewness(alltogether$ceny[n:i])
    alltogether$kurtosis[i] <- kurtosis(alltogether$ceny[n:i])
  }
  
  # render 
  output$plot <- renderPlotly({ 
    
    f <- event_data("plotly_hover")
    print(f[1,][2])
    g <- f$pointNumber[1]
    h <- g-649
    
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Stopa zwrotu",
      range = c(-0.14,0.3)
    )
    p <- plot_ly() %>%
      add_lines(data = alltogether, x = ~daty, y = ~ceny, name = "cena", text = ~paste("srednia: ", alltogether$means)) %>%
      add_lines(data = alltogether, x = ~daty, y = ~stopy, name = "stopa zwrotu", yaxis = "y2") %>%
      layout(title = "Cena i stopa zwrotu", yaxis2 = ay)
  })
  

  output$plot2 <- renderPlotly({
    d <- event_data("plotly_hover")
    b <- d$pointNumber[1]
    e <- b+649
    rangy <- (alltogether$stopy[b:e])
    if (b > 649){ 
      plot_ly(x=rangy,type = "histogram")
    }
  })
  
}
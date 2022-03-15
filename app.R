library(readxl)
library(shiny)
library(kableExtra)
library(tidyverse)
library(scales)
library(rsconnect)

Claims_Data <- read_excel("R Shiny Assignment.xlsx", sheet = "Assignment", range = "B3:D9")
Tail_Factor <- read_excel("R Shiny Assignment.xlsx", sheet = "Assignment", range = "C12:C12",
                          col_names = "Tail Factor")[[1]]

ui <- fluidPage(
  
  titlePanel("Cumulative Paid Claims"),
  
  mainPanel(tableOutput(outputId = "Cumulative_Paid_Claims_Table"),
            plotOutput(outputId = "Cumulative_Paid_Claims_Graph"))
)

server <- function(input, output){
  
  LY17DY1 <- Claims_Data[Claims_Data$`Loss Year`==2017 &
                           Claims_Data$`Development Year`<=1, ]
  cum_LY17DY1 <- round(sum(LY17DY1[, 3]))
  LY17DY2 <- Claims_Data[Claims_Data$`Loss Year`==2017 &
                           Claims_Data$`Development Year`<=2, ]
  cum_LY17DY2 <- round(sum(LY17DY2[, 3]))
  LY17DY3 <- Claims_Data[Claims_Data$`Loss Year`==2017 &
                           Claims_Data$`Development Year`<=3, ]
  cum_LY17DY3 <- round(sum(LY17DY3[, 3]))
  cum_LY17DY4 <- round(cum_LY17DY3 * Tail_Factor)
  LY18DY1 <- Claims_Data[Claims_Data$`Loss Year`==2018 &
                           Claims_Data$`Development Year`<=1, ]
  cum_LY18DY1 <- round(sum(LY18DY1[, 3]))
  LY18DY2 <- Claims_Data[Claims_Data$`Loss Year`==2018 &
                           Claims_Data$`Development Year`<=2, ]
  cum_LY18DY2 <- round(sum(LY18DY2[, 3]))
  cum_LY18DY3 <- round((cum_LY17DY3 / cum_LY17DY2)*cum_LY18DY2)
  cum_LY18DY4 <- round(cum_LY18DY3 * Tail_Factor)
  1
  LY19DY1 <- Claims_Data[Claims_Data$`Loss Year`==2019 &
                           Claims_Data$`Development Year`<=1, ]
  cum_LY19DY1 <- round(sum(LY19DY1[, 3]))
  cum_LY19DY2 <- round((sum(cum_LY17DY2, cum_LY18DY2) / sum(cum_LY17DY1, cum_LY18DY1)) *
                         cum_LY19DY1)
  cum_LY19DY3 <- round((cum_LY17DY3 / cum_LY17DY2) * cum_LY19DY2)
  cum_LY19DY4 <- round(cum_LY19DY3 * Tail_Factor)
  
  # Constructing Cumulative Paid Claims Table
  Cumulative_Paid_Claims <- data.frame(Loss_Year = c(2017, 2018, 2019),
                                       Development_Year_1 =
                                         c(cum_LY17DY1, cum_LY18DY1, cum_LY19DY1),
                                       Development_Year_2 =
                                         c(cum_LY17DY2, cum_LY18DY2, cum_LY19DY2),
                                       Development_Year_3 =
                                         c(cum_LY17DY3, cum_LY18DY3, cum_LY19DY3),
                                       Development_Year_4 =
                                         c(cum_LY17DY4, cum_LY18DY4, cum_LY19DY4))
  
  Cumulative_Paid_Claims$Development_Year_1 <-
    prettyNum(Cumulative_Paid_Claims$Development_Year_1, big.mark = ",")
  Cumulative_Paid_Claims$Development_Year_2 <-
    prettyNum(Cumulative_Paid_Claims$Development_Year_2, big.mark = ",")
  Cumulative_Paid_Claims$Development_Year_3 <-
    prettyNum(Cumulative_Paid_Claims$Development_Year_3, big.mark = ",")
  Cumulative_Paid_Claims$Development_Year_4 <-
    prettyNum(Cumulative_Paid_Claims$Development_Year_4, big.mark = ",")
  
  output$Cumulative_Paid_Claims_Table <- function(){
    kable(Cumulative_Paid_Claims, col.names = c("Loss Year", "1", "2", "3", "4"),
          align = "lcccc",
          caption = "<center><bold>Cumulative Paid Claims ($) - Table</bold></center>",
          escape = FALSE,
          format = "html") %>%
      add_header_above(c(" " = 1, "Development Year" = 4), bold = TRUE,
                       border_left = TRUE, border_right = TRUE) %>%
      row_spec(0, bold = TRUE) %>%
      column_spec(1, width = "1.1in", bold = TRUE, border_left = TRUE) %>%
      column_spec(2:4, width = "1.1in") %>%
      column_spec(5, width = "1.1in", border_right = TRUE) %>%
      kable_styling(latex_options = "hold_position", position = "center")
  }
  
  Loss_Year_2017 <- c(cum_LY17DY1, cum_LY17DY2, cum_LY17DY3, cum_LY17DY4)
  Loss_Year_2018 <- c(cum_LY18DY1, cum_LY18DY2, cum_LY18DY3, cum_LY18DY4)
  Loss_Year_2019 <- c(cum_LY19DY1, cum_LY19DY2, cum_LY19DY3, cum_LY19DY4)
  Development_Year <- c(1, 2, 3, 4)
  cum_paid_claims <- data.frame(Development_Year,
                                Loss_Year_2017,
                                Loss_Year_2018,
                                Loss_Year_2019)
  
  cum_paid_claims_grouped <- data.frame(x = cum_paid_claims$Development_Year,
                                        y = c(cum_paid_claims$Loss_Year_2017,
                                              cum_paid_claims$Loss_Year_2018,
                                              cum_paid_claims$Loss_Year_2019),
                                        group = c(rep("2017", nrow(cum_paid_claims)),
                                                  rep("2018", nrow(cum_paid_claims)),
                                                  rep("2019", nrow(cum_paid_claims))))
  
  output$Cumulative_Paid_Claims_Graph <- renderPlot({
    ggplot(cum_paid_claims_grouped, aes(x, y, col = group, label = y)) +
      geom_smooth(se = FALSE, size = 1) + geom_point(size = 3) +
      geom_text(aes(label = comma(y)),
                subset(cum_paid_claims_grouped,
                       y %in% c("819810","998640","1098504","1205710","1209320","1330252")),
                show.legend = FALSE,
                colour = "gray25",
                size = 4.5,
                hjust = "right",
                vjust = -0.75) +
      ggtitle("Cumulative Paid Claims ($) - Graph") +
      scale_x_continuous(name = "Development Year", limits = c(1, 4)) +
      scale_y_continuous(name = "", labels = comma, limits = c(500000, 1500000), expand = c(0,0)) +
      theme_bw() +
      theme(axis.text=element_text(colour = "gray25", size=12),
            axis.ticks = element_blank(),
            axis.title=element_text(colour = "gray25", size=14),
            plot.title = element_text(colour = "gray25", size = 20, hjust = 0.5),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.key.width = unit(1.5, 'cm'),
            legend.position = "bottom",
            legend.text = element_text(colour = "gray25", size=12),
            legend.title = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_rect(colour = "gray90", fill = NA)) +
      scale_color_manual(values=c("green3", "orange2", "steelblue1"))
  })
  
}

shinyApp(ui = ui, server = server)

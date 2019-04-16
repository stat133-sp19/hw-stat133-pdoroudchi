library(shiny)
library(ggplot2)

future_value <- function(amount, rate, years) {
  future_value <- amount * ((1 + rate) ^ years)
  return(future_value)
}

annuity <- function(contrib, rate, years) {
  fva <- contrib * (((1 + rate) ^ years - 1) / rate)
  return(fva)
}

growing_annuity <- function(contrib, rate, growth, years) {
  fvga <- contrib * (((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth))
  return(fvga)
}


ui <- fluidPage(
  titlePanel('Saving/Investing Scenarios'),
  fluidRow(
    column(4,
           sliderInput(inputId = 'amount',
                       label = 'Initial Amount',
                       min = 0,
                       max = 100000,
                       value = 1000,
                       step = 500,
                       pre = '$'
                       ),
           sliderInput(inputId = 'contrib',
                       label = 'Annual Contribution',
                       min = 0,
                       max = 50000,
                       value = 2000,
                       step = 500,
                       pre = '$'
                       )
           ),
    column(4, 
           sliderInput(inputId = 'rate',
                       label = 'Return Rate (in %)',
                       min = 0,
                       max = 20,
                       value = 5,
                       step = 0.1
                       ),
           sliderInput(inputId = 'growth',
                       label = 'Growth Rate (in %)',
                       min = 0,
                       max = 20,
                       value = 2,
                       step = 0.1
                       )
           ),
    column(4, 
           sliderInput(inputId = 'years',
                       label = 'Years',
                       min = 0,
                       max = 50,
                       value = 20,
                       step = 1
                       ),
           selectInput(inputId = 'facet',
                       label = 'Facet?',
                       choices = c('No', 'Yes')
                       )
           )
    ),
  tags$hr(),
  h4('Timelines'),
  plotOutput('timeline_graph'),
  h4('Balances'),
  tableOutput('balance_table')
)


server <- function(input, output) {
  output$timeline_graph <- renderPlot({
    no_contrib <- c()
    for (i in 0:input$years) {
      no_contrib[i + 1] <- future_value(input$amount, input$rate / 100, i)
    }
    fixed_contrib <- c()
    for (i in 0:input$years) {
      fixed_contrib[i + 1] <- future_value(input$amount, input$rate / 100, i) + annuity(input$contrib, input$rate / 100, i)
    }
    growing_contrib <- c()
    for (i in 0:input$years) {
      growing_contrib[i + 1] <- future_value(input$amount, input$rate / 100, i) + growing_annuity(input$contrib, input$rate / 100, input$growth / 100, i)
    }
    year <- c(0:input$years)
    if (input$facet == 'Yes') {
      all_contrib <- c(no_contrib, fixed_contrib, growing_contrib)
      modality <- c(rep('no contribution', input$years + 1), rep('fixed conribution', input$years + 1), rep('growing contribution', input$years + 1))
      facet_modalities <- data.frame(year, all_contrib, modality, row.names = NULL)
      facet_modalities$modality <- factor(facet_modalities$modality, levels = c('no contribution', 'fixed conribution', 'growing contribution'))
      ggplot(facet_modalities) +
        facet_wrap(~ modality) +
        geom_point(aes(x = year, y = all_contrib, color = modality), size = 1) +
        geom_line(aes(x = year, y = all_contrib, color = modality), size = 0.7) +
        geom_area(aes(x = year, y = all_contrib, fill = modality), alpha = 0.5) +
        labs(x = 'year', y = 'value', title = 'Three modes of investing') +
        theme_minimal()
    } else {
      modalities <- data.frame(year, no_contrib, fixed_contrib, growing_contrib, row.names = NULL)
      ggplot(modalities) +
        geom_line(aes(x = year, y = no_contrib, color = '1'), size = 0.7) +
        geom_line(aes(x = year, y = fixed_contrib, color = '2'), size = 0.7) +
        geom_line(aes(x = year, y = growing_contrib, color = '3'), size = 0.7) +
        geom_point(aes(x = year, y = no_contrib, color = '1'), size = 1) +
        geom_point(aes(x = year, y = fixed_contrib, color = '2'), size = 1) +
        geom_point(aes(x = year, y = growing_contrib, color = '3'), size = 1) +
        labs(x = 'year', y = 'value', title = 'Three modes of investing') +
        theme_minimal() + 
        scale_color_manual(values = c('1' = '#F8766D', '2' = '#00BA38', '3' = '#619CFF'),
                           labels = c('no contribution', 'fixed contribution', 'growing contribution'),
                           name = 'modality'
        )
      }
  })
  output$balance_table <- renderTable({
    no_contrib <- c()
    for (i in 0:input$years) {
      no_contrib[i + 1] <- future_value(input$amount, input$rate / 100, i)
    }
    fixed_contrib <- c()
    for (i in 0:input$years) {
      fixed_contrib[i + 1] <- future_value(input$amount, input$rate / 100, i) + annuity(input$contrib, input$rate / 100, i)
    }
    growing_contrib <- c()
    for (i in 0:input$years) {
      growing_contrib[i + 1] <- future_value(input$amount, input$rate / 100, i) + growing_annuity(input$contrib, input$rate / 100, input$growth / 100, i)
    }
    year <- c(0:input$years)
    modalities <- data.frame(year, no_contrib, fixed_contrib, growing_contrib, row.names = NULL)
  })
}


shinyApp(ui = ui, server = server)



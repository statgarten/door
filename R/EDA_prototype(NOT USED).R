
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(reactable)
library(GGally)
library(ggplot2)

obj <- board::brief(mtcars)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      shinydashboardPlus::box(
        title = "Description",
        status = 'purple',
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(
            width = 6,
            descriptionBlock(
              header = paste0(obj$desc$nrow, ' X ', obj$desc$ncol),
              numberIcon = icon('expand'),
              number = 'Data Dimension',
              marginBottom = FALSE
            )
          ),
          column(
            width = 6,
            descriptionBlock(
              header = paste0(obj$desc$missingCellCount, '(',  obj$desc$missingCellRatio,'%)'),
              numberIcon = icon('question'),
              number = 'Missing Data',
              marginBottom = FALSE
            )
          )
        )
      ),
      shinydashboardPlus::box(
        title = 'Variables',
        status = 'purple',
        solidHeader = TRUE,
        width = 12,
        reactableOutput('reactOutput')
      ),
      shinydashboardPlus::box(
         title = 'Correlation',
         status = 'success',
         solidHeader = TRUE,
         width = 12,
         plotOutput('corplot')
       ),

      selectInput(
        inputId = 'variableSelect',
        label = 'variable',
        choices = colnames(mtcars),
        selected = NULL,
        multiple = FALSE
      ),

      shinydashboardPlus::box(
        title = 'distribution',
        status = 'success',
        solidHeader = TRUE,
        width = 12,
        plotOutput('distplot'),
        textOutput('normal'),
        htmlOutput('stats'),
        textOutput('outlier')
      ),

      shinydashboardPlus::box(
        title = 'Correlation2',
        status = 'success',
        solidHeader = TRUE,
        width = 12,
        selectInput(inputId = 'var1', label = 'variable 1', choices = c('',colnames(mtcars)), selected = NULL),
        selectInput(inputId = 'var2', label = 'variable 2', choices = c( '',colnames(mtcars)), selected = NULL),
        plotOutput('corplot2')
      ),
    ),
    title = "EDA Module"
  ),
  server = function(input, output) {
    output$corplot <- renderPlot(ggcorr(obj$cors))

    obj$unif <- ifelse(obj$unif, "True", NA)
    obj$uniq <- ifelse(obj$unif, "True", NA)
    res <- data.frame(
      Name = obj$names,
      Cardinality = obj$cards,
      Zero = obj$zeros,
      Missing = obj$miss,
      isUniform = obj$unif,
      isUnique = obj$uniq
    )

    output$reactOutput <- renderReactable(
      reactable(
        res
      )
    )

    observeEvent(input$var2, {
      if(input$var2=='') return(0)
      output$corplot2 <- renderPlot(board::relation(mtcars[,input$var1], mtcars[,input$var2]) )
    })

    observeEvent(input$variableSelect, {
      # is numeric
      # dominate
      # normal
      # distribute
      # outlier
      #  describe


      if(is.numeric(mtcars[,input$variableSelect]) ){
        output$distplot <- renderPlot(board::distribute(mtcars[,input$variableSelect], name = input$variableSelect))

        ss <- board::describe(mtcars[,input$variableSelect])
        ss <- paste0(
          'total count: ', ss$count,
          " mean : ", ss$m,
          ' sd : ', ss$s,
          ' min: ', ss$q0,
          '\n25%: ', ss$q1,
          ' median: ', ss$q2,
          ' 75%: ', ss$q3,
          ' max: ', ss$q4
        )
        output$stats <- renderUI(HTML(ss))

        oo <- board::outlier(mtcars[,input$variableSelect])
        oo <- paste0(
          ifelse(oo$lo > 0, paste0('value larger than ', oo$ov, ' may consider as Outlier (', oo$lo ,' exists)'), 'large outlier not exists'),
          '\nAnd\n',
          ifelse(oo$lu > 0, paste0('value smaller than ', oo$uv, ' may consider as Outlier (', oo$lu, ' exists)'), 'small outlier not exists')
        )

        output$outlier <- renderText(oo)

        if(board::isNormal(mtcars[,input$variableSelect])) # normality)
          output$normal <- renderText('Variable may normally distributed')
        else{
          output$normal <- renderText('Variable may not normally distributed')
        }
      }

      else{
        output$distplot <- renderPlot(board::ggpie(mtcars[,input$variableSelect]))
      }


    })
  }
)

dominate -> category
isNormal -> Numeric
ditribute -> vis
outlier -> numeric
describe -> numeric

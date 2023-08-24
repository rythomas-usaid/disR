library(shiny)
library(dplyr)
library(tidyr)
library(disR)
# Sample dataframe
data <- indicators_df

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Explore DataFrame"),

  sidebarPanel(
    # First set of filters
    selectInput("ic_filter", "Filter by ic_1819:", choices = unique(data$ic_1819)),

    # Second set of filters
    selectizeInput("columns_2", "Select Columns (Set 2):", choices = c("d1", "d2", "d3", "d4"), multiple = TRUE),
    uiOutput("filter_value_2"),

    # Third set of filters
    selectizeInput("columns_3", "Select Columns (Set 3):", choices = c("d1", "d2", "d3", "d4"), multiple = TRUE),
    uiOutput("filter_value_3"),

    # Logical operator choice
    radioButtons("logical_operator", "Logical Operator:", choices = c("AND", "OR"), selected = "AND"),

    actionButton("apply_filter", "Apply Filter"),
    actionButton("reset_filter", "Reset Filter"),
    actionButton("summarize_by_year", "Summarize by Year")
  ),

    mainPanel(
      dataTableOutput("filtered_table")
    )
  )


# Define server logic
server <- function(input, output, session) {

  filtered_data <- reactiveVal(data)

  observeEvent(input$apply_filter, {
    filter_expr_ic <- NULL
    filter_expr_2 <- NULL
    filter_expr_3 <- NULL

    if (!is.null(input$ic_filter)) {
      filter_expr_ic <- data$ic_1819 == input$ic_filter
    }

    if (!is.null(input$columns_2) && length(input$columns_2) > 0) {
      filter_expr_2 <- purrr::map(input$columns_2, ~ data[[.x]] == input[[paste0("filter_value_2_", .x)]]) %>%
        purrr::reduce(ifelse(input$logical_operator == "AND", `&`, `|`))
    }

    if (!is.null(input$columns_3) && length(input$columns_3) > 0) {
      filter_expr_3 <- purrr::map(input$columns_3, ~ data[[.x]] == input[[paste0("filter_value_3_", .x)]]) %>%
        purrr::reduce(ifelse(input$logical_operator == "AND", `&`, `|`))
    }

    if (input$logical_operator == "AND") {
      if (!is.null(filter_expr_ic) && !is.null(filter_expr_2) && !is.null(filter_expr_3)) {
        filter_expr <- filter_expr_ic & filter_expr_2 & filter_expr_3
      } else if (!is.null(filter_expr_ic) && !is.null(filter_expr_2)) {
        filter_expr <- filter_expr_ic & filter_expr_2
      } else if (!is.null(filter_expr_ic) && !is.null(filter_expr_3)) {
        filter_expr <- filter_expr_ic & filter_expr_3
      } else if (!is.null(filter_expr_2) && !is.null(filter_expr_3)) {
        filter_expr <- filter_expr_2 & filter_expr_3
      } else if (!is.null(filter_expr_ic)) {
        filter_expr <- filter_expr_ic
      } else if (!is.null(filter_expr_2)) {
        filter_expr <- filter_expr_2
      } else if (!is.null(filter_expr_3)) {
        filter_expr <- filter_expr_3
      } else {
        filter_expr <- NULL
      }
    } else { # Logical operator is "OR"
      filter_expr <- ifelse(is.null(filter_expr_ic), filter_expr_2, filter_expr_ic)
      filter_expr <- ifelse(is.null(filter_expr), filter_expr_3, filter_expr)
    }

    if (!is.null(filter_expr)) {
      filtered_data(data %>% filter(filter_expr))
    } else {
      filtered_data(data)
    }
  })

  observeEvent(input$reset_filter, {
    filtered_data(data)
  })

  output$filter_value_2 <- renderUI({
    req(input$columns_2)
    filter_values <- lapply(input$columns_2, function(col) {
      unique(data[[col]])
    })
    filter_inputs <- lapply(seq_along(input$columns_2), function(i) {
      selectInput(
        paste0("filter_value_2_", input$columns_2[i]),
        paste("Filter Value (", input$columns_2[i], "):"),
        choices = c("", filter_values[[i]])
      )
    })
    do.call(tagList, filter_inputs)
  })

  output$filter_value_3 <- renderUI({
    req(input$columns_3)
    filter_values <- lapply(input$columns_3, function(col) {
      unique(data[[col]])
    })
    filter_inputs <- lapply(seq_along(input$columns_3), function(i) {
      selectInput(
        paste0("filter_value_3_", input$columns_3[i]),
        paste("Filter Value (", input$columns_3[i], "):"),
        choices = c("", filter_values[[i]])
      )
    })
    do.call(tagList, filter_inputs)
  })

  observe({
    available_group_columns <- "year"
    updateSelectizeInput(session, "group_by_columns", choices = available_group_columns, selected = "year")
  })

  observeEvent(input$summarize_by_year, {
    summarized_data <- filtered_data() %>%
      group_by(year) %>%
      summarise(Sum_Value = sum(value))

    output$filtered_table <- renderDataTable({
      summarized_data
    })
  })

  output$filtered_table <- renderDataTable({
    summarized_data <- filtered_data()

    summarized_data
  })
}

# Run the Shiny app
shinyApp(ui, server)

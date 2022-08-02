library(shiny)
library(shinythemes)
library(latex2exp)


# Define UI for application
ui <- fluidPage(theme = shinytheme("cerulean"), 
                
                # Application title
                titlePanel("Variance Estimation in Multiply Imputed Datasets"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        selectInput("method_select", 
                                    label = "Select variance estimation method.", 
                                    choices = methods,
                                    selected = NULL,
                                    multiple = FALSE, 
                                    selectize = FALSE,
                                    size = 5), 
                        selectInput("type_select", 
                                    label = "Select imputation method.", 
                                    choices = types,
                                    selected = NULL,
                                    multiple = FALSE, 
                                    selectize = FALSE,
                                    size = 5), 
                        selectInput("sample_size_select", 
                                    label = "Select sample size.", 
                                    choices = sample_sizes,
                                    selected = NULL,
                                    multiple = FALSE, 
                                    selectize = FALSE,
                                    size = 5), 
                        selectInput("percent_missing_select", 
                                    label = "Select percent of missing data.", 
                                    choices = p_miss,
                                    selected = NULL,
                                    multiple = FALSE, 
                                    selectize = FALSE,
                                    size = 5), 
                        tableOutput("summary_stats")
                    ),
                    
                    # Show a plot of the generated distribution and the dataset 
                    mainPanel(
                        plotOutput("distPlot", width = "100%"),
                        dataTableOutput('table')
                    )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    methods <- 
        list(
            "Rubin's Rules" = "rubin", 
            "MI Bootstrap Rubin" = "mi_boot_rubin", 
            "MI Bootstrap Rubin Pooled" = "mi_boot_rubin_pooled", 
            "Bootstrap MI Pooled" = "boot_mi_pooled") 
    types <- 
        list("Uncongenial" = "uncongenial", 
             "Congenial" = "congenial")
    sample_sizes <- 
        list(500, 1000, 10000)
    p_miss <- 
        list("10%" = 0.1, "30%" = 0.3, "50%" = 0.5)
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        df <- results %>%
            dplyr::filter(method == as.name(input$"method_select")) %>%
            dplyr::filter(type == as.name(input$"type_select")) %>%
            dplyr::filter(n == as.name(input$"sample_size_select")) %>%
            dplyr::filter(p_mis == as.name(input$"percent_missing_select")) %>%
            dplyr::select(-method, -type)
        
        df_jit <- df + matrix(rnorm(nrow(df) * ncol(df), sd = 0.1), ncol = ncol(df))
        
        
        output$table <- renderDataTable(df_jit %>%
                                            dplyr::select("UB", "LB", "variance") %>%
                                            mutate(across(is.numeric, round, digits = 2)))
        output$summary_stats <- 
            renderTable(tibble("Median C.I width" = median(df_jit$UB - df_jit$LB), 
                               "Median estimated variance " = median(df_jit$variance), 
                               "Estimated variance (complete)" = case_when(
                                   input$"sample_size_select" == 500 ~ 9.52, 
                                   input$"sample_size_select" == 1000 ~ 6.32, 
                                   input$"sample_size_select" == 10000 ~ 7.48), 
                               "C.I Coverage (95%)" = round((sum(df_jit$true_var < df_jit$UB & df_jit$true_var > df_jit$LB) / nrow(df_jit))*100,2)))
        
        
        ggplot(df_jit, aes(x = c(1:nrow(df_jit)), y = variance)) + 
            geom_point(aes(color = "red")) + 
            geom_errorbar(aes(ymin = LB, ymax = UB, alpha = 0.5)) + 
            coord_flip() + 
            theme_classic() + 
            labs(
                x = latex2exp::TeX("$i^{th} dataset"), 
                y = latex2exp::TeX("$\\widehat{\\sigma^2}")
            ) + 
            theme(legend.position="none") + 
            geom_hline(yintercept = case_when(
                input$"sample_size_select" == 500 ~ 9.52, 
                input$"sample_size_select" == 1000 ~ 6.32, 
                input$"sample_size_select" == 10000 ~ 7.48),
                lwd=2,colour="lightblue")
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

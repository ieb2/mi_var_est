library(tidyverse)
library(shiny)
library(shinythemes)
library(latex2exp)
set.seed(23479)

results <- read_csv("final_sim_results.csv", show_col_types = FALSE)
results$method <- as.factor(results$method)
results$type <- as.factor(results$type)

results <- results %>%
    dplyr::select(-"...1") %>%
    mutate(true_var = case_when(
        n == 500 ~ 9.52, 
        n == 1000 ~ 6.32, 
        n == 10000 ~ 7.48))

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

final_sim_results <- suppressMessages(read_csv("final_sim_results.csv") %>%
                                        dplyr::select(-"...1"))

# Organize data 
final_sim_results$method <- as.factor(final_sim_results$method)
final_sim_results$type  <- as.factor(final_sim_results$type)

final_sim_results <- final_sim_results %>%
  mutate(true_var =  case_when(
    n == 500 ~ 9.52, 
    n == 1000 ~ 6.32, 
    n == 10000 ~ 7.48))
# Outputs table of data

summary_generator <- function(df){
  tibble(
    "Method" = df$method, 
    "Imputation Approach" = df$type, 
    "Median C.I width" = median(df$UB - df$LB), 
    "Median estimated variance " = median(df$variance), 
    "Estimated variance (complete)" = case_when(
      df$n == 500 ~ 9.52, 
      df$n == 1000 ~ 6.32, 
      df$n == 10000 ~ 7.48), 
    "C.I Coverage (95%)" = round((sum(df$true_var < df$UB & df$true_var > df$LB) / nrow(df))*100,2), 
    "Sample Size" = df$n, 
    "Percentage of Missing Data" = df$p_mis)
}

grouped_results <- final_sim_results %>%
  group_by(method, type, n, p_mis) %>%
  group_split() %>%
  purrr::map(., summary_generator) %>%
  lapply(., function(x) slice(x, 1)) %>%
  do.call(rbind.data.frame, .) %>%
  mutate_if(is.numeric, round, digits=2)


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
                        plotOutput("kernel"),
                        dataTableOutput('table')
                        
                    )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$ecdf <- renderPlot({ 
    df <- grouped_results %>%
      dplyr::filter(`Method` == as.name(input$"method_select")) %>%
      dplyr::filter(`Imputation Approach` == as.name(input$"type_select")) %>%
      dplyr::filter(`Sample Size` == as.name(input$"sample_size_select")) %>%
      dplyr::filter(`Percentage of Missing Data` == as.name(input$"percent_missing_select"))
    
    levels(grouped_results$`Imputation Approach`) <- c("Congenial", "Uncongenial")
    
    ggplot(data = df, aes(`C.I Coverage (95%)`)) + 
      stat_ecdf(geom = "step", size=1) + 
      theme_bw() + 
      theme(axis.title.y = element_blank(), 
            panel.spacing=unit(1,"lines")) + 
      xlim(0,100)
  })
  
  output$kernel <- renderPlot({ 
    df <- results %>%
      dplyr::filter(method == as.name(input$"method_select")) %>%
      dplyr::filter(type == as.name(input$"type_select")) %>%
      dplyr::filter(n == as.name(input$"sample_size_select")) %>%
      dplyr::filter(p_mis == as.name(input$"percent_missing_select")) %>%
      dplyr::select(-method, -type)
    
    ggplot(data = df,
           aes(variance - true_var)) + 
      geom_density(aes(y = ..density..)) + 
      theme(axis.title.y = element_blank(), 
            panel.spacing=unit(1.5,"lines")) + 
      theme_bw() + 
      theme(axis.title.y = element_blank(), 
            panel.spacing=unit(1.5,"lines"), 
            strip.text = element_text(
              size = 9)) + 
      xlab("Bias of variance estimator") + 
      ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")})
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    df <- results %>%
      dplyr::filter(method == as.name(input$"method_select")) %>%
      dplyr::filter(type == as.name(input$"type_select")) %>%
      dplyr::filter(n == as.name(input$"sample_size_select")) %>%
      dplyr::filter(p_mis == as.name(input$"percent_missing_select")) %>%
      dplyr::select(-method, -type)
    
    df_jit <- df + matrix(rnorm(nrow(df) * ncol(df), sd = 0.09), ncol = ncol(df))
    
    
    output$table <- renderDataTable(df %>%
                                      dplyr::select("UB", "LB", "variance") %>%
                                      mutate(across(is.numeric, round, digits = 2)))
    output$summary_stats <- 
      renderTable(tibble("Median C.I width" = median(df$UB - df$LB), 
                         "Median estimated variance " = median(df$variance), 
                         "Estimated variance (complete)" = case_when(
                           input$"sample_size_select" == 500 ~ 9.52, 
                           input$"sample_size_select" == 1000 ~ 6.32, 
                           input$"sample_size_select" == 10000 ~ 7.48), 
                         "C.I Coverage (95%)" = round((sum(df$true_var < df$UB & df$true_var > df$LB) / nrow(df))*100,2)))
    
    
    ggplot(df_jit, aes(x = c(1:nrow(df_jit)), y = variance)) + 
      geom_point(aes(color = "red")) + 
      geom_errorbar(aes(ymin = LB, ymax = UB, alpha = 1)) + 
      coord_flip() + 
      theme_bw() + 
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

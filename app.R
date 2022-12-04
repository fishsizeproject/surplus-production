
library(shiny)
library(shinydashboard)
library(tidyverse)
library(MQMF)
# devtools::install_github("https://github.com/haddonm/MQMF")

ui <- 
    
    dashboardPage(
        dashboardHeader(title = "Surplus Production"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Home", tabName = "home_tab", icon = icon("dashboard")),
                menuItem("Surplus Production models 1", tabName = "surplus_prod", icon = icon("th")),
                menuItem("Surplus Production models", tabName = "aboutspms_tab", icon = icon("th"))
            )
        ),
        dashboardBody(
            tabItems(
                # Home page tab
                tabItem(tabName = "home_tab",
                        "Info about the website goes here"
                ),
                
                # Surplus Production tab
                tabItem(tabName = "surplus_prod",
                        h1("TITLE"),
                        br(), 
                        fluidRow(
                            column(width = 4, 
                                   
                                   
                                   box(title = "Adjust the sliders to set your parameters",
                                       uiOutput("all_spm_sliders"),
                                       width = 12
                                   )
                            ),
                            column(width = 8, 
                                   box(title = "BOX TITLE",
                                       plotOutput("spm_plot"),
                                       width = 12, 
                                       align="center"),
                                   box(title = "BOX TITLE 2",
                                       p("PARA 1"),
                                       p("PARA 2"),
                                       p("PARA 3"),
                                       width = 12)
                            )
                        )
                        
                ),
                
                
                # Second tab content
                tabItem(tabName = "aboutspms_tab",
                        fluidRow(
                            box(width = 12,
                                title = "Suplus production models",
                                div("Surplus production models assume that there is a biomass level (carrying capacity, K) that the stock cannot exceed. At low stock levels population growth rate is high, because there is little competition or cannibalism. As stock levels increase, the population growth rate slows down. When stock is at K level, the growth rate is 0, because the stock biomass cannot increase anymore."),
                                br(),
                                div("This is all very logical and intuitive, but the main question is - how quickly should this growth rate slow down? Should it slow down linearly, so that for each 10% increase in the biomass level, the growth rate decreases by 10%. Or perhaps initially the effect of density dependence is very small and the stock can grow very fast until it reaches some threshold level (say, 80% of K). Only then the growth rate starts decreasing rapidly. Assumptions about density depenence will have important consequences on the estimated sustainable fishing level."),
                                br(),
                                div("There are two main models that assume different density dependence assumptions. The basic one is Schaefer (1954, 1957), which assumes linear density dependence. Fox (1970) models assumes logarithmic density dependence, or in other words the decrease in r is logarithmically related to the increase in stock levels."),
                                br(),
                                div("This means that in the Schaefer assumes a symmetrical production curve, with the maximum surplus production and maximum sustainable yield (MSY) occurring at 50% of K. The Fox model assumes asymmetrical production curve with the maximum production at lower level of depletion or 37% of K. The Schaefer model therefore is more conservative than the Fox in that it requires the stock size to be higher for maximum production and generally leads to somewhat lower levels of catch."))),
                        fluidRow(
                            box(width = 6, 
                                plotOutput("plot1", height = 250)),
                            
                            box(
                                width = 6,
                                title = "Controls",
                                sliderInput("slider", "Number of observations:", 1, 100, 50)
                            )
                        )
                )
            )
        )
    )


server <- function(input, output) {
    
    
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    
    # Surplus production models server -----------------------------------------
    
    output$all_spm_sliders <- renderUI({
        list(shinyBS::popify(sliderInput("spm_slider_k",
                                         "Carrying capacity (K):",
                                         min = 100, max = 5000, value = 1000, step = 100),
                             "LABEL",
                             "LABEL"),
             shinyBS::popify(sliderInput("spm_slider_r",
                                         "Growth rate (r):",
                                         min = 0.05, max = 0.95, value = 0.3, step = 0.05),
                             "LABEL",
                             "LABEL"),
             uiOutput("spm_binit_slider"),
             shinyBS::popify(sliderInput("spm_slider_m1",
                                         "Proportion fished (Scenario 1):",
                                         min = 0.0, max = 1, value = 0.1, step = 0.05),
                             "LABEL",
                             "LABEL"),
             shinyBS::popify(sliderInput("spm_slider_m2",
                                         "Proportion fished (Scenario 2):",
                                         min = 0.0, max = 1, value = 0.2, step = 0.05),
                             "LABEL",
                             "LABEL")
             
        )
    })
    
    
    output$spm_binit_slider <- renderUI({
        shinyBS::popify(sliderInput("spm_slider_binit",
                                    "Initial stock biomass (B\u2080):",
                                    min = input$spm_slider_k*0.05,
                                    max = input$spm_slider_k,
                                    value = input$spm_slider_k*0.5,
                                    step = 10),
                        "Initial Biomass (B0)",
                        "How far the population was from carrying capacity when we started our surveys and analsyes. If surveys are started at the same time as start of fishing, set Binit to K, indicating that population was at carrying capacity and was not fished before (or at least not fished enough to have much effect on it). Remember, this is the value of exploitable biomass,not all biomass of the stock (we do not know much about juveniles and their abundance)")
    })
    
    
    
    
    output$spm_plot <- renderPlot({
        
        K <- input$spm_slider_k
        r <- input$spm_slider_r
        Binit <- input$spm_slider_binit
        u_m1 <- input$spm_slider_m1
        u_m2 <- input$spm_slider_m2
        
        Year <- c(1:100)
        Cat <- rep(NA, length = length(Year)) #catch 1 
        Bio <- rep(NA, length = length(Year)) # biomass 1
        Cat2 <- rep(NA, length = length(Year)) #catch 2 
        Bio2 <- rep(NA, length = length(Year)) # biomass 2
        Bio[1] <- Binit
        Cat[1] <- Binit*u_m1
        Bio2[1] <- Binit
        Cat2[1] <- Binit*u_m2
        
        for (i in 1:length(Cat)) {
            Bio[i+1] = Bio[i] + r*Bio[i]*(1-Bio[i]/K) - Cat[i]
            Cat[i+1] = Bio[i] * u_m1
            Bio2[i+1] = Bio2[i] + r*Bio2[i]*(1-Bio2[i]/K) - Cat2[i]
            Cat2[i+1] = Bio2[i] * u_m2
        }
        
        xlab1 <- "p_lab6" %>% rlang::sym()
        ylab1 <- "p_lab7" %>% rlang::sym()
        lab1 <- "p_lab8" %>% rlang::sym()
        
        f1 <- "p_lab15" %>% rlang::sym() %>% as.character()
        s2 <- "p_lab16" %>% rlang::sym()%>% as.character()
        
        
        tibble(t = 1:length(Bio), 
               biomass = Bio, 
               catch = Cat, 
               f_mortality = paste0(f1," (", u_m2, ")")) %>% 
            bind_rows(tibble(t = 1:length(Bio2), 
                             biomass = Bio2, 
                             catch = Cat2, 
                             f_mortality = paste0(s2," (", u_m1, ")"))) %>% 
            ggplot(aes(x = t, 
                       y = biomass,
                       col = f_mortality)) +
            geom_line(size = 1.5) +
            geom_line(aes(y = catch), lty = 2, size = 1.5) +
            ylim(0, K) +
            xlab(xlab1) +
            ylab(ylab1) +
            theme_bw(24) +
            theme(
                legend.position = c(.95, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6),
                legend.background = element_rect(colour = "black")
            ) +
            labs(colour = lab1) +
            geom_hline(yintercept = MQMF::getMSY(c(r,K,Binit=Binit, sigma=0.5), p=1.0), lty = 2) +
            scale_colour_manual(values = c("pink", "green"))
    })
    
    
}


shinyApp(ui, server)
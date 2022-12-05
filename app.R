
library(shiny)
library(bs4Dash)
library(tidyverse)
library(MQMF)
library(shinyBS)
library(plotly)
# devtools::install_github("https://github.com/haddonm/MQMF")


# User interface (UI) ----------------------------------------------------------
my_ui <-
    dashboardPage(
        title = "Surplus Production",
        header = dashboardHeader(
            title = dashboardBrand(
                title = "Surplus Production",
                color = "primary",
                image = "https://avatars.githubusercontent.com/u/99745785?s=400&u=eeb3a46b56826f1511e2c387374759c4c5fd109e&v=4"
            )
        ),
        sidebar = dashboardSidebar(
            sidebarMenu(
                menuItem(
                    text = "About SPMs", 
                    tabName = "aboutspms_tab", 
                    icon = icon("home")
                ),
                menuItem(
                    text = "Density dependence", 
                    tabName = "densdep_tab", 
                    icon = icon("list")
                ),
                menuItem(
                    text = "Parameters", 
                    tabName = "sp_modelling_tab", 
                    icon = icon("sliders-h")
                ),
                menuItem(
                    text = "More information",
                    tabName = "home_tab",
                    icon = icon("info-circle")
                )
            )
        ),
        body = dashboardBody(
            tabItems(
                
                ## About surplus production ------------------------------------
                tabItem(
                    tabName = "aboutspms_tab",
                    fluidRow(
                        box(width = 12,
                            title = "Surplus production models",
                            div("Surplus production models assume that there is a biomass level (carrying capacity, K)
                                        that the stock cannot exceed. At low stock levels population growth rate is high,
                                        because there is little competition or cannibalism. As stock levels increase, the 
                                        population growth rate slows down. When stock is at K level, the growth rate is 0, 
                                        because the stock biomass cannot increase anymore."),
                            br(),
                            div("This is all very logical and intuitive, but the main question is - how quickly should 
                                        this growth rate slow down? Should it slow down linearly, so that for each 10% increase 
                                        in the biomass level, the growth rate decreases by 10%. Or perhaps initially the effect 
                                    of density dependence is very small and the stock can grow very fast until it reaches some 
                                    threshold level (say, 80% of K). Only then the growth rate starts decreasing rapidly. 
                                    Assumptions about density depenence will have important consequences on the estimated 
                                    sustainable fishing level.")
                        )
                    ),
                ),
                
                ## Density dependence ------------------------------------------
                
                tabItem(
                    tabName = "densdep_tab",
                    fluidRow(
                        box(width = 12,
                            title = "Density dependence",
                            div("There are two main models that assume different density dependence assumptions. 
                                    The basic one is Schaefer (1954, 1957), which assumes linear density dependence. 
                                    Fox (1970) models assumes logarithmic density dependence, or in other words the 
                                    decrease in r is logarithmically related to the increase in stock levels."),
                            br(),
                            div("This means that in the Schaefer assumes a symmetrical production curve, 
                                    with the maximum surplus production and maximum sustainable yield (MSY) 
                                    occurring at 50% of K. The Fox model assumes asymmetrical production curve 
                                    with the maximum production at lower level of depletion or 37% of K. 
                                    The Schaefer model therefore is more conservative than the Fox in that 
                                    it requires the stock size to be higher for maximum production and 
                                    generally leads to somewhat lower levels of catch.")
                        )
                    ),
                    fluidRow(
                        column(
                            width = 4,
                            box(
                                width = 12,
                                title = "Paramter selection",
                                div(
                                    title = "hover over me",
                                    sliderInput(inputId = "densdep_k", 
                                                label = "Param k",
                                                min = 100,
                                                max = 5000,
                                                value = 1000,
                                                step = 100),
                                ),
                                
                                div(
                                    title = "hover over m2e",
                                    sliderInput("densdep_r", 
                                                "param r",
                                                min = 0.05, 
                                                max = 1, 
                                                value = 0.3, 
                                                step = 0.05),
                                )
                            ),
                            box(
                                title = "MSY outputs",
                                width = 12,
                                tableOutput("densdep_tab"), 
                                align="center"
                            )
                        ),
                        box(
                            width = 8,
                            title = "Visialisation",
                            div(
                                title = "hover over me2",
                                plotOutput("densdep_plot")
                            )
                        )
                    )
                    
                    
                ),
                
                ## Surplus production modelling --------------------------------
                tabItem(
                    tabName = "sp_modelling_tab",
                    fluidRow(                
                        
                        column(
                            width = 4, 
                            box(
                                title = "Parameter selection",
                                uiOutput("all_spm_sliders"),
                                width = 12
                            )
                        ),
                        column(width = 8, 
                               box(title = "Spawning stock biomass (SSB) timeseries",
                                   plotlyOutput("spm_plot"),
                                   width = 12, 
                                   align="center")#,
                               # box(title = "BOX TITLE 2",
                               #     p("PARA 1"),
                               #     p("PARA 2"),
                               #     p("PARA 3"),
                               #     width = 12)
                        )
                    )
                    
                ),
                
                
                ## About page --------------------------------------------------
                tabItem(
                    tabName = "home_tab",
                    h3("Title of the homepage"),
                    p("Info about the website goes here"),
                    p("Para 2"),
                    img(src='logos_english-removebg-preview.png', align = "center")
                    
                )
                
            )
        )
    )

# Server -----------------------------------------------------------------------
my_server <- function(input, output, session) {
    
    
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    
    ## Surplus production modelling --------------------------------------------
    
    output$all_spm_sliders <- renderUI({
        
        
        list(
            tags$div(
                title = "Carrying Capacity is the maximum biomass the system can hold.",
                sliderInput(
                    inputId = "spm_slider_k",
                    label = "Carrying capacity (K):",
                    min = 100,
                    max = 5000,
                    value = 1000,
                    step = 100
                )
            ),
            
            tags$div(
                title = "'r' refers to the intrinsic growth rate of the species.",
                sliderInput("spm_slider_r",
                            "Growth rate (r):",
                            min = 0.05,
                            max = 0.95, 
                            value = 0.3, 
                            step = 0.05)
            ),
            
            tags$div(
                title = "The biomass when time = zero",
                uiOutput("spm_binit_slider"),
            ),
            
            tags$div(
                title = "The fishing mortality (F) for the first scenario.",
                sliderInput("spm_slider_m1",
                            "Proportion fished (Scenario 1):",
                            min = 0.0, 
                            max = 1, 
                            value = 0.1, 
                            step = 0.05),
            ),
            
            tags$div(
                title = "The fishing mortality (F) for the second scenario.",
                sliderInput("spm_slider_m2",
                            "Proportion fished (Scenario 2):",
                            min = 0.0, 
                            max = 1, 
                            value = 0.2, 
                            step = 0.05)
            )
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
                        "How far the population was from carrying capacity when we started our surveys and analsyes. 
                        If surveys are started at the same time as start of fishing, set Binit to K, indicating that 
                        population was at carrying capacity and was not fished before (or at least not fished enough 
                        to have much effect on it). Remember, this is the value of exploitable biomass,not all biomass 
                        of the stock (we do not know much about juveniles and their abundance)")
    })
    
    
    
    
    output$spm_plot <- renderPlotly({
        
        req(input$spm_slider_k)
        req(input$spm_slider_r)
        req(input$spm_slider_binit)
        req(input$spm_slider_m1)
        req(input$spm_slider_m2)
        
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
        
        f1 <- "#1" %>% rlang::sym() %>% as.character()
        f2 <- "#2" %>% rlang::sym()%>% as.character()
        
        hline = MQMF::getMSY(c(r,K,Binit=Binit, sigma=0.5), p=1.0)
        
        
        tab1 <-
            tibble(t = 1:length(Bio), 
                   biomass = Bio, 
                   catch = Cat, 
                   f_mortality = paste0(f1," (", u_m1, ")")) %>% 
            bind_rows(tibble(t = 1:length(Bio2), 
                             biomass = Bio2, 
                             catch = Cat2, 
                             f_mortality = paste0(f2," (", u_m2, ")"))) 
        
        tab1 |>
            plot_ly(x = ~t, 
                    y = ~biomass) |> 
            add_lines(linetype = ~f_mortality) |> 
            layout(title = '', 
                   plot_bgcolor = "transparent", 
                   xaxis = list(title = 'Time (years)', hoverformat = '1f'), 
                   yaxis = list(title = 'Biomass (Tonnes)', hoverformat = '.1f'), 
                   legend = list(title=list(text='Scenario')))
        
        # 
        # tab1 |> 
        #     ggplot(aes(x = t |> as.numeric(), 
        #                y = biomass |> as.numeric(),
        #                col = f_mortality|> as.factor())) +
        #     geom_line(size = 1.5) +
        #     geom_line(aes(y = catch), lty = 2, size = 1.5) +
        #     ylim(0, K) +
        #     xlab(xlab1) +
        #     ylab(ylab1) +
        #     theme_bw(24) +
        #     theme(
        #         legend.position = c(.95, .95),
        #         legend.justification = c("right", "top"),
        #         legend.box.just = "right",
        #         legend.margin = margin(6, 6, 6, 6),
        #         legend.background = element_rect(colour = "black")
        #     ) +
        #     labs(colour = lab1) +
        #     geom_hline(yintercept = hline, lty = 2) +
        #     scale_colour_manual(values = c("pink", "green"))
    })
    
    
    ## Density dependence ----------------------------------------------------
    
    
    
    output$densdep_plot <- renderPlot({
        req(input$densdep_k)
        req(input$densdep_r)
        
        ## Select K parameter - maximum biomass level or carrying capacity (any units are fine - kilograms, tons, it depends on the size of the stock)
        K <- input$densdep_k
        # select r parameter - population growth rate
        r <- input$densdep_r
        
        ### 
        ## useful other vectors 
        Bt <- 1:K  
        
        ## define functions 
        prodfun <- function(r,Bt,K,p) return((r*Bt/p)*(1-(Bt/K)^p))  
        densdep <- function(Bt,K,p) return((1/p)*(1-(Bt/K)^p)) 
        
        sp <- prodfun(r,Bt,K,1.0)  # Schaefer equivalent  
        sp0 <- prodfun(r,Bt,K,p=1e-08)  # Fox equivalent  
        
        mod1 <- "p_lab21" %>% rlang::sym() %>% as.character()
        mod2 <- "p_lab22" %>% rlang::sym()%>% as.character()
        lab1 <- "p_lab23" %>% rlang::sym() %>% as.character()
        lab2 <- "p_lab24" %>% rlang::sym()%>% as.character()
        
        mycols <- c("blue", "pink")
        names(mycols) <- c(mod1, mod2)
        
        transmod <- "col1" %>% rlang::sym() %>% as.character()
        
        tibble(Bt, sp = sp0 * (max(sp)/max(sp0)), Model = mod1) %>% 
            bind_rows(tibble(Bt, sp, Model = mod2)) %>% 
            rename(!!transmod := Model) %>% 
            ggplot(aes(x = Bt, 
                       y = sp)) + 
            aes_string(colour = as.character(transmod)) +
            geom_line(size = 2) +
            theme_bw(24) +
            # xlim(0, 3000) +
            # ylim(0, 800) +
            ylab(lab1) +
            xlab(lab2) +
            theme(
                legend.position = c(.95, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6),
                legend.background = element_rect(colour = "black"),
                plot.title = element_text(hjust = 0.5)
            ) +
            scale_colour_manual(values = mycols) +
            geom_vline(xintercept = Bt[which.max(sp0)], lty = 2, size = 1, col = "blue") +
            geom_vline(xintercept = Bt[which.max(sp)], lty = 2, size = 1, col = "pink")
        
        
    })
    
    
    output$densdep_tab <- renderTable({
        
        ## Select K parameter - maximum biomass level or carrying capacity (any units are fine - kilograms, tons, it depends on the size of the stock)
        K <- input$densdep_k
        # select r parameter - population growth rate
        r <- input$densdep_r
        
        ###
        ## useful other vectors
        Bt <- 1:K
        
        ## define functions
        prodfun <- function(r,Bt,K,p) return((r*Bt/p)*(1-(Bt/K)^p))
        densdep <- function(Bt,K,p) return((1/p)*(1-(Bt/K)^p))
        
        sp <- prodfun(r,Bt,K,1.0)  # Schaefer equivalent
        sp0 <- prodfun(r,Bt,K,p=1e-08)  # Fox equivalent
        
        
        tlab1 <- "tab_lab1" %>% rlang::sym() %>% as.character()
        tlab2 <- "tab_lab2" %>% rlang::sym()%>% as.character()
        tlab3 <- "tab_lab3" %>% rlang::sym() %>% as.character()
        tlab4 <- "tab_lab4" %>% rlang::sym()%>% as.character()
        tlab5 <- "tab_lab5" %>% rlang::sym()%>% as.character()
        
        tibble(" " = c(tlab1, 
                       tlab5, 
                       tlab2),
               !!tlab3 := c(input$densdep_k, 
                            Bt[which.max(sp0)]/K, 
                            Bt[which.max(sp0)]), #for Pella model
               !!tlab4 :=  c(input$densdep_k, 
                             Bt[which.max(sp)]/K, 
                             Bt[which.max(sp)]) #for Schaefer model
        )
        
    })
    
    
}


shinyApp(ui = my_ui, 
         server = my_server)

library(shiny)
library(bs4Dash)
library(tidyverse)
library(MQMF)
library(shinyBS)
library(plotly)
library(shiny.i18n)
# devtools::install_github("https://github.com/haddonm/MQMF")

i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")


# User interface (UI) ----------------------------------------------------------
my_ui <-
    dashboardPage(
        header = dashboardHeader(
            usei18n(i18n),
            title = dashboardBrand(
                title = "Surplus Production" |> i18n$t(),
                color = "primary",
                image = "https://avatars.githubusercontent.com/u/99745785?s=400&u=eeb3a46b56826f1511e2c387374759c4c5fd109e&v=4"
            )
        ),
        sidebar = dashboardSidebar(
            usei18n(i18n),
            sidebarMenu(
                menuItem(
                    text = "About SPMs"  |> i18n$t(), 
                    tabName = "aboutspms_tab", 
                    icon = icon("home")
                ),
                menuItem(
                    text = "Parameters" |> i18n$t(), 
                    tabName = "sp_modelling_tab", 
                    icon = icon("sliders-h")
                ),
                menuItem(
                    text = "Density dependence"  |> i18n$t(), 
                    tabName = "densdep_tab", 
                    icon = icon("list")
                ),
                
                menuItem(
                    text = "More information" |> i18n$t(),
                    tabName = "home_tab",
                    icon = icon("info-circle")
                ),
                br(),
                selectInput("lang_select", 
                            label = NULL, 
                            choices = list("English" = "en", 
                                           "Lietuvių" = "lt"), 
                            selected = "en")
            )
        ),
        body = dashboardBody(
            usei18n(i18n),
            tabItems(
                
                ## About surplus production ------------------------------------
                tabItem(
                    tabName = "aboutspms_tab",
                    fluidRow(
                        
                        box(width = 12,
                            title = "Surplus production models",
                            # First paragraph
                            div("Surplus production models assume that there is a biomass level (carrying capacity, K) that the stock cannot exceed. At low stock levels population growth rate is high, because there is little competition or cannibalism. At low biomass levels population growth rate is close to the maximum population growth rate, defined by the parameter r. As stock levels increase, the population growth rate slows down. When stock is at the level defined by K, population biomass does not increase anymore and growth rate is 0. Fishing removes some biomass, but population biomass increases due to its growth rate."  |> i18n$t()),
                            br(), # line break between paragraphs
                            div(img(src = "elephant.jpg", 
                                    width = "50%"), 
                                align = "center"),
                            br(),
                            # Second paragraph
                            div("Population resilience and sustainable harvesting rate depends on the growth rate r. The maximum sustainable yield (MSY) will also depend on the absolute population carrying capacity K."  |> i18n$t()),
                            br(), 
                            div(img(src = "elephant2.jpg", 
                                    width = "50%"), 
                                align = "center"),
                            br(),
                            # Third paragraph
                            div("In the next tab you can explore how population biomass and yield will change depending on the fishing mortality level under the most common assumption that maximum sustainable yield is achieved when population biomass is at 50% of its maximum or unfished level (K). This is the Schaefer model. In this model the equilibrium maximum fishing mortality which leads to MSY is equal to half of population regeneration rate r. You can play with different r and fishing mortality F values and see how higher fishing mortality may lead to lower long-term yields. Also explore how similar yields can be achived at very different stock biomass levels. Ideally we want to maximise both the yields and the spawning stock biomass, as this will lead to a more resilient population. Remember, maximum sustainble yield (MSY) is the MAXIMUM recommended yield and not the target yield. Due to many uncertainties and environmental impacts the actual yield should be lower."  |> i18n$t()),
                            br(),
                            div("To see our other models"|> i18n$t(), 
                                a(href = "https://fishsizeproject.github.io/models/", "click here" |> i18n$t()),
                                "or to our project website"|> i18n$t(),
                                a(href = "https://sif.lt", "click here" |> i18n$t()))
                        )
                    ),
                ),
                
                ## Density dependence ------------------------------------------
                
                tabItem(
                    tabName = "densdep_tab",
                    fluidRow(
                        box(width = 12,
                            title = "Density dependence",
                            div("There are two main models that assume different density dependence assumptions. The basic one is Schaefer (1954, 1957), which assumes linear density dependence. Fox (1970) models assumes logarithmic density dependence, or in other words the decrease in r is logarithmically related to the increase in stock levels."  |> i18n$t()),
                            br(),
                            div("This means that in the Schaefer assumes a symmetrical production curve, with the maximum surplus production and maximum sustainable yield (MSY) occurring at 50% of K. The Fox model assumes asymmetrical production curve with the maximum production at lower level of depletion or 37% of K. The Schaefer model therefore is more conservative than the Fox in that 
                                    it requires the stock size to be higher for maximum production and generally leads to somewhat lower levels of catch."  |> i18n$t())
                        )
                    ),
                    fluidRow(
                        column(
                            width = 4,
                            box(
                                width = 12,
                                title = "Parameter selection" |> i18n$t(),
                                div(
                                    # title = "hover over me" |> i18n$t(),
                                    sliderInput(inputId = "densdep_k", 
                                                label = "Carrying capacity (K)" |> i18n$t(),
                                                min = 100,
                                                max = 5000,
                                                value = 1000,
                                                step = 100),
                                ),
                                
                                div(
                                    # title = "hover over m2e"  |> i18n$t(),
                                    sliderInput("densdep_r", 
                                                "Growth rate (r)"|> i18n$t(),
                                                min = 0.05, 
                                                max = 1, 
                                                value = 0.3, 
                                                step = 0.01),
                                )
                            ),
                            box(
                                title = "MSY outputs"  |> i18n$t(),
                                width = 12,
                                tableOutput("densdep_tab"), 
                                align="center"
                            )
                        ),
                        box(
                            width = 8,
                            title = "Visialisation",
                            div(
                                # title = "hover over me2",
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
                                title = "Parameter selection"  |> i18n$t(),
                                uiOutput("all_spm_sliders"),
                                width = 12
                            )
                        ),
                        column(width = 8, 
                               box(title = "Spawning stock biomass (SSB) timeseries"  |> i18n$t(),
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
                    h3("Title of the homepage"  |> i18n$t()),
                    p("Info about the website goes here"  |> i18n$t()),
                    p("Para 2"  |> i18n$t()),
                    img(src='logos_english-removebg-preview.png', align = "center")
                    
                )
                
            )
        )
    )

# Server -----------------------------------------------------------------------
my_server <- function(input, output, session) {
    
    
    observeEvent(input$lang_select,{
        update_lang(session, input$lang_select)
    })
    
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
                title = "Carrying Capacity is the maximum biomass the system can hold."  |> i18n$t(),
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
                title = "'r' refers to the intrinsic growth rate of the species."  |> i18n$t(),
                sliderInput("spm_slider_r",
                            "Growth rate (r):",
                            min = 0.05,
                            max = 0.95, 
                            value = 0.3, 
                            step = 0.01)
            ),
            
            tags$div(
                title = "The biomass when time = zero"  |> i18n$t(),
                uiOutput("spm_binit_slider"),
            ),
            
            tags$div(
                title = "The fishing mortality (F) for the first scenario."  |> i18n$t(),
                sliderInput("spm_slider_m1",
                            "Proportion fished (Scenario 1):" |> i18n$t(),
                            min = 0.0, 
                            max = 1, 
                            value = 0.1, 
                            step = 0.01),
            ),
            
            tags$div(
                title = "The fishing mortality (F) for the second scenario."  |> i18n$t(),
                sliderInput("spm_slider_m2",
                            "Proportion fished (Scenario 2):" |> i18n$t(),
                            min = 0.0, 
                            max = 1, 
                            value = 0.2, 
                            step = 0.01)
            )
        )
    })
    
    
    output$spm_binit_slider <- renderUI({
        shinyBS::popify(sliderInput("spm_slider_binit",
                                    "Initial stock biomass (B\u2080):" |> i18n$t(),
                                    min = input$spm_slider_k*0.05,
                                    max = input$spm_slider_k,
                                    value = input$spm_slider_k*0.5,
                                    step = 10),
                        "Initial Biomass (B0)" |> i18n$t(),
                        "How far the population was from carrying capacity when we started our surveys and analsyes. If surveys are started at the same time as start of fishing, set Binit to K, indicating that population was at carrying capacity and was not fished before (or at least not fished enough to have much effect on it). Remember, this is the value of exploitable biomass,not all biomass of the stock (we do not know much about juveniles and their abundance)"  |> i18n$t())
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
        
        hline = MQMF::getMSY(c(r,K,Binit=Binit, sigma=0.5), p=1.0)
        
        
        tab1 <-
            tibble(t = 1:length(Bio), 
                   biomass = Bio |> as.numeric(), 
                   catch = Cat  |> as.numeric(), 
                   f_mortality_biomass = paste0("Biomass (#1: " |> i18n$t(), u_m1, ")"),
                   f_mortality_yield = paste0("Yield (#1: " |> i18n$t(), u_m1, ")")) %>% 
            bind_rows(tibble(t = 1:length(Bio2), 
                             biomass = Bio2 |> as.numeric(), 
                             catch = Cat2 |> as.numeric(), 
                             f_mortality_biomass = paste0("Biomass (#2: " |> i18n$t(), u_m1, ")"),
                             f_mortality_yield = paste0("Yield (#2: " |> i18n$t(), u_m1, ")"))) 
        
        tab1 |>
            plot_ly(x = ~t, 
                    y = ~biomass) |> 
            add_lines(linetype = ~f_mortality_biomass, 
                      mode = "lines") |> 
            add_lines(y = ~catch, 
                      linetype =  ~f_mortality_yield,
                      mode = "lines") |> 
            layout(title = '', 
                   plot_bgcolor = "transparent", 
                   xaxis = list(title = 'Time (years)' |> i18n$t(), hoverformat = '1f'), 
                   yaxis = list(title = 'Biomass (Tonnes)'  |> i18n$t(), hoverformat = '.1f'), 
                   legend = list(title=list(text='Scenario'  |> i18n$t())))
        
        # 
        # tab1 |> 
        #     ggplot(aes(x = t |> as.numeric(), 
        #                y = biomass |> as.numeric(),
        #                col = f_mortality|> as.factor())) +
        #     geom_line(linewidth = 1.5) +
        #     geom_line(aes(y = catch), lty = 2, linewidth = 1.5) +
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
        
        mycols <- c("#1B9E77", "#D95F02")
        names(mycols) <- c("Fox" |> i18n$t(), 
                           "Schaefer" |> i18n$t())
        
        transmod <- "Model" %>% rlang::sym() %>% as.character()
        
        tibble(Bt, 
               sp = sp0 * (max(sp)/max(sp0)), 
               Model = "Fox"  |> i18n$t()) %>% 
            bind_rows(tibble(Bt, 
                             sp, 
                             Model = "Schaefer" |> i18n$t())) %>% 
            rename(!!transmod := Model) %>% 
            ggplot(aes(x = Bt, 
                       y = sp)) + 
            aes_string(colour = as.character(transmod)) +
            geom_line(linewidth = 2) +
            theme_bw(24) +
            # xlim(0, 3000) +
            # ylim(0, 800) +
            ylab("Yield (tonnes)" |> i18n$t()) +
            xlab("Stock biomass (tonnes)" |> i18n$t()) +
            theme(
                legend.position = c(.95, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6),
                legend.background = element_rect(colour = "black"),
                plot.title = element_text(hjust = 0.5)
            ) +
            scale_colour_manual(values = mycols) +
            geom_vline(xintercept = Bt[which.max(sp0)], lty = 2, linewidth = 1, col = mycols[1]) +
            geom_vline(xintercept = Bt[which.max(sp)], lty = 2, linewidth = 1, col = mycols[2])
        
        
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
        
        
        tlab3 <- "Fox"  |> i18n$t() %>% rlang::sym() %>% as.character()
        tlab4 <- "Schaefer"  |> i18n$t() %>% rlang::sym() %>% as.character()
        
        tibble(" " = c("Carrying capacity (K)" |> i18n$t(), 
                       "MSY proportion of K" |> i18n$t(), 
                       "Stock biomass to produce MSY" |> i18n$t(), 
                       "Maximum sustainable yield (MSY)" |> i18n$t() ),
               !!tlab3 := c(input$densdep_k, 
                            Bt[which.max(sp0)]/K, 
                            Bt[which.max(sp0)],
                            max(sp0 * (max(sp)/max(sp0)))), #for Fox model
               !!tlab4 :=  c(input$densdep_k, 
                             Bt[which.max(sp)]/K,
                             Bt[which.max(sp)], 
                             max(sp)) #for Schaefer model
        )
        
    })
    
    
}


shinyApp(ui = my_ui, 
         server = my_server)
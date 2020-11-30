
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

load("incur_av_loss_betas")
load("incur_cost_sav_betas")
load("incur.maxpot.betas")
load("saved_av_loss_betas")
load("saved_cost_sav_betas")
load("saved.maxpot.betas")

load("newincur_av_loss_betas")
load("newincur_cost_sav_betas")
load("newincur.maxpot.betas")
load("newsaved_av_loss_betas")
load("newsaved_cost_sav_betas")
load("newsaved.maxpot.betas")


# Define UI for application
ui = fluidPage(
    
    # Application title
    titlePanel("Add New Data to BRC Estimates"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("tactic",
                        "What Tactic?",
                        c("Conservation" = "1",
                          "Resource Isolation" = "2",
                          "Input Substitution" = "3",
                          "Inventories" = "4",
                          "Excess Capacity" = "5",
                          "Relocation" = "6",
                          "Management Effectiveness" = "7",
                          "Import Substitution" = "8",
                          "Technological Change" = "9",
                          "Production Recapture" = "10",
                          "Resource Pooling" = "11")),
            selectInput("type",
                        "Was Cost or Savings?",
                        c("Cost" = "cost",
                          "Savings" = "saving")),
            selectInput("industry",
                        "Which Industry?",
                        c("Ag, Mining, Construction" = "1",
                          "Transportation, Communication, Utilities" = "2",
                          "Manufacturing" = "3",
                          "Wholesale, Retail, Trade" = "4",
                          "Finance, Insurance, Real Estate" = "5",
                          "Service Sectors" = "6")),
            numericInput("employees",
                         "How many Employees?",
                         value = 55),
            numericInput("damage",
                         "How much property damage ($)?",
                         value = 100000),
            checkboxInput("damdummy",
                          "Property damage incurred?",
                          value = TRUE),
            sliderInput("communications",
                        "Percent BI due to communications:",
                        min = 0,
                        max = 100,
                        value = 30),
            sliderInput("moved",
                        "Percent BI due to employees moving away:",
                        min = 0,
                        max = 100,
                        value = 10),
            sliderInput("cantwork",
                        "Percent BI due to employees being unable to work:",
                        min = 0,
                        max = 100,
                        value = 10),
            sliderInput("gas",
                        "Percent BI due to gas outages:",
                        min = 0,
                        max = 100,
                        value = 10),
            sliderInput("power",
                        "Percent BI due to power outages:",
                        min = 0,
                        max = 100,
                        value = 10),
            sliderInput("supply",
                        "Percent BI due to supply chain:",
                        min = 0,
                        max = 100,
                        value = 10),
            sliderInput("transport",
                        "Percent BI due to transportation:",
                        min = 0,
                        max = 100,
                        value = 10),
            sliderInput("water",
                        "Percent BI due to water outages:",
                        min = 0,
                        max = 100,
                        value = 10),
            
            
            submitButton("Submit", icon("refresh"))
            
            
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h1("BCR Estimates:"),
            textOutput("bcrText"),
            h3(" "),
            plotOutput("bcrPlot"),
            h1("RM Estimates:"),
            textOutput("rmText"),
            h3(" "),
            plotOutput("rmPlot"),
            h1("RCE Estimates (per $10,000):"),
            textOutput("rceText"),
            h3(" "),
            plotOutput("rcePlot")
            
            
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    
    
    output$bcrText = renderText({
        
        tactic = diag(nrow = 10)
        tactic = rbind(rep(0, 10), tactic)
        
        industry = diag(nrow = 5) 
        industry = rbind(rep(0,5), industry)
        
        user.value = c(1,
                       tactic[as.numeric(input$tactic), ],
                       industry[as.numeric(input$industry),],
                       log(input$damage),
                       as.numeric(input$damdummy),
                       log(input$employees),
                       input$communications,
                       input$moved, 
                       input$cantwork, 
                       input$gas,
                       input$power,
                       input$supply,
                       input$transport, 
                       input$water)
        
        
        if(input$type == "cost") {
            
            cost = exp(user.value %*% incur.cost_sav.betas)
            avloss = exp(user.value %*% incur.av_loss.betas)
            
            newcost = exp(user.value %*% newincur.cost_sav.betas)
            newavloss = exp(user.value %*% newincur.av_loss.betas)
            
        } else {
            
            cost = exp(user.value %*% saved.cost_sav.betas)
            avloss = exp(user.value %*% saved.av_loss.betas)
            
            newcost = exp(user.value %*% newsaved.cost_sav.betas)
            newavloss = exp(user.value %*% newsaved.av_loss.betas)
        }
        
        BCR = as.vector(avloss / cost)
        newBCR = as.vector(newavloss / newcost)
        
        paste("The average BCR for a firm in your industry is ",
              round(mean(BCR, na.rm = TRUE), 3),
              ". The median BCR for a firm in your industry is ",
              round(median(BCR, na.rm = TRUE), 3),  round(median(newBCR, na.rm = TRUE), 3),
              ". The 25th percentile is ",
              round(quantile(BCR, .25, na.rm = TRUE),3),
              " and the 75th percentile is ",
              round(quantile(BCR, .75, na.rm = TRUE),3))
        
    })
    
    
    
    
    
    output$bcrPlot <- renderPlot({
        
        tactic = diag(nrow = 10)
        tactic = rbind(rep(0, 10), tactic)
        
        industry = diag(nrow = 5) 
        industry = rbind(rep(0,5), industry)
        
        user.value = c(1,
                       tactic[as.numeric(input$tactic), ],
                       industry[as.numeric(input$industry),],
                       log(input$damage),
                       as.numeric(input$damdummy),
                       log(input$employees),
                       input$communications,
                       input$moved, 
                       input$cantwork, 
                       input$gas,
                       input$power,
                       input$supply,
                       input$transport, 
                       input$water)
        
        if(input$type == "cost") {
            
            cost = exp(user.value %*% incur.cost_sav.betas)
            avloss = exp(user.value %*% incur.av_loss.betas)
            
            newcost = exp(user.value %*% newincur.cost_sav.betas)
            newavloss = exp(user.value %*% newincur.av_loss.betas)
            
        } else {
            
            cost = exp(user.value %*% saved.cost_sav.betas)
            avloss = exp(user.value %*% saved.av_loss.betas)
            
            newcost = exp(user.value %*% newsaved.cost_sav.betas)
            newavloss = exp(user.value %*% newsaved.av_loss.betas)
        }
        
        BCR = as.vector(avloss / cost)
        newBCR = as.vector(newavloss / newcost)
        BCR = as.data.frame(BCR, newBCR)
        
        xband = c(0, quantile(BCR$BCR, probs = .95))
        
        BCR %>%
            ggplot(aes(x = BCR)) +
            geom_histogram(aes(y=..density..), colour="black", fill="white") +
            geom_density(alpha=.2, fill= "red") + 
            geom_density(aes(x = newBCR), alpha = .6, fill = "lightseagreen") +
            theme_classic() +
            xlim(xband) +
            xlab("BCR Prediction") +
            ylab("Density") +
            ggtitle("BCR Prediction Distribution") +
            geom_vline(xintercept = median(BCR$BCR, na.rm = TRUE), lwd=1, linetype=2, color="black") +
            geom_vline(xintercept = median(newBCR, na.rm = TRUE), lwd=1, linetype=2, color="lightseagreen")
        
    })
    
    output$rmText = renderText({
        
        tactic = diag(nrow = 10)
        tactic = rbind(rep(0, 10), tactic)
        
        industry = diag(nrow = 5) 
        industry = rbind(rep(0,5), industry)
        
        user.value = c(1,
                       tactic[as.numeric(input$tactic), ],
                       industry[as.numeric(input$industry),],
                       log(input$damage),
                       as.numeric(input$damdummy),
                       log(input$employees),
                       input$communications,
                       input$moved, 
                       input$cantwork, 
                       input$gas,
                       input$power,
                       input$supply,
                       input$transport, 
                       input$water)
        
        user.value2 = c(1,
                        industry[as.numeric(input$industry),],
                        log(input$damage),
                        as.numeric(input$damdummy),
                        log(input$employees),
                        input$communications,
                        input$moved, 
                        input$cantwork, 
                        input$gas,
                        input$power,
                        input$supply,
                        input$transport, 
                        input$water)
        
        
        if(input$type == "cost") {
            
            maxpot = exp(user.value2 %*% incur.maxpot.betas)
            avloss = exp(user.value %*% incur.av_loss.betas)
            
            newmaxpot = exp(user.value2 %*% newincur.maxpot.betas)
            newavloss = exp(user.value %*% newincur.av_loss.betas)
            
        } else {
            
            maxpot = exp(user.value2 %*% saved.maxpot.betas)
            avloss = exp(user.value %*% saved.av_loss.betas)
            
            newmaxpot = exp(user.value2 %*% newsaved.maxpot.betas)
            newavloss = exp(user.value %*% newsaved.av_loss.betas)
        }
        
        RM = as.vector(avloss / maxpot)
        RM = ifelse(RM > 1, NA, RM)
        NACOUNT = sum(is.na(RM))
        
        newRM = as.vector(newavloss / newmaxpot)
        newRM = ifelse(newRM > 1, NA, newRM)
        newNACOUNT = sum(is.na(newRM))
        
        paste("The average RM for a firm in your industry is ",
              round(mean(RM, na.rm = TRUE), 3),
              ". The median RM for a firm in your industry is ",
              round(median(RM, na.rm = TRUE), 3), round(median(newRM, na.rm = TRUE), 3),
              ". The 25th percentile is ",
              round(quantile(RM, .25, na.rm = TRUE),3),
              " and the 75th percentile is ",
              round(quantile(RM, .75, na.rm = TRUE),3),
              "Count filtered = ", NACOUNT, newNACOUNT)
        
    })
    
    output$rmPlot <- renderPlot({
        
        tactic = diag(nrow = 10)
        tactic = rbind(rep(0, 10), tactic)
        
        industry = diag(nrow = 5) 
        industry = rbind(rep(0,5), industry)
        
        user.value = c(1,
                       tactic[as.numeric(input$tactic), ],
                       industry[as.numeric(input$industry),],
                       log(input$damage),
                       as.numeric(input$damdummy),
                       log(input$employees),
                       input$communications,
                       input$moved, 
                       input$cantwork, 
                       input$gas,
                       input$power,
                       input$supply,
                       input$transport, 
                       input$water)
        
        user.value2 = c(1,
                        industry[as.numeric(input$industry),],
                        log(input$damage),
                        as.numeric(input$damdummy),
                        log(input$employees),
                        input$communications,
                        input$moved, 
                        input$cantwork, 
                        input$gas,
                        input$power,
                        input$supply,
                        input$transport, 
                        input$water)
        
        if(input$type == "cost") {
            
            maxpot = exp(user.value2 %*% incur.maxpot.betas)
            avloss = exp(user.value %*% incur.av_loss.betas)
            
            newmaxpot = exp(user.value2 %*% newincur.maxpot.betas)
            newavloss = exp(user.value %*% newincur.av_loss.betas)
            
        } else {
            
            maxpot = exp(user.value2 %*% saved.maxpot.betas)
            avloss = exp(user.value %*% saved.av_loss.betas)
            
            newmaxpot = exp(user.value2 %*% newsaved.maxpot.betas)
            newavloss = exp(user.value %*% newsaved.av_loss.betas)
            
        }
        
        RM = as.vector(avloss / maxpot)
        RM = ifelse(RM > 1, NA, RM)
        
        newRM = as.vector(newavloss / newmaxpot)
        newRM = ifelse(newRM > 1, NA, newRM)
        
        RM = as.data.frame(RM, newRM)
        xband = c(0, quantile(RM$RM, probs = .95, na.rm = TRUE))
        
        RM %>%
            ggplot(aes(x = RM)) +
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill= "blue") +
            geom_density(aes(x = newRM), alpha = .6, fill = "lightseagreen") +
            theme_classic() +
            xlim(xband) +
            xlab("RM Prediction") +
            ylab("Density") +
            ggtitle("RM Prediction Distribution") +
            geom_vline(xintercept = median(RM$RM, na.rm = TRUE), lwd=1, linetype=2, color="black") +
            geom_vline(xintercept = median(newRM, na.rm = TRUE), lwd=1, linetype=2, color="lightseagreen")
        
    })
    
    
    
    output$rceText = renderText({
        
        tactic = diag(nrow = 10)
        tactic = rbind(rep(0, 10), tactic)
        
        industry = diag(nrow = 5) 
        industry = rbind(rep(0,5), industry)
        
        user.value = c(1,
                       tactic[as.numeric(input$tactic), ],
                       industry[as.numeric(input$industry),],
                       log(input$damage),
                       as.numeric(input$damdummy),
                       log(input$employees),
                       input$communications,
                       input$moved, 
                       input$cantwork, 
                       input$gas,
                       input$power,
                       input$supply,
                       input$transport, 
                       input$water)
        
        user.value2 = c(1,
                        industry[as.numeric(input$industry),],
                        log(input$damage),
                        as.numeric(input$damdummy),
                        log(input$employees),
                        input$communications,
                        input$moved, 
                        input$cantwork, 
                        input$gas,
                        input$power,
                        input$supply,
                        input$transport, 
                        input$water)
        
        
        if(input$type == "cost") {
            
            maxpot = exp(user.value2 %*% incur.maxpot.betas)
            avloss = exp(user.value %*% incur.av_loss.betas)
            cost = exp(user.value %*% incur.cost_sav.betas)
            
            newmaxpot = exp(user.value2 %*% newincur.maxpot.betas)
            newavloss = exp(user.value %*% newincur.av_loss.betas)
            newcost = exp(user.value %*% newincur.cost_sav.betas)
            
        } else {
            
            maxpot = exp(user.value2 %*% saved.maxpot.betas)
            avloss = exp(user.value %*% saved.av_loss.betas)
            cost = exp(user.value %*% saved.cost_sav.betas)
            
            newmaxpot = exp(user.value2 %*% newsaved.maxpot.betas)
            newavloss = exp(user.value %*% newsaved.av_loss.betas)
            newcost = exp(user.value %*% newsaved.cost_sav.betas)
        }
        
        RM = as.vector(avloss / maxpot)
        RM = ifelse(RM > 1, NA, RM)
        NACOUNT = sum(is.na(RM))
        RCE = RM / cost * 10000
        
        newRM = as.vector(newavloss / newmaxpot)
        newRM = ifelse(newRM > 1, NA, newRM)
        newNACOUNT = sum(is.na(newRM))
        newRCE = newRM / cost * 10000
        
        paste("The average RCE for a firm in your industry is ",
              round(mean(RCE, na.rm = TRUE), 3),
              ". The median RCE for a firm in your industry is ",
              round(median(RCE, na.rm = TRUE), 3), round(median(newRCE, na.rm = TRUE), 3),
              ". The 25th percentile is ",
              round(quantile(RCE, .25, na.rm = TRUE),3),
              " and the 75th percentile is ",
              round(quantile(RCE, .75, na.rm = TRUE),3),
              "Count filtered = ", NACOUNT, newNACOUNT)
        
    })
    
    output$rcePlot <- renderPlot({
        
        tactic = diag(nrow = 10)
        tactic = rbind(rep(0, 10), tactic)
        
        industry = diag(nrow = 5) 
        industry = rbind(rep(0,5), industry)
        
        user.value = c(1,
                       tactic[as.numeric(input$tactic), ],
                       industry[as.numeric(input$industry),],
                       log(input$damage),
                       as.numeric(input$damdummy),
                       log(input$employees),
                       input$communications,
                       input$moved, 
                       input$cantwork, 
                       input$gas,
                       input$power,
                       input$supply,
                       input$transport, 
                       input$water)
        
        user.value2 = c(1,
                        industry[as.numeric(input$industry),],
                        log(input$damage),
                        as.numeric(input$damdummy),
                        log(input$employees),
                        input$communications,
                        input$moved, 
                        input$cantwork, 
                        input$gas,
                        input$power,
                        input$supply,
                        input$transport, 
                        input$water)
        
        if(input$type == "cost") {
            
            maxpot = exp(user.value2 %*% incur.maxpot.betas)
            avloss = exp(user.value %*% incur.av_loss.betas)
            cost = exp(user.value %*% incur.cost_sav.betas)
            
            newmaxpot = exp(user.value2 %*% newincur.maxpot.betas)
            newavloss = exp(user.value %*% newincur.av_loss.betas)
            newcost = exp(user.value %*% newincur.cost_sav.betas)
            
        } else {
            
            maxpot = exp(user.value2 %*% saved.maxpot.betas)
            avloss = exp(user.value %*% saved.av_loss.betas)
            cost = exp(user.value %*% saved.cost_sav.betas)
            
            newmaxpot = exp(user.value2 %*% newsaved.maxpot.betas)
            newavloss = exp(user.value %*% newsaved.av_loss.betas)
            newcost = exp(user.value %*% newsaved.cost_sav.betas)
        }
        
        RM = as.vector(avloss / maxpot)
        RM = ifelse(RM > 1, NA, RM)
        RCE = RM / as.vector(cost) *10000
        
        newRM = as.vector(newavloss / newmaxpot)
        newRM = ifelse(newRM > 1, NA, newRM)
        newRCE = newRM / as.vector(newcost) *10000
        
        RCE = as.data.frame(RCE, newRCE)
        xband = c(0, quantile(RCE$RCE, probs = .95, na.rm = TRUE))
        
        RCE %>%
            ggplot(aes(x = RCE)) +
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill= "yellow") + 
            geom_density(aes(x = newRCE), alpha = .6, fill = "lightseagreen")+
            theme_classic() +
            xlim(xband) +
            xlab("RCE Prediction") +
            ylab("Density") +
            ggtitle("RCE Prediction Distribution") +
            geom_vline(xintercept = median(RCE$RCE, na.rm = TRUE), lwd=1, linetype=2, color="black") +
            geom_vline(xintercept = median(newRCE, na.rm = TRUE), lwd=1, linetype=2, color = "lightseagreen")    
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)



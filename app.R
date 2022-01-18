library(shiny)
library(tidyverse)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(shinydashboard)
source("./Data_preprocessing.R")
source("./Helper.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    title = "MAPD", 
    
    # Application title
    #titlePanel("Protein Degradability Prediction"),
    
    navbarPage(p(strong("MAPD"), style="font-size:130%"), 
               theme = shinytheme("united"), id = "navbar", 
               ###### Home Tab ##########
               tabPanel(p("Home",
                          style="font-size:130%"), fluid = T, 
                        fluidRow(
                            column(
                                8, 
                                p(style = "font-size:100%;text-align:right",
                                  a(strong('User Visiting Statistics'), href='https://datastudio.google.com/u/0/reporting/dc354ba3-7a07-4759-851a-be9a74f165ba/page/v7TbC',target="_blank")),
                                h4(p(strong("Welcome to the MAPD!"), style="text-align:left")),
                                p("This web platform incorporates protein-intrinsic features,
                                MAPD scores (predicted degradability), E2 accessibility of Ub sites in select kinases, ligandability, and disease associations.
                                It enbales rational prioritization of degradable targets for developing degraders by the TPD community.",
                                style="text-align:left; color:black;font-size:110%;"),
                                br(),
                                sidebarPanel(width = 16, 
                                    p(strong("Use this platform to:"),style="text-align:left;font-size:120%"),
                                    p(actionLink("to_priori", strong("Prioritization: ")), 
                                      "prioritize tractable targets for developing protein degraders",
                                      style="text-align:left; color:black;font-size:110%;"),
                                    p(actionLink("to_feature", strong("Features: ")), 
                                      "explore features intrinsic to protein targets",
                                      style="text-align:left; color:black;font-size:110%;"),
                                    p(actionLink("to_UB_char", strong("UBs: ")), 
                                      "check characteristics (e.g., E2 accessibility) of ubiquitination sites in select kinases",
                                      style="text-align:left; color:black;font-size:110%;"),
                                    p(actionLink("to_download", strong("Downloads: ")),
                                      "download all relevant data",
                                      style="text-align:left; color:black;font-size:110%;"),
                                    p(actionLink("to_ex_link", strong("External link: ")),
                                      "access other relevant databases",
                                      style="text-align:left; color:black;font-size:110%;"),
                                    br(),
                                    # p(strong("Source Code"),
                                    #   style="text-align:justify;color:black; font-size:150%"),
                                    p("To reproduce or extend our analysis, please visit ", 
                                      a("HERE", href = "https://liulab-dfci.github.io/MAPD/index.html"), ".",
                                      style="text-align:justify;font-size:110%;")
                                ),
                                br(),
                                hr(),
                                em(strong("Cite us!")),
                                div(style="font-size:110%;margin-bottom:8px",
                                    p(" Wubing Zhang*, Shourya S. Roy Burman*, Jiaye Chen, Katherine A. Donovan, Yang Cao, Boning Zhang, Zexian Zeng, Yi Zhang, Dian Li, Eric S. Fischer#, Collin Tokheim#, X. Shirley Liu#. Machine learning modeling of protein-intrinsic features predicts tractability of targeted protein degradation.",
                                      strong(em("bioRxiv")), "2021",
                                      a("[DOI]", href="https://doi.org/10.1101/2021.09.27.462040",target="_blank")),
                                ),

                                p(style = "font-size:100%;text-align:left",
                                  a(strong('User Visiting Statistics'), href='https://datastudio.google.com/u/0/reporting/dc354ba3-7a07-4759-851a-be9a74f165ba/page/v7TbC',target="_blank")),
                                
                                br()
                                
                            )
                        )
               ),
               ###### Prioritization Tab ##########
               tabPanel(p("Prioritization",
                          style="font-size:130%"), 
                        value = "Priori", fluid = TRUE,
                        fluidRow(
                            column(3, selectizeInput("Gene", 
                                                     p(strong("Gene or Protein(Uniprot ID):"), style="font-size:110%"),
                                                     choices = NULL, multiple = T)),
                            column(3, textAreaInput("Genetext", p(strong("OR enter a list of genes or proteins:"), 
                                                                  style="font-size:100%"), 
                                          placeholder = "Enter a list of genes or proteins HERE.",
                                          width = "250px",
                                          height = "60px")),
                            column(
                                3, selectizeInput("Column", p(strong("Column:"), style="font-size:110%"), 
                                                  choices = NULL, multiple = T))
                            ),
                        hr(),
                        fluidRow(
                            column(
                                12,tabsetPanel(type = "tabs",
                                               tabPanel("Degradability & Ligandability",
                                                        br(),
                                                        DT::dataTableOutput("Prioritizations")),
                                               tabPanel("Column Description",
                                                        br(),
                                                        DT::dataTableOutput("Database_Description"))
                                )
                            )
                            
                        )
                        
               ),
               ###### Feature Tab ##########
               tabPanel(title = p("Features",
                                  style="font-size:130%"), 
                        value = "feature", fluid = TRUE,
                        fluidRow(
                            column(
                                3, selectizeInput("Gene2", p(strong("Gene or Protein(Uniprot ID):"), 
                                                             style="font-size:110%"),
                                                  choices = NULL,
                                                  multiple = T)),
                            column(
                                6,selectInput("Features", p(strong("Feature:"), 
                                                            style="font-size:110%"),
                                              choices = c("ALL", All_features),
                                              selected = c("Ubiquitination_2", "Acetylation_1", 
                                                           "Phosphorylation_2", 
                                                           "Zecha2018_Hela_Halflife", "Length"),
                                              multiple = T)
                            ),
                            column(
                                3,selectInput("Category", p(strong("Category:"), 
                                                            style="font-size:110%"),
                                              choices = c("ALL", All_Categories),
                                              selected = NULL, multiple = T)
                            )
                            ),
                            # column(
                            #     4, selectizeInput("Protein2", "Protein (Uniprot ID):",
                            #                       choices = NULL,
                            #                       multiple = T),
                            #     textAreaInput("Proteintext2", "", 
                            #                   placeholder = "Enter a list of proteins HERE.",
                            #                   width = "400px",
                            #                   height = "200px")
                            # ),
                        hr(),
                        fluidRow(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Features",
                                                 br(),
                                                 DT::dataTableOutput("Protein_Features")),
                                        tabPanel("Feature Description",
                                                 br(),
                                                 DT::dataTableOutput("Feature_Description"))
                            )
                        )
                        
               ),
               
               ###### E2 Accessibility Tab ##########
               tabPanel(p("UBs",
                          style="font-size:130%"), fluid = TRUE,
                        value = "UB_char",
                        fluidRow(
                            column(
                                4, selectizeInput("Gene3", p(strong("Gene or Protein(Uniprot ID):"), 
                                                             style="font-size:110%"),
                                                  choices = NULL,
                                                  multiple = T)),
                                #br(),
                            column(
                                4, textAreaInput("Genetext3", "Enter a list of genes or proteins:", 
                                              placeholder = "Enter a list of genes or proteins HERE.",
                                              width = "250px",
                                              height = "60px")
                            ),
                            # column(
                            #     4, selectizeInput("Protein3", "Protein (Uniprot ID):",
                            #                       choices = NULL,
                            #                       multiple = T),
                            #     #br(),
                            #     textAreaInput("Proteintext3", "", 
                            #                   placeholder = "Enter a list of proteins HERE.",
                            #                   width = "400px",
                            #                   height = "200px")
                            # )
                        ),
                        hr(),
                        fluidRow(
                            tabsetPanel(type = "tabs",
                                        tabPanel("E2 Accessibility",
                                                 br(),
                                                 DT::dataTableOutput("E2_table")),
                                        tabPanel("Column Description",
                                                 br(),
                                                 DT::dataTableOutput("E2_Description"))
                            )
                        )
                        
               ),
               ###### Downloads Tab ##########
               tabPanel(p("Downloads", style="font-size:130%"), fluid = T,
                        # fluidRow(
                        #     column(8,
                        #            h3(p("Source Table")),
                        #            br(),
                        #            hr()
                        #     )),
                        value = "download",
                        fluidRow(
                            column(12,
                                   h3("Features"), br(),
                                   p(downloadButton("FeatureTable"), " Table of Features used for Prediction", style="font-size:110%"), br(),
                                   p(downloadButton("FeatureDes"), " Table of Feature Description", style="font-size:110%"), br()),
                            column(12,
                                   h3("Degradability & Ligandability"), br(),
                                   p(downloadButton("LigandabilityTable"), " Table of Protein Degradability & Ligandability", style="font-size:110%"),br(),
                                   p(downloadButton("LigandabilityDes"), " Column Description of Protein Degradability & Ligandability", style="font-size:110%"),br()),
                            column(12,
                                   h3("E2 Accessibility"), br(),
                                   p(downloadButton("E2Table"), " Table of E2 Accessibility", style="font-size:110%"),br(),
                                   p(downloadButton("E2Des"), " Column Description of E2 Accessibility", style="font-size:110%"),br())
                        )
               ),
               ###### External Link ##########
               tabPanel(p("External Link", style="font-size:130%"), fluid = T,
                        value = "ex_link",
                        fluidRow(
                            h3("Related Database"), br(),
                            column(3, br(),
                                   h4(a(img(src="DFCI_Proteomics_icon.png", height = "40px"),
                                     href = "https://proteomics.fischerlab.org/")),br(),
                                h4(a(img(src="PROTACDB_icon.png", height = "40px"),
                                     href = "http://cadd.zju.edu.cn/protacdb/about")),br(),
                                h4(a(img(src="PROTACpedia_icon.png", height = "40px"),
                                     href = "https://protacpedia.weizmann.ac.il/ptcb/main")),br()
                            ),
                            column(
                                4,br(),
                                h4(a(img(src="Drugbank_icon.png", height = "40px"),
                                     href = "https://go.drugbank.com/")),br(),
                                h4(a(img(src="CHEMBL_icon.png", height = "40px"),
                                     href = "https://www.ebi.ac.uk/chembl/")),br(),
                                h4(a(img(src="SLCABPP_icon.png", height = "40px"),
                                     href = "http://wren.hms.harvard.edu/cysteine_viewer/")),br()
                            ),
                            column(
                                3,br(),
                                h4(a(img(src="OncoKB_icon.png", height = "40px"),
                                     href = "https://www.oncokb.org")),br(),
                                h4(a(img(src="Clinvar_icon.jpeg", height = "40px"),
                                     href = "https://www.ncbi.nlm.nih.gov/clinvar/")),br()
                            )
                            
                        )
                        
               )
               # tabPanel(p("Statistics", style="font-size:125%"),
               #          fluid = T,
               #          htmlOutput("stats")
               # )
               
    )
)

##### Server #####
server <- function(input, output, session){
    
    # Update selectize input
    updateSelectizeInput(session, 'Gene', 
                         choices = c("ALL", as.character(unique(Preds$Gene))), 
                         selected = "ALL", server = TRUE)
    updateSelectizeInput(session, 'Gene2', 
                         choices = c("ALL", as.character(unique(Feature_info$Gene))), 
                         selected = "ALL", server = TRUE)
    updateSelectizeInput(session, 'Gene3', 
                         choices = c("ALL", as.character(unique(E2_table$Gene))), 
                         selected = "ALL", server = TRUE)
    updateSelectizeInput(session, 'Features', 
                         choices = c("ALL",unique(colnames(Feature_info)[-(1:2)])), 
                         selected = c("Ubiquitination_2", "Acetylation_1", "Phosphorylation_2",
                                      "Zecha2018_Hela_Halflife", "Length"), server = TRUE)
    updateSelectizeInput(session, 'Column', 
                         choices = c("ALL", unique(colnames(Preds)[-(1:2)])), 
                         selected = c("MAPD.score", "PROTACdb", "PROTACpedia",
                                      "DrugBank", "ChEMBL", "OncoKB", "Clinvar"), server = TRUE)
    
    getFeatures <- function(){
        temp_feature <- ""
        All_features = colnames(Feature_info)[-(1:2)]
        if ("ALL" %in% input$Features){
            if (length(input$Features) > 1){
                temp_feature <- setdiff(input$Features, "ALL")
            } else{
                temp_feature <- All_features
            }
        } else{
            temp_feature <- input$Features %>% as.character()
        }
        
        temp_Cate <- ""
        if ("ALL" %in% input$Category){
            if (length(input$Category) > 1){
                temp_Cate <- setdiff(input$Category, "ALL")
            } else{
                temp_Cate <- unique(Feature_cate$Category)
            }
        } else{
            temp_Cate <- input$Category
        }
        temp_feature = c(temp_feature, Feature_cate$Column[Feature_cate$Category%in%temp_Cate])
        return(temp_feature)
    }
    
    getColumns <- function(){
        temp_cols <- ""
        All_Cols = colnames(Preds)[-(1:2)]
        if ("ALL" %in% input$Column){
            if (length(input$Column) > 1){
                temp_cols <- setdiff(input$Column, "ALL")
            } else{
                temp_cols <- All_Cols
            }
        } else{
            temp_cols <- input$Column %>% as.character()
        }
        return(temp_cols)
    }
    
    observeEvent(input$to_priori, {
        newvalue <- "Priori"
        updateTabItems(session, "navbar", newvalue)
    })
    observeEvent(input$to_feature, {
        newvalue <- "feature"
        updateTabsetPanel(session, "navbar", newvalue)
    })
    observeEvent(input$to_UB_char, {
        newvalue <- "UB_char"
        updateTabsetPanel(session, "navbar", newvalue)
    })
    observeEvent(input$to_download, {
        newvalue <- "download"
        updateTabsetPanel(session, "navbar", newvalue)
    })
    observeEvent(input$to_ex_link, {
        newvalue <- "ex_link"
        updateTabsetPanel(session, "navbar", newvalue)
    })
    
    ###### Prioritization Tab #####################
    output$Prioritizations <- DT::renderDT({
        
        temp_gene <- ""
        if ("ALL" %in% input$Gene){
            if (length(input$Gene) > 1){
                temp_gene <- setdiff(input$Gene, "ALL")
            } else{
                temp_gene <- c(unique(Preds$Gene), unique(Preds$Entry))
            }
        } else{
            temp_gene <- input$Gene
        }
        
        # temp_Protein <- ""
        # if ("ALL" %in% input$Protein){
        #     if (length(input$Protein) > 1){
        #         temp_Protein <- input$Protein[2:length(input$Protein)]
        #     } else{
        #         temp_Protein <- unique(Preds$Entry)
        #     }
        # } else{
        #     temp_Protein <- input$Protein
        # }
        
        if (input$Genetext != ""){
            temp_gene <- str_split(input$Genetext, "[,\n][ ]*", simplify = T)
        }
        # if (input$Proteintext != ""){
        #     temp_Protein <- str_split(input$Proteintext, "[,\n][ ]*", simplify = T)
        # }
        
        temp_table <- Preds  %>%
            dplyr::select(c("Gene", "Entry", getColumns())) %>% 
        filter(Gene %in% temp_gene | Entry %in% temp_gene) 
        
        temp_table$Gene <- createLink(temp_table$Entry, temp_table$Gene)
        temp_table
        
    }, escape = FALSE, options = list(scrollX = 500), filter = "top")
    
    output$Database_Description <- renderDataTable({
        temp_table <- Preds  %>% dplyr::select(c("Gene", "Entry", getColumns()))
        temp_des_tab <- read.table("./Data/Prioritization_Description.txt", 
                                   sep = "\t", header = TRUE, quote = "")
        rownames(temp_des_tab) <- temp_des_tab$Column
        temp_des_tab[colnames(temp_table), ]
        
    }, escape = FALSE, options = list(scrollX = 500))
    
    ###### Feature Tab #######
    output$Protein_Features <- DT::renderDT({
        temp_gene <- ""
        if ("ALL" %in% input$Gene2){
            if (length(input$Gene2) > 1){
                temp_gene <- setdiff(input$Gene2, "ALL")
            } else{
                temp_gene <- c(unique(Feature_info$Gene), unique(Feature_info$Entry))
            }
        } else{
            temp_gene <- input$Gene2
        }
        
        temp_feature <- getFeatures()
        
        temp_table <- Feature_info %>%
            dplyr::select(c("Gene", "Entry", temp_feature)) %>%
            filter(Gene %in% temp_gene | Entry %in% temp_gene) 
        
        temp_table$Gene <- createLink(temp_table$Entry, temp_table$Gene)
        temp_table
        
    }, escape = FALSE, options = list(scrollX = 500), filter = "top")
    
    output$Feature_Description <- renderDataTable({
        
        temp_feature <- getFeatures()
        temp_table <- Feature_info %>%
            dplyr::select(c("Gene", "Entry", temp_feature))
        temp_des_tab <- read.table("./Data/Features_Description.txt", sep = "\t", header = TRUE)
        rownames(temp_des_tab) = temp_des_tab$Column
        temp_des_tab[colnames(temp_table), ]
        
    }, escape = FALSE, options = list(scrollX = 500))
    
    ###### E2 Accessibility Tab ######
    output$E2_table <- DT::renderDataTable({
        
        temp_gene <- ""
        if ("ALL" %in% input$Gene3){
            if (length(input$Gene3) > 1){
                temp_gene <- setdiff(input$Gene3, "ALL")
            } else{
                temp_gene <- c(unique(E2_table$Gene), unique(E2_table$Entry))
            }
        } else{
            temp_gene <- input$Gene3
        }
        
        # temp_Protein <- ""
        # if ("ALL" %in% input$Protein3){
        #     if (length(input$Protein3) > 1){
        #         temp_Protein <- input$Protein3[2:length(input$Protein3)]
        #     } else{
        #         temp_Protein <- unique(E2_table$Uniprot_AC)
        #     }
        # } else{
        #     temp_Protein <- input$Protein3
        # }
        
        if (input$Genetext3 != ""){
            temp_gene <- str_split(input$Genetext3, "[,\n][ ]*", simplify = T)
        }
        # if (input$Proteintext3 != ""){
        #     temp_Protein <- str_split(input$Proteintext3, "[,\n][ ]*", simplify = T)
        # }
        
        temp_table <- E2_table %>% filter(Gene %in% temp_gene | Entry %in% temp_gene) 
        
        #temp_table$GeneID <- createLink(temp_table$Uniprot_AC, temp_table$GeneID)
        temp_table[,unlist(lapply(temp_table, is.numeric))] <- 
            round(temp_table[,unlist(lapply(temp_table, is.numeric))], 3)
        temp_table
        
    }, escape = FALSE, options = list(scrollX = 500))
    
    output$E2_Description <- renderDataTable({
        
        temp_des_tab <- read.table("./Data/E2_Acc_Description.txt", 
                                   sep = "\t", header = TRUE, quote = "")
        temp_des_tab
        
    }, escape = FALSE, options = list(scrollX = 500))
    
    ###### Downloads Tab ##################
    output$FeatureTable <- downloadHandler(
        filename = "Feature_Table.tsv",
        content = function(file) {
            write_tsv(read.table("./Data/Features.txt", sep = "\t", 
                                 header = TRUE, quote = ""),
                      file)
        }
    )
    
    output$FeatureDes <- downloadHandler(
        filename = "Feature_Description.tsv",
        content = function(file) {
            write_tsv(read.table("./Data/Features_Description.txt", 
                                 sep = "\t", header = TRUE, quote = ""),
                      file)
        }
    )
    
    output$LigandabilityTable <- downloadHandler(
        filename = "Prioritization.tsv",
        content = function(file) {
            write_tsv(read.table("./Data/Prioritization.txt", sep = "\t", 
                                 header = TRUE, quote = ""), file)
        }
    )
    
    output$LigandabilityDes <- downloadHandler(
        filename = "Description_Prioritization.tsv",
        content = function(file) {
            write_tsv(read.table("./Data/Prioritization_Description.txt", 
                                 sep = "\t", header = TRUE, quote = ""),
                      file)
        }
    )
    
    output$E2Table <- downloadHandler(
        filename = "Lysine_E2_Acessibility.tsv",
        content = function(file) {
            write_tsv(read.table("./Data/E2_Acc.txt", sep = "\t", header = TRUE, quote = ""),
                      file)
        }
    )
    
    output$E2Des <- downloadHandler(
        filename = "Description_Lysine_E2_Acessibility.tsv",
        content = function(file) {
            write_tsv(read.table("./Data/E2_Acc_Description.txt", sep = "\t", 
                                 header = TRUE, quote = ""),
                      file)
        }
    )
    
    ############# Statistics #########
    # getPage <- function() {
    #     return(tags$iframe(src = ""
    #                        , style="width:100%;",  frameborder="0"
    #                        ,id="iframe"
    #                        , height = "500px"))
    # }
    # output$stats<-renderUI({
    #     getPage()
    # })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

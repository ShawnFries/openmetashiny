library(shiny)
#library(shinydashboard)
library(shinyjs)
#library(shinyBS)
#library(DT)
library(metafor)
#library(dplyr)

# dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem(div(img(src="meta_analysis.png"), "Data"), tabName = "data"),
#       #add image to menuItem text
#       #https://stackoverflow.com/questions/24705431/how-can-i-insert-an-image-into-the-navbar-on-a-shiny-navbarpage
#       menuItem("Meta-analysis", tabName = "meta", icon = icon("th")),
#       menuItem("Cumulative meta-analysis", tabName = "cumulative", icon = icon("dashboard")),
#       menuItem("Subgroup meta-analysis", tabName = "subgroup", icon = icon("th")),
#       menuItem("Meta-regression", tabName = "meta-reg", icon = icon("dashboard")),
#       menuItem("Leave-one-out meta-analysis", tabName = "leave-one-out", icon = icon("dashboard"))
#     )
#   ),#ends dashboardSidebar
#   dashboardBody(
#     tabItems(
#       tabItem(tabName="data"),
#       tabItem(tabName="meta"),
#       tabItem(tabName="cumulative"),
#       tabItem(tabName="subgroup"),
#       tabItem(tabName="meta-reg"),
#       tabItem(tabName="leave-one-out")
#     )
#   )#ends dashboardBody
# )

ui <- function(session) {
  fluidPage(
    useShinyjs(),
    conditionalPanel('output.page == 1',
                     titlePanel("What language would you like to use? (Welche Sprache möchten Sie benutzen?)"),
                     sidebarLayout(
                       sidebarPanel(selectInput("language",
                                                "Language (Sprache)",
                                                c("English"="en", "Deutsch"="de")
                                               )
                                   ),
                       mainPanel()
                    ),
                    actionButton("continueButton", "Continue (Weiter)")
    ),
    conditionalPanel('output.page == 2',  # TODO: Add German translation for each of these
                     titlePanel("Would you like to import/enter your own data or try one the sample datasets?"),
                     sidebarLayout(
                       sidebarPanel(selectInput("is_real_data",
                                                "User Input or Sample Data",
                                                c("Input my own"="user_input", "Sample"="sample")
                       )
                       ),
                       mainPanel()
                     ),
                     actionButton("continueButton_real_or_sample", "Continue")
    ),
    conditionalPanel('output.page == 3 && input.language == "en" && input.is_real_data == "user_input"',
                     titlePanel("What type of data do you have?"),
                     sidebarLayout(
                         sidebarPanel(selectizeInput("dataType",
                                                     "Data Type",
                                                     #TODO: Do we need this if not starting from scratch? Could just have an option to import (and infer datatype)
                                                     # or start from one of the below as a backup..
                                                     list("One piece of data from each study/studies within one group"=
                                                            c("proportion ( x ⁄ N )"="proportion",
                                                              "mean (μ)"="mean",
                                                              "event count over time (x and t)"="event count",
                                                              "regression coefficient (β)"="regression coefficient",
                                                              "Cronbach's α (aka tau-equivalent reliability, ρᴛ)"="cronbach alpha",
                                                              "generic effect size (θ, se)"="generic effect size"),
                                                          "Data on two groups per study"=
                                                            c("proportions ( x₁ ⁄ N₁ vs x₂ ⁄ N₂ )"="proportions",
                                                              "means (μ₁ vs μ₂)"="means",
                                                              "event counts over time (x₁ and t₁ vs x₂ and t₂)"="event counts",
                                                              "raw mean difference ( x₁ - x₂ )"="mean difference"),
                                                          "Data on test performance"=
                                                            c("diagnostic (TP┼FP┼FN┼TN)"="diagnostic")
                                                         )
                                                    )
                                     ),
                         mainPanel()),
                       actionButton("continueButton2", "Continue")
    ), conditionalPanel('output.page == 3 && input.language == "de" && input.is_real_data == "user_input"',
                        titlePanel("Welche Art von Daten haben Sie?"),
                        sidebarLayout(
                          sidebarPanel(selectizeInput("dataType_de",
                                                      "Datentyp",
                                                      #TODO: Do we need this if not starting from scratch? Could just have an option to import (and infer datatype)
                                                      # or start from one of the below as a backup..
                                                      list("Ein Teil der Daten von jeden Studie/Studien in einen Gruppe"=
                                                             c("Proportion ( x ⁄ N )"="proportion_de",
                                                               "mean (μ)"="mean_de",
                                                               "event count over time (x and t)"="event count_de",
                                                               "regression coefficient (β)"="regression coefficient_de",
                                                               "Cronbach's α (aka tau-equivalent reliability, ρᴛ)"="cronbach alpha_de",
                                                               "generic effect size (θ, se)"="generic effect size_de"
                                                              ),
                                                           "Daten on two groups per study"=
                                                             c("Proportionen ( x₁ ⁄ N₁ vs x₂ ⁄ N₂ )"="proportions_de",
                                                               "means (μ₁ vs μ₂)"="means_de",
                                                               "event counts over time (x₁ and t₁ vs x₂ and t₂)"="event counts_de",
                                                               "raw mean difference ( x₁ - x₂ )"="mean difference_de"
                                                              ),
                                                           "Daten an Prüfung performance"=
                                                             c("diagnostic (TP┼FP┼FN┼TN)"="diagnostic_de")
                                                      )
                          )
                          ),
                          mainPanel()),
                        actionButton("continueButton2_de", "Weiter")
    ),
    conditionalPanel('output.page == 3 && input.language == "en" && input.is_real_data == "sample"',
                     titlePanel("Which sample dataset would you like to use?"),
                     sidebarLayout(
                       sidebarPanel(selectInput("is_real_data",
                                                "User Input or Sample Data",
                                                c("Normand (1999) - Data on length of hospital stay under stroke patients under specialized care (m1i i.e. mean 1) vs under routine care (m2i)"="normand_study",
                                                  "de Bruin (2009) - Information about standard care quality and HAART-adherence in control groups (one mean per study)"="de_bruin_study",
                                                  "Egger (2001 - Effectiveness of intravenous magnesium in the prevention of death following acute myocardial infarction (ai is number of deaths in magnesium group, ci is for control group; 2 proportion data)."="egger_study"
                                                  
                                                 )
                                                # Consider including Curtis study (also 2 means like Normand)
                                                # TODO: Also consider including Trikalinos study using person-time data (dat.fine1993); need to make sure this would work with e1i, e2i etc for this data type
                       )
                       ),
                       mainPanel()),
                     actionButton("continueButton2", "Continue")
    ), conditionalPanel('output.page == 3 && input.language == "de" && input.is_real_data == "sample"',
                        titlePanel("Welche Art von Daten haben Sie?"),
                        sidebarLayout(
                          sidebarPanel(selectizeInput("dataType_de",
                                                      "Datentyp",
                                                      #TODO: Do we need this if not starting from scratch? Could just have an option to import (and infer datatype)
                                                      # or start from one of the below as a backup..
                                                      list("Ein Teil der Daten von jeden Studie/Studien in einen Gruppe"=
                                                             c("Proportion ( x ⁄ N )"="proportion_de",
                                                               "mean (μ)"="mean_de",
                                                               "event count over time (x and t)"="event count_de",
                                                               "regression coefficient (β)"="regression coefficient_de",
                                                               "Cronbach's α (aka tau-equivalent reliability, ρᴛ)"="cronbach alpha_de",
                                                               "generic effect size (θ, se)"="generic effect size_de"
                                                             ),
                                                           "Daten on two groups per study"=
                                                             c("Proportionen ( x₁ ⁄ N₁ vs x₂ ⁄ N₂ )"="proportions_de",
                                                               "means (μ₁ vs μ₂)"="means_de",
                                                               "event counts over time (x₁ and t₁ vs x₂ and t₂)"="event counts_de",
                                                               "raw mean difference ( x₁ - x₂ )"="mean difference_de"
                                                             ),
                                                           "Daten an Prüfung performance"=
                                                             c("diagnostic (TP┼FP┼FN┼TN)"="diagnostic_de")
                                                      )
                          )
                          ),
                          mainPanel()),
                        actionButton("continueButton2_de", "Weiter")
    ), uiOutput("main_page"),
    
  tags$style(type='text/css', '.navbar { 
             font-family: Times;
             font-size: 30px;}'
  ))
}

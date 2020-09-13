# packages:
library(shiny)
library(shinythemes)
library(fmsb)
library(tidyverse)
library(glue)
library(infer)


# data & cleaning:
pkmn <- read_csv("Pokemon.csv")

correct_mega <- function(Name) # function to remove Pokemon name when preceding "Mega", as the Pokemon name is given after Mega anyways
{ 
  Name[str_detect(Name, ".+(?=Mega)")] <- Name[str_detect(Name, ".+(?=Mega)")] %>% str_remove(".+(?=Mega)")
  
  return(Name)
}

pkmn <- pkmn %>% 
  select(-Total, - Legendary) %>% # dropping the Total base stats and Legendary boolean columns as I am not interested in these
  mutate(Name = purrr::map_chr(Name, correct_mega)) # applying the above function to update the Name column

# setup for server side computation:
maxmin <- rbind(apply(pkmn[,5:10], 2, max), apply(pkmn[,5:10], 2, min)) # gets the max and min of each base stat column so that the radar charts can be plotted

generations_summary <- pkmn %>% # creates a data frame containing the average base stats grouped by Generation
  group_by(Generation) %>%
  summarize(HP = mean(HP), 
            Attack = mean(Attack),
            Defense = mean(Defense),
            `Sp. Atk` = mean(`Sp. Atk`),
            `Sp. Def` = mean(`Sp. Def`),
            Speed = mean(Speed))

types_counts1 <- pkmn %>% # creates a data frame containing the counts of Pokemon grouped by Generation and Type 1 (renamed to Type afterward)
  group_by(Generation, `Type 1`) %>%
  summarize(Counts = n()) %>%
  rename(Type = `Type 1`)
types_counts2 <- pkmn %>% # creates a data frame containing the counts of Pokemon grouped by Generation and Type 2 (renamed to Type afterward)
  group_by(Generation, `Type 2`) %>%
  summarize(Counts = n()) %>%
  rename(Type = `Type 2`)
types_counts <- left_join(types_counts1, types_counts2, by = c("Generation", "Type")) # joins the two data frames created above by Generation and Type
types_counts$Counts.y <- types_counts$Counts.y %>% replace_na(0) # replaces na values in the second Counts column (some Pokemon do not have a Type 2) with 0's
types_counts <- types_counts %>% # combines (through addition) the two Counts columns into one Counts column
  mutate(Counts = Counts.x + Counts.y) %>%
  select(Generation, Type, Counts)

  
# ui:
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = div(tags$img(height = 33, width = 33, src = "pokeball.png"), "Making Comparisons Within the World of Pokemon"), 
  windowTitle = "Comparisons in Pokemon",
  tabPanel(title = "Introduction",
           tags$h1("Introduction"),
           tags$p("This is a shiny app designed to aid Pokemon fans in choosing certain (mainline) Pokemon games or Pokemon to build their team with. 
                  All Pokemon games belong to a Generation, and most allow the player to play in only one region, with each Generation pertaining to a 
                  unique region. Each Pokemon originated from one of these regions, and are classified into Generations based on the region they originated
                  from."),
           tags$hr(),
           tags$h2("Using the App"),
           tags$p("The Analysis tab has three functionalities listed under it. Each of the three subtabs have useful tools to make comparisons within the
                  world of Pokemon."),
           tags$br(),
           tags$h3("Comparing Base Stats Between Individual Pokemon"),
           tags$p("Perhaps a player is interested in whether Mega Evolution increases a Pokemon's stats drastically. Maybe the player is stuck in a decision
                  between which starter Pokemon to choose. One factor some players take into consideration when building their Pokemon team is a Pokemon's 
                  stats. This tool is helpful in making a visual side by side comparison of two Pokemon's base stats, by way of radar charts."),
           tags$br(),
           tags$h3("Comparing Average Base Stats Between Pokemon Generations"),
           tags$p("Some players have preferences when it comes to stats. Some may prefer having Pokemon with high Attack or Special Attack stats, some may 
                  prefer having Pokemon with high Defense or Special Defense stats, and some may prefer having Pokemon with balanced stats all across. This 
                  tool allows players to make a visual side by side comparison of two Generations' average base stats, by way of radar charts, similar to the 
                  first tool. This can be helpful to those trying to choose a Pokemon Generation to play, as maybe some Generations have more Pokemon with the 
                  types of stats that a player prefers."),
           tags$br(),
           tags$h3("Comparing Regional Distributions of Pokemon Types"),
           tags$p("Some players have favorite types of Pokemon. This tool allows players to compare the distributions of types between two regions, and will 
                  also perform a chi-square test to determine whether the distributions are significantly different. With this information, players can choose 
                  which Pokemon region they want to play in if that region has more of a specific type that the player is interested in using during their 
                  gameplay."),
           tags$hr(),
           tags$h2("The Data"),
           tags$p("The data used in this shiny app pertains to Pokemon from Generations 1-6, and was found ",
                  tags$a("here.", href = "https://www.kaggle.com/abcsds/pokemon"))),
  navbarMenu(title = "Analysis",
             tabPanel(title = "Comparing Base Stats Between Individual Pokemon", 
                      column(6,
                             wellPanel(selectInput(inputId = "pkmn1", label = "Pokemon 1", choices = pkmn$Name, 
                                            selectize = TRUE),
                                plotOutput(outputId = "pkmn1stats"))),
                      column(6,
                             wellPanel(selectInput(inputId = "pkmn2", label = "Pokemon 2", choices = pkmn$Name, 
                                            selectize = TRUE),
                                plotOutput(outputId = "pkmn2stats")))
                      ),
             tabPanel(title = "Comparing Average Base Stats Between Pokemon Generations",
                      column(6,
                             wellPanel(selectInput(inputId = "gen1", label = "Generation", 
                                            choices = c("Generation 1" = 1,
                                                        "Generation 2" = 2,
                                                        "Generation 3" = 3,
                                                        "Generation 4" = 4,
                                                        "Generation 5" = 5,
                                                        "Generation 6" = 6)),
                                plotOutput(outputId = "gen1stats"))),
                     column(6,
                            wellPanel(selectInput(inputId = "gen2", label = "Generation", 
                                            choices = c("Generation 1" = 1,
                                                        "Generation 2" = 2,
                                                        "Generation 3" = 3,
                                                        "Generation 4" = 4,
                                                        "Generation 5" = 5,
                                                        "Generation 6" = 6)),
                                plotOutput(outputId = "gen2stats")))
                      ),
             tabPanel(title = "Comparing Regional Distributions of Pokemon Types", 
                      fluidRow(
                        column(6,
                               wellPanel(selectInput(inputId = "region1", label = "Region", 
                                            choices = c("Kanto" = 1,
                                                        "Johto" = 2,
                                                        "Hoenn" = 3,
                                                        "Sinnoh" = 4,
                                                        "Unova" = 5,
                                                        "Kalos" = 6)),
                                plotOutput(outputId = "region1bar"))),
                        column(6,
                                wellPanel(selectInput(inputId = "region2", label = "Region", 
                                            choices = c("Kanto" = 1,
                                                        "Johto" = 2,
                                                        "Hoenn" = 3,
                                                        "Sinnoh" = 4,
                                                        "Unova" = 5,
                                                        "Kalos" = 6)),
                                plotOutput(outputId = "region2bar")))),
                      fluidRow(column(4, offset = 4, tableOutput(outputId = "chisq_results")))
                      )
             )
)


# server:
server <- function(input, output) 
{
  output$pkmn1stats <- renderPlot({
    pkmn1 <- pkmn %>% filter(Name == input$pkmn1)
    radarchart(rbind(maxmin, pkmn1[1,5:10]), axistype = 1, pfcol = "blue",
               cglcol = "grey", cglty = 1, axislabcol = "grey", title = pkmn1[1,2])
  })
  
  output$pkmn2stats <- renderPlot({
    pkmn2 <- pkmn %>% filter(Name == input$pkmn2)
    radarchart(rbind(maxmin, pkmn2[1,5:10]), axistype = 1, pfcol = "blue",
               cglcol = "grey", cglty = 1, axislabcol = "grey", title = pkmn2[1,2])
  })
  
  output$gen1stats <- renderPlot({
    gen1 <- generations_summary %>% filter(Generation == input$gen1)
    radarchart(rbind(maxmin, gen1[1,2:7]), axistype = 1, pfcol = "blue",
               cglcol = "grey", cglty = 1, axislabcol = "grey", title = gen1[1,1])
  })
  
  output$gen2stats <- renderPlot({
    gen2 <- generations_summary %>% filter(Generation == input$gen2)
    radarchart(rbind(maxmin, gen2[1,2:7]), axistype = 1, pfcol = "blue",
               cglcol = "grey", cglty = 1, axislabcol = "grey", title = gen2[1,1])
  })
  
  output$region1bar <- renderPlot({
    types_counts %>% 
      filter(Generation == input$region1) %>%
      ggplot(mapping = aes(x = Type, y = Counts)) +
      geom_bar(stat = "identity", color = "red", fill = "grey") + 
      ggtitle(glue("Number of Pokemon of Each Type from Generation {input$region1}")) +
      theme_classic()
  })
  
  output$region2bar <- renderPlot({
    types_counts %>% 
      filter(Generation == input$region2) %>%
      ggplot(mapping = aes(x = Type, y = Counts)) +
      geom_bar(stat = "identity", color = "red", fill = "grey") + 
      ggtitle(glue("Number of Pokemon of Each Type from Generation {input$region2}")) +
      theme_classic()
  })
  
  output$chisq_results <- renderTable({
    types_counts_comp <- types_counts %>% filter(Generation == input$region1 | Generation == input$region2)
    types_counts_comp %>%
      mutate(Counts = factor(Counts),
             Generation = factor(Generation)) %>%
      chisq_test(explanatory = Generation,
                 response = Counts)
  })
}


# creating the app:
shinyApp(ui = ui, server = server)

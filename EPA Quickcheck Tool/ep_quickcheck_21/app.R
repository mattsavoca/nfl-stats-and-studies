library(easypackages)
# install.packages(
#   "shinydashboard",
#   "shinyWidgets",
#   "tidyverse",
#   "janitor",
#   "skimr",
#   "slider",
#   "tidymodels",
#   "tidytext",
#   "ggtext",
#   "plotly"
# )

my_wd = file.path("~", "Dropbox", "Matt Savoca", "Projects", "NFL 2021", "projects", "ep_quickcheck", "ep_quickcheck")
#setwd(my_wd)

packages(prompt = F,
         #shiny
         "shiny", "shinydashboard", "shinyWidgets",
         
         # EDA + data wrangling
         "tidyverse", "janitor","skimr", "slider", "tidymodels", "tidytext", "ggtext", "glue",
         
         # charting
         "plotly")

# Chart Theme -----
theme_set(theme_light())


# Data
proj = read_csv("./www/proj_16.csv")
flex_stats = read_csv("./www/2020_flex_stats.csv")
qb_stats = read_csv("./www/2020_qb_stats.csv")
flex_players = flex_stats %>% pull(player_name) %>% unique()
qb_players = qb_stats %>% pull(player_name) %>% unique()

# UI -----
ui <- dashboardPage(skin = "black",
                    dashboardHeader(),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("EP Graphs", tabName = "ep", icon = icon("chart-line")),
                        menuItem("FLEX Compare", tabName = "flex_compare", icon = icon("chart-line")),
                        menuItem("QB Compare", tabName = "qb_compare", icon = icon("chart-line"))
                      ),
                      radioButtons(
                        "window",
                        "Past Games Window",
                        choices = c("5" = 5, "8" = 8, "16" = 16),
                        selected = 8,
                        inline = TRUE
                      ),
                      pickerInput("picked", "Drafted Players", 
                                  choices = proj %>% pull(full_name), 
                                  options = list(`actions-box` = TRUE),
                                  multiple = T),
                      selectizeInput("flex_players", "FLEX Compare", 
                                     choices = flex_players %>% unique(), 
                                     multiple = T,
                                     selected = c("Derrick Henry", "Alvin Kamara")),
                      selectizeInput("qb_players", "QB Compare", 
                                     choices = qb_players %>% unique(), 
                                     multiple = T,
                                     selected = c("Aaron Rodgers", "Russell Wilson"))
                    ),
                    dashboardBody(
                      tabItems(
                        # TAB 1
                        tabItem(tabName = "ep",
                                fluidRow(plotOutput("ep_plot", height = 2000))),
                        # TAB 2
                        tabItem(tabName = "flex_compare",
                                fluidRow(plotOutput("flex_compare_plot", height = 1000))),
                        # TAB 3
                        tabItem(tabName = "qb_compare",
                                fluidRow(plotOutput("qb_compare_plot", height = 1000)))
                      ))
)

# SERVER ------
server <- function(input, output, session, ...t) {
  
  
  # cDATA
  cdata <- session$clientData
  # TAB 1 - EP DRAFTER
  
  #  TAB 1 reactive data
  proj_react = reactive({
    isolate({
      proj <- proj
    })
  })
  
  # TAB 1 Plots
  output$ep_plot = renderPlot({
    proj = proj_react()
    flex.labs <- c("QB", "Flex")
    names(flex.labs) <- c("0", "1")
    proj %>%
      filter(!full_name %in% input$picked) %>%
      mutate(is_flex = if_else(position == "QB", 0, 1),
             plus_minus = ppg-ep,
             pm_color = if_else(plus_minus > 0, "green", "red")) %>%
      rowwise() %>%
      mutate(pm_min = min(ep, ppg), pm_max = max(ep, ppg)) %>%
      ungroup() %>%
      arrange(adp) %>%
      group_by(is_flex) %>%
      mutate(grp_max_ep = max(proj, na.rm = T)) %>%
      ungroup() %>%
      top_n(250, -adp) %>%
      mutate(
        # full_name = reorder(full_name, proj),
        proj = round(proj/.5)*.5,
        label_color = case_when(
          position == "QB" ~ "#ee480c",
          position == "RB" ~ "#009fee",
          position == "WR" ~ "#ee9300",
          TRUE ~ "#a933e4"),
        schedule_star = case_when(
          team %in% c("WAS", "SF", "ARI", "DET", "LAC", "SF") |
            team == "DEN" & position == "RB" ~ "*", 
          team %in% c("MIN", "CIN", "BAL", "PHI", "CAR", "TEN") ~ "_", 
          TRUE ~ ""),
        sort_metric = if_else(round(proj) > 8, ep, max_ep),
        label_color = if_else(is.na(plus_minus), "#2ac301", label_color),
        full_name = glue("<b style='color:{label_color}'>{full_name}{schedule_star}</b>"),
        full_name = reorder(full_name, proj)
        #full_name = reorder_within(x = full_name, by = sort_metric, within = round(-proj)),
      ) %>%
      ggplot()+
      aes(xmin = min_ep, xmax = max_ep, x = ep, y = full_name, group = is_flex)+
      geom_errorbarh()+
      geom_errorbarh(aes(xmin = pm_min, xmax = pm_max, color = pm_color),
                     height = 0)+
      geom_point(alpha = .6, color = "black", size = 3)+
      geom_point(aes(x = proj),alpha = .6, color = "purple", size = 3)+
      geom_point(alpha = .6, aes(color = pm_color), size = 1)+
      # geom_point(aes(x = ppg, color = position))+
      scale_color_identity()+
      stat_summary(aes(xintercept = grp_max_ep, group = is_flex), geom = "vline", color = "black", lty = 2)+
      facet_wrap(
        is_flex ~ ., scales = "free", labeller = labeller(is_flex = flex.labs)
        #round(-proj) ~ ., scales = "free", labeller = labeller(is_flex = flex.labs)
      )+
      scale_x_continuous(breaks = seq(0, 50,5))+
      labs(x = "Expected Points per Game", y = "", title = "EP by Availability",
           subtitle = "Purple: Projection")+
      coord_cartesian(xlim = c(5, 35))+
      scale_y_reordered()+
      theme(
        axis.text.y = element_markdown()
        # plot.caption = element_markdown(lineheight = 1.2)
      )
  })
  
  # TAB 2 FLEX COMPARE
  # TAB 2 reactive data
  flex_react = reactive({
    isolate({
      flex_stats <- flex_stats
    })
  })
  
  # TAB 2 plots
  output$flex_compare_plot = renderPlot({
    flex = flex_react()
    flex %>%
      filter(player_name %in% input$flex_players) %>%
      mutate(
        player_orig = player_name,
        player_name = reorder_within(player_name, value, metric)
      ) %>%
      ggplot()+
      aes(x = value, y = player_name, color = player_orig)+
      geom_col(aes(fill = player_orig), alpha = .6, show.legend = F)+
      geom_text(aes(label = round(value,3)), hjust = "inward", size = 3,
                color = "black", show.legend = F)+
      facet_wrap( ~ metric, scales = "free", nrow = 6) +
      labs(x = "", y = "", fill = "")+
      theme_light()+
      theme(
        strip.text.x = element_text(
          size = 9, color = "black", face = "bold"
        ),
        strip.text.y = element_text(
          size = 9, color = "black", face = "bold"
        ),
        strip.background = element_rect(
          color="black", fill="#ffffff", size=1.5, linetype="solid"
        )
      )+
      scale_y_reordered()
  })
  
  
  # TAB 3 QB COMPARE
  # TAB 3 reactive data
  qb_react = reactive({
    isolate({
      qb_stats <- qb_stats
    })
  })
  
  # TAB 2 plots
  output$qb_compare_plot = renderPlot({
    qb = qb_react()
    qb %>%
      filter(player_name %in% input$qb_players) %>%
      mutate(player_orig = player_name,
             player_name = reorder_within(player_name, value, metric)) %>%
      ggplot()+
      aes(x = value, y = player_name, color = player_orig)+
      geom_col(aes(fill = player_orig), alpha = .6, show.legend = F)+
      geom_text(aes(label = round(value,2)), hjust = "inward", size = 3,
                color = "black")+
      facet_wrap(~ metric, scales = "free") +
      theme_light()+
      labs(fill = "", y = "", x = "")+
      theme(
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"
        ),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"
        ),
        strip.background = element_rect(
          color="black", fill="#ffffff", size=1.5, linetype="solid"
        )
      )+
      scale_y_reordered()
    
  })
  
}

# RUN ------
shinyApp(ui, server)
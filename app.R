# libs   --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(chess.com)
library(rchess)
# devtools::install_github("paladinic/r.chess.com",force = T)

get_board = function(moves){
  
  ch = rchess::Chess$new()
  moves = strsplit(moves," ; ")[[1]]
  
  for(i in 1:length(moves)){
    ch$move(moves[i])
  }
  
  ch$plot()
  
}

TRY = function(x){
  tryCatch(x, error = function(e) NULL)
}

# ui     ----------------------------------------------------------------------

ui <- tagList(
  # HEAD ####
  tags$head(
    useShinyjs(),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Bitter:wght@100&display=swap", rel =
                "stylesheet"),
    tags$link(type = "text/css", rel = "stylesheet", href = "css/styles.css"),
    tags$link(rel = "shortcut icon", href = "/img/logo.png"),
    tags$script(src = "js/script.js"),
    tags$title("Chess.com Dashboard")
  ),

  # HEADER ####
  fluidPage(
    # HTML("<img src='img/logo.png' width=20px>"),
    div(
      id="header",
      div(br(),
             uiOutput("date_filter_ui")),
      div(
        strong(id="username_label","Username"),
        textInput(inputId = "username", label = NULL),
        actionButton(inputId = "go", label = "Go")
      ),
      div(
             br(),
             uiOutput("game_type_filter_ui"))
    ),
    tabsetPanel(
      type = "pills",
      # PAGE 1 ####
      tabPanel("Dashboard", withSpinner(uiOutput("charts_1"), color = "white")),
      # PAGE 2 ####
      tabPanel("Openings",
               br(),
               withSpinner(uiOutput("charts_2"), color = "white")),
      # PAGE 3 ####

      tabPanel(
        "Table",
        withSpinner(uiOutput("charts_3"), color = "#ffffff")
      ),

      # PAGE 4 ####

      tabPanel("Info",
               h3("Information"),
               br(),
               p("This web app is built using R and Shiny. It is hosted on ",
                 a("shinyapps.io",href="https://www.shinyapps.io"),
                 ", while the code is available",a("here.",href="https://github.com/paladinic/r_shiny_chess.com")),
               br(),
               p("The data is gathered using the ",
                 a("Chess.com API.",href="https://www.chess.com/news/view/published-data-api"),
                 "The app also uses an API interface built in R, available",a("here.",href="https://github.com/paladinic/r.chess.com")),
               br()
               )
    )
  )
)


# server ------------------------------------------------------------------
server <- function(input, output, session) {
  # get data       ####
  user_data = eventReactive(input$go, {
    df = get_user_game_data(username = input$username) %>%
      mutate(date = as.Date(date, "%d/%m/%Y")) %>%
      mutate(weekday = weekdays(date)) %>%
      mutate(months = months(date)) %>%
      mutate(hour = round(as.POSIXct(
        time, format = "%H:%M:%S", tz = "UTC"
      ), units = "hours")) %>%
      mutate(hour = strftime(hour, format = "%H:%M:%S")) %>%
      mutate(hour = substr(
        x = hour,
        start = 0,
        stop = 5
      )) %>%
      mutate(won = if_else(user_outcome == "win", 1, 0))
  })
  filtered_data = reactive({
    req(user_data())

    type = input$game_type_filter
    dates = input$date_filter


    df = user_data()
    df = df[df$game_type == type,]
    df = df[df$date >= dates[1],]
    df = df[df$date <= dates[2],]

  })
  current_rating = reactive({
    req(filtered_data())
    
    ratings = filtered_data() %>% pull(user_rating)
    
    return(ratings[length(ratings)])
    
  })
  
  # charts         #####

  font = list(color = "white",
              family = "Bitter")
  


  output$charts_1 = renderUI({
    if (is.null(TRY({
      user_data()
    }))) {
      tagList(br(),
              br(),
              em("Enter a chess.com username."))
    } else{
      tagList(fluidRow(
        column(
          6,
          br(),
          fluidRow(
            column(4,
                   plotlyOutput("overall_stats", height = "150px")),
            column(4,
                   plotlyOutput("overall_stats_white", height = "150px")),
            column(4,
                   plotlyOutput("overall_stats_black", height = "150px"))
          ),
          plotlyOutput("games_and_time", height = "200px"),
          uiOutput("date_type_filter_ui")
        ),
        column(6,
               h3(paste0("Latest Rating: ",current_rating())),
               plotlyOutput("score", height = "350px"))

      ))
    }
  })

  output$charts_2 = renderUI({
    if (is.null(TRY({
      user_data()
    }))) {
      tagList(br(),
              em("Enter a chess.com username."))
    } else{
      
      
      tabsetPanel(type = "pills",selected = "Recommend",
                          tabPanel(
                            "Openings",
                            sidebarLayout(
                              sidebarPanel(
                                h2("Filters"),
                                hr(),
                                fluidRow(column(
                                  6,
                                  radioButtons(
                                    inputId = "open_color",
                                    label = "User Color",
                                    choices = c("Black", "White"),
                                    selected = "White",
                                    inline = T
                                  )
                                ),
                                column(
                                  6,
                                  radioButtons(
                                    inputId = "inc_opp",
                                    label = "Opponent Moves",
                                    choices = c("Exclude", "Include"),
                                    selected = "Include",
                                    inline = T
                                  )
                                )),
                                hr(),
                                sliderInput(
                                  inputId = "open_depth",
                                  label = "Moves Depth",
                                  min = 1,
                                  max = 10,
                                  value = 5,
                                  step = 1
                                ),
                                hr(),
                                sliderInput(
                                  inputId = "min_open",
                                  label = "Minimum Games",
                                  min = 1,
                                  max = 10,
                                  value = 2,
                                  step = 1
                                )
                              ),
                              mainPanel(uiOutput("color_icon"),
                                        plotlyOutput("openings", height = "300px"))
                            )
                          ),
              tabPanel("Recommend",
                       sidebarLayout(sidebarPanel(
                         h2("Filters"),
                         hr(),
                         radioButtons(
                             inputId = "open_color_rec",
                             label = "User Color",
                             choices = c("Black", "White"),
                             selected = "White",
                             inline = T
                             ),
                         hr(),
                         sliderInput(
                           inputId = "open_depth_rec",
                           label = "Moves Depth",
                           min = 1,
                           max = 10,
                           value = 5,
                           step = 1
                         ),
                         hr(),
                         sliderInput(
                           inputId = "min_open_rec",
                           label = "Minimum Games",
                           min = 1,
                           max = 10,
                           value = 2,
                           step = 1
                         )
                       ),
                                     mainPanel(
                                       uiOutput("color_icon_rec"),
                                       uiOutput("recommend_ui")))))
    }
  })

  output$charts_3 = renderUI({
    if (is.null(TRY({
      user_data()
    }))) {
      tagList(br(),br(),
              em("Enter a chess.com username."))
    }else{
      tagList(
        h3("Games Table"),
        downloadButton("downloadData", "Download"),
        dataTableOutput("user_table")
      )
    }
  })

  # filters        ####

  output$date_filter_ui = renderUI({
    req(user_data())

    df = user_data()

    min = df$date %>% min()
    max = df$date %>% max()

    sliderInput(
      inputId = "date_filter",
      label = "Date",ticks = F,
      min = min,
      max = max,
      value = c(min, max),
      timeFormat = "%d/%m/%Y"
    )

  })
  output$date_type_filter_ui = renderUI({
    req(user_data())

    opts = c("Months", "Weekday", "Hour")

    radioButtons(
      inputId = "date_type_filter",
      label = NULL,
      choices = opts,
      inline = T
    )

  })
  output$game_type_filter_ui = renderUI({
    req(user_data())

    df = user_data()

    game_type = df$game_type %>% unique()

    selectInput(inputId = "game_type_filter",
                label = "Game Type",
                choices = game_type)

  })

  # table          ####

  output$user_table = renderDataTable({
    df = filtered_data() %>% select(-moves,-won,-months,-weekday,-hour)
  })

  # download table
  output$downloadData = downloadHandler(
    filename = function() {
      paste(input$username, "_data.csv", sep = "")
    },
    content = function(file) {
      write.csv(user_data(), file, row.names = FALSE)
    }
  )

  # overall stats  ####

  overall_stats = reactive({
    df = filtered_data()

    games = nrow(df)
    wins = sum(df$won)
    defeats = sum(df$opponent_outcome == "win")
    draws = sum(games - wins - defeats)

    df_defeats = df[df$opponent_outcome == "win",]
    df_defeats = df_defeats %>%
      group_by(user_outcome) %>%
      summarise(games = n())

    stats = list(
      games = games,
      wins = wins,
      defeats = defeats,
      draws = draws,
      df_defeats = df_defeats
    )

    return(stats)
  })
  overall_stats_black = reactive({
    df = filtered_data() %>%
      filter(color == "black")

    games = nrow(df)
    wins = sum(df$won)
    defeats = sum(df$opponent_outcome == "win")
    draws = sum(games - wins - defeats)

    df_defeats = df[df$opponent_outcome == "win",]
    df_defeats = df_defeats %>%
      group_by(user_outcome) %>%
      summarise(games = n())

    stats = list(
      games = games,
      wins = wins,
      defeats = defeats,
      draws = draws,
      df_defeats = df_defeats
    )

    return(stats)
  })
  overall_stats_white = reactive({
    df = filtered_data() %>%
      filter(color == "white")

    games = nrow(df)
    wins = sum(df$won)
    defeats = sum(df$opponent_outcome == "win")
    draws = sum(games - wins - defeats)

    df_defeats = df[df$opponent_outcome == "win",]
    df_defeats = df_defeats %>%
      group_by(user_outcome) %>%
      summarise(games = n())

    stats = list(
      games = games,
      wins = wins,
      defeats = defeats,
      draws = draws,
      df_defeats = df_defeats
    )

    return(stats)
  })
  output$overall_stats = renderPlotly({
    draws = overall_stats()$draws
    defeats = overall_stats()$defeats
    wins = overall_stats()$wins
    games = overall_stats()$games

    df = data.frame(
      outcome = c("draws", "wins", "defeats"),
      count = c(draws, wins, defeats)
    )

    plot_ly(
      data = df,
      labels = ~ outcome,
      values = ~ count,
      type = 'pie',
      hole = 0.6,
      textfont = list(color = '#ffffff', family = "Bitter"),
      marker = list(colors = c(
        "rgb(46, 46, 46)",
        "rgb(122, 255, 0)",
        "#aa3b00"
      ))
    ) %>%
      layout(annotations = list(text = paste0("Games\n", games),
                                showarrow = F)) %>%
      layout(
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        fig_bgcolor   = "rgba(0, 0, 0, 0)",
        showlegend = FALSE,
        title = "Total",
        font = font,
        margin = list(l = 20, r = 20)
      ) %>% 
      config(displayModeBar = FALSE) %>%
      config(displaylogo = FALSE)
      
  })
  output$overall_stats_black = renderPlotly({
    draws = overall_stats_black()$draws
    defeats = overall_stats_black()$defeats
    wins = overall_stats_black()$wins
    games = overall_stats_black()$games

    df = data.frame(
      outcome = c("draws", "wins", "defeats"),
      count = c(draws, wins, defeats)
    )

    plot_ly(
      data = df,
      labels = ~ outcome,
      values = ~ count,
      type = 'pie',
      hole = 0.6,
      textfont = list(color = '#ffffff', family = "Bitter"),
      marker = list(colors = c(
        "rgb(46, 46, 46)",
        "rgb(122, 255, 0)",
        "#aa3b00"
      ))
    ) %>%
      layout(annotations = list(text = paste0("Games\n", games),
                                showarrow = F)) %>%
      layout(
        margin = list(l = 20, r = 20),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        fig_bgcolor   = "rgba(0, 0, 0, 0)",
        showlegend = FALSE,
        title = "Black",
        font = font
      )%>% 
      config(displayModeBar = FALSE) %>%
      config(displaylogo = FALSE)
  })
  output$overall_stats_white = renderPlotly({
    draws = overall_stats_white()$draws
    defeats = overall_stats_white()$defeats
    wins = overall_stats_white()$wins
    games = overall_stats_white()$games

    df = data.frame(
      outcome = c("draws", "wins", "defeats"),
      count = c(draws, wins, defeats)
    )

    plot_ly(
      data = df,
      labels = ~ outcome,
      values = ~ count,
      type = 'pie',
      hole = 0.6,
      textfont = list(color = '#ffffff', family = "Bitter"),
      marker = list(colors = c(
        "rgb(46, 46, 46)",
        "rgb(122, 255, 0)",
        "#aa3b00"
      ))
    ) %>%
      layout(annotations = list(text = paste0("Games\n", games),
                                showarrow = F)) %>%
      layout(
        margin = list(l = 20, r = 20),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        fig_bgcolor   = "rgba(0, 0, 0, 0)",
        showlegend = FALSE,
        title = "White",
        font = font
      )%>% 
      config(displayModeBar = FALSE) %>%
      config(displaylogo = FALSE)
  })

  # score          ####

  output$score = renderPlotly({
    df = filtered_data() %>%
      mutate(user_rating = as.numeric(user_rating)) %>%
      group_by(date) %>%
      summarise(user_rating = mean(user_rating),
                games = n())

    plot_ly(
      data = df,
      type = "scatter",
      mode = "lines",
      x =  ~ date,
      y =  ~ user_rating,
      line = list(color = "#aa3b00"),
      name = "User Rating"
    ) %>%
      add_trace(
        name = "Games",
        yaxis = "y2",
        x =  ~ date,
        y =  ~ games,
        type = "bar",
        marker = list(color = "white")
      ) %>%
      layout(
        title = "Rating and Games",
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        fig_bgcolor   = "rgba(0, 0, 0, 0)",
        xaxis = list(
          showgrid = FALSE,
          title = "Date",
          rangemode = "tozero"
        ),
        yaxis = list(
          showgrid = FALSE,
          title = "Rating",
          rangemode = "tozero"
        ),
        yaxis2 = list(
          rangemode = "tozero",
          showgrid = FALSE,
          overlaying = "y",
          side = "right",
          title = "Games",
          automargin = T
        ),
        font = font,
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      yanchor = "top",
                      x = 0.5,
                      y = 1)
      )%>% 
      config(displayModeBar = FALSE) %>%
      config(displaylogo = FALSE)
  })

  # games and time ####

  games_and_time = reactive({
    df = filtered_data()
    time_group = input$date_type_filter %>%
      tolower()

    df = df %>%
      group_by(!!sym(time_group)) %>%
      summarise(wins = sum(won),
                games = n()) %>%
      mutate(win_rate = wins / games)

    if (time_group == "weekday") {
      df$weekday = factor(
        df$weekday,
        levels = c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        )
      )
    }
    if (time_group == "months") {
      df = df %>%
        full_join(data.frame(
          months = c(
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December"
          )
        ))

      df[is.na(df)] = 0

      df$months = factor(
        df$months,
        levels = c(
          "January",
          "February",
          "March",
          "April",
          "May",
          "June",
          "July",
          "August",
          "September",
          "October",
          "November",
          "December"
        )
      )
    }
    if (time_group == "hour") {
      df = df %>%
        full_join(data.frame(
          hour = c(
            "05:00",
            "06:00",
            "07:00",
            "08:00",
            "09:00",
            "10:00",
            "11:00",
            "12:00",
            "13:00",
            "14:00",
            "15:00",
            "16:00",
            "17:00",
            "18:00",
            "19:00",
            "20:00",
            "21:00",
            "22:00",
            "23:00",
            "00:00",
            "01:00",
            "02:00",
            "03:00",
            "04:00"
          )
        ))

      df[is.na(df)] = 0

      df$hour = factor(df$hour, levels = sort(as.character(df$hour))[c(6:24, 1:5)])
    }

    df

  })
  output$games_and_time = renderPlotly({
    df = games_and_time()

    plot_ly(
      data = df,
      type = "bar",
      x =  ~ get(input$date_type_filter %>% tolower()),
      y = ~ win_rate,
      name = "Win Rate",
      marker = list(color = "rgb(122, 255, 0)"),
      offsetgroup = 2
    ) %>%
      add_trace(
        type = "bar",
        marker = list(color = "white"),
        x =  ~ get(input$date_type_filter %>% tolower()),
        y =  ~ games,
        yaxis = "y2",
        name = "Games",
        offsetgroup = 1
      ) %>%
      layout(
        title = "Seasonality",
        # legend = list(x = 1.2),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        fig_bgcolor   = "rgba(0, 0, 0, 0)",
        yaxis2 = list(
          showgrid = FALSE,
          title = "Games",
          overlaying = "y",
          side = "right",
          rangemode = "tozero"
        ),
        xaxis = list(
          showgrid = FALSE,
          title = input$date_type_filter,
          rangemode = "tozero"
        ),
        yaxis = list(
          showgrid = FALSE,
          title = "Win Rate",
          tickformat = "%",
          rangemode = "tozero"
        ),
        font = font,
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5,
                      y = 1)
      )%>% 
      config(displayModeBar = FALSE) %>%
      config(displaylogo = FALSE)

  })

  # openings       ####
  
  openings = reactive({
    
    nth = input$open_depth_rec
    df = filtered_data()
    col = input$open_color_rec
    opponent = "Include"
    min_open = input$min_open_rec
    
    dt = df %>%
      mutate(opening = apply(df, 1, function(game) {
        color = game["color"]
        moves = game["moves"]
        moves = strsplit(moves, split = " ; ")[[1]]
        
        if (opponent == "Include") {
          v = 1:nth
        } else{
          if (nth == 1) {
            v = 1
          } else{
            if (color == "black") {
              v = seq(2, nth + 3, 2)
            } else{
              v = seq(1, nth + 2, 2)
            }
          }
        }
        
        moves = moves[v]
        moves = paste0(moves, collapse = " ; ")
        
      })) %>%
      group_by(opening, color) %>%
      summarise(games = n(),
                wins = sum(won)) %>%
      mutate(win_rate = wins / games) %>%
      filter(games >= min_open) %>%
      arrange(desc(win_rate)) %>%
      mutate(col = if_else(color == "white", "#ffec87", "#363636"))
    
    if (col == "White") {
      dt = dt %>% filter(color == "white")
    } else if (col == "Black") {
      dt = dt %>% filter(color == "black")
    }
    
    dt
  })
  
  output$recommend_ui = renderUI({
    tagList(
      div(id = "recommend",
            div(class="recommend_row",
              uiOutput("recommend_good_1"),
              uiOutput("recommend_good_2"),
              uiOutput("recommend_good_3")
            ),
            div(class="recommend_row",
              uiOutput("recommend_bad_1"),
              uiOutput("recommend_bad_2"),
              uiOutput("recommend_bad_3")
            ))
      )
  }) 
  
  output$recommend_good_1 = renderUI({
    opening = openings()$opening
    games = openings()$games
    rate = openings()$win_rate
    
    n = 1
    if(length(opening) >= n){
      tagList(
        h4(paste0(n,". Best")),
        p(paste0(
          opening[n])),
        p(paste0(
          input$open_color_rec),
          "Wins:",
          round(rate[n]*100,2),"% (Games: ",games[n],")"
        ),
        chessboardjsOutput(paste0("recommed_good_",n,"_plt"))
      )
    }else{NULL}
  })
  output$recommend_good_2 = renderUI({
    opening = openings()$opening
    games = openings()$games
    rate = openings()$win_rate
    
    n = 2
    if(length(opening) >= n){
      tagList(
        h4(paste0(n,". Best")),
        p(paste0(
          opening[n])),
        p(paste0(
          input$open_color_rec),
          "Wins:",
          round(rate[n]*100,2),"% (Games: ",games[n],")"
        ),
        chessboardjsOutput(paste0("recommed_good_",n,"_plt"))
      )
    }else{NULL}
  })
  output$recommend_good_3 = renderUI({
    opening = openings()$opening
    games = openings()$games
    rate = openings()$win_rate
    
    n = 3
    if(length(opening) >= n){
      tagList(
        h4(paste0(n,". Best")),
        p(paste0(
          opening[n])),
        p(paste0(
          input$open_color_rec),
          "Wins:",
          round(rate[n]*100,2),"% (Games: ",games[n],")"
        ),
        chessboardjsOutput(paste0("recommed_good_",n,"_plt"))
      )
    }else{NULL}
  })
  
  output$recommend_bad_1 = renderUI({
    opening = openings() %>%
      arrange(desc(-win_rate)) 
    games = opening$games
    rate = opening$win_rate
    opening = opening$opening
    
    n = 3
    if(length(opening) >= 4-n){
      tagList(
        h4(paste0(4-n,". Worst")),
        p(paste0(
          opening[4-n])),
        p(paste0(
          input$open_color_rec,
          "Wins:",
          round(rate[4-n]*100,2),"% (Games: ",games[4-n],")"
        )),
        chessboardjsOutput(paste0("recommed_bad_",n,"_plt"))
      )
    }else{NULL}
  })
  output$recommend_bad_2 = renderUI({
    opening = openings() %>%
      arrange(desc(-win_rate)) 
    games = opening$games
    rate = opening$win_rate
    opening = opening$opening
    
    n = 2
    if(length(opening) >= 4-n){
      tagList(
        h4(paste0(4-n,". Worst")),
        p(paste0(
          opening[4-n])),
        p(paste0(
          input$open_color_rec,
          "Wins:",
          round(rate[4-n]*100,2),"% (Games: ",games[4-n],")"
        )),
        chessboardjsOutput(paste0("recommed_bad_",n,"_plt"))
      )
    }else{NULL}
  })
  output$recommend_bad_3 = renderUI({
    opening = openings() %>%
      arrange(desc(-win_rate)) 
    games = opening$games
    rate = opening$win_rate
    opening = opening$opening
    
    n = 1
    if(length(opening) >= 4-n){
      tagList(
        h4(paste0(4-n,". Worst")),
        p(paste0(
          opening[4-n])),
        p(paste0(
          input$open_color_rec,
          "Wins:",
          round(rate[4-n]*100,2),"% (Games: ",games[4-n],")"
        )),
        chessboardjsOutput(paste0("recommed_bad_",n,"_plt"))
      )
    }else{NULL}
  })
  
  output$recommed_good_1_plt = renderChessboardjs({
    
    
    opening = openings() %>%
      arrange(desc(win_rate)) %>%
      pull(opening)
    
    n = 1
    
    if(length(opening) >= n){
      
      opening = opening[n]
      
      get_board(opening)
      
    }else{NULL}
    
  })
  output$recommed_good_2_plt = renderChessboardjs({
    
    
    opening = openings() %>%
      arrange(desc(win_rate)) %>%
      pull(opening)
    
    n = 2
    
    if(length(opening) >= n){
  
      opening = opening[n]
      
      get_board(opening)
      
    }else{NULL}
    
  })
  output$recommed_good_3_plt = renderChessboardjs({
    
    
    opening = openings() %>%
      arrange(desc(win_rate)) %>%
      pull(opening)
    
    n = 3
    
    if(length(opening) >= n){
      
      opening = opening[n]
      
      get_board(opening)
      
    }else{NULL}
    
  })
  
  output$recommed_bad_1_plt = renderChessboardjs({
    
    opening = openings() %>%
      arrange(desc(-win_rate)) %>%
      pull(opening)
    n = 3
    
    if(length(opening) >= n){

      
      opening = opening[n]
      
      get_board(opening)
      
    }else{NULL}
    
  })
  output$recommed_bad_2_plt = renderChessboardjs({
    opening = openings() %>%
      arrange(desc(-win_rate)) %>%
      pull(opening)
    n = 2
    
    if(length(opening) >= n){
      
      opening = opening[n]
      
      get_board(opening)
      
    }else{NULL}
    
  })
  output$recommed_bad_3_plt = renderChessboardjs({
    
    opening = openings() %>%
      arrange(desc(-win_rate)) %>%
      pull(opening)
    n = 1
    
    if(length(opening) >= n){
      
      
      opening = opening[n]
      
      get_board(opening)
      
    }else{NULL}
    
  })
  
  output$openings = renderPlotly({
    nth = input$open_depth
    df = filtered_data()
    col = input$open_color
    opponent = input$inc_opp
    min_open = input$min_open

    dt = df %>%
      mutate(opening = apply(df, 1, function(game) {
        color = game["color"]
        moves = game["moves"]
        moves = strsplit(moves, split = " ; ")[[1]]

        if (opponent == "Include") {
          v = 1:nth
        } else{
          if (nth == 1) {
            v = 1
          } else{
            if (color == "black") {
              v = seq(2, nth + 3, 2)
            } else{
              v = seq(1, nth + 2, 2)
            }
          }
        }

        moves = moves[v]
        moves = paste0(moves, collapse = " ; ")

      })) %>%
      group_by(opening, color) %>%
      summarise(games = n(),
                wins = sum(won)) %>%
      mutate(win_rate = wins / games) %>%
      filter(games >= min_open) %>%
      arrange(desc(win_rate)) %>%
      mutate(col = if_else(color == "white", "#ffec87", "#363636"))

    if (col == "White") {
      dt = dt %>% filter(color == "white")
    } else if (col == "Black") {
      dt = dt %>% filter(color == "black")
    }

    dt$opening <- factor(dt$opening,
                         levels = unique(dt$opening)[order(dt$win_rate,
                                                           decreasing = TRUE)])
    plot_ly(
      data = dt,
      type = "bar",
      x = ~ opening,
      marker = list(color = "rgb(122, 255, 0)"),
      y = ~ win_rate,
      name = "Win Rate",
      offsetgroup = 2
    ) %>%
      add_trace(
        marker = list(color = "white"),
        x = ~ opening,
        y =  ~ games,
        yaxis = "y2",
        name = "Games",
        offsetgroup = 1
      ) %>%
      layout(yaxis2 = list(overlaying = "y", side = "right")) %>%
      layout(
        title = "Openings",
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        fig_bgcolor   = "rgba(0, 0, 0, 0)",
        xaxis = list(
          showgrid = FALSE,
          title = "Opening",
          rangemode = "tozero"
        ),
        yaxis2 = list(
          showgrid = FALSE,
          title = "Games",
          rangemode = "tozero"
        ),
        yaxis = list(
          rangemode = "tozero",
          showgrid = FALSE,
          title = "Win Rate",
          tickformat = "%"
        ),
        font = font,
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      yanchor = "top",
                      x = 0.5,
                      y = 1)
      )%>% 
      config(displayModeBar = FALSE) %>%
      config(displaylogo = FALSE)

  })
  # color          ####
  
  
  
  output$color_icon = renderUI({
    if(input$open_color == "Black"){
      img(src="img/chess_icon_black.png",width="50px")
    }else{
      img(src="img/chess_icon_white.png",width="50px")
    }
  })
  output$color_icon_rec = renderUI({
    if(input$open_color_rec == "Black"){
      img(src="img/chess_icon_black.png",width="50px")
    }else{
      img(src="img/chess_icon_white.png",width="50px")
    }
  })
  
}

shinyApp(ui, server)

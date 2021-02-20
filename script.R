library(dplyr)
library(plotly)
library(lubridate)
library(chess.com)
library(rchess)

username = "aldopalmisano"
username = "juldepomme"
username = "claudiopaladini"
username = "sgugaz"
df0 = get_user_game_data(username)

df = df0 %>% 
  mutate(date = as.Date(date,"%d/%m/%Y")) %>% 
  mutate(weekday = weekdays(date)) %>% 
  mutate(months = months(date)) %>% 
  mutate(hour = round(as.POSIXct(time, format="%H:%M:%S", tz="UTC"), units="hours")) %>% 
  mutate(hour = strftime(hour, format="%H:%M:%S")) %>% 
  mutate(hour = substr(x = hour,start = 0,stop = 5)) %>% 
  mutate(won = if_else(user_outcome == "win",1,0))

df %>% 
  group_by(color) %>% 
  summarise(n = n(),
            won = sum(won))

# games / win rate by day of week ####

dt = df %>% 
  group_by(weekday) %>% 
  summarise(win_rate = sum(won)/n(),
            games = n())

dt$weekday = factor(dt$weekday,levels = dt$weekday[c(2,6,7,5,1,3,4)])

plot_ly(data = dt,
        x = ~weekday,
        y = ~win_rate,
        marker = list(color =  ~-win_rate,
                      colorscale='Blues'),
        type = "bar") %>% 
  add_trace(
    type = "scatter",
    mode = "lines",
    marker = list(size = 0.1),
    y = 0.5,
    showlegend = F
  )

# games / win rate by day hour    ####

dt = df %>% 
  group_by(hour) %>% 
  summarise(win_rate = sum(won)/n(),
            games = n())

plot_ly(data = dt,
        x = ~hour,
        y = ~win_rate,
        marker = list(color =  ~-win_rate,
                      colorscale='Blues'),
        type = "bar") %>% 
  add_trace(
    type = "scatter",
    mode = "lines",
    marker = list(size = 0.1),
    y = 0.5,
    showlegend = F
  )

t = df$time



# games / win rate by time range  ####

win_rate = function(df,start_date=NULL,end_date=NULL){
  
  # date filtering
  if(!is.null(start_date)){
    start_date = as.Date(start_date,"%d/%m/%Y")
    df = df %>% filter(date >= start_date)
    
  }
  if(!is.null(end_date)){
    end_date = as.Date(end_date,"%d/%m/%Y")
    df = df %>% filter(date <= end_date)
    
  }
  
  
  df = df %>% 
    group_by(user_outcome) %>% 
    summarise(n = n()) %>% 
    ungroup()
  
  games = sum(df$n)
  win_rate = df$n[df$user_outcome=="win"]/sum(df$n) 
  
  return(c(games=games,win_rate=win_rate))
}

win_rate(df,start_date = "15/01/2021")

# games / win rate by opening     ####

get_openning = function(moves,color,nth=1,opponent=F){
  
  moves = strsplit(moves,split = " ; ")[[1]]

  if(opponent){
    v = 1:nth
  }else{
    if(color=="black"){
      v = seq(2,nth+3,2)
    }else{
      v = seq(1,nth+2,2)
    }
  }
  
  moves = moves[v]
  moves = paste0(moves,collapse = " ; ")
  
}

dt = df %>% 
  mutate(openning = apply(df,1,function(game)get_openning(game["moves"],game["color"],nth =  2))) %>% 
  group_by(openning,color) %>%
  summarise(games = n(),
            wins = sum(won)) %>% 
  mutate(win_rate = wins/games) %>% 
  arrange(desc(win_rate)) %>% 
  mutate(col = if_else(color == "white","#ffec87","#363636")) 
  
dt$openning <- factor(dt$openning, 
                       levels = unique(dt$openning)[order(dt$win_rate,
                                                           decreasing = TRUE)])
plot_ly(
  data = dt,
  type = "bar",
  x = ~ openning,
  # color = "red",
  marker = list(color = "red"),
  y = ~ win_rate,
  name = "Win Rate",
  offsetgroup = 2
) %>%
  add_trace(
    # colors = "blue",
    marker = list(color = "blue"),
    x = ~ openning,
    y =  ~ games,
    yaxis = "y2",
    name = "Games",
    offsetgroup = 1
  ) %>%
  layout(yaxis2 = list(overlaying = "y", side = "right"))

# get board                       ####

get_board = function(moves){
  
  chess = Chess$new()
  
  moves = strsplit(moves," ; ")[[1]]
  moves = sapply(moves, function(m)chess$move(m))

  board = expand.grid(letters[1:8],1:8)
  board = paste0(board$Var1,board$Var2)
  board = data.frame(board = board,piece = "")
  
  board$piece = sapply(board$board,function(s){
    piece = chess$get(s)
    return(paste0(piece$color,";",piece$type))
    })
  
  return(board)
} 
get_pieces = function(moves){
  sapply(moves, function(m){
    
    chess = Chess$new()
    
    m = strsplit(m," ; ")[[1]]
    m = sapply(m, function(m)chess$move(m))
    
    board = expand.grid(letters[1:8],1:8)
    board = paste0(board$Var1,board$Var2)
    
    pieces = sapply(board,function(s){
      piece = chess$get(s)
      return(paste0(piece$color,";",piece$type))
    })
    pieces = paste0(pieces,collapse = "-")
    
    return(pieces) 
  })
} 
get_openning = function(x,nth=1){
  x = strsplit(x,split = " ; ")
  x = sapply(x, function(m){
    m = m[1:nth]
    m = paste0(m,collapse = " ; ")
  })
  
  return(x)
}

dt0 = df %>% 
  mutate(open = get_openning(moves,5) %>% 
           get_pieces()) 

dt = dt0 %>% 
  group_by(open,color) %>% 
  summarise(won = sum(won),
            games = n()) %>% 
  mutate(win_rate = won/games)%>% 
  arrange(desc(win_rate)) %>% 
  mutate(col = if_else(color == "white","#ffec87","#363636"))

# dt$open <- factor(dt$open, 
#                       levels = unique(dt$open)[order(dt$win_rate,
#                                                          decreasing = TRUE)])

col = "white"
dt = dt %>% 
  filter(color == "white")

plot_ly(data = dt,
        type = "bar",
        x = ~open,
        y = ~win_rate)

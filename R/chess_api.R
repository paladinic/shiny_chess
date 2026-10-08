# chess.com API helpers, copied from https://github.com/paladinic/r.chess.com (R/lib.R)
# so the app has no GitHub-hosted package dependency at deploy time.

get_moves = function(moves){

  moves = strsplit(x = moves,split = "...",fixed = T)[[1]]
  moves = unlist(strsplit(x = moves,split = ".",fixed = T))
  moves = moves[13:length(moves)]

  moves = moves[nchar(moves)>8]
  moves = strsplit(moves,split = " ")
  moves = sapply(moves, function(x){x[2]})
  moves = moves[!is.na(moves)]

  return(moves)

}
get_user_game_data = function(username){
  ## REQUEST       ####

  # generate user request
  user_req = paste0("https://api.chess.com/pub/player/",username,"/games/archives")

  # get user game archive
  user_res =  httr::content(httr::GET(user_req))

  ## CHECK REQUEST ####

  # if user name is not found "$archives" will be null
  if(is.null(user_res$archives)){
    cat(user_res$message)
    return(NULL)
  }else{
    cat("Gathering user data..")
  }

  ## FORMAT DATA   ####

  # get the archives object from the request content
  archive = user_res$archives

  # fill output_df with loop over archive's month data
  output_df = NULL

  for(i in 1:length(archive)){

    # get single month
    month_req = archive[[i]]

    # get month's games
    month_res = httr::content(httr::GET(month_req))

    # for each game in month
    for(j in 1:length(month_res$games)){

      # get game
      game = month_res$games[[j]]

      # get black & white
      black = unlist(game$black)
      white = unlist(game$white)

      # get game type (e.g. rapid 10min)
      time_class = game$time_class

      # get end date time
      t = game$end_time
      t = as.POSIXct(t, origin = "1970-01-01")
      date = strftime(t, format="%d/%m/%Y")
      time = strftime(t, format="%H:%M:%S")

      # get user and opponent data
      if(tolower(white["username"]) == tolower(username)){

        color = "white"
        user_username = white["username"]
        user_rating = white["rating"]
        user_outcome = white["result"]

        opponent_username = black["username"]
        opponent_rating = black["rating"]
        opponent_outcome = black["result"]

      }else{

        color = "black"
        user_rating = black["rating"]
        user_outcome = black["result"]

        opponent_username = white["username"]
        opponent_rating = white["rating"]
        opponent_outcome = white["result"]

      }


      # get moves
      if(is.null(game$pgn)){
        cat(
          "Could not retrieve moves for the following game:\n"
        )
        cat("opponent:",opponent_username,"\n")
        cat("date:",date,"\n")
        cat("time:",time,"\n")

        next
      }
      moves = paste0(get_moves(game$pgn),
                     collapse = " ; ")

      # build rows
      row = c(color,
              user_rating,
              user_outcome,
              opponent_username,
              opponent_rating,
              opponent_outcome,
              time_class,
              date,
              time,
              moves)

      # rbind rows to ouptut_df
      output_df = rbind(output_df,row)
    }

    # show progress
    # cat("\014")
    # cat("..")
  print(month_req)
  }

  # use rownames as column "color"
  output_df = as.data.frame(output_df)

  # rename output_df columns
  colnames(output_df) = c("color",
                          "user_rating",
                          "user_outcome",
                          "opponent_username",
                          "opponent_rating",
                          "opponent_outcome",
                          "game_type",
                          "date",
                          "time",
                          "moves")

  return(output_df)
}

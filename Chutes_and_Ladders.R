show_board <- function(board) {
  plot.new()
  plot.window(c(0, board["dim"][[1]][[2]]), c(0, board["dim"][[1]][[1]]), asp = 1) #xlim = columns/width, ylim = rows/height

  #vertical lines/columns
  for (column in 0:(board["dim"][[1]][[2]])) {
    segments(column, 0, column, board["dim"][[1]][[1]]) #e.g. first iteration: draw a line segment from (0,0) to (0,height)
  }
  
  #horizontal lines/rows
  for (row in 0:(board["dim"][[1]][[1]])) {
    segments(0, row, board["dim"][[1]][[2]], row) #e.g. first iteration: draw a line segment from (0,0) to (width,0)
  }
  
  #placing numbers
  for (row in 1:board["dim"][[1]][[1]]) {
    for (column in 1:board["dim"][[1]][[2]]) {
      text(column - 0.5, row - 0.5, get_num(row, column, board["dim"][[1]][[2]]))
    }
  }
  
  #placing arrows: store coordinates of each number in a vector
  arrow_pos <- c()
  for (row in 1:board["dim"][[1]][[1]]) {
    for (column in 1:board["dim"][[1]][[2]]) {
      if (row %% 2 == 1) { #if row is odd
        arrow_pos <- c(arrow_pos, column - 0.5, row - 0.5) #use this formula to store coordinates
      } else { #if row is even
        arrow_pos <- c(arrow_pos, (board["dim"][[1]][[2]] + 0.5) - column, row - 0.5) #use this formula to store coordinates
      }
    }
  }
  
  #placing chutes
  if(length(board["chutes"][[1]]) > 0) {
    starting_index <- seq(1, length(board["chutes"][[1]]), 2)
    for (i in 1:length(starting_index)) {
      start_coord_x <- (2*board["chutes"][[1]][starting_index[i]]) - 1
      start_coord_y <- start_coord_x + 1
      end_coord_x <- (2*board["chutes"][[1]][starting_index[i] + 1]) - 1
      end_coord_y <- end_coord_x + 1
      arrows(arrow_pos[start_coord_x], arrow_pos[start_coord_y], arrow_pos[end_coord_x], arrow_pos[end_coord_y], col = "red", lwd = 2)
    }
  }
  
  #placing ladders
  if(length(board["ladders"][[1]]) > 0) {
    starting_index <- seq(1, length(board["ladders"][[1]]), 2)
    for (i in 1:length(starting_index)) {
      start_coord_x <- (2*board["ladders"][[1]][starting_index[i]]) - 1
      start_coord_y <- start_coord_x + 1
      end_coord_x <- (2*board["ladders"][[1]][starting_index[i] + 1]) - 1
      end_coord_y <- end_coord_x + 1
      arrows(arrow_pos[start_coord_x], arrow_pos[start_coord_y], arrow_pos[end_coord_x], arrow_pos[end_coord_y], col = "green", lwd = 2)
    }
  }
}


#helper function: figure out which number to place in each square
get_num <- function(row, column, total_cols) {
  if (row %% 2 == 0) { #if it is an even row, fill from right to left
    return (total_cols * (row - 1) + total_cols + 1 - column)
  } else { #if it is an odd row, fill from left to right
    return (total_cols * (row - 1) + column)
  }
}


play_solo <- function(board, verbose = FALSE) {
  j <- length(board["chutes"][[1]]) / 2
  k <- length(board["ladders"][[1]]) / 2
  turn <- 0
  pos <- 0
  chutes_starting_indexes <- seq(1, length(board["chutes"][[1]]), 2)
  ladders_starting_indexes <- seq(1, length(board["ladders"][[1]]), 2)
  
  play_list <- list(turns = integer(0),
                    chute_tally = integer(j),
                    ladder_tally = integer(k),
                    move_log = c())
  
  if (verbose == TRUE) {
    while (pos != 100) {
      turn <- turn + 1
      play_list["turns"][[1]] <- turn #update turns
      cat("Turn", turn, "\n")
      cat("Start at", pos, "\n")
      spinner <- spin()
      cat("Spinner:", spinner, "\n")
      pos <- pos + spinner
      
      if (pos > 100) {
        pos <- pos - spinner
      }
      
      if (pos %in% board["chutes"][[1]][chutes_starting_indexes] == TRUE) { #check for chutes at current position
        cat("Landed on:", pos, "\n")
        cat("Chute!", "\n")
        chute_start_index <- which(board["chutes"][[1]] == pos) #vector index to find the value at which current chute starts
        chute_end_index <- chute_start_index + 1 #vector index to find the value at which current chute ends
        play_list["chute_tally"][[1]][which(board["chutes"][[1]][chutes_starting_indexes] == pos)] <- play_list["chute_tally"][[1]][which(board["chutes"][[1]][chutes_starting_indexes] == pos)] + 1 #update chute_tally
        pos <- board["chutes"][[1]][chute_end_index] #update position
        play_list["move_log"][[1]] <- c(play_list["move_log"][[1]], pos) #update move_log
      } else if (pos %in% board["ladders"][[1]][ladders_starting_indexes] == TRUE) { #check for ladders at current position
        cat("Landed on:", pos, "\n")
        cat("Ladder!", "\n")
        ladder_start_index <- which(board["ladders"][[1]] == pos) #vector index to find the value at which current ladder starts
        ladder_end_index <- ladder_start_index + 1 #vector index to find the value at which current ladder ends
        play_list["ladder_tally"][[1]][which(board["ladders"][[1]][ladders_starting_indexes] == pos)] <- play_list["ladder_tally"][[1]][which(board["ladders"][[1]][ladders_starting_indexes] == pos)] + 1 #update ladder_tally
        pos <- board["ladders"][[1]][ladder_end_index] #update position
        play_list["move_log"][[1]] <- c(play_list["move_log"][[1]], pos) #update move_log
      } else {
        play_list["move_log"][[1]] <- c(play_list["move_log"][[1]], pos)
      }
      
      cat("Turn ends at:", pos, "\n", "\n")
    }
  } else {
    while (pos != 100) {
      turn <- turn + 1
      play_list["turns"][[1]] <- turn #update turns
      spinner <- spin()
      pos <- pos + spinner
      
      if (pos > 100) {
        pos <- pos - spinner
      }
      
      if (pos %in% board["chutes"][[1]][chutes_starting_indexes] == TRUE) { #check for chutes at current position
        chute_start_index <- which(board["chutes"][[1]] == pos) #vector index to find the value at which current chute starts
        chute_end_index <- chute_start_index + 1 #vector index to find the value at which current chute ends
        play_list["chute_tally"][[1]][which(board["chutes"][[1]][chutes_starting_indexes] == pos)] <- play_list["chute_tally"][[1]][which(board["chutes"][[1]][chutes_starting_indexes] == pos)] + 1 #update chute_tally
        pos <- board["chutes"][[1]][chute_end_index] #update position
        play_list["move_log"][[1]] <- c(play_list["move_log"][[1]], pos) #update move_log
      } else if (pos %in% board["ladders"][[1]][ladders_starting_indexes] == TRUE) { #check for ladders at current position
        ladder_start_index <- which(board["ladders"][[1]] == pos) #vector index to find the value at which current ladder starts
        ladder_end_index <- ladder_start_index + 1 #vector index to find the value at which current ladder ends
        play_list["ladder_tally"][[1]][which(board["ladders"][[1]][ladders_starting_indexes] == pos)] <- play_list["ladder_tally"][[1]][which(board["ladders"][[1]][ladders_starting_indexes] == pos)] + 1 #update ladder_tally
        pos <- board["ladders"][[1]][ladder_end_index] #update position
        play_list["move_log"][[1]] <- c(play_list["move_log"][[1]], pos) #update move_log
      } else {
        play_list["move_log"][[1]] <- c(play_list["move_log"][[1]], pos)
      }
    }
  }
  
  print(play_list)
}

#helper function
spin <- function() {
  sample(6, 1)
}

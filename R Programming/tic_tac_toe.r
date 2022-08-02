clear_screen <- function() {
    cat(rep("\n", 50))
}


get_input <- function() {
    if (interactive()) {
        con <- stdin()
    } else {
        con <- "stdin"
    }
    symbol <- readLines(con = con, n = 1)
    return(symbol)
}


print_interface <- function(message = "") {
    clear_screen()
    header <-
"
*****************************
*                           *
*        Tic-Tac-Toe        *
*                           *
*****************************

"
    cat(header)
    cat(message)
    cat(rep("\n", 5))
}


select_game_mode <- function() {
    cat("Enter 1 -- to play human vs human\n")
    cat("Enter 2 -- to play human vs AI\n")
    cat("Choice: ")
    game_mode <- get_input()
    return(game_mode)
}


main <- function() {
    is_normal_header <- TRUE
    header <- "      Select game mode"
    print_interface(header)
    game_mode <- select_game_mode()
    while (game_mode != 1 && game_mode != 2) {
        if (is_normal_header) {
            header <- paste(header, "\n     Please enter 1 or 2!")
            is_normal_header <- FALSE
        }
        print_interface(header)
        game_mode <- select_game_mode()
    }
    if (game_mode == 1) {
        human_vs_human_mode()
    }
    if (game_mode == 2) {

    }
}


get_coordinate <- function() {
    repeat {
        cat("> ")
        coordinate <- get_input()
        if (coordinate == 1 || coordinate == 2 || coordinate == 3) {
            return(as.integer(coordinate))
        }
        cat("Please enter a number 1, 2, 3\n")
    }
}


make_human_move <- function(board, player_turn) {
    if (player_turn == 1) {
        header <- "    First player move"
        player_symbol <- "x"
    } else {
        header <- "   Second player move"
        player_symbol <- "o"
    }

    is_normal_header <- TRUE
    repeat {
        print_interface(header)
        print(board)
        cat("\n")
        cat("Input row:")
        row <- get_coordinate()
        cat("Input column:")
        column <- get_coordinate()
        if (board[row, column] == " ") {
            board[row, column] <- player_symbol
            return(board)
        }
        if (is_normal_header) {
            header <- paste(header, "\n\n  Please, select free space")
            is_normal_header <- FALSE
        }
        
    }
}


is_winner_line <- function(line) {
    if ((line[1] == line[2]) && (line[2] == line[3])) {
        if (line[1] != " ") {
            return(TRUE)
        }
    }
    return(FALSE)
}


check_if_game_over <- function(board) {
    for (i in 1:3) {
        line <- board[, i]
        if (is_winner_line(line)) {
            return(TRUE)
        }
        line <- board[i, ]
        if (is_winner_line(line)) {
            return(TRUE)
        }
    }
    return(FALSE)
}


human_vs_human_mode <- function() {
    board <- matrix(nrow = 3, ncol = 3, " ")
    is_game_over <- FALSE
    player_turn <- 1
    current_step = 1
    for (current_step in 1:9) {
        board <- make_human_move(board, player_turn)
        is_game_over <- check_if_game_over(board)
        if (is_game_over) {
            if (player_turn == 1) {
                header <- ("First player won!")
            } else {
                header <- ("Second player won!")
            }
            print_interface(header)
            print(board)
            return()
        }
        if (player_turn == 1) {
            player_turn <- 2
        } else {
            player_turn <- 1
        }
    }
    print_interface('       A tie!')
    print(board)
    return()
}

main()
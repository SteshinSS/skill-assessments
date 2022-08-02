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
        human_vs_computer_mode()
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
    main_diagonal <- c(board[1, 1], board[2,2], board[3,3])
    if (is_winner_line(main_diagonal)) {
        return(TRUE)
    }
    minor_diagonal <- c(board[3, 1], board[2,2], board[1, 3])
    if (is_winner_line(minor_diagonal)) {
        return(TRUE)
    }
    return(FALSE)
}


human_vs_human_mode <- function() {
    board <- matrix(nrow = 3, ncol = 3, " ")
    player_turn <- 1
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


is_almost_winner_line <- function(line, symbol) {
    total_free <- 0
    total_symbol <- 0
    for (i in 1:3) {
        if (line[i] == " ") {
            total_free <- total_free + 1
        } else {
            if (line[i] == symbol) {
                total_symbol <- total_symbol + 1
            } else {
                return(FALSE)
            }
        }
    }
    if (total_free == 1 & total_symbol == 2) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


make_computer_move <- function(board) {
    win_line <- c("o", "o", "o")

    # Try to finish any line
    for (i in 1:3) {
        if (is_almost_winner_line(board[, i], "o")) {
            board[, i] <- win_line
            return(board)
        }
        if (is_almost_winner_line(board[i, ], "o")) {
            board[i, ] <- win_line
            return(board)
        }
    }
    main_diagonal <- c(board[1,1], board[2,2], board[3,3])
    if (is_almost_winner_line(main_diagonal, "o")) {
        free_space <- which(main_diagonal == " ")
        board[free_space, free_space] <- "o"
        return(board)
    }
    minor_diagonal <- c(board[3, 1], board[2,2], board[1,3])
    if (is_almost_winner_line(minor_diagonal, "o")) {
        free_space <- which(minor_diagonal == " ")
        if (free_space == 1) {
            board[3, 1] <- "o"
            return(board)
        }
        if (free_space == 2) {
            board[2,2] <- "o"
            return(board)
        }
        if (free_space == 3) {
            board[1, 3] <- "o"
            return(board)
        }
    }

    # Try to prevent opponent winning
    for (i in 1:3) {
        if (is_almost_winner_line(board[, i], "x")) {
            board[which(board[, i] == " "), i] <- "o"
            return(board)
        }
        if (is_almost_winner_line(board[i, ], "x")) {
            board[i, which(board[i, ] == " ")] <- "o"
            return(board)
        }
    }
    if (is_almost_winner_line(main_diagonal, "x")) {
        free_space <- which(main_diagonal == " ")
        board[free_space, free_space] <- "o"
        return(board)
    }
    if (is_almost_winner_line(minor_diagonal, "x")) {
        free_space <- which(minor_diagonal == " ")
        if (free_space == 1) {
            board[3, 1] <- "o"
            return(board)
        }
        if (free_space == 2) {
            board[2,2] <- "o"
            return(board)
        }
        if (free_space == 3) {
            board[1, 3] <- "o"
            return(board)
        }
    }


    # If there are no danger, play at random
    free_space <- sample(which(board == " "), 1)
    board[free_space] <- "o"
    return(board)
}


human_vs_computer_mode <- function() {
    board <- matrix(nrow = 3, ncol = 3, " ")
    player_turn <- 1
    current_step <- 1
    repeat {
        board <- make_human_move(board, player_turn)
        is_game_over <- check_if_game_over(board)
        if (is_game_over) {
            header <- ("You won!")
            print_interface(header)
            print(board)
            return()
        }
        current_step <- current_step + 1
        if (current_step == 10) {
            print_interface('       A tie!')
            print(board)
            return()
        }

        board <- make_computer_move(board)
        is_game_over <- check_if_game_over(board)
        if (is_game_over) {
            header <- ("Computer won!")
            print_interface(header)
            print(board)
            return()
        }
        current_step <- current_step + 1

    }
}

main()
library(shiny)
library(shinyjs)

# Définition de l'interface graphique
ui <- fluidPage(
  
  # Titre du jeu
  titlePanel("Jeu de démineur"),
  body {
    background-color: black;
  }
  
  # Sélection du niveau de difficulté
  fluidRow(
    column(width = 4,
           selectInput("level", "Niveau de difficulté",
                       choices = c("Facile" = 1, "Moyen" = 2, "Difficile" = 3),
                       selected = 1))
  ),
  
  # Bouton pour commencer une nouvelle partie
  actionButton("new_game", "Nouvelle partie"),
  
  # Tableau pour le plateau de jeu
  fluidRow(
    column(width = 6,
           div(id = "game_board",
               style = "overflow: auto",
               tableOutput("board")))
  ),
  
  # Affichage du nombre de mines restantes
  fluidRow(
    column(width = 4,
           h4("Mines restantes: "),
           h4(id = "mines_left", "10"))
  ),
  
  # Affichage du temps écoulé
  fluidRow(
    column(width = 4,
           h4("Temps écoulé: "),
           h4(id = "time_elapsed", "0"))
  )
)

# Définition des fonctions pour le jeu de démineur
server <- function(input, output, session) {
  
  # Fonction pour créer un nouveau plateau de jeu
  new_board <- function(n, m, num_mines) {
    board <- matrix(0, n, m)
    for (i in 1:num_mines) {
      mine_row <- sample(1:n, 1)
      mine_col <- sample(1:m, 1)
      while (board[mine_row, mine_col] == "⚑") {
        mine_row <- sample(1:n, 1)
        mine_col <- sample(1:m, 1)
      }
      board[mine_row, mine_col] <- "⚑"
    }
    return(board)
  }
  
  # Fonction pour afficher le plateau de jeu
  output$board <- renderTable({
    if (!is.null(input$new_game)) {
      n <- 0
      m <- 0
      num_mines <- 0
      if (input$level == 1) {
        n <- 8
        m <- 8
        num_mines <- 10
      } else if (input$level == 2) {
        n <- 10
        m <- 10
        num_mines <- 20
      } else if (input$level == 3) {
        n <- 12
        m <- 12
        num_mines <- 30
      }
      board <- new_board(n, m, num_mines)
      output$mines_left <- renderText(num_mines)
      output$time_elapsed <- renderText(0)
      return(board)
    }
  })
  
}

# Lancer l'interface graphique
shinyApp(ui = ui, server = server)
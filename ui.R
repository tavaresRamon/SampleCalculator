pacman::p_load("shinythemes","shiny")

rsconnect::setAccountInfo(name='vy20g3-ramon0tavares',
                          token='8A2BEE33ED3C86D1EEFE4D1275D76F5D',
                          secret='<SECRET>')

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Calculadora de Tamanho da Amostra"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("populacao", "Tamanho da população:", value = 1000),
      numericInput("proporcao", "Proporção (%):", value = 50, min = 0, max = 100),
      numericInput("erroAmostral", "Erro amostral (%):", value = 5, min = 0, max = 100),
      selectInput("nivelConfianca", "Nível de Confiança:",
                  choices = c(90, 95, 98, 99)),
      actionButton("calcular", "Calcular")
    ),
    
    mainPanel(
      verbatimTextOutput("resultado")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calcular, {
    proporcao <- input$proporcao / 100
    erroAmostralToleravel <- input$erroAmostral / 100
    nivelDeConfianca <- match(input$nivelConfianca, c(90, 95, 98, 99))
    tamanhoPopulacao <- input$populacao
    NIVEL_CONFIANCA <- c(1.645, 1.96, 2.33, 2.58)
    
    if (proporcao >= 0 && proporcao < 1) {
      if (erroAmostralToleravel >= 0 && erroAmostralToleravel < 1) {
        if (tamanhoPopulacao > 0) {
          formula <- (tamanhoPopulacao * (NIVEL_CONFIANCA[nivelDeConfianca] ^ 2) * proporcao * (1 - proporcao)) /
            ((tamanhoPopulacao - 1) * (erroAmostralToleravel^2) + (NIVEL_CONFIANCA[nivelDeConfianca] ^ 2) * proporcao * (1 - proporcao))
        } else {
          mensagem <- "Informe um tamanho de população maior que 0!"
        }
      } else {
        mensagem <- "Ajuste corretamente o erro amostral para um valor positivo e menor que 100"
      }
    } else {
      mensagem <- "Ajuste corretamente o tamanho da proporção para um valor positivo e menor que 100"
    }
    if (exists("mensagem")) {
      output$resultado <- renderPrint({
        mensagem
      })
    }
    else if (!is.na(formula)) {
      output$resultado <- renderPrint({
        paste("Para uma população de ",tamanhoPopulacao)
        paste("Tamanho da amostra:", round(formula))
      })
    } else {
      output$resultado <- renderPrint({
        "Valores de entrada inválidos. Verifique os valores fornecidos."
      })
    }
  })
}

shinyApp(ui, server)


#############################################################################################################
#                                       Entrando com identificador de usuário
#############################################################################################################

login_details <- data.frame(user = c("sam", "pam", "ron"),     # Logins e Senhas para acesso
                            pswd = c("123", "123", "123"))
login <- box(
  title = "Login",
  textInput("userName", "Username"),
  passwordInput("passwd", "Password"),    # Estruturando a caixa de login que fica na página principal
  br(),
  actionButton("Login", "Log in")
)

# Entrando com o server

shinyServer(function(input, output, session){ 
  
  # Comando para retornar a página de login
  
  login.page = paste(
    isolate(session$clientData$url_protocol),
    "//",
    isolate(session$clientData$url_hostname),
    ":",
    isolate(session$clientData$url_port),
    sep = ""
  )
  
  USER <- reactiveValues(Logged = F)
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(login_details$user %in% Username)
          Id.password <- which(login_details$pswd %in% Password)
          if (length(Id.username) > 0 & length(Id.password) > 0){
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
  })
  output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE) {
      div(
        sidebarUserPanel(
          isolate(input$userName),
          subtitle = a(icon("usr"), "Logout", href = login.page)
        ),
        sidebarMenu(
          
          # É necessário que parte da interface pessoal (SidebarMenu) esteja no server já que ele vai ser ocultado enquanto
          # não entrarem com o login e a senha 
          
          menuItem("Análise Processual", icon = icon("user-tie"), # Criando a aba de Análise Processual com seus respectivos filtros
                   
                   menuSubItem(icon = NULL,
                               selectInput("Classe1", "Selecione o mês de análise:", choices=c("Janeiro","Fevereiro","Março","Abril","Maio",
                                                                                               "Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro","Todos os meses"),multiple = TRUE, selected = TRUE), tabName = "Classes"),
                   menuSubItem(icon = NULL,
                               selectInput("Classe2", "Selecione o ano de análise:", choices=c("2019","2020","2021","Todos os anos"),multiple = TRUE, selected = TRUE), tabName = "Classes"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("Classe3", "Selecione a classe do processo:", choices=c("Ação Civil Coletiva","Ação Civil de Improbabilidade Administrativa","Ação Civil Pública Cível","Ação de Exigir Contas","Ação Popular","Alimentos - Lei Especial nº 5.478/68","Alvará Judicial","Arresto","Busca e Apreensão","Busca e Apreensão em Alien.",
                                                                                                   "Carta de Ordem Cível","Carta Precatória Cível","Cautelar Fiscal","Cautelar Inominada","Consignação em Pagamento","Cumprimento de Sentença","Cumprimento de Sentença Contra", "Cumprimento Provisório de Senten.","Desapropriação","Despejo por Falta de Pagamento","Embargos à Execução",
                                                                                                   "Embargos à Execução Fiscal","Embargos de Terceiro Cível","Execução de Incompetência","Execução de Título Extrajudicial","Exibição","Exibição de Documento ou Csa Civ.","Habeas Data","Imissão na Posse","Impugnação ao Valor da Causa Cível","Impugnação de Assistência Judiciária","Incidentes","Interdito Prbitório",
                                                                                                   "Liquidação de Sentença pelo Proce.","Liquidação por Arbitramento","Liquidação Provisória de Sentença","Liquidação Provisória Arbitramento","Mandado de Segurança Cível","Mandado de Segurança Coletivo","Monitória","Notificação","Notificação para Explicações","Oposição","Organização e Fiscalização de Funda.","Petição Cível","Procedimento Comum Cível",
                                                                                                   "Procedimento Sumário","Produção Antecipada de Provas","Protesto","Reintegração/Manutenção de Pos.","Restauração de Autos","Retificação de Registro de Imóvel","Tutela Antecipada Antecedente","Usucapião","Registro Inválido"),multiple = TRUE, selected = TRUE), tabName = "Classes"),
                   
                   menuSubItem(icon = NULL,
                               actionButton("Classe4", "Visualizar"))), 
          
          
          menuItem("Manual de Uso", tabName = "Manual", icon = icon("tasks")), # Criando a aba Manual de Uso
          
          menuItem("Quadro resumo", tabName = "Resumo", icon = icon("chart-line")), # Criando a aba de Resumo dos dados
          
          # Eu deixei essa parte como comentário pq seria a visualização por dias decorridos 
          
          #menuItem("Visualização dos dados", tabName = "visual", icon = icon("street-view")),  
          
          menuItem("Visualização dos dados", tabName = "visual2", icon = icon("street-view")), # Criando a aba de Visualização dos dados (tempo que falta em relação aos prazos)
          
          menuItem("Seleção de Prazos", tabName = "prazos", icon = icon("calendar"), # Criando a aba de Prazos com seus respectivos filtros
                   menuSubItem(icon = NULL,
                               textInput("Entradai1", "Número do Processo", value = "Enter text..."), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai1 != 'Enter text...'",
                     textInput("Entradai2", "Apelido do Processo", value = "Enter text..."), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai1 != 'Enter text...'",
                     selectInput("Entradai3", "Selecione a Classe do Processo:", choices=c("--//--","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                                                                                           "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                                                                                           "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                                                                                           "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                                                                                           "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                                                                                           "Recebimento e conclusão do processo","Sentença"),multiple = FALSE, selected = NULL)),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Recebimento, triagem e conclusão do processo'",
                     sliderInput("Entradai4", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Despacho para notificação do Requerido'",
                     sliderInput("Entradai5", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Expedição de notificação'",
                     sliderInput("Entradai6", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Cumprimento da notificação'",
                     sliderInput("Entradai7", "Prazo de duração da etapa", min = 0, max = 10, value = 3), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Oferecimento de manifestação por escrito'",
                     sliderInput("Entradai8", "Prazo de duração da etapa", min = 0, max = 30, value = 15), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Decisão rejeição da ação ou recebimento pet. inicial'",
                     sliderInput("Entradai9", "Prazo de duração da etapa", min = 0, max = 45, value = 30), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Expedição de intimação e/ou citação'",
                     sliderInput("Entradai10", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Cumprimento da intimação e/ou citação'",
                     sliderInput("Entradai11", "Prazo de duração da etapa", min = 0, max = 7, value = 3), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Contestação e/ou agravo de instrumento'",
                     sliderInput("Entradai12", "Prazo de duração da etapa", min = 0, max = 25, value = 15), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Ato ordinatório (Réplica)'",
                     sliderInput("Entradai13", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Expedição de intimação'",
                     sliderInput("Entradai14", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Cumprimento da intimação'",
                     sliderInput("Entradai15", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Réplica'",
                     sliderInput("Entradai16", "Prazo de duração da etapa", min = 0, max = 25, value = 15), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Ato ordinatório (Provas)'",
                     sliderInput("Entradai17", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Expedição de intimação (Requerente)'",
                     sliderInput("Entradai18", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Cumprimento  da intimação'",
                     sliderInput("Entradai19", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Provas'",
                     sliderInput("Entradai20", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Expedição de intimação (Requerido)'",
                     sliderInput("Entradai201", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Cumprimento  da intimação 1'",
                     sliderInput("Entradai202", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Provas 1'",
                     sliderInput("Entradai203", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Decisão saneamento'",
                     sliderInput("Entradai21", "Prazo de duração da etapa", min = 0, max = 15, value = 10), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Expedição de intimação (Requerente) 1'",
                     sliderInput("Entradai22", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Cumprimento da intimação 2'",
                     sliderInput("Entradai23", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Pedido de esclarecimento ou solicitação de ajustes'",
                     sliderInput("Entradai24", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Expedição de intimação (Requerido) 1'",
                     sliderInput("Entradai25", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Cumprimento da intimação 3'",
                     sliderInput("Entradai26", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Pedido de esclarecimento ou solicitação de ajustes 1'",
                     sliderInput("Entradai27", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Prova pericial'",
                     sliderInput("Entradai28", "Prazo de duração da etapa", min = 0, max = 110, value = 90), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Prova oral (audiência de instrução e julgamento)'",
                     sliderInput("Entradai29", "Prazo de duração da etapa", min = 0, max = 75, value = 60), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Ato ordinário (Alegações finais)'",
                     sliderInput("Entradai30", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Expedição de intimação (Requerente) 2'",
                     sliderInput("Entradai31", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Cumprimento da intimação 4'",
                     sliderInput("Entradai32", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Alegações finais'",
                     sliderInput("Entradai33", "Prazo de duração da etapa", min = 0, max = 20, value = 15), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Expedição de intimação (Requerido) 2'",
                     sliderInput("Entradai34", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Cumprimento da intimação 5'",
                     sliderInput("Entradai35", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Alegações finais 1'",
                     sliderInput("Entradai36", "Prazo de duração da etapa", min = 0, max = 20, value = 15), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Recebimento e conclusão do processo'",
                     sliderInput("Entradai37", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.Entradai3 == 'Sentença'",
                     sliderInput("Entradai38", "Prazo de duração da etapa", min = 0, max = 40, value = 30), tabName = "prazos"),
                   
                   menuSubItem(icon = NULL,
                               actionButton("Entradai39", "Criar"))),
          
          menuItem("Atualização dos Prazos", tabName = "AtuaPra", icon = icon("cog", lib = "glyphicon"), # Criando a aba de Atualização dos Prazos com seus respectivos filtros
                   menuSubItem(icon = NULL,
                               selectInput("AtuaPra1", "Apelido do Processo:", choices=sort(as.character(Status[,3])), multiple = FALSE, selected = TRUE), tabName = "AtuaPra"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("AtuaPra2", "Selecione a Fase:", choices=c("--//--","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                                                                                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                                                                                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                                                                                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                                                                                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                                                                                      "Recebimento e conclusão do processo","Sentença"),multiple = FALSE, selected = TRUE), tabName = "AtuaPra"),
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Recebimento, triagem e conclusão do processo'",
                     sliderInput("AtuaPra4", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Despacho para notificação do Requerido'",
                     sliderInput("AtuaPra5", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Expedição de notificação'",
                     sliderInput("AtuaPra6", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Cumprimento da notificação'",
                     sliderInput("AtuaPra7", "Prazo de duração da etapa", min = 0, max = 10, value = 3), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Oferecimento de manifestação por escrito'",
                     sliderInput("AtuaPra8", "Prazo de duração da etapa", min = 0, max = 30, value = 15), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Decisão rejeição da ação ou recebimento pet. inicial'",
                     sliderInput("AtuaPra9", "Prazo de duração da etapa", min = 0, max = 45, value = 30), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Expedição de intimação e/ou citação'",
                     sliderInput("AtuaPra10", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Cumprimento da intimação e/ou citação'",
                     sliderInput("AtuaPra11", "Prazo de duração da etapa", min = 0, max = 7, value = 3), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Contestação e/ou agravo de instrumento'",
                     sliderInput("AtuaPra12", "Prazo de duração da etapa", min = 0, max = 25, value = 15), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Ato ordinatório (Réplica)'",
                     sliderInput("AtuaPra13", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Expedição de intimação'",
                     sliderInput("AtuaPra14", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Cumprimento da intimação'",
                     sliderInput("AtuaPra15", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Réplica'",
                     sliderInput("AtuaPra16", "Prazo de duração da etapa", min = 0, max = 25, value = 15), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Ato ordinatório (Provas)'",
                     sliderInput("AtuaPra17", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Expedição de intimação (Requerente)'",
                     sliderInput("AtuaPra18", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Cumprimento  da intimação'",
                     sliderInput("AtuaPra19", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Provas'",
                     sliderInput("AtuaPra20", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Expedição de intimação (Requerido)'",
                     sliderInput("AtuaPra21", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Cumprimento  da intimação 1'",
                     sliderInput("AtuaPra22", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Provas 1'",
                     sliderInput("AtuaPra23", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Decisão saneamento'",
                     sliderInput("AtuaPra24", "Prazo de duração da etapa", min = 0, max = 15, value = 10), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Expedição de intimação (Requerente) 1'",
                     sliderInput("AtuaPra25", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Cumprimento da intimação 2'",
                     sliderInput("AtuaPra26", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Pedido de esclarecimento ou solicitação de ajustes'",
                     sliderInput("AtuaPra27", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Expedição de intimação (Requerido) 1'",
                     sliderInput("AtuaPra28", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Cumprimento da intimação 3'",
                     sliderInput("AtuaPra29", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Pedido de esclarecimento ou solicitação de ajustes 1'",
                     sliderInput("AtuaPra30", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Prova pericial'",
                     sliderInput("AtuaPra31", "Prazo de duração da etapa", min = 0, max = 110, value = 90), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Prova oral (audiência de instrução e julgamento)'",
                     sliderInput("AtuaPra32", "Prazo de duração da etapa", min = 0, max = 75, value = 60), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Ato ordinário (Alegações finais)'",
                     sliderInput("AtuaPra33", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Expedição de intimação (Requerente) 2'",
                     sliderInput("AtuaPra34", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Cumprimento da intimação 4'",
                     sliderInput("AtuaPra35", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Alegações finais'",
                     sliderInput("AtuaPra36", "Prazo de duração da etapa", min = 0, max = 20, value = 15), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Expedição de intimação (Requerido) 2'",
                     sliderInput("AtuaPra37", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Cumprimento da intimação 5'",
                     sliderInput("AtuaPra38", "Prazo de duração da etapa", min = 0, max = 10, value = 5), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Alegações finais 1'",
                     sliderInput("AtuaPra39", "Prazo de duração da etapa", min = 0, max = 20, value = 15), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Recebimento e conclusão do processo'",
                     sliderInput("AtuaPra40", "Prazo de duração da etapa", min = 0, max = 5, value = 1), tabName = "prazos"),
                   
                   conditionalPanel(
                     condition = "input.AtuaPra2 == 'Sentença'",
                     sliderInput("AtuaPra41", "Prazo de duração da etapa", min = 0, max = 40, value = 30), tabName = "prazos"),
                   
                   menuSubItem(icon = NULL,
                               actionButton("AtuaPra42", "Atualizar"))),
          
          menuItem("Criação dos Dados", tabName = "entrada", icon = icon("database"), # Criando a aba de Criação dos Dados com seus respectivos filtros
                   menuSubItem(icon = NULL,
                               textInput("Entrada1", "Número do Processo", value = "Enter text..."), tabName = "entrada"),
                   
                   conditionalPanel(
                     condition = "input.Entrada1 != 'Enter text...'",
                     textInput("Entrada2", "Apelido do Processo", value = "Enter text..."), tabName = "entrada"),
                   
                   conditionalPanel(
                     condition = "input.Entrada1 != 'Enter text...'",
                     selectInput("Entrada3", "Classificação do processo:", choices=c("Aberto","Encerrado"),multiple = FALSE, selected = NULL)),
                   
                   conditionalPanel(
                     condition = "input.Entrada1 != 'Enter text...'",
                     selectInput("Entrada5", "Selecione a classe do processo:", choices=c("Ação Civil Coletiva","Ação Civil de Improbabilidade Administrativa","Ação Civil Pública Cível","Ação de Exigir Contas","Ação Popular","Alimentos - Lei Especial nº 5.478/68","Alvará Judicial","Arresto","Busca e Apreensão","Busca e Apreensão em Alien.",
                                                                                          "Carta de Ordem Cível","Carta Precatória Cível","Cautelar Fiscal","Cautelar Inominada","Consignação em Pagamento","Cumprimento de Sentença","Cumprimento de Sentença Contra", "Cumprimento Provisório de Senten.","Desapropriação","Despejo por Falta de Pagamento","Embargos à Execução",
                                                                                          "Embargos à Execução Fiscal","Embargos de Terceiro Cível","Execução de Incompetência","Execução de Título Extrajudicial","Exibição","Exibição de Documento ou Coisa Civ.","Habeas Data","Imissão na Posse","Impugnação ao Valor da Causa Cível","Impugnação de Assistência Judiciária","Incidentes","Interdito Proibitório",
                                                                                          "Liquidação de Sentença pelo Proce.","Liquidação por Arbitramento","Liquidação Provisória de Sentença","Liquidação Provisória Arbitramento","Mandado de Segurança Cível","Mandado de Segurança Coletivo","Monitória","Notificação","Notificação para Explicações","Oposição","Organização e Fiscalização de Funda.","Petição Cível","Procedimento Comum Cível",
                                                                                          "Procedimento Sumário","Produção Antecipada de Provas","Protesto","Reintegração/Manutenção de Pos.","Restauração de Autos","Retificação de Registro de Imóvel","Tutela Antecipada Antecedente","Usucapião","Registro Inválido"),multiple = FALSE, selected = FALSE)),
                   
                   conditionalPanel(
                     condition = "input.Entrada1 != 'Enter text...'",
                     dateInput('Entrada51',
                               label = 'Data de Início',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada1 != 'Enter text...'",
                     selectInput("Entrada6", "Selecione a Fase ", choices=c("--//--","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                                                                            "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                                                                            "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                                                                            "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                                                                            "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                                                                            "Recebimento e conclusão do processo","Sentença")
                                 ,multiple = FALSE, selected = TRUE)),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Recebimento, triagem e conclusão do processo'",
                     dateInput('Entrada7',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Despacho para notificação do Requerido'",
                     dateInput('Entrada8',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Expedição de notificação'",
                     dateInput('Entrada9',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Cumprimento da notificação'",
                     dateInput('Entrada10',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Oferecimento de manifestação por escrito'",
                     dateInput('Entrada11',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Decisão rejeição da ação ou recebimento pet. inicial'",
                     dateInput('Entrada12',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Expedição de intimação e/ou citação'",
                     dateInput('Entrada13',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Cumprimento da intimação e/ou citação'",
                     dateInput('Entrada14',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Contestação e/ou agravo de instrumento'",
                     dateInput('Entrada15',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Ato ordinatório (Réplica)'",
                     dateInput('Entrada16',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Expedição de intimação'",
                     dateInput('Entrada17',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Cumprimento da intimação'",
                     dateInput('Entrada18',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Réplica'",
                     dateInput('Entrada19',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Ato ordinatório (Provas)'",
                     dateInput('Entrada20',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Expedição de intimação (Requerente)'",
                     dateInput('Entrada21',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Cumprimento  da intimação'",
                     dateInput('Entrada22',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Provas'",
                     dateInput('Entrada23',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Expedição de intimação (Requerido)'",
                     dateInput('Entrada24',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Cumprimento da intimação 1'",
                     dateInput('Entrada25',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Provas 1'",
                     dateInput('Entrada26',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Decisão saneamento'",
                     dateInput('Entrada27',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Expedição de intimação (Requerente) 1'",
                     dateInput('Entrada28',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Cumprimento da intimação 2'",
                     dateInput('Entrada29',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Pedido de esclarecimento ou solicitação de ajustes'",
                     dateInput('Entrada30',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Expedição de intimação (Requerido) 1'",
                     dateInput('Entrada31',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Cumprimento da intimação 3'",
                     dateInput('Entrada32',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Pedido de esclarecimento ou solicitação de ajustes 1'",
                     dateInput('Entrada33',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Prova pericial'",
                     dateInput('Entrada34',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Prova oral (audiência de instrução e julgamento)'",
                     dateInput('Entrada35',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Ato ordinário (Alegações finais)'",
                     dateInput('Entrada36',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Expedição de intimação (Requerente) 2'",
                     dateInput('Entrada37',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Cumprimento da intimação 4'",
                     dateInput('Entrada38',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Alegações finais'",
                     dateInput('Entrada39',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Expedição de intimação (Requerido) 2'",
                     dateInput('Entrada40',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Cumprimento da intimação 5'",
                     dateInput('Entrada41',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Alegações finais 1'",
                     dateInput('Entrada42',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Recebimento e conclusão do processo'",
                     dateInput('Entrada43',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   conditionalPanel(
                     condition = "input.Entrada6 == 'Sentença'",
                     dateInput('Entrada44',
                               label = 'Data do termino',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   
                   menuSubItem(icon = NULL,
                               actionButton("Entrada45", "Criar"))),
          
          
          menuItem("Atualização dos dados", tabName = "atualizacao", icon = icon("sync"), # Criando a aba de Atualização dos Dados com seus respectivos filtros
                   menuSubItem(icon = NULL,
                               selectInput("Entradas1", "Apelido do Processo:", choices=sort(as.character(Status[,3])), multiple = FALSE, selected = TRUE), tabName = "atualizacao"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("Entradas2", "Classificação do Processo:", choices=c("Aberto", "Encerrado"), multiple = FALSE, selected = TRUE), tabName = "atualizacao"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("Entradas4", "Selecione a Fase:", choices=c("--//--","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                                                                                       "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                                                                                       "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                                                                                       "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                                                                                       "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                                                                                       "Recebimento e conclusão do processo","Sentença"),multiple = FALSE, selected = TRUE), tabName = "atualizacao"),
                   
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Recebimento, triagem e conclusão do processo'",
                     dateInput('Entradas6',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Despacho para notificação do Requerido'",
                     dateInput('Entradas8',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Expedição de notificação'",
                     dateInput('Entradas10',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Cumprimento da notificação'",
                     dateInput('Entradas12',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Oferecimento de manifestação por escrito'",
                     dateInput('Entradas14',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Decisão rejeição da ação ou recebimento pet. inicial'",
                     dateInput('Entradas16',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Expedição de intimação e/ou citação'",
                     dateInput('Entradas18',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Cumprimento da intimação e/ou citação'",
                     dateInput('Entradas20',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Contestação e/ou agravo de instrumento'",
                     dateInput('Entradas22',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Ato ordinatório (Réplica)'",
                     dateInput('Entradas24',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Expedição de intimação'",
                     dateInput('Entradas26',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Cumprimento da intimação'",
                     dateInput('Entradas28',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Réplica'",
                     dateInput('Entradas30',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Ato ordinatório (Provas)'",
                     dateInput('Entradas32',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Expedição de intimação (Requerente)'",
                     dateInput('Entradas34',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Cumprimento  da intimação'",
                     dateInput('Entradas36',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Provas'",
                     dateInput('Entradas38',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Expedição de intimação (Requerido)'",
                     dateInput('Entradas40',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Cumprimento da intimação 1'",
                     dateInput('Entradas42',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Provas 1'",
                     dateInput('Entradas44',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Decisão saneamento'",
                     dateInput('Entradas46',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Expedição de intimação (Requerente) 1'",
                     dateInput('Entradas48',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Cumprimento da intimação 2'",
                     dateInput('Entradas50',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Pedido de esclarecimento ou solicitação de ajustes'",
                     dateInput('Entradas52',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Expedição de intimação (Requerido) 1'",
                     dateInput('Entradas54',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Cumprimento da intimação 3'",
                     dateInput('Entradas56',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Pedido de esclarecimento ou solicitação de ajustes 1'",
                     dateInput('Entradas58',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Prova pericial'",
                     dateInput('Entradas60',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Prova oral (audiência de instrução e julgamento)'",
                     dateInput('Entradas62',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Ato ordinário (Alegações finais)'",
                     dateInput('Entradas64',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Expedição de intimação (Requerente) 2'",
                     dateInput('Entradas66',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Cumprimento da intimação 4'",
                     dateInput('Entradas68',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Alegações finais'",
                     dateInput('Entradas70',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Expedição de intimação (Requerido) 2'",
                     dateInput('Entradas72',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Cumprimento da intimação 5'",
                     dateInput('Entradas74',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Alegações finais 1'",
                     dateInput('Entradas76',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Recebimento e conclusão do processo'",
                     dateInput('Entradas78',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   conditionalPanel(
                     condition = "input.Entradas4 == 'Sentença'",
                     dateInput('Entradas80',
                               label = 'Data de Fim',
                               value = Sys.Date(),
                               language = "pt-BR"
                     )),
                   
                   
                   menuSubItem(icon = NULL,
                               actionButton("Entradas81", "Atualizar"))),
          
          
          menuItem("Dashboard", icon = icon("dashboard"), # Criando a aba de Dashboard com seus respectivos filtros
                   menuSubItem(icon = NULL,
                               selectInput("dashboard1", "Selecione o mês de análise:", choices=c("Janeiro","Fevereiro","Março","Abril","Maio",
                                                                                                  "Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro","Todos os meses"),multiple = TRUE, selected = TRUE), tabName = "dashboard"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("dashboard2", "Selecione o ano de análise:", choices=c("2019","2020","2021","Todos os anos"),multiple = TRUE, selected = TRUE), tabName = "dashboard"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("dashboard4", "Selecione a classificação:", choices=c("Aberto","Encerrado","Ambos"),multiple = TRUE, selected = TRUE), tabName = "dashboard"),
                   
                   
                   menuSubItem(icon = NULL,
                               selectInput("dashboard3", "Selecione a classe do processo:", choices=c("Ação Civil Coletiva","Ação Civil de Improbabilidade Administrativa","Ação Civil Pública Cível","Ação de Exigir Contas","Ação Popular","Alimentos - Lei Especial nº 5.478/68","Alvará Judicial","Arresto","Busca e Apreensão","Busca e Apreensão em Alien.",
                                                                                                      "Carta de Ordem Cível","Carta Precatória Cível","Cautelar Fiscal","Cautelar Inominada","Consignação em Pagamento","Cumprimento de Sentença","Cumprimento de Sentença Contra", "Cumprimento Provisório de Senten.","Desapropriação","Despejo por Falta de Pagamento","Embargos à Execução",
                                                                                                      "Embargos à Execução Fiscal","Embargos de Terceiro Cível","Execução de Incompetência","Execução de Título Extrajudicial","Exibição","Exibição de Documento ou Coisa Civ.","Habeas Data","Imissão na Posse","Impugnação ao Valor da Causa Cível","Impugnação de Assistência Judiciária","Incidentes","Interdito Proibitório",
                                                                                                      "Liquidação de Sentença pelo Proce.","Liquidação por Arbitramento","Liquidação Provisória de Sentença","Liquidação Provisória Arbitramento","Mandado de Segurança Cível","Mandado de Segurança Coletivo","Monitória","Notificação","Notificação para Explicações","Oposição","Organização e Fiscalização de Funda.","Petição Cível","Procedimento Comum Cível",
                                                                                                      "Procedimento Sumário","Produção Antecipada de Provas","Protesto","Reintegração/Manutenção de Pos.","Restauração de Autos","Retificação de Registro de Imóvel","Tutela Antecipada Antecedente","Usucapião","Registro Inválido"),multiple = FALSE, selected = FALSE), tabName = "dashboard"),
                   
                   menuSubItem(icon = NULL,
                               actionButton("dashboard5", "Visualizar"))),
          
          
          menuItem("Pré-requisitos", tabName = "requisitos", icon = icon("calculator"))) # Criando a aba de Pré requisitos
        
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$Logged == TRUE) {
      
    } else {   # Final do código para login 
      login
    }
  })
  
  observe({  # Representa o primeiro critério de atualização do banco de dados (no total são 3)
    # Nesse momento o servidor será reiniciado a cada 20.000 milésimos ou 20 segundos para que os dados de criação e atualização sejam atualizados na aba de visualização e os gráficos fiquem sempre atualizados também
    
    invalidateLater(200000, session) # Código para reiniciar
    
    ######################### Entrando com os bancos de dados para criação #################################
    
    #dados <<- read_excel("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\Base2.xlsx", sheet=1, col_names=TRUE)
    dadosP <<- read_excel("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\Base2.xlsx", sheet=2, col_names=TRUE)
    Date <<- read_excel("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\Base2.xlsx", sheet=3, col_names=TRUE)
    
    dados <- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\completo.csv")
    AtuaP <<- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\output_prazos2.csv")
    Date2 <<- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\Data.csv")
    Date4 <<- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\Data2.csv")
    Prazo <<- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\output_prazos.csv")
    dados2 <<- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\output.csv")
    dados5 <<- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\output2.csv")
    Status <<- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\output.csv")
    
    dados <- dados[,-1]
    Prazo <<- Prazo[,-1]
    dados2 <<- dados2[,-1]
    dados5 <<- dados5[,-1]
    Date2 <<- Date2[,-1]
    Date4 <<- Date4[,-1]
    AtuaP <<- AtuaP[,-1]
    
    
    names(dados) <- c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                      "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                      "Recebimento e conclusão do processo","Sentença")
    
    
    # Como algumas colunas podem assumir valores 0 (tempo decorrido), foi criado o loop abaixo para substituir o 0 por 0.1
    
    for (i in 1:nrow(dados)) {
      if(dados[i,8] == 0 & dados[i,10] > 0){dados[i,8] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,8] == 0 & dados[i,9] > 0){dados[i,8] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,9] == 0 & dados[i,10] > 0){dados[i,9] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,13] == 0 & dados[i,14] > 0){dados[i,13] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,16] == 0 & dados[i,18] > 0){dados[i,16] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,16] == 0 & dados[i,17] > 0){dados[i,16] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,17] == 0 & dados[i,18] > 0){dados[i,17] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,20] == 0 & dados[i,22] > 0){dados[i,20] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,20] == 0 & dados[i,21] > 0){dados[i,20] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,21] == 0 & dados[i,22] > 0){dados[i,21] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,24] == 0 & dados[i,25] > 0){dados[i,24] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,28] == 0 & dados[i,29] > 0){dados[i,28] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,31] == 0 & dados[i,32] > 0){dados[i,31] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,36] == 0 & dados[i,38] > 0){dados[i,36] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,36] == 0 & dados[i,37] > 0){dados[i,36] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,37] == 0 & dados[i,38] > 0){dados[i,37] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,40] == 0 & dados[i,41] > 0){dados[i,40] = 0.1}
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,43] == 0 & dados[i,44] > 0){dados[i,43] = 0.1}
    }
    
    ################################ Fazendo a limpeza da Base de Prazos ########################################
    
    #Foram retiradas as linhas com Nº do Processo igual a "Enter text...", ou seja, o nome padrão de entrada
    
    dadosP <- Prazo
    
    linhas_tabela_limpeza2 <- grep("Enter text...", dadosP$Nº.Processo, fixed=TRUE)
    
    for (i in 1:length(linhas_tabela_limpeza2)) {
      if(length(linhas_tabela_limpeza2) != 0){
        dadosP <- dadosP[-c(linhas_tabela_limpeza2[i]),]
      }
    }
    
    ######################## Entrando com a Base de Prazos advindas da Atualização ##########################
    
    names(AtuaP) <- c("Nº Processo","Apelido","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                      "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                      "Recebimento e conclusão do processo","Sentença")
    
    
    names(dadosP) <- c("Nº Processo","Apelido","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                       "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                       "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                       "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                       "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                       "Recebimento e conclusão do processo","Sentença")
    
    dadosP <- rbind(dadosP,AtuaP)
    
    # Foi removido o processo mais antigo e substituido pelo processo mais novo 
    
    linhas_tabela_limpeza_Prazos <- grep(AtuaP$Apelido, dadosP$Apelido, fixed=TRUE)
    
    if(length(linhas_tabela_limpeza_Prazos) != 0){
      dadosP <- dadosP[-c(linhas_tabela_limpeza_Prazos[1:(length(linhas_tabela_limpeza_Prazos)-1)]),]
    }
    
    
    ######################## Juntando os Prazos já atualizados com o Banco de dados ##########################
    
    names(dados) <- c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                      "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                      "Recebimento e conclusão do processo","Sentença")
    
    names(dadosP) <- c("Nº Processo","Apelido", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                       "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                       "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                       "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                       "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                       "Recebimento e conclusão do processo","Sentença")
    
    dadosPrazo <- merge(dados,dadosP, by="Nº Processo")
    
    # O loop abaixo foi para realizar a análise dos Status dos processos e colocar as denominações "OK", "Atrasado" e "Alerta"
    
    # O processo terá o Status "OK" se o tempo decorrido for 80% menor que o delimitado no prazo
    
    for (i in 1:nrow(dadosPrazo)) { # Para colocar o Status de "OK" com o primeiro estágio igual a 0
      
      locali <- grep('^0',dadosPrazo[i,1:44])
      
      if(length(locali) != 0 & length(locali) > 1){
        
        if(dadosPrazo[i,3] == "Aberto" & dadosPrazo[i,7] == 0 & dadosPrazo[i,locali[2]] == 0 & 
           dadosPrazo[i,locali[2]+1] == 0 & dadosPrazo[i,locali[2]-1] < dadosPrazo[i,locali[2]+38]*0.80){ dadosPrazo[i,4] = "Ok"}
      }
      
      else{dadosPrazo[i,4] = dadosPrazo[i,4]}
    }
    
    for (i in 1:nrow(dadosPrazo)) { # Para colocar o Status de "OK" com o primeiro estágio diferente de 0
      
      locali <- grep("^0",dadosPrazo[i,1:44])
      
      if(length(locali) != 0 & length(locali) > 1){
        
        if(dadosPrazo[i,3] == "Aberto" & dadosPrazo[i,7] != 0 & dadosPrazo[i,locali[1]] == 0 & 
           dadosPrazo[i,locali[1]+1] == 0 & dadosPrazo[i,locali[1]-1] < dadosPrazo[i,locali[1]+38]*0.80){dadosPrazo[i,4] = "Ok"}
      }
      else{dadosPrazo[i,4] = dadosPrazo[i,4]}
    }
    
    for (i in 1:nrow(dadosPrazo)) { # Para colocar o Status de "OK" no ultimo termo (Sentença)
      
      if(dadosPrazo[i,3] == "Aberto" & dadosPrazo[i,44] != 0 & dadosPrazo[i,44] < dadosPrazo[i,44+39]*0.80){ dadosPrazo[i,4] = "Ok"}
      
    }
    
    # O processo terá o Status "Alerta" se o tempo decorrido for 80% maior que o delimitado no prazo
    
    for (i in 1:nrow(dadosPrazo)) { # Para colocar o Status de "Alerta" com o primeiro estágio igual a 0
      
      locali <- grep("0",dadosPrazo[i,1:44])
      
      if(length(locali) != 0 & length(locali) > 1){
        
        if(dadosPrazo[i,3] == "Aberto" & dadosPrazo[i,7] == 0 & dadosPrazo[i,locali[2]] == 0 & 
           dadosPrazo[i,locali[2]+1] == 0 & dadosPrazo[i,locali[2]-1] >= dadosPrazo[i,locali[2]+38]*0.80 & dadosPrazo[i,locali[2]-1] <= dadosPrazo[i,locali[2]+38]
        ){ dadosPrazo[i,4] = "Alerta"}}
      
      else{dadosPrazo[i,4] = dadosPrazo[i,4]}
    }
    
    for (i in 1:nrow(dadosPrazo)) { # Para colocar o Status de "Alerta" com o primeiro estágio diferente de 0
      
      locali <- grep("^0",dadosPrazo[i,1:44])
      
      if(length(locali) != 0 & length(locali) > 1){
        
        if(dadosPrazo[i,3] == "Aberto" & dadosPrazo[i,7] != 0 & dadosPrazo[i,locali[1]] == 0 & 
           dadosPrazo[i,locali[1]+1] == 0 & dadosPrazo[i,locali[1]-1] >= dadosPrazo[i,locali[1]+38]*0.80 & dadosPrazo[i,locali[1]-1] <= dadosPrazo[i,locali[1]+38]
        ){ dadosPrazo[i,4] = "Alerta"}}
      
      else{dadosPrazo[i,4] = dadosPrazo[i,4]}
    }
    
    for (i in 1:nrow(dadosPrazo)) { # Para colocar o Status de "Alerta" no ultimo termo (Sentença)
      
      if(dadosPrazo[i,3] == "Aberto" & dadosPrazo[i,44] != 0 & dadosPrazo[i,44] >= dadosPrazo[i,44+39]*0.80 & dadosPrazo[i,44] <= dadosPrazo[i,44+39])
      { dadosPrazo[i,4] = "Alerta"}
      
    }
    
    # O processo terá o Status "Atrasado" se o tempo decorrido for maior que o delimitado no prazo
    
    for (i in 1:nrow(dadosPrazo)) { # Para colocar o Status de "Atrasado" com o primeiro estágio igual a 0
      
      locali <- grep("^0",dadosPrazo[i,1:44])
      
      if(length(locali) != 0 & length(locali) > 1){
        
        if(dadosPrazo[i,3] == "Aberto" & dadosPrazo[i,7] == 0 & dadosPrazo[i,locali[2]] == 0 & 
           dadosPrazo[i,locali[2]+1] == 0 & dadosPrazo[i,locali[2]-1] > dadosPrazo[i,locali[2]+38]){ dadosPrazo[i,4] = "Atrasado"}
      }
      
      else{dadosPrazo[i,4] = dadosPrazo[i,4]}
    }
    
    for (i in 1:nrow(dadosPrazo)) { # Para colocar o Status de "Atrasado" com o primeiro estágio diferente de 0
      
      locali <- grep("^0",dadosPrazo[i,1:44])
      
      if(length(locali) != 0 & length(locali) > 1){
        
        if(dadosPrazo[i,3] == "Aberto" & dadosPrazo[i,7] != 0 & dadosPrazo[i,locali[1]] == 0 & 
           dadosPrazo[i,locali[1]+1] == 0 & dadosPrazo[i,locali[1]-1] > dadosPrazo[i,locali[1]+38]){ dadosPrazo[i,4] = "Atrasado"}
      } 
      
      else{dadosPrazo[i,4] = dadosPrazo[i,4]}
    }
    
    for (i in 1:nrow(dadosPrazo)) { # Para colocar o Status de "Atrasado" no ultimo termo (Sentença)
      
      if(dadosPrazo[i,3] == "Aberto" & dadosPrazo[i,44] != 0 & dadosPrazo[i,44] > dadosPrazo[i,44+39]){ dadosPrazo[i,4] = "Atrasado"}
      
    }
    
    dados <- dadosPrazo[,1:44]
    
    names(dados) <- c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                      "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                      "Recebimento e conclusão do processo","Sentença")
    
    ################################### Entrando com a tabela de Datas  #######################################
    
    # Seria uma limpeza do banco de dados das Datas que foram criadas na Aba Criação de Dados
    
    names(Date2) <- c("Nº Processo","Apelido", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                      "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                      "Recebimento e conclusão do processo","Sentença")
    
    names(Date) <- c("Nº Processo","Apelido", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                     "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                     "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                     "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                     "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                     "Recebimento e conclusão do processo","Sentença")
    
    
    # Removendo os apelidos que foram duplicados no processo
    
    linhas_tabela_limpeza_Prazos <- grep(Date2$Apelido,Date$Apelido, fixed=TRUE)
    
    if(length(linhas_tabela_limpeza_Prazos) == 0){
      Date <- rbind(Date,Date2)
    }
    
    if(length(linhas_tabela_limpeza_Prazos) != 0){
      Date <- Date
    }
    
    # Removendo os processos que possuem Apelidos igual a "Enter text...", ou seja, saída padrão do sistema
    
    linhas_tabela_limpeza2 <- grep("Enter text...", Date$Apelido, fixed=TRUE)
    
    for (i in 1:length(linhas_tabela_limpeza2)) {
      if(length(linhas_tabela_limpeza2) != 0){
        Date <- Date[-c(linhas_tabela_limpeza2[i]),]
      }
    }
    
    names(Date) <- c("Nº Processo","Apelido", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                     "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                     "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                     "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                     "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                     "Recebimento e conclusão do processo","Sentença")
    
    names(Date4) <- c("Nº Processo","Apelido", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                      "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                      "Recebimento e conclusão do processo","Sentença")
    
    # Juntado as Datas acima com as Datas atualizadas da aba de Atualização de Dados
    
    Date <- rbind(Date,Date4)
    
    # Removendo os apelidos que foram duplicados no processo
    
    linhas_tabela_limpeza_Prazos <- grep(Date4$Apelido,Date$Apelido, fixed=TRUE)
    
    if(length(linhas_tabela_limpeza_Prazos) != 0){
      Date <- Date[-linhas_tabela_limpeza_Prazos[1],]
    }
    
    
    # Juntando as bases de datas atualizadas com o banco de tempos decorridos pelo processo
    
    dadosData <<- merge(dados,Date, by="Nº Processo")
    
    # Foram realizados loops para descobrir em qual periodo de tempo foi realizado a última movimentação no processo
    
    for (i in 1:nrow(dadosData)) { # Identificando o ano
      localiza <- grep("^0",dadosData[i,1:44])
      if(length(localiza) != 0 & length(localiza) > 1){
        if(dadosData[i,7] == 0 & dadosData[i,localiza[2]] == 0 & dadosData[i,localiza[2]+1] == 0){ dadosData[i,84] = substring(dadosData[i,localiza[2]+39],1,4)}
      }
    }
    
    
    for (i in 1:nrow(dadosData)) { # Identificando o ano
      localiza <- grep("^0",dadosData[i,1:44])
      if(length(localiza) != 0 & length(localiza) > 1){
        if(dadosData[i,7] != 0 & dadosData[i,localiza[1]] == 0 & dadosData[i,localiza[1]+1] == 0){ dadosData[i,84] = substring(dadosData[i,localiza[2]+39],1,4)}
      }
    }
    
    for (i in 1:nrow(dadosData)) { # Identificando o ano
      if(length(localiza) == 0 & dadosData[i,44] != 0){
        dadosData[i,84] = substring(dadosData[i,83],1,4)}}
    
    
    for (i in 1:nrow(dadosData)) { # Caso o ultimo termo seja diferente de 0
      
      if(dadosData[i,44] != 0){dadosData[i,84] = substring(dadosData[i,83],1,4)}
      
    }
    
    
    for (i in 1:nrow(dadosData)) { # Identificando o mês
      localiza <- grep("^0",dadosData[i,1:44])
      if(length(localiza) != 0 & length(localiza) > 1){
        if(dadosData[i,7] == 0 & dadosData[i,localiza[2]] == 0 & dadosData[i,localiza[2]+1] == 0){ dadosData[i,85] = substring(dadosData[i,localiza[2]+39],6,7)}
      }
    }
    
    for (i in 1:nrow(dadosData)) { # Identificando o mês
      if(length(localiza) == 0 & dadosData[i,44] != 0){
        dadosData[i,85] = substring(dadosData[i,83],6,7)}}
    
    
    for (i in 1:nrow(dadosData)) { # Identificando o mês
      localiza <- grep("^0",dadosData[i,1:44])
      if(length(localiza) != 0 & length(localiza) > 1){
        if(dadosData[i,7] != 0 & dadosData[i,localiza[1]] == 0 & dadosData[i,localiza[1]+1] == 0){ dadosData[i,85] = substring(dadosData[i,localiza[2]+39],6,7)}
      }
    }
    
    
    for (i in 1:nrow(dadosData)) { # Caso o ultimo termo seja diferente de 0
      
      if(dadosData[i,44] != 0)
      { dadosData[i,85] = substring(dadosData[i,83],6,7)}
      
    }
    
    names(dadosData)[names(dadosData) == "V84"] <- "Ano"
    names(dadosData)[names(dadosData) == "V85"] <- "Mes"
    
    dadosData$Mes <- as.numeric(dadosData$Mes)
    
    for (i in 1:nrow(dadosData)) {
      if(dadosData[i,85] == 1){dadosData[i,85] = "Janeiro"}
      if(dadosData[i,85] == 2){dadosData[i,85] = "Fevereiro"}
      if(dadosData[i,85] == 3){dadosData[i,85] = "Março"}
      if(dadosData[i,85] == 4){dadosData[i,85] = "Abril"}
      if(dadosData[i,85] == 5){dadosData[i,85] = "Maio"}
      if(dadosData[i,85] == 6){dadosData[i,85] = "Junho"}
      if(dadosData[i,85] == 7){dadosData[i,85] = "Julho"}
      if(dadosData[i,85] == 8){dadosData[i,85] = "Agosto"}
      if(dadosData[i,85] == 9){dadosData[i,85] = "Setembro"}
      if(dadosData[i,85] == 10){dadosData[i,85] = "Outubro"}
      if(dadosData[i,85] == 11){dadosData[i,85] = "Novembro"}
      if(dadosData[i,85] == 12){dadosData[i,85] = "Dezembro"}
    }
    
    #Calculando o tempo médio que cada processo demorou em suas etapas 
    
    dadosData[,86] <- rowMeans(dadosData[,7:44])
    names(dadosData)[names(dadosData) == "V86"] <- "Media"
    
    # Calculando a tempo total que cada processo demorou em suas etapas
    
    dadosData[,87] <- rowSums(dadosData[,7:44])
    names(dadosData)[names(dadosData) == "V87"] <- "Soma"
    
    ############################# Estruturando os Gráficos para a Aba Manual de Uso ###########################
    
    output$histogramManual1 <- renderPlot({ ########################## Imagem 1 - Manual de Uso
      fit <- survfit(Surv(time, status) ~ sex, data = lung)
      
      ggsurvplot(
        fit,
        data = lung,
        size = 1,                 # change line size
        palette =
          c("#E7B800", "#2E9FDF"),# custom color palettes
        conf.int = TRUE,          # Add confidence interval
        pval = TRUE,              # Add p-value
        legend.labs =
          c("Ok", "Alerta"),  
        ggtheme = theme_bw()     # Change ggplot2 theme
      )
    })
    
    output$histogramManual4 <- renderPlot({ ########################## Imagem 4 - Manual de Uso
      
      supp <- c("OJ","OJ","OJ","VC","VC","VC","VC")
      dose <- c(0.5,0.5,0.5,1,1,1,2)
      len <- c(15,10,22,10,31,25,10)
      sd <- c(3,2,2,5,4,5,5)
      df3 <- data.frame(supp,dose,len,sd)
      df3$supp <- as.character(df3$supp)
      
      for (i in 1:nrow(df3)) {
        if(df3$supp[i]=="OJ"){df3$supp[i] = "Encerrado"} else{df3$supp[i] = "Aberto"}
      }
      
      for (i in 1:nrow(df3)) {
        if(df3$dose[i]==0.5){df3$dose[i] = "Processo 1"} else if(df3$dose[i]==1){df3$dose[i] = "Processo 2"} else{df3$dose[i] = "Processo 3"}
      }
      
      df5 <- df2
      sf22 <- df3
      sf33 <- df5
      names(sf22) <- c("Status", "dose", "Data de uma Ação de interesse Y", "Tempo de vida")
      names(sf33) <- c("Status", "dose", "Data de uma Ação de interesse Y", "Tempo de vida")
      
      df4 <- rbind(sf22,sf33)
      df4$`Data de uma Ação de interesse Y`[13] <- 20
      ggplot(df4, aes(x = dose, y = `Data de uma Ação de interesse Y`)) +
        geom_point(aes(color = Status, size = `Tempo de vida`), alpha = 0.5) +
        scale_color_manual(values = c("#999999", "orange")) +
        scale_size(range = c(0.5, 12))+  theme(axis.title.x=element_blank(),
                                               axis.text.x=element_text("Classe do processo"),
                                               axis.ticks.x=element_blank())
      
    })
    
    output$histogramManual3 <- renderPlot({ ########################## Imagem 3 - Manual de Uso
      
      # make some data
      df <- ToothGrowth
      df$dose <- as.factor(df$dose)
      data_summary <- function(data, varname, groupnames){
        require(plyr)
        summary_func <- function(x, col){
          c(mean = mean(x[[col]], na.rm=TRUE),
            sd = sd(x[[col]], na.rm=TRUE))
        }
        data_sum<-ddply(data, groupnames, .fun=summary_func,
                        varname)
        data_sum <- rename(data_sum, c("mean" = varname))
        return(data_sum)
      }
      
      df2 <- data_summary(ToothGrowth, varname="len",
                          groupnames=c("supp", "dose"))
      
      df2$supp <- as.character(df2$supp)
      
      for (i in 1:nrow(df2)) {
        if(df2$supp[i]=="OJ"){df2$supp[i] = "Encerrado"} else{df2$supp[i] = "Aberto"}
      }
      
      for (i in 1:nrow(df2)) {
        if(df2$dose[i]==0.5){df2$dose[i] = "Processo 1"} else if(df2$dose[i]==1){df2$dose[i] = "Processo 2"} else{df2$dose[i] = "Processo 3"}
      }
      
      names(df2) <- c("Processo", "dose", "Número de processos", "Tempo de vida")
      # Convert dose to a factor variable
      df2$dose=as.factor(df2$dose)
      ggplot(df2, aes(x=dose, y=`Número de processos`, fill=Processo)) +
        geom_bar(stat="identity", color="black", position=position_dodge()) +
        geom_errorbar(aes(ymin=`Número de processos`, ymax=`Número de processos`+`Tempo de vida`), width=.2,
                      position=position_dodge(.9)) + theme(legend.position="bottom")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x=element_text(df2$dose),
                                                                                             axis.ticks.x=element_blank())+
        scale_fill_manual("Status", values = c("Aberto" = "#999999", "Encerrado" = "orange"))
      
      
      
    })
    
    output$histogramManual2 <- renderPlot({ ########################## Imagem 2 - Manual de Uso
      
      lung2 <- lung
      
      for (i in 1:nrow(lung2)) {
        if(lung2$sex[i]==1){lung2$sex[i] = "Encerrado"} else {lung2$sex[i] = "Aberto"}
      }
      
      names(lung2) <-  c("inst","time","status","age","Status")
      fit2 <- survfit(Surv(time, status) ~ Status, data = lung2)
      
      ggsurvplot(fit2, conf.int = TRUE, data = lung2,
                 palette = c("#E7B800", "#2E9FDF"),
                 fun = "event", legend.labs =
                   c("Ok", "Alerta"),  
                 ggtheme = theme_bw()) # Change ggplot2 theme
    })
    
    output$histogramManual5 <- renderPlot({ ########################## Imagem 5 - Manual de Uso
      resumo1 <- data.frame(table(dados$`Classe do Processo`, by=dados$Status))
      
      resumo1 <- resumo1[-c(grep( "Em análise",resumo1$by)),]
      
      names(resumo1)[names(resumo1) == "by"] <- "Status"
      resumo1 <- resumo1[resumo1$Freq != 0,]
      resumo1 <- resumo1[order(resumo1$Freq),]
      
      resumo1$soma <- ave(resumo1$Freq, resumo1$Var1, FUN=cumsum)
      
      ggplot(data=resumo1, aes(x=Var1, y=Freq, fill=Status)) +
        geom_bar(stat="identity")+ coord_flip() + theme(legend.position = "top")+
        geom_text(aes(y=soma, label=Freq), vjust=1.1, 
                  color="white", size=5,hjust = 2)+
        scale_fill_brewer(palette="Paired")+
        theme_minimal()+  theme(axis.title.x=element_blank(),
                                axis.text.x=element_text(),
                                axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                      axis.text.y=element_text(),
                                                                      axis.ticks.y=element_blank())
      
    })
    
    output$histogramManual7 <- renderPlot({ ########################## Imagem 7 - Manual de Uso
      
      qqPlot(lung$age, xlab = "", ylab = "", col = c("black"),col.lines="red",lwd = 1.5)
    })
    
    output$histogramManual8 <- renderPlot({ ########################## Imagem 8 - Manual de Uso
      lung2 <- lung
      names(lung2) <- c("inst","time","status1","Tempo de vida", "Status")
      Ajuste <-coxph(Surv(time, status1) ~ Status + `Tempo de vida`, data = lung2)
      cox.zph(Ajuste)
      
      par(mfrow=c(2,1))
      ggcoxzph(cox.zph(Ajuste))
    })
    
    output$histogramManual11 <- renderPlot({ ########################## Imagem 11 - Manual de Uso
      
      Classe <- c("Desapropriação","Desapropriação","Ação Civil Coletiva","Ação Civil Coletiva","Busca e Apreensão","Busca e Apreensão")
      Status <- c("Aberto","Encerrado","Aberto","Encerrado","Aberto","Encerrado")
      Media <- c(10,8.7,15,12,19,18)
      resumo2 <- data.frame(Status,Classe,Media)
      
      names(resumo2) <- c("Status","Classe","Media")
      
      ggplot(resumo2, aes(x=Classe, y=Media, group=Status)) +
        geom_line(aes(linetype=Status, color=Status), size=1)+
        geom_point(aes(color=Status),size=3)+theme_bw()+
        theme(legend.position="bottom")
      
    })
    
    output$histogramManual12 <- renderPlot({ ########################## Imagem 6 - Manual de Uso
      resumo3 <- aggregate(Media ~ Classificação+`Classe do Processo`, data=dadosData, mean, na.rm=TRUE)
      
      resumo3 <- resumo3[resumo3$Classificação=="Aberto",]
      
      for (i in 1:nrow(resumo3)) {
        resumo3[i,4] <- resumo3[i,3]/sum(resumo3[,3])
      }
      
      names(resumo3) <- c("Status","Classe","Media", "Prop")
      
      bp<- ggplot(resumo3, aes(x="", y=Prop, fill=Classe))+
        geom_bar(width = 1, stat = "identity") 
      
      pie <- bp + coord_polar("y", start=0)
      
      pie + scale_fill_brewer(palette="Blues")+
        theme_minimal()+geom_text(aes(label = paste(round(Prop, 1), "%")),
                                  position = position_stack(vjust = 0.5)) 
      
    })
    
    ############################# Estruturando os Gráficos para a Aba Quadro Resumo ###########################
    
    output$Resumo1 <- renderPlot({ ########################## Imagem 1 - Quadro Resumo
      
      resumo <- data.frame(table(dados$`Classe do Processo`, by=dados$Classificação))
      
      names(resumo)[names(resumo) == "by"] <- "Status"
      
      ggplot(data=resumo, aes(x=Var1, y=Freq, fill=Status)) +
        geom_bar(stat="identity")+
        geom_text(aes(y=Freq, label=Freq), vjust=1.6, 
                  color="white", size=5)+
        scale_fill_brewer(palette="Paired")+
        theme_minimal()+ggtitle("           Número total de Processos Segundo sua Classificação")+  theme(axis.title.x=element_blank(),
                                                                                                          axis.text.x=element_text(),
                                                                                                          axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                                axis.text.y=element_text(),
                                                                                                                                                axis.ticks.y=element_blank())
      
    })
    
    
    output$Resumo2 <- renderPlot({ ########################## Imagem 2 - Quadro Resumo
      
      resumo1 <- data.frame(table(dados$`Classe do Processo`, by=dados$Status))
      
      resumo1 <- resumo1[-c(grep( "Em análise",resumo1$by)),]
      
      names(resumo1)[names(resumo1) == "by"] <- "Status"
      resumo1 <- resumo1[resumo1$Freq != 0,]
      resumo1 <- resumo1[order(resumo1$Freq),]
      
      resumo1$soma <- ave(resumo1$Freq, resumo1$Var1, FUN=cumsum)
      
      ggplot(data=resumo1, aes(x=Var1, y=Freq, fill=Status)) +
        geom_bar(stat="identity")+ coord_flip() + theme(legend.position = "top")+
        geom_text(aes(y=soma, label=Freq), vjust=1.1, 
                  color="white", size=5,hjust = 2)+
        scale_fill_brewer(palette="Paired")+
        theme_minimal()+ggtitle("          Número total de Processos Segundo seu Status")+  theme(axis.title.x=element_blank(),
                                                                                                  axis.text.x=element_text(),
                                                                                                  axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                        axis.text.y=element_text(),
                                                                                                                                        axis.ticks.y=element_blank())
      
    })
    
    output$Resumo3 <- renderPlot({ ########################## Imagem 3 - Quadro Resumo
      
      resumo2 <- aggregate(Media ~ Classificação+`Classe do Processo`, data=dadosData, mean, na.rm=TRUE)
      
      names(resumo2) <- c("Status","Classe","Media")
      
      ggplot(resumo2, aes(x=Classe, y=Media, group=Status)) +
        geom_line(aes(linetype=Status, color=Status), size=1)+
        geom_point(aes(color=Status),size=3)+theme_bw()+
        theme(legend.position="bottom")+ggtitle("          Tempo médio de duração dos processos em dias")
      
    })
    
    output$Resumo4 <- renderPlot({ ########################## Imagem 4 - Quadro Resumo
      
      resumo3 <- aggregate(Media ~ Classificação+`Classe do Processo`, data=dadosData, mean, na.rm=TRUE)
      
      resumo3 <- resumo3[resumo3$Classificação=="Aberto",]
      
      for (i in 1:nrow(resumo3)) {
        resumo3[i,4] <- resumo3[i,3]/sum(resumo3[,3])
      }
      
      names(resumo3) <- c("Status","Classe","Media", "Prop")
      
      bp<- ggplot(resumo3, aes(x="", y=Prop, fill=Classe))+
        geom_bar(width = 1, stat = "identity") 
      
      pie <- bp + coord_polar("y", start=0)
      
      pie + scale_fill_brewer(palette="Blues")+
        theme_minimal()+ggtitle("          Percentual do tempo médio nos processos Abertos")+geom_text(aes(label = paste(round(Prop, 1), "%")),
                                                                                                       position = position_stack(vjust = 0.5)) 
      
    })
    
    # Abaixo foram realizados os filtros para a aba Análise Processual com seus respectivos gráficos
    
    # Obs.: O mesmo número de imagem irá se repetir 4 vezes, pois representa as 4 possibilidades de filtros para essa aba
    
    observeEvent(input$Classe4, { 
      
      ############################################# Imagem número 1
      
      if(input$Classe1 != "Todos os meses" & input$Classe2 != "Todos os anos"){
        
        output$histogram5 <- renderPlot({ 
          dadosData2 <- subset(dadosData, dadosData$Mes == input$Classe1)
          dadosData3 <- subset(dadosData2, dadosData2$Ano == input$Classe2)
          dadosData4 <- subset(dadosData3, dadosData3$`Classe do Processo` == (input$Classe3)[1] | dadosData3$`Classe do Processo` == (input$Classe3)[2] | dadosData3$`Classe do Processo` == (input$Classe3)[3])
          
          dat1 <- dadosData4[,c(3,5)]
          dat2 <- data.frame(table(dat1$`Classe do Processo`,by=dat1$Classificação))
          dat2$Var1 <- as.factor(dat2$Var1)
          dat2 <- dat2[dat2$Freq != 0,]
          
          
          ggplot(dat2, aes(x=Var1, y=Freq, fill=by)) +
            geom_bar(stat="identity", color="black", position=position_dodge()) +
            geom_errorbar(aes(ymin=Freq, ymax=Freq+0.2), width=.2,
                          position=position_dodge(.9)) + theme(legend.position="bottom")+  theme(axis.title.x=element_blank(),
                                                                                                 axis.text.x=element_text(dat2$Var1),
                                                                                                 axis.ticks.x=element_blank())+
            scale_fill_manual("Status", values = c("Aberto" = "#999999", "Encerrado" = "orange"))+ ggtitle("    Aberturas e fechamento de processos")
          
          
        })}
      
      ############################################# Imagem número 1
      
      if(input$Classe1 == "Todos os meses" & input$Classe2 != "Todos os anos"){
        
        output$histogram5 <- renderPlot({ 
          
          dadosData3 <- dadosData[dadosData$Ano == c(input$Classe2),]
          dadosData4 <- subset(dadosData3, dadosData3$`Classe do Processo` == (input$Classe3)[1] | dadosData3$`Classe do Processo` == (input$Classe3)[2] | dadosData3$`Classe do Processo` == (input$Classe3)[3])
          
          dat1 <- dadosData4[,c(3,5)]
          dat2 <- data.frame(table(dat1$`Classe do Processo`,by=dat1$Classificação))
          dat2$Var1 <- as.factor(dat2$Var1)
          dat2 <- dat2[dat2$Freq != 0,]
          
          ggplot(dat2, aes(x=Var1, y=Freq, fill=by)) +
            geom_bar(stat="identity", color="black", position=position_dodge()) +
            geom_errorbar(aes(ymin=Freq, ymax=Freq+0.2), width=.2,
                          position=position_dodge(.9)) + theme(legend.position="bottom")+  theme(axis.title.x=element_blank(),
                                                                                                 axis.text.x=element_text(dat2$Var1),
                                                                                                 axis.ticks.x=element_blank())+
            scale_fill_manual("Status", values = c("Aberto" = "#999999", "Encerrado" = "orange"))+ ggtitle("    Aberturas e fechamento de processos")
          
          
        })}
      
      ############################################# Imagem número 1
      
      if(input$Classe1 != "Todos os meses" & input$Classe2 == "Todos os anos"){
        
        output$histogram5 <- renderPlot({ 
          
          dadosData2 <- dadosData[dadosData$Mes == c(input$Classe1),]
          dadosData4 <- subset(dadosData2, dadosData2$`Classe do Processo` == (input$Classe3)[1] | dadosData2$`Classe do Processo` == (input$Classe3)[2] | dadosData2$`Classe do Processo` == (input$Classe3)[3])
          
          dat1 <- dadosData4[,c(3,5)]
          dat2 <- data.frame(table(dat1$`Classe do Processo`,by=dat1$Classificação))
          dat2$Var1 <- as.factor(dat2$Var1)
          dat2 <- dat2[dat2$Freq != 0,]
          
          ggplot(dat2, aes(x=Var1, y=Freq, fill=by)) +
            geom_bar(stat="identity", color="black", position=position_dodge()) +
            geom_errorbar(aes(ymin=Freq, ymax=Freq+0.2), width=.2,
                          position=position_dodge(.9)) + theme(legend.position="bottom")+  theme(axis.title.x=element_blank(),
                                                                                                 axis.text.x=element_text(dat2$Var1),
                                                                                                 axis.ticks.x=element_blank())+
            scale_fill_manual("Status", values = c("Aberto" = "#999999", "Encerrado" = "orange"))+ ggtitle("    Aberturas e fechamento de processos")
          
          
        })}    
      
      ############################################# Imagem número 1
      
      if(input$Classe1 == "Todos os meses" & input$Classe2 == "Todos os anos"){
        
        output$histogram5 <- renderPlot({
          
          dadosData4 <- subset(dadosData, dadosData$`Classe do Processo` == (input$Classe3)[1] | dadosData$`Classe do Processo` == (input$Classe3)[2] | dadosData$`Classe do Processo` == (input$Classe3)[3])
          
          dat1 <- dadosData4[,c(3,5)]
          dat2 <- data.frame(table(dat1$`Classe do Processo`,by=dat1$Classificação))
          dat2$Var1 <- as.factor(dat2$Var1)
          dat2 <- dat2[dat2$Freq != 0,]
          
          ggplot(dat2, aes(x=Var1, y=Freq, fill=by)) +
            geom_bar(stat="identity", color="black", position=position_dodge()) +
            geom_errorbar(aes(ymin=Freq, ymax=Freq+0.2), width=.2,
                          position=position_dodge(.9)) + theme(legend.position="bottom")+  theme(axis.title.x=element_blank(),
                                                                                                 axis.text.x=element_text(dat2$Var1),
                                                                                                 axis.ticks.x=element_blank())+
            scale_fill_manual("Status", values = c("Aberto" = "#999999", "Encerrado" = "orange"))+ ggtitle("    Aberturas e fechamento de processos")
          
          
        })}
      
      ############################################# Imagem número 3
      
      if(input$Classe1 != "Todos os meses" & input$Classe2 != "Todos os anos"){
        
        output$histogram6 <- renderPlot({ 
          
          dadosData2 <- dadosData[dadosData$Mes == c(input$Classe1),]
          dadosData3 <- dadosData2[dadosData2$Ano == c(input$Classe2),]
          dadosData4 <- subset(dadosData3, dadosData3$`Classe do Processo` == (input$Classe3)[1] | dadosData3$`Classe do Processo` == (input$Classe3)[2] | dadosData3$`Classe do Processo` == (input$Classe3)[3])
          
          dadosData4$media2 <- rowMeans(dadosData4[,7:34])
          pericia <- dadosData4$`Classe do Processo`
          pericia <- as.data.frame(pericia)
          pericia$Status <- as.character(dadosData4$Classificação) 
          pericia$x <- dadosData4[,34]
          pericia$medias <- dadosData4$media2
          names(pericia) <- c("grupo1","Status","Tempo médio da (Prova pericial)","Média")
          pericia$grupo1 <- as.factor(pericia$grupo1)
          
          pericia %>%
            arrange(desc(Média)) %>%
            ggplot(aes(x=grupo1, y=`Tempo médio da (Prova pericial)`, size=Média, color=Status)) +
            scale_color_manual(values = c("orange","#999999")) +
            geom_point(alpha=0.5) +
            scale_size(range = c(20, 14))+  theme(axis.title.x=element_blank(),
                                                  axis.text.x=element_text("Classe do processo"),
                                                  axis.ticks.x=element_blank())+ ggtitle("Tempo médio de duração vs Tempo médio em (Prova Pericial)")
        })}
      
      
      ############################################# Imagem número 3
      
      if(input$Classe1 == "Todos os meses" & input$Classe2 != "Todos os anos"){
        
        output$histogram6 <- renderPlot({ 
          
          dadosData3 <- dadosData[dadosData$Ano == c(input$Classe2),]
          dadosData4 <- subset(dadosData3, dadosData3$`Classe do Processo` == (input$Classe3)[1] | dadosData3$`Classe do Processo` == (input$Classe3)[2] | dadosData3$`Classe do Processo` == (input$Classe3)[3])
          
          dadosData4$media2 <- rowMeans(dadosData4[,7:34])
          pericia <- dadosData4$`Classe do Processo`
          pericia <- as.data.frame(pericia)
          pericia$Status <- as.character(dadosData4$Classificação) 
          pericia$x <- dadosData4[,34]
          pericia$medias <- dadosData4$media2
          names(pericia) <- c("grupo1","Status","Tempo médio da (Prova pericial)","Média")
          pericia$grupo1 <- as.factor(pericia$grupo1)
          
          pericia %>%
            arrange(desc(Média)) %>%
            ggplot(aes(x=grupo1, y=`Tempo médio da (Prova pericial)`, size=Média, color=Status)) +
            scale_color_manual(values = c("orange","#999999")) +
            geom_point(alpha=0.5) +
            scale_size(range = c(10, 24))+  theme(axis.title.x=element_blank(),
                                                  axis.text.x=element_text("Classe do processo"),
                                                  axis.ticks.x=element_blank())+ ggtitle("Tempo médio de duração vs Tempo médio em (Prova Pericial)")
        })}
      
      ############################################# Imagem número 3
      
      if(input$Classe1 != "Todos os meses" & input$Classe2 == "Todos os anos"){
        
        output$histogram6 <- renderPlot({ 
          
          dadosData2 <- dadosData[dadosData$Mes == c(input$Classe1),]
          dadosData4 <- subset(dadosData2, dadosData2$`Classe do Processo` == (input$Classe3)[1] | dadosData2$`Classe do Processo` == (input$Classe3)[2] | dadosData2$`Classe do Processo` == (input$Classe3)[3])
          
          dadosData4$media2 <- rowMeans(dadosData4[,7:34])
          pericia <- dadosData4$`Classe do Processo`
          pericia <- as.data.frame(pericia)
          pericia$Status <- as.character(dadosData4$Classificação) 
          pericia$x <- dadosData4[,34]
          pericia$medias <- dadosData4$media2
          names(pericia) <- c("grupo1","Status","Tempo médio da (Prova pericial)","Média")
          pericia$grupo1 <- as.factor(pericia$grupo1)
          
          pericia %>%
            arrange(desc(Média)) %>%
            ggplot(aes(x=grupo1, y=`Tempo médio da (Prova pericial)`, size=Média, color=Status)) +
            scale_color_manual(values = c("orange","#999999")) +
            geom_point(alpha=0.5) +
            scale_size(range = c(10, 24))+  theme(axis.title.x=element_blank(),
                                                  axis.text.x=element_text("Classe do processo"),
                                                  axis.ticks.x=element_blank())+ ggtitle("Tempo médio de duração vs Tempo médio em (Prova Pericial)")
        })}
      
      ############################################# Imagem número 3
      
      if(input$Classe1 == "Todos os meses" & input$Classe2 == "Todos os anos"){
        
        output$histogram6 <- renderPlot({ 
          
          dadosData4 <- subset(dadosData, dadosData$`Classe do Processo` == (input$Classe3)[1] | dadosData$`Classe do Processo` == (input$Classe3)[2] | dadosData$`Classe do Processo` == (input$Classe3)[3])
          
          dadosData4$media2 <- rowMeans(dadosData4[,7:34])
          pericia <- dadosData4$`Classe do Processo`
          pericia <- as.data.frame(pericia)
          pericia$Status <- as.character(dadosData4$Classificação) 
          pericia$x <- dadosData4[,34]
          pericia$medias <- dadosData4$media2
          names(pericia) <- c("grupo1","Status","Tempo médio da (Prova pericial)","Média")
          pericia$grupo1 <- as.factor(pericia$grupo1)
          
          pericia %>%
            arrange(desc(Média)) %>%
            ggplot(aes(x=grupo1, y=`Tempo médio da (Prova pericial)`, size=Média, color=Status)) +
            scale_color_manual(values = c("orange","#999999")) +
            geom_point(alpha=0.5) +
            scale_size(range = c(10, 24))+  theme(axis.title.x=element_blank(),
                                                  axis.text.x=element_text("Classe do processo"),
                                                  axis.ticks.x=element_blank())+ ggtitle("Tempo médio de duração vs Tempo médio em (Prova Pericial)")
        })}
      
      
      ############################################# Imagem número 2
      
      if(input$Classe1 != "Todos os meses" & input$Classe2 != "Todos os anos"){
        
        output$histogram61 <- renderPlot({ 
          
          dadosData2 <- dadosData[dadosData$Mes == c(input$Classe1),]
          dadosData3 <- dadosData2[dadosData2$Ano == c(input$Classe2),]
          dadosData4 <- subset(dadosData3, dadosData3$`Classe do Processo` == (input$Classe3)[1] | dadosData3$`Classe do Processo` == (input$Classe3)[2] | dadosData3$`Classe do Processo` == (input$Classe3)[3])
          
          
          resumoab <- data.frame(table(dadosData4$`Classe do Processo`, by=dadosData4$Status))
          
          resumoab <- resumoab[-c(grep( "Em análise",resumoab$by)),]
          
          names(resumoab)[names(resumoab) == "by"] <- "Status"
          resumoab <- resumoab[resumoab$Freq != 0,]
          resumoab <- resumoab[order(resumoab$Freq),]
          
          resumoab$soma <- ave(resumoab$Freq, resumoab$Var1, FUN=cumsum)
          
          ggplot(data=resumoab, aes(x=Var1, y=Freq, fill=Status)) +
            geom_bar(stat="identity")+ coord_flip() + theme(legend.position = "top")+
            geom_text(aes(y=soma, label=Freq), vjust=1.1, 
                      color="white", size=5,hjust = 2)+
            scale_fill_manual(values = c("#999999", "orange","#153E7E"))+
            theme_minimal()+ggtitle("          Número total de Processos Segundo seu Status")+  theme(axis.title.x=element_blank(),
                                                                                                      axis.text.x=element_text(),
                                                                                                      axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                            axis.text.y=element_text(),
                                                                                                                                            axis.ticks.y=element_blank())
          
          
        })}
      
      
      ############################################# Imagem número 2
      
      if(input$Classe1 == "Todos os meses" & input$Classe2 != "Todos os anos"){
        
        output$histogram61 <- renderPlot({ 
          
          dadosData3 <- dadosData[dadosData$Ano == c(input$Classe2),]
          dadosData4 <- subset(dadosData3, dadosData3$`Classe do Processo` == (input$Classe3)[1] | dadosData3$`Classe do Processo` == (input$Classe3)[2] | dadosData3$`Classe do Processo` == (input$Classe3)[3])
          
          
          resumoab <- data.frame(table(dadosData4$`Classe do Processo`, by=dadosData4$Status))
          
          resumoab <- resumoab[-c(grep( "Em análise",resumoab$by)),]
          
          names(resumoab)[names(resumoab) == "by"] <- "Status"
          resumoab <- resumoab[resumoab$Freq != 0,]
          resumoab <- resumoab[order(resumoab$Freq),]
          
          resumoab$soma <- ave(resumoab$Freq, resumoab$Var1, FUN=cumsum)
          
          ggplot(data=resumoab, aes(x=Var1, y=Freq, fill=Status)) +
            geom_bar(stat="identity")+ coord_flip() + theme(legend.position = "top")+
            geom_text(aes(y=soma, label=Freq), vjust=1.1, 
                      color="white", size=5,hjust = 2)+
            scale_fill_manual(values = c("#999999", "orange","#153E7E"))+
            theme_minimal()+ggtitle("          Número total de Processos Segundo seu Status")+  theme(axis.title.x=element_blank(),
                                                                                                      axis.text.x=element_text(),
                                                                                                      axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                            axis.text.y=element_text(),
                                                                                                                                            axis.ticks.y=element_blank())
          
          
        })}
      
      ############################################# Imagem número 2
      
      if(input$Classe1 != "Todos os meses" & input$Classe2 == "Todos os anos"){
        
        output$histogram61 <- renderPlot({ 
          
          dadosData2 <- dadosData[dadosData$Mes == c(input$Classe1),]
          dadosData4 <- subset(dadosData2, dadosData2$`Classe do Processo` == (input$Classe3)[1] | dadosData2$`Classe do Processo` == (input$Classe3)[2] | dadosData2$`Classe do Processo` == (input$Classe3)[3])
          
          
          resumoab <- data.frame(table(dadosData4$`Classe do Processo`, by=dadosData4$Status))
          
          resumoab <- resumoab[-c(grep( "Em análise",resumoab$by)),]
          
          names(resumoab)[names(resumoab) == "by"] <- "Status"
          resumoab <- resumoab[resumoab$Freq != 0,]
          resumoab <- resumoab[order(resumoab$Freq),]
          
          resumoab$soma <- ave(resumoab$Freq, resumoab$Var1, FUN=cumsum)
          
          ggplot(data=resumoab, aes(x=Var1, y=Freq, fill=Status)) +
            geom_bar(stat="identity")+ coord_flip() + theme(legend.position = "top")+
            geom_text(aes(y=soma, label=Freq), vjust=1.1, 
                      color="white", size=5,hjust = 2)+
            scale_fill_manual(values = c("#999999", "orange","#153E7E"))+
            theme_minimal()+ggtitle("          Número total de Processos Segundo seu Status")+  theme(axis.title.x=element_blank(),
                                                                                                      axis.text.x=element_text(),
                                                                                                      axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                            axis.text.y=element_text(),
                                                                                                                                            axis.ticks.y=element_blank())
          
          
        })}
      
      ############################################# Imagem número 2
      
      if(input$Classe1 == "Todos os meses" & input$Classe2 == "Todos os anos"){
        
        output$histogram61 <- renderPlot({ 
          
          dadosData4 <- subset(dadosData, dadosData$`Classe do Processo` == (input$Classe3)[1] | dadosData$`Classe do Processo` == (input$Classe3)[2] | dadosData$`Classe do Processo` == (input$Classe3)[3])
          
          
          resumoab <- data.frame(table(dadosData4$`Classe do Processo`, by=dadosData4$Status))
          
          resumoab <- resumoab[-c(grep( "Em análise",resumoab$by)),]
          
          names(resumoab)[names(resumoab) == "by"] <- "Status"
          resumoab <- resumoab[resumoab$Freq != 0,]
          resumoab <- resumoab[order(resumoab$Freq),]
          
          resumoab$soma <- ave(resumoab$Freq, resumoab$Var1, FUN=cumsum)
          
          ggplot(data=resumoab, aes(x=Var1, y=Freq, fill=Status)) +
            geom_bar(stat="identity")+ coord_flip() + theme(legend.position = "top")+
            geom_text(aes(y=soma, label=Freq), vjust=1.1, 
                      color="white", size=5,hjust = 2)+
            scale_fill_manual(values = c("#999999", "orange","#153E7E"))+
            theme_minimal()+ggtitle("          Número total de Processos Segundo seu Status")+  theme(axis.title.x=element_blank(),
                                                                                                      axis.text.x=element_text(),
                                                                                                      axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                            axis.text.y=element_text(),
                                                                                                                                            axis.ticks.y=element_blank())
          
          
        })}
      
      ############################################# Imagem número 4
      
      if(input$Classe1 != "Todos os meses" & input$Classe2 != "Todos os anos"){
        
        output$histogram62 <- renderPlot({ 
          
          dadosData2 <- dadosData[dadosData$Mes == c(input$Classe1),]
          dadosData3 <- dadosData2[dadosData2$Ano == c(input$Classe2),]
          dadosData4 <- subset(dadosData3, dadosData3$`Classe do Processo` == (input$Classe3)[1] | dadosData3$`Classe do Processo` == (input$Classe3)[2] | dadosData3$`Classe do Processo` == (input$Classe3)[3])
          
          resumorp <- dadosData4 %>% 
            group_by(Classificação,`Classe do Processo`) %>% 
            summarise_at(vars(Media) ,funs(mean(., na.rm=TRUE)))
          
          resumorp <- resumorp[resumorp$Classificação=="Aberto",]
          
          for (i in 1:nrow(resumorp)) {
            resumorp[i,4] <- resumorp[i,3]/sum(resumorp[,3])
          }
          
          names(resumorp) <- c("Status","Classe","Media", "Prop")
          
          bp<- ggplot(resumorp, aes(x="", y=Prop, fill=Classe))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_manual(values = c("#999999", "orange","#153E7E"))+
            theme_minimal()+ggtitle("          Percentual do tempo médio nos processos Abertos")+geom_text(aes(label = paste(round(Prop, 1), "%")),
                                                                                                           position = position_stack(vjust = 0.5)) 
          
        })}
      
      
      ############################################# Imagem número 4
      
      if(input$Classe1 == "Todos os meses" & input$Classe2 != "Todos os anos"){
        
        output$histogram62 <- renderPlot({ 
          
          dadosData3 <- dadosData[dadosData$Ano == c(input$Classe2),]
          dadosData4 <- subset(dadosData3, dadosData3$`Classe do Processo` == (input$Classe3)[1] | dadosData3$`Classe do Processo` == (input$Classe3)[2] | dadosData3$`Classe do Processo` == (input$Classe3)[3])
          
          resumorp <- dadosData4 %>% 
            group_by(Classificação,`Classe do Processo`) %>% 
            summarise_at(vars(Media) ,funs(mean(., na.rm=TRUE)))
          
          resumorp <- resumorp[resumorp$Classificação=="Aberto",]
          
          for (i in 1:nrow(resumorp)) {
            resumorp[i,4] <- resumorp[i,3]/sum(resumorp[,3])
          }
          
          names(resumorp) <- c("Status","Classe","Media", "Prop")
          
          bp<- ggplot(resumorp, aes(x="", y=Prop, fill=Classe))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_manual(values = c("#999999", "orange","#153E7E"))+
            theme_minimal()+ggtitle("          Percentual do tempo médio nos processos Abertos")+geom_text(aes(label = paste(round(Prop, 1), "%")),
                                                                                                           position = position_stack(vjust = 0.5)) 
          
        })}
      
      ############################################# Imagem número 4
      
      if(input$Classe1 != "Todos os meses" & input$Classe2 == "Todos os anos"){
        
        output$histogram62 <- renderPlot({ 
          
          dadosData2 <- dadosData[dadosData$Mes == c(input$Classe1),]
          dadosData4 <- subset(dadosData2, dadosData2$`Classe do Processo` == (input$Classe3)[1] | dadosData2$`Classe do Processo` == (input$Classe3)[2] | dadosData2$`Classe do Processo` == (input$Classe3)[3])
          
          resumorp <- dadosData4 %>% 
            group_by(Classificação,`Classe do Processo`) %>% 
            summarise_at(vars(Media) ,funs(mean(., na.rm=TRUE)))
          
          resumorp <- resumorp[resumorp$Classificação=="Aberto",]
          
          for (i in 1:nrow(resumorp)) {
            resumorp[i,4] <- resumorp[i,3]/sum(resumorp[,3])
          }
          
          names(resumorp) <- c("Status","Classe","Media", "Prop")
          
          bp<- ggplot(resumorp, aes(x="", y=Prop, fill=Classe))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_manual(values = c("#999999", "orange","#153E7E"))+
            theme_minimal()+ggtitle("          Percentual do tempo médio nos processos Abertos")+geom_text(aes(label = paste(round(Prop, 1), "%")),
                                                                                                           position = position_stack(vjust = 0.5)) 
          
        })}
      
      ############################################# Imagem número 4
      
      if(input$Classe1 == "Todos os meses" & input$Classe2 == "Todos os anos"){
        
        output$histogram62 <- renderPlot({ 
          
          dadosData4 <- subset(dadosData, dadosData$`Classe do Processo` == (input$Classe3)[1] | dadosData$`Classe do Processo` == (input$Classe3)[2] | dadosData$`Classe do Processo` == (input$Classe3)[3])
          
          resumorp <- dadosData4 %>% 
            group_by(Classificação,`Classe do Processo`) %>% 
            summarise_at(vars(Media) ,funs(mean(., na.rm=TRUE)))
          
          resumorp <- resumorp[resumorp$Classificação=="Aberto",]
          
          for (i in 1:nrow(resumorp)) {
            resumorp[i,4] <- resumorp[i,3]/sum(resumorp[,3])
          }
          
          names(resumorp) <- c("Status","Classe","Media", "Prop")
          
          bp<- ggplot(resumorp, aes(x="", y=Prop, fill=Classe))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_manual(values = c("#999999", "orange","#153E7E"))+
            theme_minimal()+ggtitle("          Percentual do tempo médio nos processos Abertos")+geom_text(aes(label = paste(round(Prop, 1), "%")),
                                                                                                           position = position_stack(vjust = 0.5)) 
          
        })}
      
      
    })
    
    
    
    observeEvent(input$dashboard5, { # saídas referentes ao DASHBOARD e Pré-Requisitos
      
      ################################# Estruturando os Info Box do Dashboard #################################
      
      
      # Número de Processos Encerrados 
      
      if(input$dashboard1 != "Todos os meses" & input$dashboard2 != "Todos os anos" & input$dashboard4 == "Ambos"){
        
        Encerrados1 <- dadosData[dadosData$Mes == c(input$dashboard1),]
        Encerrados2 <- Encerrados1[Encerrados1$Ano == c(input$dashboard2),]
        Encerrados3 <- Encerrados2[Encerrados2$`Classe do Processo` == c(input$dashboard3),]
        Encerrados4 <<- Encerrados3[Encerrados3$Classificação == "Encerrado",]
        
      }
      
      if(input$dashboard1 == "Todos os meses" & input$dashboard2 != "Todos os anos" & input$dashboard4 == "Ambos"){
        
        Encerrados2 <- dadosData[dadosData$Ano == c(input$dashboard2),]
        Encerrados3 <- Encerrados2[Encerrados2$`Classe do Processo` == c(input$dashboard3),]
        Encerrados4 <<- Encerrados3[Encerrados3$Classificação == "Encerrado",]
      }
      
      if(input$dashboard1 != "Todos os meses" & input$dashboard2 == "Todos os anos" & input$dashboard4 == "Ambos"){
        
        Encerrados1 <- dadosData[dadosData$Mes == c(input$dashboard1),]
        Encerrados3 <- Encerrados1[Encerrados1$`Classe do Processo` == c(input$dashboard3),]
        Encerrados4 <<- Encerrados3[Encerrados3$Classificação == "Encerrado",]
        
      }
      
      if(input$dashboard1 == "Todos os meses" & input$dashboard2 == "Todos os anos" & input$dashboard4 == "Ambos"){
        
        Encerrados3 <- dadosData[dadosData$`Classe do Processo` == c(input$dashboard3),]
        Encerrados4 <<- Encerrados3[Encerrados3$Classificação == "Encerrado",]
        
      }
      
      
      
      ########################################## Classificação igual AMBOS ######################################
      
      ############################################# Caso número 1
      
      if(input$dashboard1 != "Todos os meses" & input$dashboard2 != "Todos os anos" & input$dashboard4 == "Ambos"){
        
        dadosData2 <- dadosData[dadosData$Mes == c(input$dashboard1),]
        dadosData3 <- dadosData2[dadosData2$Ano == c(input$dashboard2),]
        dadosData4 <<- dadosData3[dadosData3$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
        output$histogram10 <- renderPlot({ # Imagem 2 - Pré requisitos
          
          Ajuste <- coxph(Surv(Soma, Classificação) ~ Status, data = dadosData4)
          cox.zph(Ajuste)
          par(mfrow=c(2,1))
          ggcoxzph(cox.zph(Ajuste))
        })
        
        
        output$histogram9 <- renderPlot({ # Imagem 1 - Pré requisitos
          
          qqPlot(Ajuste$residuals, xlab = "", ylab = "", col = c("black"),col.lines="red",lwd = 1.5)
        })
        
      }
      
      ############################################# Caso número 2
      
      if(input$dashboard1 == "Todos os meses" & input$dashboard2 != "Todos os anos" & input$dashboard4 == "Ambos"){
        
        dadosData3 <- dadosData2[dadosData2$Ano == c(input$dashboard2),]
        dadosData4 <- dadosData3[dadosData3$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
        output$histogram10 <- renderPlot({ # Imagem 2 - Pré requisitos
          Ajuste <- coxph(Surv(Soma, Classificação) ~ Status, data = dadosData4)
          cox.zph(Ajuste)
          par(mfrow=c(2,1))
          ggcoxzph(cox.zph(Ajuste))
        })
        
        
        output$histogram9 <- renderPlot({ # Imagem 1 - Pré requisitos
          
          qqPlot(Ajuste$residuals, xlab = "", ylab = "", col = c("black"),col.lines="red",lwd = 1.5)
        })
        
      }
      
      ############################################# Caso número 3
      
      if(input$dashboard1 != "Todos os meses" & input$dashboard2 == "Todos os anos" & input$dashboard4 == "Ambos"){
        
        dadosData2 <- dadosData[dadosData$Mes == c(input$dashboard1),]
        dadosData4 <- dadosData2[dadosData2$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
        output$histogram10 <- renderPlot({ # Imagem 2 - Pré requisitos
          Ajuste <- coxph(Surv(Soma, Classificação) ~ Status, data = dadosData4)
          cox.zph(Ajuste)
          par(mfrow=c(2,1))
          ggcoxzph(cox.zph(Ajuste))
        })
        
        
        output$histogram9 <- renderPlot({ # Imagem 1 - Pré requisitos
          
          qqPlot(Ajuste$residuals, xlab = "", ylab = "", col = c("black"),col.lines="red",lwd = 1.5)
        })
        
      }
      
      ############################################# Caso número 4
      
      if(input$dashboard1 == "Todos os meses" & input$dashboard2 == "Todos os anos" & input$dashboard4 == "Ambos"){
        
        dadosData4 <- dadosData[dadosData$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
        output$histogram10 <- renderPlot({ # Imagem 2 - Pré requisitos
          Ajuste <- coxph(Surv(Soma, Classificação) ~ Status, data = dadosData4)
          cox.zph(Ajuste)
          par(mfrow=c(2,1))
          ggcoxzph(cox.zph(Ajuste))
        })
        
        
        output$histogram9 <- renderPlot({ # Imagem 1 - Pré requisitos
          
          qqPlot(Ajuste$residuals, xlab = "", ylab = "", col = c("black"),col.lines="red",lwd = 1.5)
        })
        
      }
      
      
      
      
      ########################################## Classificação igual ABERTO ######################################
      
      ############################################# Caso número 1
      
      if(input$dashboard1 != "Todos os meses" & input$dashboard2 != "Todos os anos" & input$dashboard4 == "Aberto"){
        
        dadosData2 <- dadosData[dadosData$Mes == c(input$dashboard1),]
        dadosData3 <- dadosData2[dadosData2$Ano == c(input$dashboard2),]
        dadosData4 <- dadosData3[dadosData3$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
      }
      
      ############################################# Caso número 2
      
      if(input$dashboard1 == "Todos os meses" & input$dashboard2 != "Todos os anos" & input$dashboard4 == "Aberto"){
        
        dadosData3 <- dadosData2[dadosData2$Ano == c(input$dashboard2),]
        dadosData4 <- dadosData3[dadosData3$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
      }
      
      ############################################# Caso número 3
      
      if(input$dashboard1 != "Todos os meses" & input$dashboard2 == "Todos os anos" & input$dashboard4 == "Aberto"){
        
        dadosData2 <- dadosData[dadosData$Mes == c(input$dashboard1),]
        dadosData4 <- dadosData2[dadosData2$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
      }
      
      ############################################# Caso número 4
      
      if(input$dashboard1 == "Todos os meses" & input$dashboard2 == "Todos os anos" & input$dashboard4 == "Aberto"){
        
        dadosData4 <- dadosData[dadosData$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
        
      }
      
      
      ########################################## Classificação igual Encerrado ######################################
      
      ############################################# Caso número 1
      
      if(input$dashboard1 != "Todos os meses" & input$dashboard2 != "Todos os anos" & input$dashboard4 == "Encerrado"){
        
        dadosData2 <- dadosData[dadosData$Mes == c(input$dashboard1),]
        dadosData3 <- dadosData2[dadosData2$Ano == c(input$dashboard2),]
        dadosData4 <- dadosData3[dadosData3$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
      }
      
      ############################################# Caso número 2
      
      if(input$dashboard1 == "Todos os meses" & input$dashboard2 != "Todos os anos" & input$dashboard4 == "Encerrado"){
        
        dadosData3 <- dadosData2[dadosData2$Ano == c(input$dashboard2),]
        dadosData4 <- dadosData3[dadosData3$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
      }
      
      ############################################# Caso número 3
      
      if(input$dashboard1 != "Todos os meses" & input$dashboard2 == "Todos os anos" & input$dashboard4 == "Encerrado"){
        
        dadosData2 <- dadosData[dadosData$Mes == c(input$dashboard1),]
        dadosData4 <- dadosData2[dadosData2$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
      }
      
      ############################################# Caso número 4
      
      if(input$dashboard1 == "Todos os meses" & input$dashboard2 == "Todos os anos" & input$dashboard4 == "Encerrado"){
        
        dadosData4 <- dadosData[dadosData$`Classe do Processo` == c(input$dashboard3),]
        
        # Pegando a parte da planilha que tenho interesse 
        
        dadoscens <- dadosData4[,7:44]
        
        # Transformando todos os períodos em um unico vetor
        
        vetor1 <- as.vector(t(as.matrix(dadoscens)))
        
        # Calculando os termos que são ou não censura
        
        matriz <- data.frame()
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,1] != 0 & dadoscens[i,2] == 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0){
            matriz[i,1] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,2] != 0 & dadoscens[i,3] == 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0){
            matriz[i,1] = 0
            matriz[i,2] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,3] != 0 & dadoscens[i,4] == 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 ){
            matriz[i,1:2] = 0
            matriz[i,3] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,4] != 0 & dadoscens[i,5] == 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0){
            matriz[i,1:3] = 0
            matriz[i,4] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,5] != 0 & dadoscens[i,6] == 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0){
            matriz[i,1:4] = 0
            matriz[i,5] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,6] != 0 & dadoscens[i,7] == 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0){
            matriz[i,1:5] = 0
            matriz[i,6] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,7] != 0 & dadoscens[i,8] == 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0){
            matriz[i,1:6] = 0
            matriz[i,7] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,8] != 0 & dadoscens[i,9] == 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0){
            matriz[i,1:7] = 0
            matriz[i,8] = 1  
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,9] != 0 & dadoscens[i,10] == 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0){
            matriz[i,1:8] = 0
            matriz[i,9] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,10] != 0 & dadoscens[i,11] == 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0){
            matriz[i,1:9] = 0
            matriz[i,10] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,11] != 0 & dadoscens[i,12] == 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0){
            matriz[i,1:10] = 0
            matriz[i,11] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,12] != 0 & dadoscens[i,13] == 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0){
            matriz[i,1:11] = 0
            matriz[i,12] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,13] != 0 & dadoscens[i,14] == 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0){
            matriz[i,1:12] = 0
            matriz[i,13] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,14] != 0 & dadoscens[i,15] == 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0){
            matriz[i,1:13] = 0
            matriz[i,14] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,15] != 0 & dadoscens[i,16] == 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0){
            matriz[i,1:14] = 0
            matriz[i,15] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,16] != 0 & dadoscens[i,17] == 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0){
            matriz[i,1:15] = 0
            matriz[i,16] = 1   
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,17] != 0 & dadoscens[i,18] == 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0){
            matriz[i,1:16] = 0
            matriz[i,17] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,18] != 0 & dadoscens[i,19] == 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0){
            matriz[i,1:17] = 0
            matriz[i,18] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,19] != 0 & dadoscens[i,20] == 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0){
            matriz[i,1:18] = 0
            matriz[i,19] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,20] != 0 & dadoscens[i,21] == 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0){
            matriz[i,1:19] = 0
            matriz[i,20] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,21] != 0 & dadoscens[i,22] == 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0){
            matriz[i,1:20] = 0
            matriz[i,21] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,22] != 0 & dadoscens[i,23] == 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0){
            matriz[i,1:21] = 0
            matriz[i,22] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,23] != 0 & dadoscens[i,24] == 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0){
            matriz[i,1:22] = 0
            matriz[i,23] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,24] != 0 & dadoscens[i,25] == 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0){
            matriz[i,1:23] = 0
            matriz[i,24] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,25] != 0 & dadoscens[i,26] == 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0){
            matriz[i,1:24] = 0
            matriz[i,25] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,26] != 0 & dadoscens[i,27] == 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0){
            matriz[i,1:25] = 0
            matriz[i,26] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,27] != 0 & dadoscens[i,28] == 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 ){
            matriz[i,1:26] = 0
            matriz[i,27] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,28] != 0 & dadoscens[i,29] == 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0){
            matriz[i,1:27] = 0
            matriz[i,28] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,29] != 0 & dadoscens[i,30] == 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0){
            matriz[i,1:28] = 0
            matriz[i,29] = 1 
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,30] != 0 & dadoscens[i,31] == 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0){
            matriz[i,1:29] = 0
            matriz[i,30] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,31] != 0 & dadoscens[i,32] == 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0){
            matriz[i,1:30] = 0
            matriz[i,31] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,32] != 0 & dadoscens[i,33] == 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0){
            matriz[i,1:31] = 0
            matriz[i,32] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,33] != 0 & dadoscens[i,34] == 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0){
            matriz[i,1:32] = 0
            matriz[i,33] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,34] != 0 & dadoscens[i,35] == 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0){
            matriz[i,1:33] = 0
            matriz[i,34] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,35] != 0 & dadoscens[i,36] == 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:34] = 0
            matriz[i,35] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,36] != 0 & dadoscens[i,37] == 0 & dadoscens[i,38] == 0){
            matriz[i,1:35] = 0
            matriz[i,36] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,37] != 0 & dadoscens[i,38] == 0){
            matriz[i,1:36] = 0
            matriz[i,37] = 1
          }
        }
        
        for (i in 1:nrow(dadoscens)) {
          if(dadoscens[i,38] != 0){
            matriz[i,1:37] = 0
            matriz[i,38] = 1
          }
        }
        
        # Juntando os bancos em um único
        
        dadoscensura <- as.data.frame(vetor1)
        dadoscensura[,2]<- as.vector(t(as.matrix(matriz)))
        
        nome <- data.frame()
        fator <- as.character(dadosData4[,4])
        
        for (i in 1:length(fator)) {
          nome[i,1:38] <- rep(fator[i],38)
        }
        
        dadoscensura[,3]<- as.vector(t(as.matrix(nome)))
        
        names(dadoscensura) <- c("valor","censura","status")
        
        for (i in 1:nrow(dadoscensura)) {
          if(dadoscensura[i,3] == "Ok"){dadoscensura[i,3] = 1}
          if(dadoscensura[i,3] == "Alerta"){dadoscensura[i,3] = 2}
          if(dadoscensura[i,3] == "Atrasado"){dadoscensura[i,3] = 3}
        }
        
        dadoscensura[,3] <- as.numeric(dadoscensura[,3])
        
        #Removendo os NA
        
        if(length(which(is.na(dadoscensura$censura))) != 0){
          dadoscensura <- dadoscensura[-which(is.na(dadoscensura$censura)),]
        }
        
        #Arrumando a ordem da legenda
        
        caso1 <- grep("Ok",fator)
        caso2 <- grep("Alerta",fator)
        caso3 <- grep("Atrasado",fator)
        
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) == 0){rotulos <- c("Ok")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Ok","Alerta")}
        if(length(caso1) != 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Ok","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) == 0){rotulos <- c("Alerta")}
        if(length(caso1) == 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Alerta","Atrasado")}
        
        if(length(caso1) == 0 & length(caso2) == 0 & length(caso3) != 0){rotulos <- c("Atrasado")}
        if(length(caso1) != 0 & length(caso2) != 0 & length(caso3) != 0){rotulos <- c("Ok","Alerta","Atrasado")}
        
        fit <- survfit(Surv(valor, censura) ~ status,type="kaplan-meier",conf.type="log",data = dadoscensura)
        
        output$histogram1 <- renderPlot({ # Imagem 1 - DASHBOARD
          ggsurvplot(
            fit,   pval = TRUE, 
            data = dadoscensura,
            legend.labs = rotulos,
            palette =c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram2 <- renderPlot({ # Imagem 2 - Dashboard
          ggsurvplot(
            fit,
            data = dadoscensura, fun = "event", 
            legend.labs = rotulos,
            palette =
              c("#2E9FDF","#E7B800","#CC0000"),# custom color palettes
            size = 1,# change line size
            conf.int = TRUE,# Add p-value 
            ggtheme = theme_bw()# Change ggplot2 theme
          )
        })
        
        output$histogram3 <- renderPlot({ # Imagem 3 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          banco[,3] <- 1
          
          names(banco) <- c("processo","tempo", "cor")
          
          banco[,2] <- format(round(banco[,2], 2), nsmall = 2)
          
          level_order <- c("Sentença","Recebimento e conclusão do processo","Alegações finais 1","Cumprimento da intimação 5","Expedição de intimação (Requerido) 2","Alegações finais","Cumprimento da intimação 4",
                           "Expedição de intimação (Requerente) 2","Ato ordinário (Alegações finais)","Prova oral (audiência de instrução e julgamento)","Prova pericial","Pedido de esclarecimento ou solicitação de ajustes 1","Cumprimento da intimação 3",
                           "Expedição de intimação (Requerido) 1","Pedido de esclarecimento ou solicitação de ajustes","Cumprimento da intimação 2","Expedição de intimação (Requerente) 1","Decisão saneamento","Provas 1","Cumprimento da intimação 1",
                           "Expedição de intimação (Requerido)","Provas","Cumprimento  da intimação","Expedição de intimação (Requerente)","Ato ordinatório (Provas)","Réplica","Cumprimento da intimação","Expedição de intimação","Ato ordinatório (Réplica)",
                           "Contestação e/ou agravo de instrumento","Cumprimento da intimação e/ou citação","Expedição de intimação e/ou citação","Decisão rejeição da ação ou recebimento pet. inicial","Oferecimento de manifestação por escrito",
                           "Cumprimento da notificação","Expedição de notificação","Despacho para notificação do Requerido","Recebimento, triagem e conclusão do processo")
          
          
          ggplot(data=banco, aes(factor(processo, level = level_order), y=tempo, fill=cor))  + coord_flip() +
            geom_bar(stat="identity") + geom_text(aes(y = tempo, label = tempo), color = "black",size=3,hjust = 0)+
            guides(fill=FALSE) +
            theme_minimal()+ggtitle("   Prazo médio de execução das Tarefas (dias)")+  theme(axis.title.x=element_blank(),
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),
                                                                                                                                   axis.text.y=element_text(),
                                                                                                                                   axis.ticks.y=element_blank())
          
        })
        
        
        output$histogram4 <- renderPlot({ # Imagem 4 - Dashboard
          
          medias <- colMeans(dadosData4[,7:44])
          medias <- as.data.frame(medias)
          
          layout <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                      "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                      "Recebimento e conclusão do processo","Sentença")
          
          banco <- data.frame(layout,medias[,1])
          
          names(banco) <- c("processo","tempo")
          banco$processo <- as.character(banco$processo)
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento, triagem e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Despacho para notificação do Requerido"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de notificação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da notificação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Oferecimento de manifestação por escrito"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão rejeição da ação ou recebimento pet. inicial"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação e/ou citação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação e/ou citação"){banco[i,1] <- "Central de Mandados"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Contestação e/ou agravo de instrumento"){banco[i,1] <- "Requerido / Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Réplica)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Réplica"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinatório (Provas)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento  da intimação"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Provas 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Decisão saneamento"){banco[i,1] <- "Gabinete"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 1"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 3"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Pedido de esclarecimento ou solicitação de ajustes 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova pericial"){banco[i,1] <- "Perito"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Prova oral (audiência de instrução e julgamento)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Ato ordinário (Alegações finais)"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerente) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 4"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais"){banco[i,1] <- "Requerente"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Expedição de intimação (Requerido) 2"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Cumprimento da intimação 5"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Alegações finais 1"){banco[i,1] <- "Requerido"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Recebimento e conclusão do processo"){banco[i,1] <- "Secretaria"}
            
          }
          
          for (i in 1:nrow(banco)) {
            if(banco[i,1] == "Sentença"){banco[i,1] <- "Gabinete"}
            
          }
          
          df11 <- aggregate(tempo ~ processo, data=banco, mean, na.rm=TRUE)
          
          total <- sum(df11$tempo)
          for (i in 1:nrow(df11)) {
            df11[i,2] <- (df11[i,2]/total)*100
          }
          
          df11$tempo <- round((df11$tempo))
          
          bp<- ggplot(df11, aes(x="", y=tempo, fill=processo))+
            geom_bar(width = 1, stat = "identity") 
          
          pie <- bp + coord_polar("y", start=0)
          
          pie + scale_fill_brewer(palette="Blues")+
            theme_minimal()+ggtitle("      Percentual do prazo médio de execução (dias) - Unidades Responsáveis")+geom_text(aes(label = paste(round(tempo, 1), "%"), x = 1.6) ,size=3.5,
                                                                                                                            position = position_stack(vjust = 0.6)) 
          
          
        })
        
        
      }
      
    })
    
    
    
    
    isolate({ #Serve apenas para criar a tabela com os prazos iniciais
      
      base0 <- c("Secretaria", "Gabinete","Secretaria", "Central de mandados","Requerido","Gabinete","Secretaria","Central de mandados","Requerido/Requerente",
                 "Secretaria","Secretaria","Secretaria","Requerente","Secretaria","Secretaria","Secretaria","Requerente","Secretaria","Secretaria","Requerido","Gabinete",
                 "Secretaria","Secretaria","Requerente","Secretaria","Secretaria","Requerido","Perito","Secretaria","Secretaria","Secretaria","Secretaria","Requerente","Secretaria","Secretaria","Requerido","Secretaria","Gabinete")
      
      base1 <- c("Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                 "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                 "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                 "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                 "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                 "Recebimento e conclusão do processo","Sentença")
      
      base2 <- c(1,1,1,3,15,30,1,3,15,1,1,5,15,1,1,5,5,1,5,5,10,1,5,5,1,5,5,90,60,1,1,5,15,1,5,15,1,30)
      
      base <- data.frame(base0,base1,base2)
      
      names(base)<- c("Unidade Responsável","Fase do Processo", "Tempo de Duração (dias)")
      
      base$`Tempo de Duração (dias)` <- as.character(base$`Tempo de Duração (dias)`)
    }) 
    
    # Plotagem da tabela de prazos iniciais
    
    output$data1 <- renderTable(base, align = c("cccc"), rownames = TRUE)
    
    observeEvent(input$AtuaPra42, { # Tabela de Atualização dos Prazos
      
      dadosAtuap <- dadosP[dadosP$Apelido == input$AtuaPra1,]
      
      if(input$AtuaPra2 == "Recebimento, triagem e conclusão do processo"){dadosAtuap[,3] <- input$AtuaPra4}
      
      if(input$AtuaPra2 == "Despacho para notificação do Requerido"){dadosAtuap[,4] <- input$AtuaPra5}
      
      if(input$AtuaPra2 == "Expedição de notificação"){dadosAtuap[,5] <- input$AtuaPra6}
      
      if(input$AtuaPra2 == "Cumprimento da notificação"){dadosAtuap[,6] <- input$AtuaPra7}
      
      if(input$AtuaPra2 == "Oferecimento de manifestação por escrito"){dadosAtuap[,7] <- input$AtuaPra8}
      
      if(input$AtuaPra2 == "Decisão rejeição da ação ou recebimento pet. inicial"){dadosAtuap[,8] <- input$AtuaPra9}
      
      if(input$AtuaPra2 == "Expedição de intimação e/ou citação"){dadosAtuap[,9] <- input$AtuaPra10}
      
      if(input$AtuaPra2 == "Cumprimento da intimação e/ou citação"){dadosAtuap[,10] <- input$AtuaPra11}
      
      if(input$AtuaPra2 == "Contestação e/ou agravo de instrumento"){dadosAtuap[,11] <- input$AtuaPra12}
      
      if(input$AtuaPra2 == "Ato ordinatório (Réplica)"){dadosAtuap[,12] <- input$AtuaPra13}
      
      if(input$AtuaPra2 == "Expedição de intimação"){dadosAtuap[,13] <- input$AtuaPra14}
      
      if(input$AtuaPra2 == "Cumprimento da intimação"){dadosAtuap[,14] <- input$AtuaPra15}
      
      if(input$AtuaPra2 == "Réplica"){dadosAtuap[,15] <- input$AtuaPra16}
      
      if(input$AtuaPra2 == "Ato ordinatório (Provas)"){dadosAtuap[,16] <- input$AtuaPra17}
      
      if(input$AtuaPra2 == "Expedição de intimação (Requerente)"){dadosAtuap[,17] <- input$AtuaPra18}
      
      if(input$AtuaPra2 == "Cumprimento  da intimação"){dadosAtuap[,18] <- input$AtuaPra19}
      
      if(input$AtuaPra2 == "Provas"){dadosAtuap[,19] <- input$AtuaPra20}
      
      if(input$AtuaPra2 == "Expedição de intimação (Requerido)"){dadosAtuap[,20] <- input$AtuaPra21}
      
      if(input$AtuaPra2 == "Cumprimento da intimação 1"){dadosAtuap[,21] <- input$AtuaPra22}
      
      if(input$AtuaPra2 == "Provas 1"){dadosAtuap[,22] <- input$AtuaPra23}
      
      if(input$AtuaPra2 == "Decisão saneamento"){dadosAtuap[,23] <- input$AtuaPra24}
      
      if(input$AtuaPra2 == "Expedição de intimação (Requerente) 1"){dadosAtuap[,24] <- input$AtuaPra25}
      
      if(input$AtuaPra2 == "Cumprimento da intimação 2"){dadosAtuap[,25] <- input$AtuaPra26}
      
      if(input$AtuaPra2 == "Pedido de esclarecimento ou solicitação de ajustes"){dadosAtuap[,26] <- input$AtuaPra27}
      
      if(input$AtuaPra2 == "Expedição de intimação (Requerido) 1"){dadosAtuap[,27] <- input$AtuaPra28}
      
      if(input$AtuaPra2 == "Cumprimento da intimação 3"){dadosAtuap[,28] <- input$AtuaPra29}
      
      if(input$AtuaPra2 == "Pedido de esclarecimento ou solicitação de ajustes 1"){dadosAtuap[,29] <- input$AtuaPra30}
      
      if(input$AtuaPra2 == "Prova pericial"){dadosAtuap[,30] <- input$AtuaPra31}
      
      if(input$AtuaPra2 == "Prova oral (audiência de instrução e julgamento)"){dadosAtuap[,31] <- input$AtuaPra32}
      
      if(input$AtuaPra2 == "Ato ordinário (Alegações finais)"){dadosAtuap[,32] <- input$AtuaPra33}
      
      if(input$AtuaPra2 == "Expedição de intimação (Requerente) 2"){dadosAtuap[,33] <- input$AtuaPra34}
      
      if(input$AtuaPra2 == "Cumprimento da intimação 4"){dadosAtuap[,34] <- input$AtuaPra35}
      
      if(input$AtuaPra2 == "Alegações finais"){dadosAtuap[,35] <- input$AtuaPra36}
      
      if(input$AtuaPra2 == "Expedição de intimação (Requerido) 2"){dadosAtuap[,36] <- input$AtuaPra37}
      
      if(input$AtuaPra2 == "Cumprimento da intimação 5"){dadosAtuap[,37] <- input$AtuaPra38}
      
      if(input$AtuaPra2 == "Alegações finais 1"){dadosAtuap[,38] <- input$AtuaPra39}
      
      if(input$AtuaPra2 == "Recebimento e conclusão do processo"){dadosAtuap[,39] <- input$AtuaPra40}
      
      if(input$AtuaPra2 == "Sentença"){dadosAtuap[,40] <- input$AtuaPra41}
      
      write.csv(dadosAtuap, "output_prazos2.csv")
      
      output$table5 <- DT::renderDataTable(dadosAtuap, escape = FALSE, extensions = c('FixedColumns',"FixedHeader","Buttons","ColReorder","Scroller"),
                                           options = list(dom = 'Bfrtip', colReorder = TRUE,searchHighlight = TRUE,
                                                          buttons = c('copy', 'print'),lengthMenu = c(5, 10), pageLength = 15, scrollX = TRUE,
                                                          fixedHeader=TRUE, fixedColumns = list(leftColumns = 3, rightColumns = 0),paging = TRUE, searching = TRUE, info = FALSE,sort = FALSE,
                                                          initComplete = JS("function(settings, json) {",
                                                                            "$(this.api().table().header()).css({'background-color': '#f2f5f9', 'color': 'black'});",
                                                                            "}")
                                           ))
    })
    
    
    
    
    
    observeEvent(input$Entradai39, { # Tabela de Prazos
      
      dadosi <- data.frame(input$Entradai1,input$Entradai2,input$Entradai4,input$Entradai5,input$Entradai6,
                           input$Entradai7,input$Entradai8,input$Entradai9,input$Entradai10,input$Entradai11,input$Entradai12,input$Entradai13,
                           input$Entradai14,input$Entradai15,input$Entradai16,input$Entradai17,input$Entradai18,input$Entradai19,input$Entradai20,input$Entradai201,input$Entradai202,input$Entradai203,
                           input$Entradai21,input$Entradai22,input$Entradai23,input$Entradai24,input$Entradai25,input$Entradai26,input$Entradai27,
                           input$Entradai28,input$Entradai29,input$Entradai30,input$Entradai31,input$Entradai32,input$Entradai33,input$Entradai34,
                           input$Entradai35,input$Entradai36,input$Entradai37,input$Entradai38)
      
      names(dadosi) <- c("Nº Processo","Apelido","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                         "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                         "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                         "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                         "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                         "Recebimento e conclusão do processo","Sentença")
      
      names(dadosP) <- c("Nº Processo","Apelido","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                         "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                         "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                         "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                         "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                         "Recebimento e conclusão do processo","Sentença")
      
      
      dadosi2 <- rbind(dadosP,dadosi)
      
      
      #Removendo os lixos das tabelas 
      
      linhas_tabela_limpeza <- grep(dadosi$Apelido, dadosi2$Apelido, fixed=TRUE)
      
      if(length(linhas_tabela_limpeza) >1){
        dadosi2 <- dadosi2[-c(linhas_tabela_limpeza[-1]),]
      }
      
      dadosi <- dadosi2
      
      write.csv(dadosi, "output_prazos.csv")
      
      rownames(dadosi) <- NULL
      
      output$table4 <- DT::renderDataTable(dadosi, escape = FALSE, extensions = c('FixedColumns',"FixedHeader","Buttons","ColReorder","Scroller"),
                                           options = list(dom = 'Bfrtip', colReorder = TRUE,searchHighlight = TRUE,
                                                          buttons = c('copy', 'print'),lengthMenu = c(5, 10), pageLength = 15, scrollX = TRUE, scrollY  = 500,
                                                          fixedHeader=TRUE, fixedColumns = list(leftColumns = 3, rightColumns = 0),paging = TRUE, searching = TRUE, info = FALSE,sort = FALSE,
                                                          initComplete = JS("function(settings, json) {",
                                                                            "$(this.api().table().header()).css({'background-color': '#f2f5f9', 'color': 'black'});",
                                                                            "}")
                                           ))
    })
    
    observeEvent(input$Entrada45, { # Tabela de Criação
      
      dados1 <- data.frame(input$Entrada1,input$Entrada2,input$Entrada3,c("Em análise"), input$Entrada5, format(as.Date(input$Entrada51), "%Y-%m-%d") , input$Entrada7-input$Entrada51,input$Entrada8-input$Entrada7,input$Entrada9-input$Entrada8,input$Entrada10-input$Entrada9,input$Entrada11-input$Entrada10,input$Entrada12-input$Entrada11,input$Entrada13-input$Entrada12,input$Entrada14-input$Entrada13,
                           input$Entrada15-input$Entrada14,input$Entrada16-input$Entrada15,input$Entrada17-input$Entrada16,input$Entrada18-input$Entrada17,input$Entrada19-input$Entrada18,input$Entrada20-input$Entrada19,input$Entrada21-input$Entrada20,input$Entrada22-input$Entrada21,input$Entrada23-input$Entrada22,input$Entrada24-input$Entrada23,input$Entrada25-input$Entrada24,input$Entrada26-input$Entrada25,
                           input$Entrada27-input$Entrada26,input$Entrada28-input$Entrada27,input$Entrada29-input$Entrada28,input$Entrada30-input$Entrada29,input$Entrada31-input$Entrada30,input$Entrada32-input$Entrada31,input$Entrada33-input$Entrada32,input$Entrada34-input$Entrada33,input$Entrada35-input$Entrada34,input$Entrada36-input$Entrada35,input$Entrada37-input$Entrada36,input$Entrada38-input$Entrada37,
                           input$Entrada39-input$Entrada38,input$Entrada40-input$Entrada39,input$Entrada41-input$Entrada40,input$Entrada42-input$Entrada41,input$Entrada43-input$Entrada42,input$Entrada44-input$Entrada43)
      
      
      names(dados1) <- c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                         "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                         "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                         "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                         "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                         "Recebimento e conclusão do processo","Sentença")
      
      names(dados) <- c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                        "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                        "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                        "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                        "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                        "Recebimento e conclusão do processo","Sentença")
      
      
      dados$`Data Inicial` <- format(as.Date(dados$`Data Inicial`), "%Y-%m-%d")
      
      dados2 <- rbind(dados,dados1)
      
      #Removendo os lixos das tabelas 
      
      linhas_tabela_limpeza <- grep(dados1$Apelido, dados2$Apelido, fixed=TRUE)
      
      if(length(linhas_tabela_limpeza) >1){
        dados2 <- dados2[-c(linhas_tabela_limpeza[-1]),]
      }
      
      write.csv(dados2, "output.csv")
      
      # Estruturando a tabela de datas
      
      Data <- data.frame(input$Entrada1,input$Entrada2,input$Entrada7,input$Entrada8,input$Entrada9,input$Entrada10,input$Entrada11,input$Entrada12,input$Entrada13,input$Entrada14,input$Entrada15,input$Entrada16,input$Entrada17,input$Entrada18,input$Entrada19,
                         input$Entrada20,input$Entrada21,input$Entrada22,input$Entrada23,input$Entrada24,input$Entrada25,input$Entrada26,input$Entrada27,input$Entrada28,input$Entrada29,input$Entrada30,input$Entrada31,input$Entrada32,input$Entrada33,input$Entrada34,
                         input$Entrada35,input$Entrada36,input$Entrada37,input$Entrada38,input$Entrada39,input$Entrada40,input$Entrada41,input$Entrada42,input$Entrada43,input$Entrada44)
      
      names(Data) <- c("Nº Processo","Apelido","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                       "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                       "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                       "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                       "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                       "Recebimento e conclusão do processo","Sentença")
      
      write.csv(Data, "Data.csv")
      
      # Output da tabela
      
      output$table2 <- DT::renderDataTable({
        dt2 = data.frame(dados2)
        
        rownames(dt2) <- NULL
        
        datatable(dt2, colnames =c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                                   "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                                   "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                                   "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                                   "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                                   "Recebimento e conclusão do processo","Sentença"), escape = FALSE, extensions = c('FixedColumns',"FixedHeader","Buttons","ColReorder","Scroller"),filter = "top",
                  options = list(dom = 'Bfrtip', colReorder = TRUE,searchHighlight = TRUE,
                                 buttons = c('copy', 'print'),lengthMenu = c(5, 10), pageLength = 15, scrollX = TRUE, scrollY  = 500,
                                 fixedHeader=TRUE, fixedColumns = list(leftColumns = 3, rightColumns = 0),paging = TRUE, searching = TRUE, info = FALSE,sort = FALSE,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#f2f5f9', 'color': 'black'});",
                                                   "}")
                  )) %>%
          formatStyle( 
            'Classificação',
            target = 'row', fontWeight = styleEqual(c("Encerrado"), c('bold'))
          )%>%
          formatStyle( 
            'Status',
            target = 'row',
            backgroundColor = styleEqual(c("Ok","Alerta", "Atrasado"), c('white','yellow', 'red'))
          )
      }
      )
    })
    
    
    observeEvent(input$Entradas81, { # Tabela de atualização
      
      dados3 <- dados[dados$Apelido == input$Entradas1,]
      Dateiii <- Date[Date$Apelido == input$Entradas1,]
      
      dados3[,3] <- input$Entradas2
      
      if(input$Entradas4 == "Recebimento, triagem e conclusão do processo"){dados3[,7] <- input$Entradas6 - as.Date(dados3$`Data Inicial`)}
      
      if(input$Entradas4 == "Despacho para notificação do Requerido"){dados3[,8] <- input$Entradas8 - as.Date(Dateiii[1,3][[1]],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Expedição de notificação"){dados3[,9] <- input$Entradas10 - as.Date(Dateiii[1,4][[1]],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Cumprimento da notificação"){dados3[,10] <- input$Entradas12 - as.Date(Dateiii[1,5],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Oferecimento de manifestação por escrito"){dados3[,11] <- input$Entradas14 - as.Date(Dateiii[1,6],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Decisão rejeição da ação ou recebimento pet. inicial"){dados3[,12] <- input$Entradas16 - as.Date(Dateiii[1,7],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Expedição de intimação e/ou citação"){dados3[,13] <- input$Entradas18 - as.Date(Dateiii[1,8],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Cumprimento da intimação e/ou citação"){dados3[,14] <- input$Entradas20 - as.Date(Dateiii[1,9],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Contestação e/ou agravo de instrumento"){dados3[,15] <- input$Entradas22 - as.Date(Dateiii[1,10],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Ato ordinatório (Réplica)"){dados3[,16] <- input$Entradas24 - as.Date(Dateiii[1,11],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Expedição de intimação"){dados3[,17] <- input$Entradas26 - as.Date(Dateiii[1,12],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Cumprimento da intimação"){dados3[,18] <- input$Entradas28 - as.Date(Dateiii[1,13],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Réplica"){dados3[,19] <- input$Entradas30 - as.Date(Dateiii[1,14],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Ato ordinatório (Provas)"){dados3[,20] <- input$Entradas32 - as.Date(Dateiii[1,15],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Expedição de intimação (Requerente)"){dados3[,21] <- input$Entradas34 - as.Date(Dateiii[1,16],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Cumprimento  da intimação"){dados3[,22] <- input$Entradas36 - as.Date(Dateiii[1,17],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Provas"){dados3[,23] <- input$Entradas38 - as.Date(Dateiii[1,18],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Expedição de intimação (Requerido)"){dados3[,24] <- input$Entradas40 - as.Date(Dateiii[1,19],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Cumprimento da intimação 1"){dados3[,25] <- input$Entradas42 - as.Date(Dateiii[1,20],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Provas 1"){dados3[,26] <- input$Entradas44 - as.Date(Dateiii[1,21],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Decisão saneamento"){dados3[,27] <- input$Entradas46 - as.Date(Dateiii[1,22],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Expedição de intimação (Requerente) 1"){dados3[,28] <- input$Entradas48 - as.Date(Dateiii[1,23],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Cumprimento da intimação 2"){dados3[,29] <- input$Entradas50 - as.Date(Dateiii[1,24],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Pedido de esclarecimento ou solicitação de ajustes"){dados3[,30] <- input$Entradas52 - as.Date(Dateiii[1,25],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Expedição de intimação (Requerido) 1"){dados3[,31] <- input$Entradas54 - as.Date(Dateiii[1,26],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Cumprimento da intimação 3"){dados3[,32] <- input$Entradas56 - as.Date(Dateiii[1,27],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Pedido de esclarecimento ou solicitação de ajustes 1"){dados3[,33] <- input$Entradas58 - as.Date(Dateiii[1,28],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Prova pericial"){dados3[,34] <- input$Entradas60 - as.Date(Dateiii[1,29],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Prova oral (audiência de instrução e julgamento)"){dados3[,35] <- input$Entradas62 - as.Date(Dateiii[1,30],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Ato ordinário (Alegações finais)"){dados3[,36] <- input$Entradas64 - as.Date(Dateiii[1,31],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Expedição de intimação (Requerente) 2"){dados3[,37] <- input$Entradas66 - as.Date(Dateiii[1,32],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Cumprimento da intimação 4"){dados3[,38] <- input$Entradas68 - as.Date(Dateiii[1,33],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Alegações finais"){dados3[,39] <- input$Entradas70 - as.Date(Dateiii[1,34],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Expedição de intimação (Requerido) 2"){dados3[,40] <- input$Entradas72 - as.Date(Dateiii[1,35],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Cumprimento da intimação 5"){dados3[,41] <- input$Entradas74 - as.Date(Dateiii[1,36],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Alegações finais 1"){dados3[,42] <- input$Entradas76 - as.Date(Dateiii[1,37],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Recebimento e conclusão do processo"){dados3[,43] <- input$Entradas78 - as.Date(Dateiii[1,38],"%Y-%m-%d")}
      
      if(input$Entradas4 == "Sentença"){dados3[,44] <- input$Entradas80 - as.Date(Dateiii[1,39],"%Y-%m-%d")}
      
      # Para colocar as Datas 
      
      names(Date) <- c("Nº Processo","Apelido", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                       "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                       "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                       "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                       "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                       "Recebimento e conclusão do processo","Sentença")
      
      Date3 <- subset(Date, Apelido == input$Entradas1, select = c("Nº Processo","Apelido", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                                                                   "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                                                                   "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                                                                   "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                                                                   "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                                                                   "Recebimento e conclusão do processo","Sentença"))
      
      
      if(nrow(Date3) >= 2){Date3 <- Date3[1,]}
      
      names(Date3) <- c("Nº Processo","Apelido","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                        "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                        "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação 1","Provas 1","Decisão saneamento",
                        "Expedição de intimação (Requerente) 1","Cumprimento da intimação 2","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido) 1","Cumprimento da intimação 3","Pedido de esclarecimento ou solicitação de ajustes 1",
                        "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente) 2","Cumprimento da intimação 4","Alegações finais","Expedição de intimação (Requerido) 2","Cumprimento da intimação 5","Alegações finais 1",
                        "Recebimento e conclusão do processo","Sentença")
      
      if(input$Entradas4 == "Recebimento, triagem e conclusão do processo"){Date3[1,3] <- input$Entradas6}
      
      if(input$Entradas4 == "Despacho para notificação do Requerido"){Date3[1,4] <- input$Entradas8}
      
      if(input$Entradas4 == "Expedição de notificação"){Date3[1,5] <- input$Entradas10}
      
      if(input$Entradas4 == "Cumprimento da notificação"){Date3[1,6] <- input$Entradas12}
      
      if(input$Entradas4 == "Oferecimento de manifestação por escrito"){Date3[1,7] <- input$Entradas14}
      
      if(input$Entradas4 == "Decisão rejeição da ação ou recebimento pet. inicial"){Date3[1,8] <- input$Entradas16}
      
      if(input$Entradas4 == "Expedição de intimação e/ou citação"){Date3[1,9] <- input$Entradas18}
      
      if(input$Entradas4 == "Cumprimento da intimação e/ou citação"){Date3[1,10] <- input$Entradas20}
      
      if(input$Entradas4 == "Contestação e/ou agravo de instrumento"){Date3[1,11] <- input$Entradas22}
      
      if(input$Entradas4 == "Ato ordinatório (Réplica)"){Date3[1,12] <- input$Entradas24}
      
      if(input$Entradas4 == "Expedição de intimação"){Date3[1,13] <- input$Entradas26}
      
      if(input$Entradas4 == "Cumprimento da intimação"){Date3[,14] <- input$Entradas28}
      
      if(input$Entradas4 == "Réplica"){Date3[1,15] <- input$Entradas30}
      
      if(input$Entradas4 == "Ato ordinatório (Provas)"){Date3[1,16] <- input$Entradas32}
      
      if(input$Entradas4 == "Expedição de intimação (Requerente)"){Date3[1,17] <- input$Entradas34}
      
      if(input$Entradas4 == "Cumprimento  da intimação"){Date3[1,18] <- input$Entradas36}
      
      if(input$Entradas4 == "Provas"){Date3[1,19] <- input$Entradas38}
      
      if(input$Entradas4 == "Expedição de intimação (Requerido)"){Date3[1,20] <- input$Entradas40}
      
      if(input$Entradas4 == "Cumprimento da intimação 1"){Date3[1,21] <- input$Entradas42}
      
      if(input$Entradas4 == "Provas 1"){Date3[1,22] <- input$Entradas44}
      
      if(input$Entradas4 == "Decisão saneamento"){Date3[1,23] <- input$Entradas46}
      
      if(input$Entradas4 == "Expedição de intimação (Requerente) 1"){Date3[1,24] <- input$Entradas48}
      
      if(input$Entradas4 == "Cumprimento da intimação 2"){Date3[1,25] <- input$Entradas50}
      
      if(input$Entradas4 == "Pedido de esclarecimento ou solicitação de ajustes"){Date3[1,26] <- input$Entradas52}
      
      if(input$Entradas4 == "Expedição de intimação (Requerido) 1"){Date3[1,27] <- input$Entradas54}
      
      if(input$Entradas4 == "Cumprimento da intimação 3"){Date3[1,28] <- input$Entradas56}
      
      if(input$Entradas4 == "Pedido de esclarecimento ou solicitação de ajustes 1"){Date3[1,29] <- input$Entradas58}
      
      if(input$Entradas4 == "Prova pericial"){Date3[1,30] <- input$Entradas60}
      
      if(input$Entradas4 == "Prova oral (audiência de instrução e julgamento)"){Date3[1,31] <- input$Entradas62}
      
      if(input$Entradas4 == "Ato ordinário (Alegações finais)"){Date3[1,32] <- input$Entradas64}
      
      if(input$Entradas4 == "Expedição de intimação (Requerente) 2"){Date3[1,33] <- input$Entradas66}
      
      if(input$Entradas4 == "Cumprimento da intimação 4"){Date3[1,34] <- input$Entradas68}
      
      if(input$Entradas4 == "Alegações finais"){Date3[1,35] <- input$Entradas70}
      
      if(input$Entradas4 == "Expedição de intimação (Requerido) 2"){Date3[1,36] <- input$Entradas72}
      
      if(input$Entradas4 == "Cumprimento da intimação 5"){Date3[1,37] <- input$Entradas74}
      
      if(input$Entradas4 == "Alegações finais 1"){Date3[1,38] <- input$Entradas76}
      
      if(input$Entradas4 == "Recebimento e conclusão do processo"){Date3[1,39] <- input$Entradas78}
      
      if(input$Entradas4 == "Sentença"){Date3[1,40] <- input$Entradas80}
      
      names(Date3) <- c("Nº Processo","Apelido","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                        "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                        "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                        "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                        "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                        "Recebimento e conclusão do processo","Sentença")
      
      write.csv(Date3,"Data2.csv")
      write.csv(dados3, "output2.csv")
      
      output$table3 <- DT::renderDataTable({
        dt3 = data.frame(dados3)
        
        datatable(dt3, colnames =c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                                   "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                                   "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                                   "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                                   "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                                   "Recebimento e conclusão do processo","Sentença"), escape = FALSE, extensions = c('FixedColumns',"FixedHeader","Buttons","ColReorder","Scroller"),
                  options = list(dom = 'Bfrtip', colReorder = TRUE,searchHighlight = TRUE,
                                 buttons = c('copy', 'print'),lengthMenu = c(5, 10), pageLength = 15, scrollX = TRUE,
                                 fixedHeader=TRUE, fixedColumns = list(leftColumns = 3, rightColumns = 0),paging = TRUE, searching = TRUE, info = FALSE,sort = FALSE,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#f2f5f9', 'color': 'black'});",
                                                   "}")
                  )) %>% 
          formatStyle(
            'Status',
            target = 'row',
            backgroundColor = styleEqual(c("Ok","Alerta", "Atrasado"), c('white','yellow', 'red'))
          )
      }
      )
      
    })
    
    
    
    isolate({
      
      #Arrumando o banco de dados
      
      dadosi2 <- dados[order(dados$Apelido),]
      dadosdif <- merge(dadosi2,dadosP,by = "Apelido")
      
      dadosdif2 <- dadosdif[,7:44]-dadosdif[,46:83]
      dadosdif2 <- data.frame(dadosdif[,1:6],dadosdif2)
      
      names(dadosdif2) <- c("Apelido","Nº Processo","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                            "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                            "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                            "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                            "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                            "Recebimento e conclusão do processo","Sentença")
      
      dadosdif2 <<- dadosdif2[order(dadosdif2$Apelido),]
      
      # Estruturando os loops para colocar Status OK
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,7] == 0 & dadosi2[i,8] == 0 & dadosi2[i,9] == 0){
          dadosdif2[i,7:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,7] != 0 & dadosi2[i,8] == 0 & dadosi2[i,9] == 0 & dadosi2[i,10] == 0){
          dadosdif2[i,8:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,8] != 0 & dadosi2[i,9] == 0 & dadosi2[i,10] == 0 & dadosi2[i,11] == 0){
          dadosdif2[i,7] = "OK"
          dadosdif2[i,9:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,9] != 0 & dadosi2[i,10] == 0 & dadosi2[i,11] == 0 & dadosi2[i,12] == 0){
          dadosdif2[i,7:8] = "OK"
          dadosdif2[i,10:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,10] != 0 & dadosi2[i,11] == 0 & dadosi2[i,12] == 0 & dadosi2[i,13] == 0){
          dadosdif2[i,7:9] = "OK"
          dadosdif2[i,11:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,11] != 0 & dadosi2[i,12] == 0 & dadosi2[i,13] == 0 & dadosi2[i,14] == 0){
          dadosdif2[i,7:10] = "OK"
          dadosdif2[i,12:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,12] != 0 & dadosi2[i,13] == 0 & dadosi2[i,14] == 0 & dadosi2[i,15] == 0){
          dadosdif2[i,7:11] = "OK"
          dadosdif2[i,13:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,13] != 0 & dadosi2[i,14] == 0 & dadosi2[i,15] == 0 & dadosi2[i,16] == 0){
          dadosdif2[i,7:12] = "OK"
          dadosdif2[i,14:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,14] != 0 & dadosi2[i,15] == 0 & dadosi2[i,16] == 0 & dadosi2[i,17] == 0){
          dadosdif2[i,7:13] = "OK"
          dadosdif2[i,15:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,15] != 0 & dadosi2[i,16] == 0 & dadosi2[i,17] == 0 & dadosi2[i,18] == 0){
          dadosdif2[i,7:14] = "OK"
          dadosdif2[i,16:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,16] != 0 & dadosi2[i,17] == 0 & dadosi2[i,18] == 0 & dadosi2[i,19] == 0){
          dadosdif2[i,7:15] = "OK"
          dadosdif2[i,17:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,17] != 0 & dadosi2[i,18] == 0 & dadosi2[i,19] == 0 & dadosi2[i,20] == 0){
          dadosdif2[i,7:16] = "OK"
          dadosdif2[i,18:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,18] != 0 & dadosi2[i,19] == 0 & dadosi2[i,20] == 0 & dadosi2[i,21] == 0){
          dadosdif2[i,7:17] = "OK"
          dadosdif2[i,19:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,19] != 0 & dadosi2[i,20] == 0 & dadosi2[i,21] == 0 & dadosi2[i,22] == 0){
          dadosdif2[i,7:18] = "OK"
          dadosdif2[i,20:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,20] != 0 & dadosi2[i,21] == 0 & dadosi2[i,22] == 0 & dadosi2[i,23] == 0){
          dadosdif2[i,7:19] = "OK"
          dadosdif2[i,21:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,21] != 0 & dadosi2[i,22] == 0 & dadosi2[i,23] == 0 & dadosi2[i,24] == 0){
          dadosdif2[i,7:20] = "OK"
          dadosdif2[i,22:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,22] != 0 & dadosi2[i,23] == 0 & dadosi2[i,24] == 0 & dadosi2[i,25] == 0){
          dadosdif2[i,7:21] = "OK"
          dadosdif2[i,23:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,23] != 0 & dadosi2[i,24] == 0 & dadosi2[i,25] == 0 & dadosi2[i,26] == 0){
          dadosdif2[i,7:22] = "OK"
          dadosdif2[i,24:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,24] != 0 & dadosi2[i,25] == 0 & dadosi2[i,26] == 0 & dadosi2[i,27] == 0){
          dadosdif2[i,7:23] = "OK"
          dadosdif2[i,25:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,25] != 0 & dadosi2[i,26] == 0 & dadosi2[i,27] == 0 & dadosi2[i,28] == 0){
          dadosdif2[i,7:24] = "OK"
          dadosdif2[i,26:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,26] != 0 & dadosi2[i,27] == 0 & dadosi2[i,28] == 0 & dadosi2[i,29] == 0){
          dadosdif2[i,7:25] = "OK"
          dadosdif2[i,27:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,27] != 0 & dadosi2[i,28] == 0 & dadosi2[i,29] == 0 & dadosi2[i,30] == 0){
          dadosdif2[i,7:26] = "OK"
          dadosdif2[i,28:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,28] != 0 & dadosi2[i,29] == 0 & dadosi2[i,30] == 0 & dadosi2[i,31] == 0){
          dadosdif2[i,7:27] = "OK"
          dadosdif2[i,29:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,29] != 0 & dadosi2[i,30] == 0 & dadosi2[i,31] == 0 & dadosi2[i,32] == 0){
          dadosdif2[i,7:28] = "OK"
          dadosdif2[i,30:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,30] != 0 & dadosi2[i,31] == 0 & dadosi2[i,32] == 0 & dadosi2[i,33] == 0){
          dadosdif2[i,7:29] = "OK"
          dadosdif2[i,31:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,31] != 0 & dadosi2[i,32] == 0 & dadosi2[i,33] == 0 & dadosi2[i,34] == 0){
          dadosdif2[i,7:30] = "OK"
          dadosdif2[i,32:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,32] != 0 & dadosi2[i,33] == 0 & dadosi2[i,34] == 0 & dadosi2[i,35] == 0){
          dadosdif2[i,7:31] = "OK"
          dadosdif2[i,33:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,33] != 0 & dadosi2[i,34] == 0 & dadosi2[i,35] == 0 & dadosi2[i,36] == 0){
          dadosdif2[i,7:32] = "OK"
          dadosdif2[i,34:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,34] != 0 & dadosi2[i,35] == 0 & dadosi2[i,36] == 0 & dadosi2[i,37] == 0){
          dadosdif2[i,7:33] = "OK"
          dadosdif2[i,35:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,35] != 0 & dadosi2[i,36] == 0 & dadosi2[i,37] == 0 & dadosi2[i,38] == 0){
          dadosdif2[i,7:34] = "OK"
          dadosdif2[i,36:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,36] != 0 & dadosi2[i,37] == 0 & dadosi2[i,38] == 0 & dadosi2[i,39] == 0){
          dadosdif2[i,7:35] = "OK"
          dadosdif2[i,37:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,37] != 0 & dadosi2[i,38] == 0 & dadosi2[i,39] == 0 & dadosi2[i,40] == 0){
          dadosdif2[i,7:36] = "OK"
          dadosdif2[i,38:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,38] != 0 & dadosi2[i,39] == 0 & dadosi2[i,40] == 0 & dadosi2[i,41] == 0){
          dadosdif2[i,7:37] = "OK"
          dadosdif2[i,39:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,39] != 0 & dadosi2[i,40] == 0 & dadosi2[i,41] == 0 & dadosi2[i,42] == 0){
          dadosdif2[i,7:38] = "OK"
          dadosdif2[i,40:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,40] != 0 & dadosi2[i,41] == 0 & dadosi2[i,42] == 0 & dadosi2[i,43] == 0){
          dadosdif2[i,7:39] = "OK"
          dadosdif2[i,41:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,41] != 0 & dadosi2[i,42] == 0 & dadosi2[i,43] == 0 & dadosi2[i,44] == 0){
          dadosdif2[i,7:40] = "OK"
          dadosdif2[i,42:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,42] != 0 & dadosi2[i,43] == 0 & dadosi2[i,44] == 0){
          dadosdif2[i,7:41] = "OK"
          dadosdif2[i,43:44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,43] != 0 & dadosi2[i,44] == 0){
          dadosdif2[i,7:42] = "OK"
          dadosdif2[i,44] = 0
        }
      }
      
      for (i in 1:nrow(dadosi2)) {
        if(dadosi2[i,44] != 0){
          dadosdif2[i,7:43] = "OK"
        }
      }
      
      rownames(dadosdif2) <- NULL
      
      dadosdif2$`Data Inicial` <- format(as.Date(dadosdif2$`Data Inicial`), "%d/%m/%Y")
    })
    
    
    # Tabela de Visualização com os OKS
    
    output$tablevisual <- DT::renderDataTable({
      
      datatable(dadosdif2,colnames =c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                                      "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                                      "Recebimento e conclusão do processo","Sentença"),  extensions = c('FixedColumns',"FixedHeader","Buttons","ColReorder","Scroller"),filter = "top",
                options = list(dom = 'Bfrtip', colReorder = TRUE, 
                               buttons = c('copy', 'print'),lengthMenu = c(5, 10), pageLength = 15, scrollX = TRUE, scrollY  = 500,
                               fixedHeader=TRUE, fixedColumns = list(leftColumns = 3, rightColumns = 0),
                               initComplete = JS("function(settings, json) {",
                                                 "$(this.api().table().header()).css({'background-color': '#f2f5f9', 'color': 'black'});",
                                                 "}")
                )) %>%
        formatStyle( 
          'Classificação',
          target = 'row', fontWeight = styleEqual(c("Encerrado"), c('bold'))
        )%>%
        formatStyle( 
          'Status',
          target = 'row',
          backgroundColor = styleEqual(c("Ok","Alerta", "Atrasado"), c('white','yellow', 'red'))
        )
      
      
      
    }
    )
    
    dadosteste <- dados2[nrow(dados2),]
    
    names(dadosteste) <- c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                           "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                           "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                           "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                           "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                           "Recebimento e conclusão do processo","Sentença")
    
    names(dados) <- c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                      "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                      "Recebimento e conclusão do processo","Sentença")
    
    #Removendo os lixos das tabelas 
    
    
    
    linhas_tabela_limpeza_Prazos <- grep(dadosteste$Apelido,dados$Apelido, fixed=TRUE)
    
    if(length(linhas_tabela_limpeza_Prazos) == 0){
      dados <- rbind(dados,dadosteste)
    }
    
    if(length(linhas_tabela_limpeza_Prazos) != 0){
      dados <- dados
    }
    
    linhas_tabela_limpeza2 <- grep("Enter text...", dados$Apelido, fixed=TRUE)
    
    for (i in 1:length(linhas_tabela_limpeza2)) {
      if(length(linhas_tabela_limpeza2) != 0){
        dados <- dados[-c(linhas_tabela_limpeza2[i]),]
      }
    }
    
    
    write.csv(dados,'teste.csv')
    
    # Tabela de Visualização com os períodos
    
    #output$table1 <- DT::renderDataTable({
    
    # names(dados) <- c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
    #                   "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
    #                    "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
    #                    "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
    #                    "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
    #                    "Recebimento e conclusão do processo","Sentença")
    
    #  dados$`Data Inicial` <- format(as.Date(dados$`Data Inicial`), "%d/%m/%Y")
    
    #  dt = data.frame(dados)
    
    #  rownames(dt) <- NULL
    
    #datatable(dt,colnames =c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
    #                         "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
    #                         "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
    #                         "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
    #                         "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
    #                         "Recebimento e conclusão do processo","Sentença"), escape = FALSE, extensions = c('FixedColumns',"FixedHeader","Buttons","ColReorder","Scroller"),filter = "top",
    #          options = list(dom = 'Bfrtip', colReorder = TRUE,searchHighlight = TRUE, 
    #                         buttons = c('copy', 'print'),lengthMenu = c(5, 10), pageLength = 15, scrollX = TRUE, scrollY  = 500,
    #                         fixedHeader=TRUE, fixedColumns = list(leftColumns = 3, rightColumns = 0), searching = TRUE,sort = FALSE,
    #                         initComplete = JS("function(settings, json) {",
    #                                           "$(this.api().table().header()).css({'background-color': '#f2f5f9', 'color': 'black'});",
    #                                           "}")
    #          )) %>%
    #  formatStyle( 
    #    'Classificação',
    #    target = 'row', fontWeight = styleEqual(c("Encerrado"), c('bold'))
    #  )%>%
    #  formatStyle( 
    #    'Status',
    #    target = 'row',
    #    backgroundColor = styleEqual(c("Ok","Alerta", "Atrasado"), c('white','yellow', 'red'))
    #  )
    #}
    #)
    
    
  })
  
  observe({
    
    
    invalidateLater(200000, session)
    
    comparacao <- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\completo.csv")
    comparacao <- comparacao[,-1]
    
    dadosteste <- dados2[nrow(dados2),]
    
    linhas_tabela_limpeza_Prazos <- grep(dadosteste$Apelido,comparacao$Apelido, fixed=TRUE)
    
    if(length(linhas_tabela_limpeza_Prazos) == 0){
      dados <- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\teste.csv")
      dados <- dados[,-1]
    }
    
    if(length(linhas_tabela_limpeza_Prazos) != 0){
      dados <- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\completo.csv")
      dados <- dados[,-1]
      
      ####################### Entrando com o banco de dados para Atualização #############################
      
      names(dados5) <- c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                         "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                         "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                         "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                         "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                         "Recebimento e conclusão do processo","Sentença")
      
      
      names(dados) <- c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                        "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                        "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                        "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                        "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                        "Recebimento e conclusão do processo","Sentença")
      
      dados <- rbind(dados,dados5)
      
      #Removendo os lixos das tabelas 
      
      linhas_tabela_limpeza <- grep(dados5$`Nº Processo`, dados$`Nº Processo`, fixed=TRUE)
      
      if(length(linhas_tabela_limpeza) != 0){
        dados <- dados[-c(linhas_tabela_limpeza[1:(length(linhas_tabela_limpeza)-1)]),]
      }
      
      linhas_tabela_limpeza2 <- grep("Enter text...", dados$`Nº Processo`, fixed=TRUE)
      
      for (i in 1:length(linhas_tabela_limpeza2)) {
        if(length(linhas_tabela_limpeza2) != 0){
          dados <- dados[-c(linhas_tabela_limpeza2[i]),]
        }
      }
      
    }
    
    names(dados) <- c("Nº Processo","Apelido","Classificação","Status","Classe do Processo", "Data Inicial", "Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                      "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                      "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                      "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                      "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                      "Recebimento e conclusão do processo","Sentença")
    
    # Estruturando os loops para colocar +1 dia
    
    for (i in 1:nrow(dados)) {
      if(dados[i,7] == 0 & dados[i,8] == 0 & dados[i,9] == 0 & dados[i,3] == "Aberto"){
        dados[i,7] = dados[i,7] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,7] != 0 & dados[i,8] == 0 & dados[i,9] == 0 & dados[i,10] == 0 & dados[i,3] == "Aberto"){
        dados[i,7] = dados[i,7] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,8] != 0 & dados[i,9] == 0 & dados[i,10] == 0 & dados[i,11] == 0 & dados[i,3] == "Aberto"){
        dados[i,8] = dados[i,8] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,9] != 0 & dados[i,10] == 0 & dados[i,11] == 0 & dados[i,12] == 0 & dados[i,3] == "Aberto"){
        dados[i,9] = dados[i,9] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,10] != 0 & dados[i,11] == 0 & dados[i,12] == 0 & dados[i,13] == 0 & dados[i,3] == "Aberto"){
        dados[i,10] = dados[i,10] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,11] != 0 & dados[i,12] == 0 & dados[i,13] == 0 & dados[i,14] == 0 & dados[i,3] == "Aberto"){
        dados[i,11] = dados[i,11] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,12] != 0 & dados[i,13] == 0 & dados[i,14] == 0 & dados[i,15] == 0 & dados[i,3] == "Aberto"){
        dados[i,12] = dados[i,12] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,13] != 0 & dados[i,14] == 0 & dados[i,15] == 0 & dados[i,16] == 0 & dados[i,3] == "Aberto"){
        dados[i,13] = dados[i,13] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,14] != 0 & dados[i,15] == 0 & dados[i,16] == 0 & dados[i,17] == 0 & dados[i,3] == "Aberto"){
        dados[i,14] = dados[i,14] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,15] != 0 & dados[i,16] == 0 & dados[i,17] == 0 & dados[i,18] == 0 & dados[i,3] == "Aberto"){
        dados[i,15] = dados[i,15] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,16] != 0 & dados[i,17] == 0 & dados[i,18] == 0 & dados[i,19] == 0 & dados[i,3] == "Aberto"){
        dados[i,16] = dados[i,16] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,17] != 0 & dados[i,18] == 0 & dados[i,19] == 0 & dados[i,20] == 0 & dados[i,3] == "Aberto"){
        dados[i,17] = dados[i,17] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,18] != 0 & dados[i,19] == 0 & dados[i,20] == 0 & dados[i,21] == 0 & dados[i,3] == "Aberto"){
        dados[i,18] = dados[i,18] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,19] != 0 & dados[i,20] == 0 & dados[i,21] == 0 & dados[i,22] == 0 & dados[i,3] == "Aberto"){
        dados[i,19] = dados[i,19] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,20] != 0 & dados[i,21] == 0 & dados[i,22] == 0 & dados[i,23] == 0 & dados[i,3] == "Aberto"){
        dados[i,20] = dados[i,20] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,21] != 0 & dados[i,22] == 0 & dados[i,23] == 0 & dados[i,24] == 0 & dados[i,3] == "Aberto"){
        dados[i,21] = dados[i,21] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,22] != 0 & dados[i,23] == 0 & dados[i,24] == 0 & dados[i,25] == 0 & dados[i,3] == "Aberto"){
        dados[i,22] = dados[i,22] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,23] != 0 & dados[i,24] == 0 & dados[i,25] == 0 & dados[i,26] == 0 & dados[i,3] == "Aberto"){
        dados[i,23] = dados[i,23] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,24] != 0 & dados[i,25] == 0 & dados[i,26] == 0 & dados[i,27] == 0 & dados[i,3] == "Aberto"){
        dados[i,24] = dados[i,24] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,25] != 0 & dados[i,26] == 0 & dados[i,27] == 0 & dados[i,28] == 0 & dados[i,3] == "Aberto"){
        dados[i,25] = dados[i,25] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,26] != 0 & dados[i,27] == 0 & dados[i,28] == 0 & dados[i,29] == 0 & dados[i,3] == "Aberto"){
        dados[i,26] = dados[i,26] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,27] != 0 & dados[i,28] == 0 & dados[i,29] == 0 & dados[i,30] == 0 & dados[i,3] == "Aberto"){
        dados[i,27] = dados[i,27] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,28] != 0 & dados[i,29] == 0 & dados[i,30] == 0 & dados[i,31] == 0 & dados[i,3] == "Aberto"){
        dados[i,28] = dados[i,28] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,29] != 0 & dados[i,30] == 0 & dados[i,31] == 0 & dados[i,32] == 0 & dados[i,3] == "Aberto"){
        dados[i,29] = dados[i,29] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,30] != 0 & dados[i,31] == 0 & dados[i,32] == 0 & dados[i,33] == 0 & dados[i,3] == "Aberto"){
        dados[i,30] = dados[i,30] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,31] != 0 & dados[i,32] == 0 & dados[i,33] == 0 & dados[i,34] == 0 & dados[i,3] == "Aberto"){
        dados[i,31] = dados[i,31] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,32] != 0 & dados[i,33] == 0 & dados[i,34] == 0 & dados[i,35] == 0 & dados[i,3] == "Aberto"){
        dados[i,32] = dados[i,32] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,33] != 0 & dados[i,34] == 0 & dados[i,35] == 0 & dados[i,36] == 0 & dados[i,3] == "Aberto"){
        dados[i,33] = dados[i,33] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,34] != 0 & dados[i,35] == 0 & dados[i,36] == 0 & dados[i,37] == 0 & dados[i,3] == "Aberto"){
        dados[i,34] = dados[i,34] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,35] != 0 & dados[i,36] == 0 & dados[i,37] == 0 & dados[i,38] == 0 & dados[i,3] == "Aberto"){
        dados[i,35] = dados[i,35] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,36] != 0 & dados[i,37] == 0 & dados[i,38] == 0 & dados[i,39] == 0 & dados[i,3] == "Aberto"){
        dados[i,36] = dados[i,36] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,37] != 0 & dados[i,38] == 0 & dados[i,39] == 0 & dados[i,40] == 0 & dados[i,3] == "Aberto"){
        dados[i,37] = dados[i,37] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,38] != 0 & dados[i,39] == 0 & dados[i,40] == 0 & dados[i,41] == 0 & dados[i,3] == "Aberto"){
        dados[i,38] = dados[i,38] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,39] != 0 & dados[i,40] == 0 & dados[i,41] == 0 & dados[i,42] == 0 & dados[i,3] == "Aberto"){
        dados[i,39] = dados[i,39] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,40] != 0 & dados[i,41] == 0 & dados[i,42] == 0 & dados[i,43] == 0 & dados[i,3] == "Aberto"){
        dados[i,40] = dados[i,40] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,41] != 0 & dados[i,42] == 0 & dados[i,43] == 0 & dados[i,44] == 0 & dados[i,3] == "Aberto"){
        dados[i,41] = dados[i,41] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,42] != 0 & dados[i,43] == 0 & dados[i,44] == 0 & dados[i,3] == "Aberto"){
        dados[i,42] = dados[i,42] + 1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,43] != 0 & dados[i,44] == 0 & dados[i,3] == "Aberto"){
        dados[i,43] = dados[i,43] +1
      }
    }
    
    for (i in 1:nrow(dados)) {
      if(dados[i,44] != 0 & dados[i,3] == "Aberto"){
        dados[i,44] = dados[i,44] + 1
      }
    }
    
    write.csv(dados,'completo.csv')
    
  })
  
  observe({
    
    invalidateLater(3600000, session)
    
    Atualizacao <- data.frame("1235XXX","Apelido 1")
    Atualizacao[,c(3:40)] <- "2020-01-13 09:00:00" 
    
    names(Atualizacao) <- c("Nº Processo","Apelido","Recebimento, triagem e conclusão do processo","Despacho para notificação do Requerido", "Expedição de notificação","Cumprimento da notificação","Oferecimento de manifestação por escrito",
                            "Decisão rejeição da ação ou recebimento pet. inicial","Expedição de intimação e/ou citação","Cumprimento da intimação e/ou citação","Contestação e/ou agravo de instrumento","Ato ordinatório (Réplica)","Expedição de intimação",
                            "Cumprimento da intimação","Réplica","Ato ordinatório (Provas)","Expedição de intimação (Requerente)","Cumprimento  da intimação","Provas","Expedição de intimação (Requerido)","Cumprimento da intimação","Provas","Decisão saneamento",
                            "Expedição de intimação (Requerente)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes","Expedição de intimação (Requerido)","Cumprimento da intimação","Pedido de esclarecimento ou solicitação de ajustes",
                            "Prova pericial","Prova oral (audiência de instrução e julgamento)","Ato ordinário (Alegações finais)","Expedição de intimação (Requerente)","Cumprimento da intimação","Alegações finais","Expedição de intimação (Requerido)","Cumprimento da intimação","Alegações finais",
                            "Recebimento e conclusão do processo","Sentença")
    
    write.csv(Atualizacao,"Data2.csv")
  })
  
})



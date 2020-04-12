############################################# Entrando com a Interface pessoal ############################################ 

#Entrando com os pacotes necessários para análise

#install.packages("shiny")
library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("survival")
library(survival)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("survminer")
library(survminer)
#install.packages("curl")
library(curl)
#install.packages("car")
library(car)
#install.packages("agricolae")
library(agricolae)
#install.packages("DT")
library(DT)
#install.packages("knitr")
library(knitr)
#install.packages("readxl")
library(readxl)
#install.packages("viridis")
library(viridis)
#install.packages("hrbrthemes")
library(hrbrthemes)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("plotly")
library(plotly)

#install.packages('rsconnect')
rsconnect::setAccountInfo(name='fernandocesartcc',
                          token='87E6637E8D04FDF555C54E9BBD22ABD3',
                          secret='tU2+Jpe+M//mT+Q9NDn0LUNh92XCksAWn+quHjU6')

library(rsconnect)
setwd("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\pasta")
#deployApp()

#Criação da interface pessoal 

page <-  dashboardPage(skin = "green",
                       # Foi modificado a formatação do texto no topo da página, adicionados links para sites externos e quadros de mensagens no topo da tela
                       
                       header <- dashboardHeader(title = span(
                         "5ª Vara da Justiça Federal", # Título do Dashboard
                         style = "font-family: Tahoma; font-weight: bold" 
                       ),titleWidth = "400px",
                       tags$li(a(href = 'https://sei.df.gov.br/sip/login.php?sigla_orgao_sistema=GDF&sigla_sistema=SEI', # página externa
                                 icon("wifi"),
                                 title = "Página SEI"),
                               class = "dropdown"),
                       tags$li(a(href = 'https://portal.trf1.jus.br/sjdf/', # página externa
                                 icon("link"),
                                 #img(src = 'https://pbs.twimg.com/profile_images/865670431830818816/1l4aMf6A_400x400.jpg', # opção de colocar uma foto no icone de acesso
                                 title = "Página da Justiça Federal", height = "30px"),
                               #style = "padding-top:10px; padding-bottom:10px;",
                               class = "dropdown"),
                       dropdownMenu(type = "message", #Criando uma mensagem de notificação
                                    messageItem(from = "Desenvolvedores", message = "Bem vindo a versão Beta do Dashboard", href="https://shiny.rstudio.com/"), # Mensagem de Boas vindas
                                    messageItem(from = "Desenvolvedores", message = "Em caso de erros informar ao desenvolvedor", icon = icon("r-project")) #Notificação de erro
                       )),
                       
                       sidebar <- dashboardSidebar( uiOutput("sidebarpanel")),
                       
                       body <- dashboardBody( uiOutput("body"),
                                              
                                              tabItems(
                                                tabItem(tabName = "dashboard", # Adicionando as caixas de InfoBox na aba DashBoard 
                                                        
                                                        fluidRow(
                                                          infoBoxOutput("Encerrados"),
                                                          infoBoxOutput("Sobrevivencia"),
                                                          infoBoxOutput("Risco_Acumulado"),
                                                          infoBoxOutput("Risco"),
                                                          infoBoxOutput("Tempo_medio"),
                                                          infoBoxOutput("Vida_media")),
                                                        
                                                        fluidRow(
                                                          box(title = strong("Função de sobrevivência"), status = "primary",solidHeader = TRUE,plotOutput("histogram1")),#Gráfico 1
                                                          box(title = strong("Função de risco"), status = "warning",solidHeader = TRUE,plotOutput("histogram2")),#Gráfico 2
                                                          box(title = strong("Prazo médio de execução das tarefas (dias)"),status = "primary", solidHeader = TRUE,plotlyOutput("histogram3")),#Gráfico 3
                                                          box(title = strong("Percentual do prazo médio de execução das tarefas (dias)"),status = "warning", solidHeader = TRUE,plotlyOutput("histogram4"))#Gráfico 4
                                                          
                                                        )),
                                                
                                                tabItem(tabName = "Resumo", # Adicionando as caixas de InfoBox na aba Resumo 
                                                        
                                                        fluidRow(
                                                          infoBoxOutput("Processos"),
                                                          infoBoxOutput("N_Encerrado"),
                                                          infoBoxOutput("N_Aberto"),
                                                          infoBoxOutput("N_ok"),
                                                          infoBoxOutput("N_alerta"),
                                                          infoBoxOutput("N_atrasado")),
                                                          
                                                        fluidRow(
                                                          box(title = strong("Total de Processos Segundo sua Classificação"), status = "primary", solidHeader = TRUE, plotlyOutput("Resumo1")), #Gráfico 1
                                                          box(title = strong("Total de Processos Segundo seu Status"), status = "warning", solidHeader = TRUE, plotlyOutput("Resumo2")), #Gráfico 2
                                                          box(title = strong("Tempo médio de duração dos processos em dias"), status = "primary", solidHeader = TRUE, plotlyOutput("Resumo3")), #Gráfico 3
                                                          box(title = strong("Percentual do tempo médio nos processos Abertos"), status = "warning", solidHeader = TRUE, plotlyOutput("Resumo4")) #Gráfico 4
                                                          
                                                        )),
                                                
                                                
                                                tabItem(tabName = "Classes", # Adicionando as caixas de texto na aba Análise Processual 
                                                        
                                                        fluidRow(
                                                          tabBox(
                                                            side = "right", height = "220px", width = '90%',
                                                            title = "Funcionamento dos processos",
                                                            # The id lets us use input$tabset1 on the server to find the current ta
                                                            tabPanel(" ",p(em("Os processos públicos administrados pela"),em(strong("5ª Vara da Justiça Federal")),em("constituem cinquenta e cinco diferentes categorias de pleitos divididos em: Ações Civis Coletivas, Ações Populares, Buscas e Apreensões, Arrestos, Protestos e etc. Dessa maneira, a instituição possui como encargo a necessidade de triagem das fases processuais contabilizadas desde o recebimento até a conclusão do mesmo pleito, sendo tais etapas contabilizadas em trinta e oito estágios divididos em seis setores internos a instituição (Secretaria, Gabinete, Central de Mandados, Requerido, Requerente e Perito).", align = "center")),
                                                                     
                                                                     p(em("Cada uma das trinta e oito fases pré-definidas no quadro abaixo, possuem tempo limite de duração estruturado em artigos do Código de Processo Civil"),em(strong("nº 297, 802, 896, 1.065, 1.106, 536 e 508")),em(". Necessitando-se assim da colaboração e compreensão dos advogados e servidores para realização das atividades dentro dos prazos delimitados, uma vez que nos termos das leis acima mencionadas, a principio não haverá nenhuma hipótese de prorrogação.", align = "left"))
                                                                     , height = "350px", width = '100%',  style = "font-size: 16px")
                                                          ),
                                                          
                                                          tabBox(
                                                            side = "right", height = "1320px", width = '90%',
                                                            title = "Tabela com o tempo limite dos processos segundo a lei",
                                                            # The id lets us use input$tabset1 on the server to find the current ta
                                                            tabPanel("","", height = "350px", width = '100%'),
                                                            fluidRow(align = "center",
                                                                     tableOutput("data1"))
                                                          )
                                                        ),
                                                        
                                                        fluidRow(
                                                          box(title = strong("Aberturas e fechamento de processos"), status = "primary", solidHeader = TRUE, plotlyOutput("histogram5")),#Gráfico 1
                                                          box(title = strong("Total de Processos segundo seu Status"), status = "warning", solidHeader = TRUE, plotlyOutput("histogram61")), #Gráfico 2
                                                          box(title = strong("Tempo médio vs Prova Pericial"), status = "primary", solidHeader = TRUE, plotlyOutput("histogram6")),
                                                          box(title = strong("Percentual do tempo médio nos processos Abertos"),status = "warning", solidHeader = TRUE, plotlyOutput("histogram62"))) #Gráfico 4
                                                ),
                                                
                                                tabItem(tabName = "Manual", # Adicionando as caixas de texto na aba Manual de uso  
                                                        
                                                        fluidRow(
                                                          tabBox(
                                                            side = "right", height = "820px", width = '90%',
                                                            title = "Descrição sobre o funcionamento do software",
                                                            # The id lets us use input$tabset1 on the server to find the current ta
                                                            tabPanel(" ",p(em("Com o intuito de criar medidas descritivas capazes de informar os advogados e servidores da instituição sobre os prazos remanescentes para elaboração e estruturação dos processos descritos como de interesse, desenvolveu-se com parceria da "),em(strong("5ª Vara da Justiça Federal")),em("um sistema automatizado por meio  do software R em conjunto com o pacote Shiny, produzindo-se assim um layout gráfico robusto e de simples manuseio para a instituição.", align = "center")), height = "350px", width = '100%'),
                                                            p(em("O sistema consiste de 10 abas com diferentes funcionalidades e denominações (Análise processual, Manual de uso, Quadro resumo, Visualização dos dados, Seleção dos prazos, Atualização dos prazos, Criação dos dados, Atualização dos dados, Dashboard e Pré-requisitos). Dessa maneira, será descrito abaixo a finalidade de cada uma dessas abas.")),
                                                            tags$br(),
                                                            tags$div(tags$ul(
                                                              tags$li(a(span(icon("user-tie"), style = "color:black")),strong(span("Análise Processual: ", style = "color:black")),em("Serve para informar o usuário sobre os prazos limites de cada umas das etapas, conjuntamente com o nome do setor interno responsável por esta. Além de disponibilizar algumas médias descritivas com seleção de filtros para classe do processo, mês e ano.")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("tasks"), style = "color:black")),strong(span("Manual de uso: ", style = "color:black")),em("Informa o utilizador da página a respeito de informações referentes as demais abas existentes no sistema, além instruir em relação as interpretações de saídas gráficas disponibilizadas pelo software.")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("chart-line"), style = "color:black")),strong(span("Quadro resumo: ", style = "color:black")),em("Apresenta um conjunto de medidas descritivas referentes ao banco de dados como um todo. Nessa aba não foram disponibilizados filtros para análise pontual.")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("street-view"), style = "color:black")),strong(span("Visualização dos dados: ", style = "color:black")),em("Mostra a diferença de tempo entre os termos adentrados no sistema para o banco de dados referente aos prazos limites do processo e o conjunto de valores já decorrido do mesmo processo.")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("calendar"), style = "color:black")),strong(span("Seleção de prazos: ", style = "color:black")),em("Serve para adicionar ao sistema o prazo referente ao processo criado, possibilitando também, um breve aumento ou diminuição do tempo limite pré-estabelecido para cada etapa processual.")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("cog"), style = "color:black")),strong(span("Atualização dos prazos: ", style = "color:black")),em("EEm casos de necessidade, essa aba possibilita o acréscimo ou redução do prazo para determinadas etapas do processo. Vale ressaltar que tais mudanças não devem ser feitas por livre arbítrio.")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("database"), style = "color:black")),strong(span("Criação dos dados: ", style = "color:black")),em("Nessa aba ocorre a possibilidade de adentrar com novos processos já existentes na instituição ao sistema. Vale ressaltar a necessidade de preenchimento simultâneo com a aba Seleção de prazos.")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("sync"), style = "color:black")),strong(span("Atualização dos dados: ", style = "color:black")),em("Em casos de necessidade, essa aba possibilita a mudança nas datas de termino das atividades internas referentes a instituição para as determinadas classes de processos. Vale ressaltar que tais mudanças não devem ser feitas por livre arbítrio.")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("dashboard"), style = "color:black")),strong(span("Dashboard: ", style = "color:black")),em("Mostra algumas informações referentes a análise de sobrevivência para as classes de processos em interesse. Como por exemplo, a probabilidade do processo ser encerrado antes de ser finalizado ou o tempo médio que a classe processual leva para sua conclusão.")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("calculator"), style = "color:black")),strong(span("Pré-requisitos: ", style = "color:black")),em("Serve para informar sobre a qualidade de ajuste dos dados ao modelo proposto na análise de sobrevivência na aba Dashboard."))), height = "350px", width = '100%',  style = "font-size: 16px")
                                                          ),
                                                          
                                                          tabBox(
                                                            side = "right", height = "750px", width = '90%',
                                                            title = "Explicação sobre os gráficos utilizados",
                                                            # The id lets us use input$tabset1 on the server to find the current ta
                                                            tabPanel(" ",p(em("As saídas gráficas apresentadas nessa aba representam um resumo de todos os Plots contidos no sistema. Dessa maneira, será descrito abaixo de maneira simples e sucinta como se discorre a usabilidade e interpretabilidade de cada um dos gráficos."), height = "370px", width = '100%'),
                                                                     tags$br(),
                                                                     tags$div(tags$ul(
                                                                       
                                                                       tags$li(a(span(icon("chart-pie"), style = "color:black")),strong(span("Função de Sobrevivência: ", style = "color:black")),em("Serve para comparar as probabilidades de sobrevivência ao longo do tempo entre dois grupos distintos (processos que apresentam seu status “ok” e processos com o status “Alerta”). Ou seja, calcula a chance de que uma classe processual não seja encerrada antes de sua conclusão efetiva para um determinado período de tempo.")),
                                                                       tags$br(),
                                                                       tags$li(a(span(icon("chart-line"), style = "color:black")),strong(span("Função de risco Acumulado: ", style = "color:black")),em("Representa o risco acumulado de um processo ser encerrado naquele período de tempo sem que necessariamente este seja concluído efetivamente.")),
                                                                       tags$br(),
                                                                       tags$li(a(span(icon("chart-area"), style = "color:black")),strong(span("Gráfico de colunas: ", style = "color:black")),em("Descreve o número de observações entre as classes de processos selecionadas que possuem classificação aberta ou encerrada dentre as delimitações impostas pelos filtros.")),
                                                                       tags$br(),
                                                                       tags$li(a(span(icon("chart-bar"), style = "color:black")),strong(span("Gráfico de bolhas: ", style = "color:black")),em("Mostra que para uma determinada fase do processo, qual seria o tempo médio de duração das classes processuais segundo sua classificação em Aberto e Encerrado. As bolhas que apresentam menores comprimentos são aquelas que possuem melhores resultados.")),
                                                                       tags$br(),
                                                                       tags$li(a(span(icon("project-diagram"), style = "color:black")),strong(span("Gráfico de barras: ", style = "color:black")),em("Demonstra a frequência de observações entre as classes processuais para os status de Ok, Alerta e Atrasado. Leva-se em consideração as delimitações impostas pelos filtros.")),
                                                                       tags$br(),
                                                                       tags$li(a(span(icon("chart-pie"), style = "color:black")),strong(span("Gráfico de setores: ", style = "color:black")),em("Representado numa escala percentual, o Plot descreve qual das classes processuais demandou da instituição um maior período de tempo até sua conclusão.")),
                                                                       tags$br(),
                                                                       tags$li(a(span(icon("chart-line"), style = "color:black")),strong(span("Resíduos de Cox-Snell: ", style = "color:black")),em("É utilizado para medir a qualidade de ajuste do modelo de Cox. Caso esteja bem ajustada, o gráfico gerado devera apresentar uma estrutura próxima a uma reta.")),
                                                                       tags$br(),
                                                                       tags$li(a(span(icon("chart-area"), style = "color:black")),strong(span("Resíduos de Schoenfeld: ", style = "color:black")),em("Serve para verificar a suposição de riscos proporcionais na análise de sobrevivência. Caso o modelo seja apropriado, não deverá haver tendências nos tempos analisados.")),
                                                                       tags$br(),
                                                                       tags$li(a(span(icon("chart-bar"), style = "color:black")),strong(span("Gráfico de barras: ", style = "color:black")),em("Representa o tempo médio de duração em dias das atividades internas da instituição segundo a fase de cada processo.")),
                                                                       tags$br(),
                                                                       tags$li(a(span(icon("project-diagram"), style = "color:black")),strong(span("Gráfico de linhas: ", style = "color:black")),em("Descreve o tempo médio de duração das classes processuais segundo sua classificação em Aberto ou Encerrado."))),  style = "font-size: 16px"))
                                                          )
                                                        ),
                                                        
                                                        fluidRow(
                                                          box(title = "Função de Sobrevivência", status = "primary", solidHeader = TRUE, plotOutput("histogramManual1")), #Gráfico 1
                                                          box(title = "Função de risco Acumulado", status = "warning", solidHeader = TRUE,plotOutput("histogramManual2")), #Gráfico 2
                                                          box(title = "Gráfico de colunas", status = "primary", solidHeader = TRUE,plotlyOutput("histogramManual3")), #Gráfico 3
                                                          box(title = "Gráfico de bolhas", status = "warning", solidHeader = TRUE,plotlyOutput("histogramManual4")), #Gráfico 4
                                                          box(title = "Gráfico de barras", status = "primary", solidHeader = TRUE,plotlyOutput("histogramManual5")), #Gráfico 5
                                                          box(title = "Gráfico de setores", status = "warning", solidHeader = TRUE,plotlyOutput("histogramManual12")),#Gráfico 12
                                                          box(title = "Resíduos de Cox-Snell", status = "primary", solidHeader = TRUE,plotOutput("histogramManual7")), #Gráfico 7
                                                          box(title = "Resíduos de Schoenfeld", status = "warning", solidHeader = TRUE,plotOutput("histogramManual8")), #Gráfico 8
                                                          box(title = "Gráfico de barras", status = "primary", solidHeader = TRUE,plotlyOutput("histogramManual9")), #Gráfico 9
                                                          box(title = "Gráfico de linhas", status = "warning", solidHeader = TRUE,plotlyOutput("histogramManual11")) #Gráfico 11
                                                          
                                                        )
                                                ),
                                                
                                                tabItem(tabName = "requisitos", # Adicionando as caixas de texto na aba Requisitos
                                                        
                                                        fluidRow(
                                                          tabBox(
                                                            side = "right", height = "260px", width = '90%',
                                                            title = "Análise sobre os pré-requisitos do modelo",
                                                            # The id lets us use input$tabset1 on the server to find the current ta
                                                            tabPanel(" ",
                                                              tags$div(tags$ul(
                                                              
                                                              tags$li(a(span(icon(""), style = "color:black")),strong(span("Resíduos de Cox-Snell: ", style = "color:black")),em("O gráfico de resíduos de Cox-Snell serve para verificar a qualidade de ajuste do modelo de Cox. Se este estiver bem ajustado, os (ei’s) podem ser olhados como uma amostra censurada de uma distribuição exponencial padrão e, então, o gráfico de H(ei) versus (ei) deveria ser aproximadamente uma reta.")),
                                                              tags$br(),
                                                              tags$li(a(span(icon(""), style = "color:black")),strong(span("Resíduos de Schoenfeld: ", style = "color:black")),em("•	O resíduo de Schoenfeld é a diferença entre os valores observados de covariáveis de um indivíduo com tempo de ocorrência do evento (ti) e os valores esperados em (ti) dado o grupo de risco R(ti). Haverá tantos vetores de resíduos quanto covariáveis ajustadas no modelo, e que estes são definidos somente nos tempos de ocorrência do evento."),p(em("Considerando-se o gráfico de resíduos padronizados de Schoenfeld contra o tempo é possível verificar a existência ou não de proporcionalidade, ou seja, se as suposições de riscos proporcionais forem satisfeitas não deverá existir nenhuma tendência sistemática no gráfico (Ho: p=0).")))
                                                              )), height = "350px", width = '100%',  style = "font-size: 16px")
                                                          )),
                                                        
                                                        fluidRow(
                                                          box(width = 6,
                                                              title = "Resíduos de Cox-Snell",
                                                              status = "primary",
                                                              solidHeader = TRUE,
                                                              collapsible = FALSE,
                                                              height = '600px',
                                                              plotOutput("histogram9",height = '540px')), #Gráfico 1
                                                         
                                                           box(width = 6,
                                                              title = "Resíduos de Schoenfeld",
                                                              status = "warning",
                                                              solidHeader = TRUE,
                                                              collapsible = FALSE,
                                                              height = '600px',
                                                              plotOutput("histogram10",height = '540px'))) #Gráfico 2
                                                ),
                                                
                                                # Eu ocultei a saída da visualização dos processos por tempo decorrido
                                                
                                                #tabItem(
                                                #  tabName = "visual",
                                                #  fluidRow(
                                                #    dataTableOutput("table1")
                                                #  )
                                                #),
                                                
                                                tabItem(
                                                  tabName = "visual2",
                                                  fluidRow(
                                                    dataTableOutput("tablevisual")
                                                  )
                                                ),
                                                
                                                tabItem(
                                                  tabName = "entrada",
                                                  fluidRow(
                                                    dataTableOutput("table2")
                                                  )
                                                ),
                                                tabItem(
                                                  tabName = "atualizacao",
                                                  fluidRow(
                                                    dataTableOutput("table3")
                                                  )
                                                ),
                                                
                                                tabItem(
                                                  tabName = "prazos",
                                                  fluidRow(
                                                    dataTableOutput("table4")
                                                  )
                                                ),
                                                
                                                tabItem(
                                                  tabName = "AtuaPra",
                                                  fluidRow(
                                                    dataTableOutput("table5")
                                                  )
                                                )
                                              )
                       )
)


ui <- dashboardPage(header,sidebar, body)
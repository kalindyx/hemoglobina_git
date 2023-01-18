library(shiny)
library(ggplot2)
library(shinydashboard)
library(hrbrthemes)
library(shinyWidgets)
library(DT)
dados <- readRDS("data/dados.rds")

# Define UI
ui <- navbarPage("Hemoglobina",
                 tabPanel("Home",align_items = "center",
                          tabName = "home",
                          icon = icon("house-user"),
                          column(3,tags$img(src = "imagemUA.png", height = 70, width = 200),),
                          headerPanel(" "),
                          column(7,tags$a(img(src = "MEM.png", height = 100, width = 250), 
                                                  href = "https://www.ua.pt/pt/estatisticamedica/mestrado_em_estatistica_medica"),
                                 p(a("Homepage",href = "https://www.ua.pt/pt/estatisticamedica/mestrado_em_estatistica_medica")),),
                          column(1,tags$a(img(src = "Shiny.png", height = 150, width = 490), 
                                          href = "https://shiny.rstudio.com/")),
                          headerPanel(" "),
                          column(1),
                          column(1),
                          mainPanel(
                              h2("Introdução ao pacote", a(strong(span("Shiny", style="color:#104e8b")), href = "https://shiny.rstudio.com/"),"do R: aplicações web para a visualização de dados", align="center"), br(),
                              p(h5("Sala 11.2.21",align="center")), p(h5("Departamento de Matemática",align="center")),br(),br(), p(h3(span("Gabriela Nogueira e Matilde Jóia", style = "color:#5cacee"),align="center")),
                              p(h4(strong("Sob orientação do Prof. Pedro Sá Couto"),align="center")), p(h5(em("No âmbito da Unidade Curricular de Seminários de Estatística Médica"),align="center")),
                              br(),br(),br(), p(h4("18 janeiro 2023",align="center"))
                            ),
                          setBackgroundColor(
                            color = "aliceblue",
                            gradient = c("linear", "radial"),
                            direction = c("bottom", "top", "right", "left"),
                            shinydashboard = F
                          )
                 ),
                 tabPanel("Widgets",
                          fluidRow(
                            box(
                              title = "Botão de Submissão", width = 4, solidHeader = TRUE, status = "success",
                              tags$head(tags$script(src = "message-handler.js")),
                              actionButton("button1", "Clica aqui."),
                              helpText("Clicar para realizar ação.")
                            ),
                            box(
                              title = "Checkbox única", width = 4, solidHeader = TRUE, status = "primary",
                              checkboxInput("checkbox", label = "Opção 1", value = TRUE),
                              helpText("Selecionar para alterar o valor lógico para TRUE."),
                              br(),
                              checkboxGroupInput("checkGroup", label = h3("Checkbox múltipla"), 
                                                 choices = list("Opção 1"=1,"Opção 2"=2, "Opção 3"=3), selected=1),
                              helpText("Podemos escolher apenas uma ou várias opções ao mesmo tempo.")
                            ),
                            box(
                              title = "Datas", width = 4, solidHeader = TRUE, status = "primary",
                              dateInput("date", label = h3("Data"), value = NULL),
                              helpText("Selecionar data."),
                              dateRangeInput("dates", label = h3("Intervalo de tempo")),
                              helpText("Selecionar intervalo.")
                            )
                          ),
                          fluidRow(
                            box(
                              title = "Inputs numéricos, de texto e ficheiros", width = 4, solidHeader = TRUE, status = "primary",
                              numericInput("num", label = h3("Input Numérico"), value = 1),
                              helpText("Inserir valor numérico."),
                              
                              textInput("text", label = h3("Input de texto"), value = "Introduzir texto aqui."),
                              
                              fileInput("file", label = h3("Upload de ficheiro"))
                            ),
                            box(
                              title = "Botões de radio e caixas de seleção", width = 4, solidHeader = TRUE, status = "primary",
                              radioButtons("radio", label = h3("Botões de rádio"), choices = list("Opção 1" = 1, "Opção 2" = 2, "Opção 3" = 3), selected = 1),
                              selectInput("select", label = h3("Caixa de seleção"), choices = list("Opção 1" = 1, "Opção 2" = 2, "Opção 3" = 3), selected = 1)
                            ),
                            box(
                              title = "Sliders", width = 4, solidHeader = TRUE, status = "primary",
                              sliderInput("slider1", label = h3("Slider"), min = 0, max = 100, value = 50),
                              helpText("Definir valor."),
                              br(),
                              sliderInput("slider2", label = h3("Slider - Intervalo"), min = 0, max = 100, value = c(40, 60)),
                              helpText("Definir intervalo de valores.")
                            )
                          )
                          ),
                 tabPanel("DataTable",
                          h3("Hemoglobina em atletas australianos"),
                          helpText("Selecionar variáveis para ordenar as colunas. Usar a barra de pesquisa para filtrar os dados."),
                          DT::dataTableOutput("tabela1")
                 ),
                 navbarMenu("Boxplots",
                            tabPanel("Estáticos", 
                                     sidebarLayout(
                                       sidebarPanel("Concentração de Hemoglobina",
                                                    radioButtons("plot_type", "", c("por modalidade"="m", "por modalidade em cada sexo"="ms"))),
                                       mainPanel(plotOutput("plot1")))),
                            tabPanel("Interativos",
                                     tabsetPanel(id = "inTabset",
                                                 tabPanel("Feminino", 
                                                          selectInput("Fem", h5("Seleciona os desportos que pretendes visualizar:"), choices = list("Netball"="Netball", "Gym" = "Gym","BBall" = "BBall", "Tennis" = "Tennis", "Rowing" = "Rowing", "T400m" = "T400m", "Swim" = "Swim", "TSprnt"= "TSprnt", "Field" = "Field"), multiple = TRUE),
                                                          mainPanel(plotOutput("plot2"))),
                                                 tabPanel("Masculino",
                                                          selectInput("Mas", h5("Seleciona os desportos que pretendes visualizar:"), choices = list("BBall" = "BBall", "Tennis" = "Tennis", "Rowing" = "Rowing", "T400m" = "T400m", "Swim" = "Swim", "TSprnt"= "TSprnt", "Field" = "Field", "WPolo" = "WPolo"), multiple = TRUE),
                                                          mainPanel(plotOutput("plot3"))),
                                                 tabPanel("Ambos",
                                                          selectInput("Amb", h5("Seleciona os desportos que pretendes visualizar:"), choices = list("Netball"="Netball", "Gym" = "Gym","BBall" = "BBall", "Tennis" = "Tennis", "Rowing" = "Rowing", "T400m" = "T400m", "Swim" = "Swim", "TSprnt"= "TSprnt", "Field" = "Field", "WPolo" = "WPolo"), multiple = TRUE),
                                                          mainPanel(plotOutput("plot4")))))),
                 tabPanel("Barplot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("sexo", h5("Definir sexo(s) a visualizar:"),choices = list ("Masculino" = "M", "Feminino" = "F"), multiple = TRUE),),
                            mainPanel(textOutput("selected_sexo1"),
                                      plotOutput("plot5"),))),
                 )
                                     


server<-function(input, output, session) {
  observeEvent(input$button1, {
    session$sendCustomMessage(type = 'testmessage', message = 'Ação realizada com sucesso.')
  }) 
  output$tabela1 <- DT::renderDataTable({
    dados
  })
  output$plot1 <- renderPlot({
    if (input$plot_type == "m") {
      ggplot(dados, aes(x=reorder(Sport,HGB,median), y=HGB, fill=Sport)) +
        geom_boxplot() +
        theme(legend.position="none", plot.margin= unit(c(0.5,1,0.5,0.5), "cm")) +
        scale_fill_brewer(palette="Spectral") +
        ggtitle("Concentração de Hemoglobina por Modalidade") +
        xlab("Modalidade")} 
    else if (input$plot_type == "ms") {
      ggplot(dados, aes(x=reorder(Sport,HGB,median), y=HGB, fill=Sex)) + 
        geom_boxplot(width = 0.75, alpha=0.65) +
        facet_wrap(~Sex) +
        theme(legend.position="none", axis.text.x = element_text(angle=40), 
              plot.margin= unit(c(0.5,1,0.5,0.5), "cm")) +
        scale_fill_brewer(palette="Set1", labels = c("Feminino","Masculino"),name="Sexo") +
        ggtitle("Concentração de Hemoglobina por Modalidade em cada Sexo") +
        xlab("Modalidade")
        }
  })
  output$plot2 <- renderPlot({
    ggplot(dados[dados$Sport%in%input$Fem & dados$Sex=="F",], aes(x=reorder(Sport,HGB,median), y=HGB, fill=Sex)) + 
      geom_boxplot(width = 0.75, alpha=0.65, fill="#E41A1C") +
      theme(legend.position="none", axis.text.x = element_text(angle=40), 
            plot.margin= unit(c(0.5,1,0.5,0.5), "cm")) +
      scale_fill_brewer(labels = c("Feminino","Masculino"),name="Sexo") +
      ggtitle("Concentração de Hemoglobina por Modalidade no Sexo Feminino") +
      xlab("Modalidade")
  })
  output$plot3 <- renderPlot({
    ggplot(dados[dados$Sport%in%input$Mas & dados$Sex=="M",], aes(x=reorder(Sport,HGB,median), y=HGB, fill=Sex)) + 
      geom_boxplot(width = 0.75, alpha=0.90, fill= "#377EB8") +
      theme(legend.position="none", axis.text.x = element_text(angle=40), 
            plot.margin= unit(c(0.5,1,0.5,0.5), "cm")) +
      scale_fill_brewer(labels = c("Feminino","Masculino"),name="Sexo") +
      ggtitle("Concentração de Hemoglobina por Modalidade no Sexo Masculino") +
      xlab("Modalidade")
  })
  output$plot4 <- renderPlot({
    ggplot(dados[dados$Sport%in%input$Amb,], aes(x=reorder(Sport,HGB,median), y=HGB, fill=Sex)) + 
      geom_boxplot(width = 0.75, alpha=0.65) +
      theme(legend.position="none", axis.text.x = element_text(angle=40), 
            plot.margin= unit(c(0.5,1,0.5,0.5), "cm")) +
      scale_fill_brewer(palette="Set1", labels = c("Feminino","Masculino"),name="Sexo") +
      ggtitle("Concentração de Hemoglobina por Modalidade em cada Sexo") +
      xlab("Modalidade")
  })
  output$selected_sexo1 <- renderText({ 
    paste("Foi selecionado o sexo", input$sexo)
  })
  output$plot5 <- renderPlot({
    ggplot(dados[dados$Sex%in%input$sexo,], aes(x=Sport, fill=Sex)) +
      geom_bar(position = position_dodge(), width = 0.75, alpha=0.75) +
      ggtitle("Sexo por Desporto") +
      xlab("Desporto")+
      ylab("Nº de indivíduos") +
      scale_fill_brewer(palette="Set1",name="Sexo")
  })
  output$selected_sexo2 <- renderText({ 
    paste("Foi selecionado o sexo", input$sexo)
  })
  output$selected_desporto <- renderText({ 
    paste("Foi selecionada a modalidade", input$desporto)
  })
  
  output$selected_variavel <- renderText({ 
    paste("Foi selecionada a variável", input$variavel)
  })
  output$plot6 <- renderPlot({
    ggplot(dados[dados$Sex%in%input$sexo & dados$Sport%in%input$desporto,], aes(x=input$variavel)) +
      geom_density(fill="#69B3A2", color="#69B3A2", alpha=0.8) +
      ggtitle("Curva de Distribuição") +
      theme_ipsum() +
      xlim(10,20)
  })
  

}

shinyApp(ui = ui, server = server)

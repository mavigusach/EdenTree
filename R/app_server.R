#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#Delimitar o tamanho suportado de arquivo para upload
options(shiny.maxRequestSize=30*1024^2)
app_server <- function(input, output, session) {
  # Your application server logic
#Variaveis globais
  cx <- reactiveValues(atv=NULL,custo=NULL,receita=NULL)
  values_inputs <- reactiveValues(file_input_arquivo_state = NULL)
  dados_tabela <- reactiveValues(producao=NULL,metricas_economicas=NULL)
  range <- reactiveValues(horizonte=1)
  df_atividades_ha<-data.frame()
  atv_valor_volume_ano<-c()
  #Tabela IMA ICA - RESET
  dados_tb_prod <- data.frame()
  #Refresh para Home
  observeEvent(input$refresh_home,{
    shinyjs::refresh()
  })
  output$render_pagina <- renderUI({
  fluidRow(column(12,

    uiOutput("dados_basicos"),
    uiOutput("rotacao_silvicultural"),
    uiOutput("caixa"),
    uiOutput("rotacao_economica")


    ))
  })
observeEvent(input$download_relatorio,{
  shinyscreenshot::screenshot(
    selector = "body",
    filename = "Relatorio_EdenTree",
    id = "",
    scale = 1,
    timer = 0,
    download = TRUE,
    server_dir = NULL
  )
})
  observeEvent(input$btn_caixa_atv,{

    if(isTruthy(input$caixa_atividade) && isTruthy(input$caixa_valor)){
      if(input$caixa_unidade=="R$/ha"){
      #df_atividades_ha <<- rbind(df_atividades_ha,data.frame(Atividade = input$caixa_atividade , Valor = input$caixa_valor, Unidade = input$caixa_unidade, Etiqueta = ))

      #View(df_atividades_ha)
      for(cont in 0:range$horizonte){
        cx[[paste("atv_",cont,sep = "")]]<<- append(cx[[paste("atv_",cont,sep = "")]],paste(input$caixa_atividade," | ",input$caixa_unidade," | ", input$caixa_valor, sep = ""))
      }


      } else if(input$caixa_unidade=="R$/m³"){
      if(input$check_arquivo_rotacao_silvicultural==TRUE){
      for(cont in 1:range$horizonte){
        cont_2<-as.numeric(dados_tabela$producao[1,"Ano"])+cont-1
        atv_valor_volume_ano[cont] <- as.numeric(dados_tabela$producao$Volume[cont])*as.numeric(input$caixa_valor)

        #print(atv_valor_volume_ano)
        dados_atv_m3<-paste(input$caixa_atividade," | ","R$/ha"," | ", atv_valor_volume_ano[cont], sep = "")

        cx[[paste("atv_",cont_2,sep = "")]]<<- append(cx[[paste("atv_",cont_2,sep = "")]],dados_atv_m3)
      }
      } else {
        for(cont in 1:range$horizonte){
          atv_valor_volume_ano[cont] <- as.numeric(dados_tabela$producao$Volume[cont])*as.numeric(input$caixa_valor)

          print(atv_valor_volume_ano)
          dados_atv_m3<-paste(input$caixa_atividade," | ","R$/ha"," | ", atv_valor_volume_ano[cont], sep = "")

          cx[[paste("atv_",cont,sep = "")]]<<- append(cx[[paste("atv_",cont,sep = "")]],dados_atv_m3)
        }
      }
      }
    }
    if(input$caixa_custo_terra==TRUE && isTruthy(input$dados_basicos_valor_terra) && isTruthy(input$dados_basicos_taxa)){
      for(cont in 1:range$horizonte){
        cx[[paste("custo_independente_volume_",cont,sep = "")]]<<- paste("Custo da Terra"," | ","R$/ha"," | ",as.numeric(input$dados_basicos_valor_terra)*(as.numeric(input$dados_basicos_taxa)/100))
      }
    }

  })


  #Reset na BucketList - Caixa
  observeEvent(input$btn_reset_caixa,{
    for(cont in 0:range$horizonte){

      cx[[paste("atv_",cont,sep = "")]]<<-NULL
      cx[[paste("custo_independente_volume_",cont,sep = "")]]<<-NULL
      cx$receita<<-NULL
      cx$custo_dependente_volume<<-NULL

    }
    bs4Dash::updateBox("rotacao_economica",action = "remove")
    dados_tabela$metricas_economicas<<- data.frame("Ano" = 0, "VPL" = 0,"VPL_Infinito" = 0,"RLPE" = 0, "VET" = 0)
  })
  #Observe Horizonte de Planejamento
  observeEvent(input$dados_basicos_horizonte,{

    if(isTruthy(input$dados_basicos_horizonte)==T){
    range$horizonte<<-as.numeric(input$dados_basicos_horizonte)
    } else {
      range$horizonte<<-1
    }
  #Reset na BucketList - Caixa
    for(cont in 0:range$horizonte){

      cx[[paste("atv_",cont,sep = "")]]<<-NULL
      cx[[paste("custo_independente_volume_",cont,sep = "")]]<<-NULL
      cx$receita<<-NULL
      cx$custo_dependente_volume<<-NULL

    }

#Tabela IMA - ICA - Producao
    if(input$check_arquivo_rotacao_silvicultural==FALSE){
      bs4Dash::updateBox("caixa",action = "remove")
      bs4Dash::updateBox("rotacao_economica",action = "remove")

      output$resultado_rotacao_silvicultural<- renderUI({})
      output$resultado_rotacao_economica<- renderUI({})

      dados_tb_prod <<- data.frame(Ano = 1, Volume = 0) %>% dplyr::mutate(IMA = 0) %>% dplyr::mutate(ICA =  0)

    dados_tabela$producao <<- dados_tb_prod
    output$tabela_volume <- DT::renderDT(round(dados_tabela$producao,4), selection = 'none', editable = list(
      target = 'cell', disable = list(columns = c(0, 2, 3,4))), rownames = FALSE,extensions = "Buttons",colnames= c("Ano","Volume (m³/ha)","IMA (m³/ha/ano)","ICA (m³/ha/ano)"),
      options = list(paging = TRUE,    ## paginate the output
                     scrollX = TRUE,   ## enable scrolling on X axis
                     dom = 'lBrtip',
                     lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                     scrollY = TRUE,
                     buttons =list('copy', 'print', list(extend = 'collection',buttons = list(list(extend = 'csv', filename = "Planilha_EdenTree_Rotacao_Silvicultural_CSV",title = NULL),list(extend = 'excel', filename = "Planilha_EdenTree_Rotacao_Silvicultural_EXCEL",title = NULL)),text = 'Download'))

      )
      )
  }

  })
  observeEvent(input$tabela_volume_cell_edit, {
    info = input$tabela_volume_cell_edit
    #str(info)
    i = info$row
    j = info$col + 1  ## offset by 1
    x = info$value

    if(is.numeric(x) == TRUE && x>0 | is.double(x) == TRUE && x>0){
      dados_tb_prod <<- DT::editData(dados_tb_prod, input$tabela_volume_cell_edit, 'tabela_volume', rownames = FALSE)
      if(i==1 && j==2){
        dados_tb_prod[i,j+2]<<-as.numeric(dados_tb_prod[i,j])
        dados_tb_prod[i,j+1]<<-as.numeric(dados_tb_prod[i,j])/as.numeric(dados_tb_prod[i,j-1])###  IMA = Volume/Idade
      } else {
        dados_tb_prod[i,j-1]<<- i
        dados_tb_prod[i,j+2]<<-as.numeric(dados_tb_prod[i,j])-as.numeric(dados_tb_prod[i-1,2])
        dados_tb_prod[i,j+1]<<-as.numeric(dados_tb_prod[i,j])/as.numeric(dados_tb_prod[i,j-1])###  IMA = Volume/Idade
      }
      if(nrow(dados_tb_prod)<as.numeric(range$horizonte) && j==2){
        dados_tb_prod <- rbind(dados_tb_prod, c(i+1,0,0,0))

      }
      dados_tabela$producao <<- dados_tb_prod
    } else {
      output$tabela_volume <- DT::renderDT(round(dados_tabela$producao,4), selection = 'none', editable = list(
        target = 'cell', disable = list(columns = c(0, 2, 3,4))), rownames = FALSE,extensions = "Buttons",colnames= c("Ano","Volume (m³/ha)","IMA (m³/ha/ano)","ICA (m³/ha/ano)"),
        options = list(paging = TRUE,    ## paginate the output
                       scrollX = TRUE,   ## enable scrolling on X axis
                       scrollY = TRUE,
                       lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                       dom = 'lBrtip',
                       buttons = list('copy', 'print', list( extend = 'collection',buttons = list(list(extend = 'csv', filename = "Planilha_EdenTree_Rotacao_Silvicultural_CSV",title = NULL),list(extend = 'excel', filename = "Planilha_EdenTree_Rotacao_Silvicultural_EXCEL",title = NULL)),text = 'Download'))

        )
      )
    }
    if(any(dados_tabela$producao["Volume"]==0, na.rm = TRUE)==FALSE){
      bs4Dash::updateBox("caixa",action = "restore")
    } else {
      bs4Dash::updateBox("caixa",action = "remove")
      bs4Dash::updateBox("rotacao_economica",action = "remove")
    }

  })

#Upload Arquivo Tabela Volume IMA ICA Producao
  observeEvent(input$arquivo_rs, {
    values_inputs$file_input_arquivo_state<- 'uploaded'
    ext_file_input <- toString(tools::file_ext(input$arquivo_rs$name))
    if(ext_file_input!= "xlsx"){
      values_inputs$file_input_arquivo_state<- "reset"
      bs4Dash::updateBox("caixa",action = "remove")
    } else {
      analisa_colunas<-readxl::read_excel(input$arquivo_rs$datapath, col_names = TRUE)
      #View(analisa_colunas)
      verifica_colunas_nomes<-colnames(analisa_colunas)
      #View(verifica_colunas_nomes)
      if(length(verifica_colunas_nomes)<2){
        values_inputs$file_input_arquivo_state<- "reset"
        bs4Dash::updateBox("caixa",action = "remove")

      } else if (any("Ano"==verifica_colunas_nomes)==FALSE | any("Volume"==verifica_colunas_nomes)==FALSE){
        values_inputs$file_input_arquivo_state<- "reset"
        bs4Dash::updateBox("caixa",action = "remove")

      } else if(is.numeric(analisa_colunas$Ano)==TRUE && sum(is.na(analisa_colunas$Ano))==0 && is.numeric(analisa_colunas$Volume)==TRUE && sum(is.na(analisa_colunas$Volume))==0 && nrow(analisa_colunas)>1){
        #Verifica se Ano/Volume  possui valor numerico e se possui celulas vazias
        valida_ano<-c()
        valida_ano[1]<-TRUE
        for (cont in 2:nrow(analisa_colunas)) {
          if(as.numeric(analisa_colunas[cont,"Ano"])!=(as.numeric(analisa_colunas[cont-1,"Ano"]))+1){
            valida_ano[cont]<-FALSE
          } else {
            valida_ano[cont]<-TRUE
          }
        }

        if(any(valida_ano==FALSE)==TRUE){
          values_inputs$file_input_arquivo_state<- "reset"
          bs4Dash::updateBox("caixa",action = "remove")
        } else{
          IMA<-c()
          ICA<-c()
          for (cont in 1:nrow(analisa_colunas)) {
            IMA[cont]<- as.numeric(analisa_colunas[cont,"Volume"])/as.numeric(analisa_colunas[cont,"Ano"])
            if(cont==1){
              ICA[cont]<-as.numeric(analisa_colunas[cont,"Volume"])
            } else {
              ICA[cont]<-as.numeric(analisa_colunas[cont,"Volume"])-as.numeric(analisa_colunas[cont-1,"Volume"])
            }
          }
          n_anos_arquivo<-as.numeric(analisa_colunas[nrow(analisa_colunas),"Ano"])
          updateNumericInput(inputId = "dados_basicos_horizonte", value = n_anos_arquivo)
          analisa_colunas<-cbind(analisa_colunas,IMA)
          analisa_colunas<-cbind(analisa_colunas,ICA)
          dados_tabela$producao<-analisa_colunas
          bs4Dash::updateBox("caixa",action = "restore")
        }
      } else{
        values_inputs$file_input_arquivo_state<- "reset"
        bs4Dash::updateBox("caixa",action = "remove")
      }
    }
  })
  file_input_arquivo <- reactive({
    if (is.null(values_inputs$file_input_arquivo_state)) {
      return(NULL)
    } else if (values_inputs$file_input_arquivo_state == 'uploaded') {
      return("preenchido")
    } else if (values_inputs$file_input_arquivo_state == 'reset') {
      return(NULL)
    }
  })
  status_icon_file_input <- reactive({
    if (is.null(values_inputs$file_input_arquivo_state)==TRUE) {
      return(div(icon("file-arrow-up",lib = "font-awesome"),"Status:",icon("spinner",lib = "font-awesome")," Aguardando Arquivo!"))
    } else if (values_inputs$file_input_arquivo_state=='uploaded') {
      return(div(icon("file-arrow-up",lib = "font-awesome"),"Status:",icon("check",lib = "font-awesome")," Arquivo Válido!"))
    } else if (values_inputs$file_input_arquivo_state=='reset') {
      return(div(icon("file-arrow-up",lib = "font-awesome"),"Status:",icon("xmark",lib = "font-awesome")," Arquivo Inválido!",p(icon("triangle-exclamation",lib = "font-awesome"),"Verifique a extensão do arquivo e sua estrutura!")))
    }
  })
  output$icon_file_input_status <- renderUI({
    status_icon_file_input()
  })
observeEvent(input$check_arquivo_rotacao_silvicultural,{
  output$resultado_rotacao_silvicultural<- renderUI({})
  output$resultado_rotacao_economica<- renderUI({})
  bs4Dash::updateBox("rotacao_economica",action = "remove")
  bs4Dash::updateBox("caixa",action = "remove")
  if(input$check_arquivo_rotacao_silvicultural==TRUE){
    shinyjs::disable(id="dados_basicos_horizonte")
    dados_tb_prod <<- data.frame(Ano = 1, Volume = 0) %>% dplyr::mutate(IMA = 0) %>% dplyr::mutate(ICA =  0)
    dados_tabela$producao<<-dados_tb_prod
    output$tabela_volume <- DT::renderDT(round(dados_tabela$producao,4), selection = 'none', editable = FALSE, rownames = FALSE,extensions = "Buttons",colnames= c("Ano","Volume (m³/ha)","IMA (m³/ha/ano)","ICA (m³/ha/ano)"),
      options = list(paging = TRUE,    ## paginate the output
                     scrollX = TRUE,   ## enable scrolling on X axis
                     scrollY = TRUE,
                     lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                     dom = 'lBrtip',
                     buttons =list('copy', 'print',list( extend = 'collection',buttons = list(list(extend = 'csv', filename = "Planilha_EdenTree_Rotacao_Silvicultural_CSV",title = NULL),list(extend = 'excel', filename = "Planilha_EdenTree_Rotacao_Silvicultural_EXCEL",title = NULL)),text = 'Download'))
      )
    )

    output$arquivo_rotacao_silvicultural<- renderUI({
    div(
      bs4Dash::box(
        width = 12,
        title = "Arquivo",
        status = "primary",
        collapsible = F,
        collapsed = F,
      fileInput(inputId = "arquivo_rs", label = uiOutput("icon_file_input_status"),buttonLabel = "Selecione...", placeholder = "Nenhum arquivo selecionado", accept = "xlsx")
      )
      )
    })
    output$modelo_arquivo_rotacao_silvicultural<- renderUI({
      div(
      bs4Dash::box(
        width = 12,
        title =  downloadLink("baixar_planilha_modelo", label = div(icon("cloud-arrow-down",lib = "font-awesome"),"Modelo")),
        status = "primary",
        collapsible = F,
        collapsed = F,
        DT::datatable(data.frame(
          Ano  = c("4", "5"),
          Volume  = c("132,78", "206,38")
        ),colnames= c("Ano","Volume (m³/ha)"),
        options = list(paging = FALSE,    ## paginate the output
                       scrollY = TRUE,   ## enable scrolling on Y axis
                       scrollX = TRUE,   ## enable scrolling on X axis
                       dom = 't',
                       columnDefs = list(list(targets = '_all', className = 'dt-center'))
        ),
        extensions = 'Buttons',
        selection = 'single', ## enable selection of a single row
        rownames = FALSE              ## don't show row numbers/names
        )

      ))
    })
  } else {
    updateNumericInput(inputId = "dados_basicos_horizonte", value = 1)
    shinyjs::enable(id="dados_basicos_horizonte")
    output$arquivo_rotacao_silvicultural<- renderUI({})
    output$modelo_arquivo_rotacao_silvicultural<- renderUI({})
    values_inputs$file_input_arquivo_state<<- NULL

    dados_tb_prod <<- data.frame(Ano = 1, Volume = 0) %>% dplyr::mutate(IMA = 0) %>% dplyr::mutate(ICA =  0)
    dados_tabela$producao<<-dados_tb_prod
    output$tabela_volume <- DT::renderDT(round(dados_tabela$producao,4), selection = 'none',colnames= c("Ano","Volume (m³/ha)","IMA (m³/ha/ano)","ICA (m³/ha/ano)") ,editable = list(
      target = 'cell', disable = list(columns = c(0, 2, 3,4))), rownames = FALSE,extensions = "Buttons",
      options = list(paging = TRUE,    ## paginate the output
                     scrollX = TRUE,   ## enable scrolling on X axis
                     scrollY = TRUE,   ## enable scrolling on Y axis
                     lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                     dom = 'lBrtip',
                     buttons=list('copy', 'print', list(extend = 'collection',buttons = list(list(extend = 'csv', filename = "Planilha_EdenTree_Rotacao_Silvicultural_CSV",title = NULL),list(extend = 'excel', filename = "Planilha_EdenTree_Rotacao_Silvicultural_EXCEL",title = NULL)),text = 'Download'))

      )
    )
  }
})
#Download Planilha Modelo
output$baixar_planilha_modelo<- downloadHandler(
  filename = function() {
    paste("Planilha_Modelo_EdenTree.xlsx", sep="")
  },
  content = function(file) {
    file.copy(file.path("inst/app/www/PLANILHA_MODELO/Planilha_Modelo_EdenTree.xlsx"),file)
  }
)
#Ano da Rotacao Silvicultural
  observeEvent(dados_tabela$producao,{
    resultado_ima_rs<-0
    for (cont in 1:nrow(dados_tabela$producao)) {
     if(as.numeric(dados_tabela$producao[cont,"IMA"])==max(as.numeric(dados_tabela$producao[,"IMA"])) && resultado_ima_rs<as.numeric(dados_tabela$producao[cont,"IMA"])){
       resultado_ima_rs<-as.numeric(dados_tabela$producao[cont,"IMA"])
       resultado_ano_rs<-paste("Ano ",as.numeric(dados_tabela$producao[cont,"Ano"]),sep = "")
     }
    }
    if(all(dados_tabela$producao["Volume"]==0, na.rm = TRUE)==FALSE){
    output$resultado_rotacao_silvicultural<- renderUI({
      bs4Dash::blockQuote(paste("Rotação Silvicultural: ",toString(resultado_ano_rs),sep = ""),color = "primary")
    }
    )
    }

  })
#Ano da Rotacao Economica
  observeEvent(dados_tabela$metricas_economicas,{
    resultado_vpl_re<- 0
    resultado_ano_re<-paste("Ano ",as.numeric(0),sep = "")
    verifica_vpl_negativo<-FALSE



    for (cont in 1:nrow(dados_tabela$metricas_economicas)) {
      if(as.numeric(dados_tabela$metricas_economicas[cont,"VPL"])==max(as.numeric(dados_tabela$metricas_economicas[,"VPL"])) && all(as.numeric(dados_tabela$metricas_economicas[,"VPL"])==as.numeric(dados_tabela$metricas_economicas[cont,"VPL"]))==FALSE){
        resultado_vpl_re<-as.numeric(dados_tabela$metricas_economicas[cont,"VPL"])
        resultado_ano_re<-paste("Ano ",as.numeric(dados_tabela$metricas_economicas[cont,"Ano"]),sep = "")
      }
    }
    if(all(dados_tabela$metricas_economicas["VPL"]==0, na.rm = TRUE)==FALSE){
      output$resultado_rotacao_economica<- renderUI({
        bs4Dash::blockQuote(paste("Rotação Econômica: ",toString(resultado_ano_re),sep = ""),color = "primary")
      }
      )
    }

  })
  #Grafico Producao - Ima - ICA
  output$grafico_prod_ima_ica<- renderPlot({
    ggplot2::ggplot(data = dados_tabela$producao)+
      {if(all(dados_tabela$producao["Volume"]==0, na.rm = TRUE)==FALSE && nrow(dados_tabela$producao)>1)ggplot2::geom_line(mapping = ggplot2::aes(x= Ano, y = Volume,color="Produção (m³/ha)"),na.rm=TRUE)}+
      {if(all(dados_tabela$producao["Volume"]==0, na.rm = TRUE)==FALSE && nrow(dados_tabela$producao)>1)ggplot2::geom_line(mapping = ggplot2::aes(x= Ano, y = IMA,color="IMA (m³/ha/ano)"),na.rm=TRUE)}+
      {if(all(dados_tabela$producao["Volume"]==0, na.rm = TRUE)==FALSE && nrow(dados_tabela$producao)>1)ggplot2::geom_line(mapping = ggplot2::aes(x= Ano, y = ICA,color="ICA (m³/ha/ano)"),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "Legenda:", values = c("Produção (m³/ha)" = "darkred","IMA (m³/ha/ano)" = "darkgreen", "ICA (m³/ha/ano)" = "darkblue"))+
      ggplot2::xlab("Ano")+
      ggplot2::ylab("Métricas")+
      ggplot2::theme_classic()+
      ggplot2::theme(legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=15),axis.text = ggplot2::element_text(size=12))
    })
  output$grafico_rotacao_economica<- renderPlot({
    ggplot2::ggplot(data = dados_tabela$metricas_economicas)+
      {if(all(dados_tabela$metricas_economicas["VPL"]==0, na.rm = TRUE)==FALSE && nrow(dados_tabela$metricas_economicas)>1)ggplot2::geom_line(mapping = ggplot2::aes(x= Ano, y = VPL,color="VPL (R$/ha)"),na.rm=TRUE)}+
      {if(all(dados_tabela$metricas_economicas["VPL_Infinito"]==0, na.rm = TRUE)==FALSE  && nrow(dados_tabela$metricas_economicas)>1)ggplot2::geom_line(mapping = ggplot2::aes(x= Ano, y = VPL_Infinito,color="VPL Infinito (R$/ha)"),na.rm=TRUE)}+
      {if(all(dados_tabela$metricas_economicas["RLPE"]==0, na.rm = TRUE)==FALSE  && nrow(dados_tabela$metricas_economicas)>1)ggplot2::geom_line(mapping = ggplot2::aes(x= Ano, y = RLPE,color="RLPE (R$/ha)"),na.rm=TRUE)}+
      {if(all(dados_tabela$metricas_economicas["VET"]==0, na.rm = TRUE)==FALSE  && nrow(dados_tabela$metricas_economicas)>1)ggplot2::geom_line(mapping = ggplot2::aes(x= Ano, y = VET,color="VET (R$/ha)"),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "Legenda:", values = c("VPL (R$/ha)" = "orange","VPL Infinito (R$/ha)" = "purple", "RLPE (R$/ha)" = "magenta" , "VET (R$/ha)" = "darkcyan"))+
      ggplot2::xlab("Ano")+
      ggplot2::ylab("Métricas")+
      ggplot2::theme_classic()+
      ggplot2::theme(legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=15),axis.text = ggplot2::element_text(size=12))
  })

  output$dados_basicos <- renderUI({
    fluidRow(
    bs4Dash::box(id="dados_basicos",
                 title = "Dados",
                 collapsible = F,
                 collapsed = F,
                 status = "primary",
                 width = 12,
                 dropdownMenu = bs4Dash::boxDropdown(
                   div(style="text-align:center;",
                       bs4Dash::boxDropdownItem(actionLink(inputId ="download_relatorio",label = "Download Relatório"), id = "dropdownItem_download_relatorio"),
                   ),icon = icon("cloud-arrow-down",lib = "font-awesome")),
                 fluidRow(
                   column(width = 6,
                          textInput(inputId = "dados_basicos_nome_projeto", label = "Nome do Projeto:")
                   ),
                   column(width = 6,
                         numericInput(inputId = "dados_basicos_horizonte", label = "Horizonte de Planejamento (anos):", value = "1", min = "1")
                   )
                 ),
                 fluidRow(
                   column(width = 6,
                          numericInput(inputId = "dados_basicos_taxa", label = "Taxa (%):", value = "0", min = "0", max = "100")
                   ),
                   column(width = 6,
                          numericInput(inputId = "dados_basicos_valor_terra", label = "Valor da Terra (R$/ha):", value = "0", min = "0")
                   )
                 )
    )
    )
  })
   output$rotacao_silvicultural <- renderUI({
    fluidRow(

      bs4Dash::box(id="rotacao_silvicultural",
                   title = "Rotação Silvicultural",
                   footer = div(
                     fluidRow(column(width = 12,
                                     checkboxInput(inputId = "check_arquivo_rotacao_silvicultural",label = "Adicionar Arquivo")
                     )),
                     fluidRow(
                     column(width = 6,uiOutput("modelo_arquivo_rotacao_silvicultural")),
                     column(width = 6, uiOutput("arquivo_rotacao_silvicultural"))
                                )),
                   collapsible = F,
                   collapsed = F,
                   status = "primary",
                   width = 12,
                   fluidRow(
                     column(width = 6,
                     DT::DTOutput("tabela_volume")
                     ),
                     column(width = 6,
                            fluidRow(column(width = 12,
                            plotOutput("grafico_prod_ima_ica")
                            )),
                            fluidRow(column(width = 12,
                                            uiOutput("resultado_rotacao_silvicultural")
                            ))
                            )
                     )

      )
    )
  })
  output$caixa <- renderUI({
    fluidRow(
      bs4Dash::box(id="caixa",
                   title = "Caixa",
                   collapsible = F,
                   collapsed = F,
                   status = "primary",
                   width = 12,
                   fluidRow(
                     column(width = 4,
                            textInput(inputId = "caixa_atividade", label = "Atividade:")
                            ),
                     column(width = 4,
                            numericInput(inputId = "caixa_valor", label = "Valor:", value = 0, min = 0)
                     ),
                     column(width = 4,
                            radioButtons(inputId = "caixa_unidade", label = "Unidade:", choices = c("R$/ha","R$/m³"))
                     )

                   ),
                   fluidRow(
                     column(width = 12,
                     hr(),
                            checkboxInput(inputId = "caixa_custo_terra", label = "Adicionar Custo da Terra"),
                     hr()
                     )
                     ),
                   fluidRow(
                     column(width = 6,
                            actionButton(width = "100%", inputId = "btn_caixa_atv", label = "Criar Atividade!"),
                            ),
                     column(width = 6,
                            actionButton(width = "100%", inputId = "btn_reset_caixa", label = "Reset"),
                     )
                   ),

                   lapply(0:range$horizonte,function(cont) {
                   fluidRow(
                     column(width = 12,
                     sortable::bucket_list(
                       header = paste("Ano ",cont,sep = ""),
                       group_name = paste("bucket_list_group_",cont,sep = ""),
                       orientation = "horizontal",
                       sortable::add_rank_list(
                         text = "Atividades",
                         labels = cx[[paste("atv_",cont,sep = "")]],
                         input_id = paste("caixa_atividades_",cont,sep = "")
                       ),

                         sortable::add_rank_list(
                           text = "Custos Independentes do Volume",
                           labels = cx[[paste("custo_independente_volume_",cont,sep = "")]],
                           input_id = paste("caixa_custos_independentes_volume_",cont,sep = "")
                         ),
                       sortable::add_rank_list(
                         text = "Custos Dependentes do Volume",
                         labels = cx$custo_dependente_volume,
                         input_id = paste("caixa_custos_dependentes_volume_",cont,sep = "")
                       ),

                          sortable::add_rank_list(
                                 text = "Receitas",
                                 labels = cx$receita,
                                 input_id = paste("caixa_receitas_",cont,sep = "")
                               )


                     )
                     )
                   )
                   }),
                   fluidRow(
                     column(width = 12,
                            actionButton(width = "100%", inputId = "btn_caixa_gerar", label = "Gerar!"),
                     )
                   )
                   )
    )
  })
  output$rotacao_economica<- renderUI({
    fluidRow(
      bs4Dash::box(id="rotacao_economica",
                   title = "Rotação Econômica",
                   collapsible = F,
                   collapsed = F,
                   status = "primary",
                   width = 12,
                   fluidRow(
                     column(width = 6,
                            DT::DTOutput("tabela_rotacao_economica")
                            ),
                     column(width = 6,
                            fluidRow(column(width = 12,
                            plotOutput("grafico_rotacao_economica"))),
                            fluidRow(column(width = 12,
                            uiOutput("resultado_rotacao_economica")))
                            )
                   )
                   )
    )
  })
#Calculos
  VPL<-c()
  RLPE<-c()
  VPLinf<<-c()
  VET<<-c()
  VPL_CUSTOS_INDEPENDENTES_VOLUME<-0
  VPL_CUSTOS_DEPENDENTES_VOLUME<-0
observeEvent(input$btn_caixa_gerar,{
  if(isTruthy(input$dados_basicos_taxa)==TRUE && as.numeric(input$dados_basicos_taxa)){
  bs4Dash::updateBox("rotacao_economica",action = "restore")
  } else {
    bs4Dash::updateBox("rotacao_economica",action = "remove")
    showNotification("Preencha corretamente a Taxa!",type = "error")
  }
  #ORGANIZANDO CUSTOS E RECEITAS POR ANO EM TABELA
   MT1_CUSTOS_INDEPENDENTES <- matrix(nrow=0, ncol=4)
   colnames(MT1_CUSTOS_INDEPENDENTES) <- c("Atividade","Unidade","Valor","Ano")

  for (cont in 0:range$horizonte) {

   if(isTruthy(input[[paste("caixa_custos_independentes_volume_",cont,sep = "")]])){
    corta_dados_caixa<-matrix((unlist(strsplit(input[[paste("caixa_custos_independentes_volume_",cont,sep = "")]]," | ", fixed = T))), ncol=3, byrow=T)
    corta_dados_caixa <- cbind(corta_dados_caixa, cont)
    colnames(corta_dados_caixa) <- c("Atividade","Unidade","Valor","Ano")

    MT1_CUSTOS_INDEPENDENTES<- rbind(MT1_CUSTOS_INDEPENDENTES, corta_dados_caixa)

   }

  }
   MT1_RECEITAS <- matrix(nrow=0, ncol=4)
   colnames(MT1_RECEITAS) <- c("Atividade","Unidade","Valor","Ano")

   for (cont in 0:range$horizonte) {

     if(isTruthy(input[[paste("caixa_receitas_",cont,sep = "")]])){
       corta_dados_caixa<-matrix((unlist(strsplit(input[[paste("caixa_receitas_",cont,sep = "")]]," | ", fixed = T))), ncol=3, byrow=T)
       corta_dados_caixa <- cbind(corta_dados_caixa, cont)
       colnames(corta_dados_caixa) <- c("Atividade","Unidade","Valor","Ano")

       #View(corta_dados_caixa)
       MT1_RECEITAS<- rbind(MT1_RECEITAS, corta_dados_caixa)

     }

   }
   MT1_CUSTOS_DEPENDENTES <- matrix(nrow=0, ncol=4)
   colnames(MT1_CUSTOS_DEPENDENTES) <- c("Atividade","Unidade","Valor","Ano")

   for (cont in 0:range$horizonte) {

     if(isTruthy(input[[paste("caixa_custos_dependentes_volume_",cont,sep = "")]])){
       corta_dados_caixa<-matrix((unlist(strsplit(input[[paste("caixa_custos_dependentes_volume_",cont,sep = "")]]," | ", fixed = T))), ncol=3, byrow=T)
       corta_dados_caixa <- cbind(corta_dados_caixa, cont)
       colnames(corta_dados_caixa) <- c("Atividade","Unidade","Valor","Ano")

       MT1_CUSTOS_DEPENDENTES<- rbind(MT1_CUSTOS_DEPENDENTES, corta_dados_caixa)

     }

   }

  #MONTANDO O FLUXO DE CAIXA
  #ANO | RECEITA | CUSTO | TOTAL
  MFLUXO <- matrix(nrow=(range$horizonte+1), ncol=5,0)


#  #DISTRIBUINDO OS CUSTOS/RECEITAS ANUAIS - SUBTOTAIS
  #Validando preenchimento dos buckets
  valida_bucket_custos_independentes_volume<-c()
  valida_bucket_receitas<-c()
  valida_bucket_custos_dependentes_volume<-c()
  for (cont in 0:range$horizonte) {
  if(isTruthy(input[[paste("caixa_custos_independentes_volume_",cont,sep = "")]])){
    valida_bucket_custos_independentes_volume[cont+1]<-TRUE
  } else {
    valida_bucket_custos_independentes_volume[cont+1]<-FALSE
  }
  if(isTruthy(input[[paste("caixa_receitas_",cont,sep = "")]])){
      valida_bucket_receitas[cont+1]<-TRUE
    } else {
      valida_bucket_receitas[cont+1]<-FALSE
    }
    if(isTruthy(input[[paste("caixa_custos_dependentes_volume_",cont,sep = "")]])){
      valida_bucket_custos_dependentes_volume[cont+1]<-TRUE
    } else {
      valida_bucket_custos_dependentes_volume[cont+1]<-FALSE
    }
  }
  if(any(valida_bucket_custos_independentes_volume==TRUE)==TRUE){
    valida_bucket_custos_independentes_volume_final<- TRUE
  } else {
    valida_bucket_custos_independentes_volume_final<- FALSE
  }
  if(any(valida_bucket_receitas==TRUE)==TRUE){
    valida_bucket_receitas_final<- TRUE
  } else {
    valida_bucket_receitas_final<- FALSE
  }
  if(any(valida_bucket_custos_dependentes_volume==TRUE)==TRUE){
    valida_bucket_custos_dependentes_volume_final<- TRUE
  } else {
    valida_bucket_custos_dependentes_volume_final<- FALSE
  }
for(cont in 1:nrow(MT1_CUSTOS_INDEPENDENTES)){
  if(valida_bucket_custos_independentes_volume_final==TRUE){
      MFLUXO[(as.numeric(MT1_CUSTOS_INDEPENDENTES[cont,4])+1),3] <- MFLUXO[(as.numeric(MT1_CUSTOS_INDEPENDENTES[cont,4])+1),3] + as.numeric(MT1_CUSTOS_INDEPENDENTES[cont,3]) #custos independentes do volume
  }
}
for(cont in 1:nrow(MT1_RECEITAS)){
  if(valida_bucket_receitas_final==TRUE){
      MFLUXO[(as.numeric(MT1_RECEITAS[cont,4])+1),2] <- MFLUXO[(as.numeric(MT1_RECEITAS[cont,4])+1),2] + as.numeric(MT1_RECEITAS[cont,3]) #receitas
  }
}
  for(cont in 1:nrow(MT1_CUSTOS_DEPENDENTES)){
    if(valida_bucket_custos_dependentes_volume_final==TRUE){
      MFLUXO[(as.numeric(MT1_CUSTOS_DEPENDENTES[cont,4])+1),4] <- MFLUXO[(as.numeric(MT1_CUSTOS_DEPENDENTES[cont,4])+1),4] + as.numeric(MT1_CUSTOS_DEPENDENTES[cont,3]) #custos dependentes do volume
    }
  }
#CALCULO DO TOTAL
MFLUXO[,5]=MFLUXO[,2]-MFLUXO[,3]

#CALCULANDO O VPL / RLPE
VPL<<-c()
RLPE<<-c()
VPLinf<<-c()
VET<<-c()
if(isTruthy(input$dados_basicos_taxa) && isTruthy(input$dados_basicos_valor_terra) && isTruthy(input$dados_basicos_horizonte)){
  for(cont in 1:nrow(MFLUXO)){

   #VPL
  if(cont==1){
    VPL[cont]<<-as.numeric(0)
    VPL[cont]<<- as.numeric(VPL[cont])+as.numeric((MFLUXO[cont,5]/(1+(as.numeric(input$dados_basicos_taxa)/100))^(cont-1)))
    VPL_CUSTOS_INDEPENDENTES_VOLUME<<- (MFLUXO[cont,3])*-1
  } else{

      VPL_CUSTOS_INDEPENDENTES_VOLUME<<- VPL_CUSTOS_INDEPENDENTES_VOLUME + ((as.numeric((MFLUXO[cont,3]/(1+(as.numeric(input$dados_basicos_taxa)/100))^(cont-1))))*-1)
      VPL_CUSTOS_DEPENDENTES_VOLUME<<- ((as.numeric((MFLUXO[cont,4]/(1+(as.numeric(input$dados_basicos_taxa)/100))^(cont-1))))*-1)
      VPL[cont]<<- VPL_CUSTOS_INDEPENDENTES_VOLUME +as.numeric((MFLUXO[cont,2]/(1+(as.numeric(input$dados_basicos_taxa)/100))^(cont-1)))+VPL_CUSTOS_DEPENDENTES_VOLUME
  }
    #RLPE | VPL infinito | VET
    if(cont==1){
      RLPE[cont] <<- NA
      VPLinf[cont] <<- NA
      VET[cont] <<- NA


    } else {
      RLPE[cont] <<- (VPL[cont]*(input$dados_basicos_taxa/100)*(1+(input$dados_basicos_taxa/100))^(cont-1))/(((1+(input$dados_basicos_taxa/100))^(cont-1))-1)
      VPLinf[cont] <<- RLPE[cont]/(as.numeric(input$dados_basicos_taxa)/100)
      VET[cont] <<- VPLinf[cont] + as.numeric(input$dados_basicos_valor_terra)
    }



  }

dados_tabela$metricas_economicas<<- data.frame("Ano" = 0:range$horizonte, "VPL" = VPL,"VPL_Infinito" = VPLinf,"RLPE" = RLPE, "VET" = VET)
output$tabela_rotacao_economica <- DT::renderDT(round(dados_tabela$metricas_economicas,4), selection = 'none', colnames= c("Ano","VPL (R$/ha)","VPL Infinito (R$/ha)","RLPE (R$/ha)","VET (R$/ha)"),editable = list(
  target = 'cell', disable = list(columns = c(0, 2, 3,4))), rownames = FALSE,extensions = "Buttons",
  options = list(paging = TRUE,    ## paginate the output
                 scrollX = TRUE,   ## enable scrolling on X axis
                 scrollY = TRUE,
                 lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                 dom = 'lBrtip',
                 buttons =list('copy', 'print', list( extend = 'collection',buttons = list(list(extend = 'csv', filename = "Planilha_EdenTree_Rotacao_Economica_CSV",title = NULL),list(extend = 'excel', filename = "Planilha_EdenTree_Rotacao_Economica_EXCEL",title = NULL)),text = 'Download'))

  )
)
}
if(all(dados_tabela$metricas_economicas[,"VPL"]==0)){
  output$resultado_rotacao_economica<- renderUI({})
}

})


  #Rodape
output$render_Rodape_Tutorial <- renderUI({
  p(style = "font-weight: bold;margin: 0px;",actionLink(inputId = "modal_tutorial",label = "•Tutorial"))
})
observeEvent(input$modal_tutorial,{
  showModal(modalDialog(
    size = "xl",
    title = div(icon("person-chalkboard",lib = "font-awesome"),"Tutorial") ,
    easyClose = TRUE,
    footer = modalButton("Fechar"),
    bs4Dash::bs4Accordion(
      id="Accordion_Tutorial",
      bs4Dash::bs4AccordionItem(
        title = "1.Dados",
        status = "primary",
        solidHeader = T,
        collapsed = F,
        bs4Dash::blockQuote(p("1.1.Insira o Nome do Projeto:"),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/13.png"))
        ,color = "primary"),
        bs4Dash::blockQuote(p("1.2.Insira o Horizonte de Planejamento em anos:"),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/15.png")),
        p("•Em caso de upload de arquivo, será definido automaticamente.")
        ,color = "primary"),
        bs4Dash::blockQuote(p("1.3.Insira a Taxa de Juros Anual em Porcentagem:"),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/14.png"))
        ,color = "primary"),
        bs4Dash::blockQuote(p("1.4.Insira o Valor da Terra em R$/ha:"),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/16.png"))
        ,color = "primary"),
        bs4Dash::blockQuote(p("1.5.Ao final, clique no ícone ",icon("cloud-arrow-down",lib = "font-awesome")," para baixar o relatório completo.")
        ,color = "primary")
      ),
      bs4Dash::bs4AccordionItem(
        title = "2.Rotação Silvicultural",
        status = "primary",
        solidHeader = T,
        bs4Dash::blockQuote(p("2.1.Insira os Dados Anuais de Volume em m³/ha:"),
        bs4Dash::blockQuote(p('2.1.1.Manualmente, dando um duplo clique sobre cada célula da planilha na coluna "Volume (m³/ha)":'),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/2.png"))
        ,color = "primary"),
        bs4Dash::blockQuote(p('2.1.2.Automaticamente, ative a caixa "Adicionar Arquivo" e insira um arquivo do MS Excel (.xlsx) estruturado conforme o Modelo disponível:'),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/4.png")),
        p('•Clique em "',icon("cloud-arrow-down",lib = "font-awesome"),'Modelo" para baixar a planilha disponibilizada como Modelo.'),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/5.png")),
        p('•Verifique o status do arquivo, não serão aceitos arquivos em que a coluna "Anos" não siga uma sequência crescente ou que possuam apenas um ano.')
        ,color = "primary")
        ,color = "primary"),
        bs4Dash::blockQuote(p("2.2.Confira os resultados gerados:"),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/11.png")),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/12.png"))
        ,color = "primary")
      ),
      bs4Dash::bs4AccordionItem(
        title = "3.Caixa",
        status = "primary",
        solidHeader = T,
        bs4Dash::blockQuote(p("3.1.Insira o Nome da Atividade:"),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 100px;",src = "www/TUTORIAL/17.png"))
        ,color = "primary"),
        bs4Dash::blockQuote(p("3.2.Insira o Valor da Atividade:"),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 100px;",src = "www/TUTORIAL/18.png"))
        ,color = "primary"),
        bs4Dash::blockQuote(p("3.3.Insira o Unidade do Valor da Atividade:"),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 100px;",src = "www/TUTORIAL/19.png"))
        ,color = "primary"),
        bs4Dash::blockQuote(p('3.4.Ative a caixa "Adicionar Custo da Terra" para adicioná-lo automaticamente aos "Custos Independentes do Volume":'),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 20px;",src = "www/TUTORIAL/7.png")),
        p('•Após adicionar o Custo da Terra, somente ative novamente a caixa "Adicionar Custo da Terra" para atualizá-lo.'),
        p('•Sempre que for necessário atualizar o "Custo da Terra", repita o passos a partir do 3.4.')
        ,color = "primary"),
        bs4Dash::blockQuote(p('3.5.Clique em "Criar Atividade!".'),
                            p('•Utilize "Reset" para apagar todos os dados do "Caixa" e recomeçar.')
        ,color = "primary"),
        bs4Dash::blockQuote(p('3.6.Monte o fluxo de caixa para cada ano, classificando as atividades realizadas através do método "arrasta e solta":'),
        div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/8.png")),
        p('•As atividades não realizadas no ano deverão ser mantidas no setor "Atividades", sendo este setor não contabilizado nos cálculos.')
        ,color = "primary"),
        bs4Dash::blockQuote(p('3.7.Clique em "Gerar!".')
        ,color = "primary")
      ),
      bs4Dash::bs4AccordionItem(
        title = "4.Rotação Econômica",
        status = "primary",
        solidHeader = T,
        bs4Dash::blockQuote(p("4.1.Confira os resultados gerados:"),
                            div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/9.png")),
                            div(align="center", img(class="responsive_img_tutorial_tabs",style="max-height: 500px;",src = "www/TUTORIAL/10.png"))
                            ,color = "primary")
      )
    )
  ))
})
output$render_Rodape_Sobre_Nos <- renderUI({
  p(style = "font-weight: bold;",actionLink(inputId = "modal_sobre_nos",label = "•Sobre Nós"))
})
observeEvent(input$modal_sobre_nos,{
  showModal(modalDialog(
    size = "xl",
    title = div(icon("address-card",lib = "font-awesome"),"Sobre Nós") ,
    easyClose = TRUE,
    footer = modalButton("Fechar"),
    div(align="center",
        bs4Dash::blockQuote(
          h1("Sobre a EdenTree"),
          div(
            fluidRow(
              p("Aplicação Web destinada à análise da viabilidade financeira de projetos florestais desenvolvida no Laboratório de Estudos e Projetos em Manejo Florestal (LEMAF) da Universidade Federal de Lavras (UFLA).")
            ),
          )
          , color= "primary"),
        bs4Dash::blockQuote(
          h1("Nossa Equipe"),
          div(
            fluidRow(
              #height de cada linha 300px para medio,grande
              #height de cada linha 315px para pequeno
              # HTML(
              #   '<iframe width="100%" height="350px" src="www/LINKEDIN/linkedin_users.html" frameborder="0" scrolling="yes"></iframe>'
              # )

              column(width = 4,
                     bs4Dash::blockQuote(style="text-align:left;",h5(HTML("<a href='https://www.linkedin.com/in/carolina-jarochinski/' target='_blank'>Dra. Carolina Souza Jarochinski e Silva</a>")),p("•E-mail: carolina.jsilva@ufla.br"),color = "primary")
              ),
              column(width = 4,
                     bs4Dash::blockQuote(style="text-align:left;",h5(HTML("<a href='https://www.linkedin.com/in/samuel-jos%C3%A9-silva-soares-da-rocha-652a49111/' target='_blank'>Dr. Samuel José Silva Soares da Rocha</a>")),p("•E-mail: samuel.rocha@ufla.br"),color = "primary")
              ),
              column(width = 4,
                     bs4Dash::blockQuote(style="text-align:left;",h5(HTML("<a href='https://www.linkedin.com/in/marcelo-vitor-chaves-888681284' target='_blank'>Marcelo Vitor Gualberto Santos Chaves</a>")),p("•E-mail: marcelo160102@gmail.com"),color = "primary")
              )
            ),
          )
          , color= "primary")
        )
  ))
})
  output$render_Rodape_Direitos_Ano <- renderText({
    paste("©", format(Sys.Date(), "%Y")," - Todos os direitos reservados.")
  })
  output$render_Rodape_Versao_Codename <- renderUI({
    actionLink(inputId = "modal_codename",label = "AppVersion:1.0 - Codename: openeyes")
  })
  observeEvent(input$modal_codename,{
    showModal(modalDialog(
      title = div(icon("code",lib = "font-awesome"),"openeyes") ,
      easyClose = TRUE,
      size="xl",
      footer = modalButton("Fechar"),
      bs4Dash::box(width = 12,
                   title = div(icon("eye",lib = "font-awesome"),"Gênesis 3:7-9"),
                   status = "primary",
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   HTML(
                     '<iframe width="100%" height="500" src="https://www.youtube.com/embed/Iymkk85ELxw?si=kF-fmBPe65FAlpqx" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'
                       )
      )
    ))
  })
}

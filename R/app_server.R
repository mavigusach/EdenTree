#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#Delimitar o tamanho suportado de arquivo para upload
options(shiny.maxRequestSize=30*1024^2)
app_server <- function(input, output, session) {
  #Banco de dados de moedas
  library(isocountry)
  #data(isocurrency)
  # Your application server logic
  #Variaveis globais
  cx <- reactiveValues(atv=NULL,custo=NULL,receita=NULL)
  values_inputs <- reactiveValues(file_input_arquivo_state = NULL)
  dados_tabela <- reactiveValues(producao=NULL,metricas_economicas=NULL,metricas_economicas_total=NULL)
  range <- reactiveValues(horizonte=1)
  df_atividades_ha<-data.frame()
  atv_valor_volume_ano<-c()
  area_automatica<-FALSE
  #Tabela IMA ICA - RESET
  dados_tb_prod <- data.frame()

#Idioma
idioma <- reactiveValues(
  
  #Secao Dados
  label_dados = "Data",
  moeda = "USD",
  area= "1",
  valor_da_terra = "Value of Land",
  #Secao Rotacao Silvicultural
  label_rotacao_silvicultural = "Optimal Biological Rotation Age",
  rs_ano = "",
  rs_volume = "Volume (m³/ha)",
  rs_volume_2 = "Volume",
  rs_IMA = "MAI (m³/ha/year)",
  rs_ICA = "CAI (m³/ha/year)",
  rs_producao = "Production (m³/ha)",
  rs_metricas = "Metrics",
  rs_adicionar_arquivo = "Add File",
  rs_label_modelo = "Example",
  rs_label_arquivo = "File",
  rs_label_status = "Status",
  rs_label_status_aguardando = "Waiting for File!",
  rs_label_status_valido = "Valid File!",
  rs_label_status_invalido = "Invalid File!",
  rs_label_status_invalido_aviso = "Check the file extension and its structure!",
  rs_arquivo_exemplo = "EN-USA",

  #Secao Caixa
  label_caixa = "Cash Flow",
  fc_add_custo_terra = "Add Cost of Land",
  fc_custo_terra = "Cost of Land",
  fc_ano = "Year",
  fc_atividades = "Activities",
  fc_custos_independentes = "Volume – Independent Costs",
  fc_custos_dependentes = "Volume – Dependent Costs",
  fc_receitas = "Revenues",
  fc_label_atividade = "Activity:",
  fc_label_valor = "Value:",
  fc_label_unidade = "Unit:",
  fc_label_unidade_1 = "USD/ha",
  fc_label_unidade_2 = "USD/m³",

  #Secao Rotacao Economica
  label_rotacao_economica = "Optimal Financial Rotation Age",
  re_metricas = "Metrics",
  re_ano = "",
  re_VPL = "NPV (USD/ha)",
  re_VPL_infinito = "Infinite NPV (USD/ha)",
  re_RLPE = "EAA (USD/ha)",
  re_VET = "LEV (USD/ha)",
  re_TIR = "IRR (%)",
  re_PAYBACK ="Dicounted Payback",
  re_VPL_total = "NPV (USD/ha)",
  re_VPL_infinito_total = "Infinite NPV (USD/ha)",
  re_RLPE_total = "EAA (USD/ha)",
  re_VET_total = "LEV (USD/ha)",
  re_TIR_total = "IRR (%)",
  re_PAYBACK_total ="Dicounted Payback",

  #Rodape
  label_idioma = "Language",
  label_sobre_nos = "About Us",
  label_tutorial = "Tutorial",
  label_direitos = "All rights reserved.",
  label_easteregg = "Genesis 3:7-9",

  #Sobre Nos
  label_sobre_eden_tree = "About EdenTree",
  sobre_eden_tree = "Web application designed to analyze the financial viability of forestry projects developed at the Laboratory of Studies and Projects in Forest Management (LEMAF) of the Federal University of Lavras (UFLA).",
  equipe = "Our Team",
  artigo ="Access our paper!",

  #Tutorial
  tutorial_title = "Tutorial",
  tutorial_1_Dados = "1. Data",
  tutorial_1_Dados_1_1 = "1.1. Enter the Project Name:",
  tutorial_1_Dados_1_2 = "1.2. Enter the Planning Horizon in years:",
  tutorial_1_Dados_1_2_note = "•In case of file upload, it will be set automatically.",
  tutorial_1_Dados_1_3 = "1.3. Enter the Annual Interest Rate in Percentage:",
  tutorial_1_Dados_1_4 = "1.4. Enter the Total Area in ha:",
  tutorial_1_Dados_1_5 = "1.5. Enter Value of Land for total area in ha:",
  tutorial_1_Dados_1_6 = "1.6. Once you have completed the analysis, click on the icon to download the report: ",
  tutorial_2_Rotacao_Silvicultural = "2. Optimal Biological Rotation Age",
  tutorial_2_Rotacao_Silvicultural_2_1 = "2.1. Enter the Annual Volume Data in m³/ha:",
  tutorial_2_Rotacao_Silvicultural_2_1_1 = '2.1.1. Manually, by double-clicking each cell in the "Volume (m³/ha)" column of the spreadsheet:',
  tutorial_2_Rotacao_Silvicultural_2_1_2 = '2.1.2. Automatically, activate the "Add File" box and insert an MS Excel (.xlsx) file structured according to the available Example:',
  tutorial_2_Rotacao_Silvicultural_2_1_2_note_1 = '•Click "Example" to download the spreadsheet provided as a Example.',
  tutorial_2_Rotacao_Silvicultural_2_1_2_note_2 = '•Check the file status, files where the "Years" column does not follow an increasing sequence or have only one year will not be accepted.',
  tutorial_2_Rotacao_Silvicultural_2_2 = "2.2. Check the generated results:",
  tutorial_3_Caixa = "3. Cash Flow",
  tutorial_3_Caixa_3_1 = "3.1. Enter the Activity Name:",
  tutorial_3_Caixa_3_2 = "3.2. Enter the Activity Value:",
  tutorial_3_Caixa_3_3 = "3.3. Enter the Activity Value Unit:",
  tutorial_3_Caixa_3_4 = '3.4. Activate the "Add Cost of Land" box to automatically add it to the "Volume Independent Costs":',
  tutorial_3_Caixa_3_4_1 = 'After adding the Land Cost, only activate the "Add Land Cost" box again to update it.',
  tutorial_3_Caixa_3_4_2 = 'Whenever it is necessary to update the "Land Cost", repeat the steps from 3.4.',
  tutorial_3_Caixa_3_5 = '3.5. Click "Create Activity!".',
  tutorial_3_Caixa_3_5_note_1 = '•Use "Reset" to clear all data from "Cash" and start over.',
  tutorial_3_Caixa_3_5_note_2 = '•Note that the values in X hectares have been converted to 1 hectare.',
  tutorial_3_Caixa_3_6 = '3.6. Build the cash flow for each year by classifying the activities performed using the "drag and drop" method:',
  tutorial_3_Caixa_3_6_note = '•Activities not performed in the year should be kept in the "Activities" section, which will not be accounted for in the calculations.',
  tutorial_3_Caixa_3_7 = '3.7. Click "Generate!".',
  tutorial_4_Rotacao_Economica = "4. Optimal Financial Rotation Age",
  tutorial_4_Rotacao_Economica_4_1 = "4.1. Check the generated results:",
  tutorial_img = "EN-USA",

  #Botoes
  btn_fechar = "Dismiss",
  btn_arquivo = "Browse...",
  btn_criar_atividade = "Create Activity!",
  btn_reiniciar = "Reset",
  btn_gerar = "Generate!",

  #Placeholder
  placeholder_arquivo = "No file selected",

  #Avisos
  aviso_taxa = "Fill in the Rate field correctly!",
  aviso_area = "Fill in the Area field correctly!",
  aviso_area_2 = "Area automatically set to 1 ha!",
  aviso_volume_invalido = "Invalid volume value!",
  
  label_aviso_responsabilidade = "Disclaimer",
  aviso_responsabilidade = "EdenTree was developed to support the financial analysis of forestry projects, offering users a tool based on silvicultural and economic indicators. However, all decisions made using the results are the sole responsibility of the user, and the developers assume no legal liability for any outcomes. The current version (1.0) is in the testing phase and has limitations due to its simplified structure, such as the absence of detailed reports, a limited set of financial indicators, and no advanced tools like sensitivity analysis. These factors may restrict its use in more complex scenarios. Future updates aim to address these gaps by adding features like report generation, new financial metrics (e.g., Benefit-Cost Ratio), sensitivity simulations, and modeling for different rotation cycles and log assortments—enhancing the tool’s robustness and alignment with the needs of the forestry sector"
                         )
#Moeda
observeEvent(input$dados_basicos_moeda,{
  updateSelectInput(session = session,inputId = "dados_basicos_moeda",selected = input$dados_basicos_moeda)
  idioma$moeda <- input$dados_basicos_moeda
  # idioma$fc_label_unidade_1 <- paste(idioma$moeda,"/",idioma$area,"ha",sep = "")
  # idioma$fc_label_unidade_2 <- paste(idioma$moeda,"/m³",sep = "")
  
  #PARA SOMENTE 1 HA
  if(!is.null(input$idioma)){
  if(input$idioma=="EN-USA"){
      idioma$re_VPL <- paste("NPV (",idioma$moeda,"/ha)",sep = "")
      idioma$re_VPL_infinito <- paste("Infinite NPV (",idioma$moeda,"/ha)",sep = "")
      idioma$re_RLPE <- paste("EAA (",idioma$moeda,"/ha)",sep = "")
      idioma$re_VET <- paste("LEV (",idioma$moeda,"/ha)",sep = "")
      idioma$re_TIR <- paste("IRR (","%)",sep = "")
      idioma$re_PAYBACK <- paste("Discounted Payback",sep = "")
  } else if(input$idioma=="PT-BR"){
      idioma$re_VPL <- paste("VPL (",idioma$moeda,"/ha)",sep = "")
      idioma$re_VPL_infinito <- paste("VPL Infinito (",idioma$moeda,"/ha)",sep = "")
      idioma$re_RLPE <- paste("AAE (",idioma$moeda,"/ha)",sep = "")
      idioma$re_VET <- paste("VET (",idioma$moeda,"/ha)",sep = "")
      idioma$re_TIR <- paste("TIR (","%)",sep = "")
      idioma$re_PAYBACK <- paste("Payback com desconto",sep = "")
  }
  }
  #PARA AREA TOTAL
  if(is.null(input$idioma)){
    if(idioma$area=='1'){
      idioma$re_VPL_total <- paste("NPV (",idioma$moeda,"/ha)",sep = "")
      idioma$re_VPL_infinito_total <- paste("Infinite NPV (",idioma$moeda,"/ha)",sep = "")
      idioma$re_RLPE_total <- paste("EAA (",idioma$moeda,"/ha)",sep = "")
      idioma$re_VET_total <- paste("LEV (",idioma$moeda,"/ha)",sep = "")
      idioma$re_TIR_total <- paste("IRR (","%)",sep = "")
      idioma$re_PAYBACK_total <- paste("Discounted Payback",sep = "")
    } else {
      idioma$re_VPL_total <- paste("NPV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
      idioma$re_VPL_infinito_total <- paste("Infinite NPV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
      idioma$re_RLPE_total <- paste("EAA (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
      idioma$re_VET_total <- paste("LEV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
      idioma$re_TIR_total <- paste("IRR (","%)",sep = "")
      idioma$re_PAYBACK_total <- paste("Discounted Payback",sep = "")
    }
  } else {
    if(input$idioma=="EN-USA"){
      if(idioma$area=='1'){
        idioma$re_VPL_total <- paste("NPV (",idioma$moeda,"/ha)",sep = "")
        idioma$re_VPL_infinito_total <- paste("Infinite NPV (",idioma$moeda,"/ha)",sep = "")
        idioma$re_RLPE_total <- paste("EAA (",idioma$moeda,"/ha)",sep = "")
        idioma$re_VET_total <- paste("LEV (",idioma$moeda,"/ha)",sep = "")
        idioma$re_TIR_total <- paste("IRR (","%)",sep = "")
        idioma$re_PAYBACK_total <- paste("Discounted Payback",sep = "")
      } else {
        idioma$re_VPL_total <- paste("NPV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_VPL_infinito_total <- paste("Infinite NPV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_RLPE_total <- paste("EAA (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_VET_total <- paste("LEV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_TIR_total <- paste("IRR (","%)",sep = "")
        idioma$re_PAYBACK_total <- paste("Discounted Payback",sep = "")
      }
    } else if(input$idioma=="PT-BR"){
      if(idioma$area=='1'){
        idioma$re_VPL_total <- paste("VPL (",idioma$moeda,"/ha)",sep = "")
        idioma$re_VPL_infinito_total <- paste("VPL Infinito (",idioma$moeda,"/ha)",sep = "")
        idioma$re_RLPE_total <- paste("AAE (",idioma$moeda,"/ha)",sep = "")
        idioma$re_VET_total <- paste("VET (",idioma$moeda,"/ha)",sep = "")
        idioma$re_TIR_total <- paste("TIR (","%)",sep = "")
        idioma$re_PAYBACK_total <- paste("Payback com desconto",sep = "")
      } else {
        idioma$re_VPL_total <- paste("VPL (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_VPL_infinito_total <- paste("VPL Infinito (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_RLPE_total <- paste("AAE (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_VET_total <- paste("VET (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_TIR_total <- paste("TIR (","%)",sep = "")
        idioma$re_PAYBACK_total <- paste("Payback com desconto",sep = "")
      }
    }
  }
})
observeEvent(input$idioma,{
  if(input$idioma=="EN-USA"){
    removeModal()
    bs4Dash::updateBox("caixa",action = "remove")
    bs4Dash::updateBox("rotacao_economica",action = "remove")
    dados_tb_prod <<- data.frame(Ano = 1, Volume = 0) %>% dplyr::mutate(IMA = 0) %>% dplyr::mutate(ICA =  0)
    dados_tabela$producao <<- dados_tb_prod
    #Secao Dados
    updateTextInput(session = session,label = "Project Name:", inputId = "dados_basicos_nome_projeto")
    updateTextInput(session = session,label = "Currency ($):", inputId = "dados_basicos_moeda")
    updateTextInput(session = session,label = "Planning Horizon (years):", inputId = "dados_basicos_horizonte")
    updateTextInput(session = session,label = "Rate (%):", inputId = "dados_basicos_taxa")
    updateTextInput(session = session,label = "Area (ha):", inputId = "dados_basicos_area")
    updateActionLink(session = session,inputId = "download_relatorio",label = "Download Report")
    idioma$label_dados<- "Data"
    idioma$valor_da_terra <- "Value of Land"
    #Secao Rotacao Silvicultural
    idioma$label_rotacao_silvicultural <- "Optimal Biological Rotation Age"
    idioma$rs_ano <- ""
    idioma$rs_volume <- "Volume (m³/ha)"
    idioma$rs_volume_2 <- "Volume"
    idioma$rs_IMA <- "MAI (m³/ha/year)"
    idioma$rs_ICA <- "CAI (m³/ha/year)"
    idioma$rs_producao <- "Production (m³/ha)"
    idioma$rs_metricas <- "Metrics"
    idioma$rs_adicionar_arquivo <- "Add File"
    idioma$rs_label_modelo <- "Example"
    idioma$rs_label_arquivo <- "File"
    idioma$rs_label_status <- "Status"
    idioma$rs_label_status_aguardando <- "Waiting for File!"
    idioma$rs_label_status_valido <- "Valid File!"
    idioma$rs_label_status_invalido <- "Invalid File!"
    idioma$rs_label_status_invalido_aviso <- "Check the file extension and its structure!"
    idioma$rs_arquivo_exemplo <- "EN-USA"

    #Secao Caixa
    idioma$label_caixa <- "Cash Flow"
    idioma$fc_add_custo_terra <- "Add Cost of Land"
    idioma$fc_custo_terra <- "Cost of Land"
    idioma$fc_ano <-"Year"
    idioma$fc_atividades <- "Activities"
    idioma$fc_custos_independentes <- "Volume – Independent Costs"
    idioma$fc_custos_dependentes <- "Volume – Dependent Costs"
    idioma$fc_receitas <- "Revenues"
    idioma$fc_label_atividade <- "Activity:"
    idioma$fc_label_valor <- "Value:"
    idioma$fc_label_unidade <- "Unit:"

    #Secao Rotacao Economica
    idioma$label_rotacao_economica <- "Optimal Financial Rotation Age"
    idioma$re_ano <- ""
    idioma$re_metricas <- "Metrics"
    idioma$re_VPL <- paste("NPV (",idioma$moeda,"/ha)",sep = "")
    idioma$re_VPL_infinito <- paste("Infinite NPV (",idioma$moeda,"/ha)",sep = "")
    idioma$re_RLPE <- paste("EAA (",idioma$moeda,"/ha)",sep = "")
    idioma$re_VET <- paste("LEV (",idioma$moeda,"/ha)",sep = "")

    #Rodape
    idioma$label_idioma<- "Language"
    updateSelectInput(session,"idioma",selected = input$idioma)
    idioma$label_sobre_nos <- "About Us"
    idioma$label_tutorial <- "Tutorial"
    idioma$label_direitos <- "All rights reserved."
    idioma$label_easteregg<- "Genesis 3:7-9"

    #Sobre Nos
    idioma$label_sobre_eden_tree <- "About EdenTree"
    idioma$sobre_eden_tree <- "Web application designed to analyze the financial viability of forestry projects developed at the Laboratory of Studies and Projects in Forest Management (LEMAF) of the Federal University of Lavras (UFLA)."
    idioma$equipe <- "Our Team"
    idioma$artigo <- "Access our paper!"
    #Tutorial
    idioma$tutorial_title <- "Tutorial"
    idioma$tutorial_1_Dados <- "1. Data"
    idioma$tutorial_1_Dados_1_1 <- "1.1. Enter the Project Name:"
    idioma$tutorial_1_Dados_1_2 <- "1.2. Enter the Planning Horizon in years:"
    idioma$tutorial_1_Dados_1_2_note <- "•In case of file upload, it will be set automatically."
    idioma$tutorial_1_Dados_1_3 <- "1.3. Enter the Annual Interest Rate in Percentage:"
    idioma$tutorial_1_Dados_1_4 <- "1.4. Enter the Total Area in ha:"
    idioma$tutorial_1_Dados_1_5 <- "1.5. Enter Value of Land for total area in ha:"
    idioma$tutorial_1_Dados_1_6 <- "1.6. Once you have completed the analysis, click on the icon to download the report: "
    idioma$tutorial_2_Rotacao_Silvicultural <- "2. Optimal Biological Rotation Age"
    idioma$tutorial_2_Rotacao_Silvicultural_2_1 <- "2.1. Enter the Annual Volume Data in m³/ha:"
    idioma$tutorial_2_Rotacao_Silvicultural_2_1_1 <- '2.1.1. Manually, by double-clicking each cell in the "Volume (m³/ha)" column of the spreadsheet:'
    idioma$tutorial_2_Rotacao_Silvicultural_2_1_2 <- '2.1.2. Automatically, activate the "Add File" box and insert an MS Excel (.xlsx) file structured according to the available Model:'
    idioma$tutorial_2_Rotacao_Silvicultural_2_1_2_note_1 <- '•Click "Example" to download the spreadsheet provided as a Example.'
    idioma$tutorial_2_Rotacao_Silvicultural_2_1_2_note_2 <- '•Check the file status, files where the "Years" column does not follow an increasing sequence or have only one year will not be accepted.'
    idioma$tutorial_2_Rotacao_Silvicultural_2_2 <- "2.2. Check the generated results:"
    idioma$tutorial_3_Caixa <- "3. Cash Flow"
    idioma$tutorial_3_Caixa_3_1 <- "3.1. Enter the Activity Name:"
    idioma$tutorial_3_Caixa_3_2 <- "3.2. Enter the Activity Value:"
    idioma$tutorial_3_Caixa_3_3 <- "3.3. Enter the Activity Value Unit:"
    idioma$tutorial_3_Caixa_3_4 <- '3.4. Activate the "Add Cost of Land" box to automatically add it to the "Volume Independent Costs":'
    idioma$tutorial_3_Caixa_3_4_1 <- 'After adding the Land Cost, only activate the "Add Land Cost" box again to update it.'
    idioma$tutorial_3_Caixa_3_4_2 <- 'Whenever it is necessary to update the "Land Cost", repeat the steps from 3.4.'
    idioma$tutorial_3_Caixa_3_5 <- '3.5. Click "Create Activity!".'
    idioma$tutorial_3_Caixa_3_5_note_1 <- '•Use "Reset" to clear all data from "Cash" and start over.'
    idioma$tutorial_3_Caixa_3_5_note_2 <- '•Note that the values in X hectares have been converted to 1 hectare.'
    idioma$tutorial_3_Caixa_3_6 <- '3.6. Build the cash flow for each year by classifying the activities performed using the "drag and drop" method:'
    idioma$tutorial_3_Caixa_3_6_note <- '•Activities not performed in the year should be kept in the "Activities" section, which will not be accounted for in the calculations.'
    idioma$tutorial_3_Caixa_3_7 <- '3.7. Click "Generate!".'
    idioma$tutorial_4_Rotacao_Economica <- "4. Optimal Financial Rotation Age"
    idioma$tutorial_4_Rotacao_Economica_4_1 <- "4.1. Check the generated results:"
    idioma$tutorial_img <- "EN-USA"

    #Botoes
    idioma$btn_fechar <- "Dismiss"
    idioma$btn_arquivo <- "Browse..."
    idioma$btn_criar_atividade <- "Create Activity!"
    idioma$btn_reiniciar <- "Reset"
    idioma$btn_gerar <- "Generate!"

    #Placeholder
    idioma$placeholder_arquivo <- "No file selected"

    #Avisos
    idioma$aviso_taxa <- "Fill in the Rate field correctly!"
    idioma$aviso_area <- "Fill in the Area field correctly!"
    idioma$aviso_area_2 <- "Area automatically set to 1 ha!"
    idioma$aviso_volume_invalido <- "Invalid volume value!"
    idioma$label_aviso_responsabilidade <- "Disclaimer"
    idioma$aviso_responsabilidade <- "EdenTree was developed to support the financial analysis of forestry projects, offering users a tool based on silvicultural and economic indicators. However, all decisions made using the results are the sole responsibility of the user, and the developers assume no legal liability for any outcomes. The current version (1.0) is in the testing phase and has limitations due to its simplified structure, such as the absence of detailed reports, a limited set of financial indicators, and no advanced tools like sensitivity analysis. These factors may restrict its use in more complex scenarios. Future updates aim to address these gaps by adding features like report generation, new financial metrics (e.g., Benefit-Cost Ratio), sensitivity simulations, and modeling for different rotation cycles and log assortments—enhancing the tool’s robustness and alignment with the needs of the forestry sector"

    

  } else if(input$idioma=="PT-BR") {
    removeModal()
    bs4Dash::updateBox("caixa",action = "remove")
    bs4Dash::updateBox("rotacao_economica",action = "remove")
    dados_tb_prod <<- data.frame(Ano = 1, Volume = 0) %>% dplyr::mutate(IMA = 0) %>% dplyr::mutate(ICA =  0)
    dados_tabela$producao <<- dados_tb_prod
    #Secao Dados
    updateTextInput(session = session,label = "Nome do Projeto:", inputId = "dados_basicos_nome_projeto")
    updateTextInput(session = session,label = "Moeda ($):", inputId = "dados_basicos_moeda")
    updateTextInput(session = session,label = "Horizonte de Planejamento (anos):", inputId = "dados_basicos_horizonte")
    updateTextInput(session = session,label = "Taxa (%):", inputId = "dados_basicos_taxa")
    updateTextInput(session = session,label = "Área (ha):", inputId = "dados_basicos_area")
    updateActionLink(session = session,inputId = "download_relatorio",label = "Baixar Relatório")
    idioma$label_dados<- "Dados"
    idioma$valor_da_terra <- "Valor da Terra"
    #Secao Rotacao Silvicultural
    idioma$label_rotacao_silvicultural <- "Rotação Silvicultural"
    idioma$rs_ano <- "Ano"
    idioma$rs_volume <- "Volume (m³/ha)"
    idioma$rs_volume_2 <- "Volume"
    idioma$rs_IMA <- "IMA (m³/ha/ano)"
    idioma$rs_ICA <- "ICA (m³/ha/ano)"
    idioma$rs_producao <- "Produção (m³/ha)"
    idioma$rs_metricas <- "Métricas"
    idioma$rs_adicionar_arquivo <- "Adicionar Arquivo"
    idioma$rs_label_modelo <- "Exemplo"
    idioma$rs_label_arquivo <- "Arquivo"
    idioma$rs_label_status <- "Status"
    idioma$rs_label_status_aguardando <- "Arguardando Arquivo!"
    idioma$rs_label_status_valido <- "Arquivo Válido!"
    idioma$rs_label_status_invalido <- "Arquivo Inválido!"
    idioma$rs_label_status_invalido_aviso <- "Verifique a extensão do arquivo e sua estrutura!"
    idioma$rs_arquivo_exemplo <- "PT-BR"

    #Secao Caixa
    idioma$label_caixa <- "Fluxo de Caixa"
    idioma$fc_add_custo_terra <- "Adicionar Custo da Terra"
    idioma$fc_custo_terra <- "Custo da Terra"
    idioma$fc_ano <-"Ano"
    idioma$fc_atividades <- "Atividades"
    idioma$fc_custos_independentes <- "Volume – Custos Independentes"
    idioma$fc_custos_dependentes <- "Volume – Custos Dependentes"
    idioma$fc_receitas <- "Receitas"
    idioma$fc_label_atividade <- "Atividade:"
    idioma$fc_label_valor <- "Valor:"
    idioma$fc_label_unidade <- "Unidade:"

    #Secao Rotacao Economica
    idioma$label_rotacao_economica <- "Rotação Econômica"
    idioma$re_ano <- "Ano"
    idioma$re_metricas <- "Métricas"
    idioma$re_VPL <- paste("VPL (",idioma$moeda,"/ha)",sep = "")
    idioma$re_VPL_infinito <- paste("VPL Infinito (",idioma$moeda,"/ha)",sep = "")
    idioma$re_RLPE <- paste("AAE (",idioma$moeda,"/ha)",sep = "")
    idioma$re_VET <- paste("VET (",idioma$moeda,"/ha)",sep = "")

    #Rodape
    idioma$label_idioma<- "Idioma"
    updateSelectInput(session,"idioma",selected = input$idioma)
    idioma$label_sobre_nos <- "Sobre Nós"
    idioma$label_tutorial <- "Tutorial"
    idioma$label_direitos <- "Todos os direitos reservados."
    idioma$label_easteregg<- "Gênesis 3:7-9"

    #Sobre Nos
    idioma$label_sobre_eden_tree <- "Sobre a EdenTree"
    idioma$sobre_eden_tree <- "Aplicação Web destinada à análise da viabilidade financeira de projetos florestais desenvolvida no Laboratório de Estudos e Projetos em Manejo Florestal (LEMAF) da Universidade Federal de Lavras (UFLA)."
    idioma$equipe <- "Nossa Equipe"
    idioma$artigo <- "Acesse nosso artigo!"
    #Tutorial
    idioma$tutorial_title <- "Tutorial"
    idioma$tutorial_1_Dados <- "1. Dados"
    idioma$tutorial_1_Dados_1_1 <- "1.1. Insira o Nome do Projeto:"
    idioma$tutorial_1_Dados_1_2 <- "1.2. Insira o Horizonte de Planejamento em anos:"
    idioma$tutorial_1_Dados_1_2_note <- "•Em caso de upload de arquivo, será definido automaticamente."
    idioma$tutorial_1_Dados_1_3 <- "1.3. Insira a Taxa de Juros Anual em Porcentagem:"
    idioma$tutorial_1_Dados_1_4 <- "1.4. Insira a Área total em ha:"
    idioma$tutorial_1_Dados_1_5 <- "1.5. Insira o Valor da Terra para a área total em ha:"
    idioma$tutorial_1_Dados_1_6 <- "1.6. Ao finalizar toda a análise, clique no ícone para baixar o relatório: "
    idioma$tutorial_2_Rotacao_Silvicultural <- "2. Rotação Silvicultural"
    idioma$tutorial_2_Rotacao_Silvicultural_2_1 <- "2.1. Insira os Dados Anuais de Volume em m³ para a Área total em ha e observe que o Volume m³/X ha será convertido para Volume m³/1 ha:"
    idioma$tutorial_2_Rotacao_Silvicultural_2_1_1 <- '2.1.1. Manualmente, dando um duplo clique sobre cada célula da planilha na coluna "Volume (m³/ha)":'
    idioma$tutorial_2_Rotacao_Silvicultural_2_1_2 <- '2.1.2. Automaticamente, ative a caixa "Adicionar Arquivo" e insira um arquivo do MS Excel (.xlsx) estruturado conforme o Exemplo disponível:'
    idioma$tutorial_2_Rotacao_Silvicultural_2_1_2_note_1 <- '•Clique em "Exemplo" para baixar a planilha disponibilizada como Exemplo.'
    idioma$tutorial_2_Rotacao_Silvicultural_2_1_2_note_2 <- '•Verifique o status do arquivo, não serão aceitos arquivos em que a coluna "Anos" não siga uma sequência crescente ou que possuam apenas um ano.'
    idioma$tutorial_2_Rotacao_Silvicultural_2_2 <- "2.2. Confira os resultados gerados:"
    idioma$tutorial_3_Caixa <- "3. Fluxo de Caixa"
    idioma$tutorial_3_Caixa_3_1 <- "3.1. Insira o Nome da Atividade:"
    idioma$tutorial_3_Caixa_3_2 <- "3.2. Insira o Valor da Atividade:"
    idioma$tutorial_3_Caixa_3_3 <- "3.3. Insira a Unidade do Valor da Atividade:"
    idioma$tutorial_3_Caixa_3_4 <- '3.4. Ative a caixa "Adicionar Custo da Terra" para adicioná-lo automaticamente aos "Custos Independentes do Volume":'
    idioma$tutorial_3_Caixa_3_4_1 <- 'Após adicionar o Custo da Terra, somente ative novamente a caixa "Adicionar Custo da Terra" para atualizá-lo.'
    idioma$tutorial_3_Caixa_3_4_2 <- 'Sempre que for necessário atualizar o "Custo da Terra", repita o passo a partir do 3.4.'
    idioma$tutorial_3_Caixa_3_5 <- '3.5. Clique em "Criar Atividade!".'
    idioma$tutorial_3_Caixa_3_5_note_1 <- '•Utilize "Reset" para apagar todos os dados do "Caixa" e recomeçar.'
    idioma$tutorial_3_Caixa_3_5_note_2 <- '•Observe que os valores em X hectares foram convertidos para 1 hectare.'
    idioma$tutorial_3_Caixa_3_6 <- '3.6. Monte o fluxo de caixa para cada ano, classificando as atividades realizadas através do método "arrasta e solta":'
    idioma$tutorial_3_Caixa_3_6_note <- '•As atividades não realizadas no ano deverão ser mantidas no setor "Atividades", sendo este setor não contabilizado nos cálculos.'
    idioma$tutorial_3_Caixa_3_7 <- '3.7. Clique em "Gerar!".'
    idioma$tutorial_4_Rotacao_Economica <- "4. Rotação Econômica"
    idioma$tutorial_4_Rotacao_Economica_4_1 <- "4.1. Confira os resultados gerados:"
    idioma$tutorial_img <- "PT-BR"


    #Botoes
    idioma$btn_fechar <- "Fechar"
    idioma$btn_arquivo <- "Selecione..."
    idioma$btn_criar_atividade <- "Criar Atividade!"
    idioma$btn_reiniciar <- "Reiniciar"
    idioma$btn_gerar <- "Gerar!"

    #Placeholder
    idioma$placeholder_arquivo <- "Nenhum arquivo selecionado"

    #Avisos
    idioma$aviso_taxa <- "Preencha corretamente o campo de Taxa!"
    idioma$aviso_area <- "Preencha corretamente o campo de Área!"
    idioma$aviso_area_2 <- "Área definida automaticamente como 1 ha!"
    idioma$aviso_volume_invalido <- "Valor de volume inválido!"
    
    idioma$label_aviso_responsabilidade <- "Aviso de Isenção de Responsabilidade"
    idioma$aviso_responsabilidade <- "A EdenTree foi desenvolvida para apoiar a análise financeira de projetos florestais, oferecendo aos usuários uma ferramenta baseada em indicadores silviculturais e econômicos. No entanto, todas as decisões tomadas com base nos resultados são de responsabilidade exclusiva do usuário, e os desenvolvedores não assumem qualquer responsabilidade legal por eventuais consequências. A versão atual (1.0) encontra-se em fase de testes e apresenta limitações decorrentes de sua estrutura simplificada, como a ausência de relatórios detalhados, número reduzido de indicadores financeiros e falta de ferramentas avançadas, como a análise de sensibilidade. Esses fatores podem restringir seu uso em cenários mais complexos. Atualizações futuras buscarão superar essas limitações por meio da inclusão de geração de relatórios, novos indicadores financeiros (como o Índice Benefício-Custo), simulações com diferentes cenários e modelagem para ciclos com uma ou duas rotações e diversos sortimentos de madeira — aumentando a robustez da ferramenta e sua aderência às demandas do setor florestal."
      }
})

  #Refresh para Home
  observeEvent(input$refresh_home,{
    shinyjs::refresh()
  })
  output$render_pagina <- renderUI({
    div(
    #Aviso responsabilidade
    showModal(modalDialog(
      size = "xl",
      title = div(icon("triangle-exclamation",lib = "font-awesome"),toString(idioma$label_aviso_responsabilidade)) ,
      easyClose = TRUE,
      footer = modalButton(toString(idioma$btn_fechar)),
      div(
          bs4Dash::blockQuote(
            div(
              fluidRow(
                p(idioma$aviso_responsabilidade)
              ),
            )
            , color= "primary")
      )
    )),
  fluidRow(column(12,

    uiOutput("dados_basicos"),
    uiOutput("rotacao_silvicultural"),
    uiOutput("caixa"),
    uiOutput("rotacao_economica")


    )))
  })
observeEvent(input$download_relatorio,{
  shinyscreenshot::screenshot(
    selector = "body",
    filename = "Report_EdenTree",
    id = "",
    scale = 1,
    timer = 0,
    download = TRUE,
    server_dir = NULL
  )
})
  observeEvent(input$btn_caixa_atv,{

    if(isTruthy(input$caixa_atividade) && isTruthy(input$caixa_valor)){

      if(input$caixa_unidade==paste(idioma$moeda,"/",idioma$area,"ha",sep = "")){
        
      #df_atividades_ha <<- rbind(df_atividades_ha,data.frame(Atividade = input$caixa_atividade , Valor = input$caixa_valor, Unidade = input$caixa_unidade, Etiqueta = ))

      #View(df_atividades_ha)
      for(cont in 0:range$horizonte){
        cx[[paste("atv_",cont,sep = "")]]<<- append(cx[[paste("atv_",cont,sep = "")]],paste(input$caixa_atividade," | ",paste(idioma$moeda,"/ha",sep = "")," | ", (as.numeric(input$caixa_valor)/input$dados_basicos_area), sep = ""))
      }


      } else if(input$caixa_unidade==paste(idioma$moeda,"/m³",sep = "")){
      if(input$check_arquivo_rotacao_silvicultural==TRUE){
      for(cont in 1:range$horizonte){
        cont_2<-as.numeric(dados_tabela$producao[1,"Ano"])+cont-1
        atv_valor_volume_ano[cont] <- as.numeric(dados_tabela$producao$Volume[cont])*as.numeric(input$caixa_valor)

        #print(atv_valor_volume_ano)
        dados_atv_m3<-paste(input$caixa_atividade," | ",paste(idioma$moeda,"/ha",sep = "")," | ", atv_valor_volume_ano[cont], sep = "")

        cx[[paste("atv_",cont_2,sep = "")]]<<- append(cx[[paste("atv_",cont_2,sep = "")]],dados_atv_m3)
      }
      } else {
        for(cont in 1:range$horizonte){
          atv_valor_volume_ano[cont] <- as.numeric(dados_tabela$producao$Volume[cont])*as.numeric(input$caixa_valor)

          #print(atv_valor_volume_ano)
          dados_atv_m3<-paste(input$caixa_atividade," | ",idioma$moeda,"/ha"," | ", atv_valor_volume_ano[cont], sep = "")

          cx[[paste("atv_",cont,sep = "")]]<<- append(cx[[paste("atv_",cont,sep = "")]],dados_atv_m3)
        }
      }
      }
    }
    if(input$caixa_custo_terra==TRUE && isTruthy(input$dados_basicos_valor_terra_total) && isTruthy(input$dados_basicos_taxa)){
      for(cont in 1:range$horizonte){
        cx[[paste("custo_independente_volume_",cont,sep = "")]]<<- paste(idioma$fc_custo_terra," | ",paste(idioma$moeda,"/ha",sep = "")," | ",as.numeric(input$dados_basicos_valor_terra)*(as.numeric(input$dados_basicos_taxa)/100))
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
    dados_tabela$metricas_economicas<<- data.frame("Ano" = 0, "VPL" = 0,"VPL_Infinito" = 0,"RLPE" = 0, "VET" = 0,"TIR" = 0)
    dados_tabela$metricas_economicas_total<<- data.frame("Ano" = 0, "VPL" = 0,"VPL_Infinito" = 0,"RLPE" = 0, "VET" = 0,"TIR" = 0)
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
      target = 'cell', disable = list(columns = c(0, 2, 3,4))), rownames = FALSE,extensions = "Buttons",colnames= c(idioma$rs_ano,idioma$rs_volume,idioma$rs_IMA,idioma$rs_ICA),
      options = list(paging = TRUE,    ## paginate the output
                     scrollX = TRUE,   ## enable scrolling on X axis
                     dom = 'lBrtip',
                     lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                     scrollY = TRUE,
                     buttons =list('copy', 'print', list(extend = 'collection',buttons = list(list(extend = 'csv', filename = "Spreadsheet_EdenTree_Silvicultural_Rotation_CSV",title = NULL),list(extend = 'excel', filename = "Spreadsheet_EdenTree_Silvicultural_Rotation_EXCEL",title = NULL)),text = 'Download'))

      )
      )
  }

  })
  
  #Observe Valor Terra
  observeEvent(input$dados_basicos_valor_terra_total,{
    if(is.na(input$dados_basicos_valor_terra_total) | is.null(input$dados_basicos_valor_terra_total) | input$dados_basicos_valor_terra_total=="NA"){
      updateNumericInput(session = session,inputId = "dados_basicos_valor_terra_total",value = "0",min = "0")
    }
    updateNumericInput(session = session,inputId = "dados_basicos_valor_terra",label = paste(idioma$valor_da_terra," (",idioma$moeda,"/","ha):",sep = ""),value = (input$dados_basicos_valor_terra_total/input$dados_basicos_area),min = "0")
  })
  #Observe Area
  observeEvent(input$dados_basicos_area,{
    idioma$area<-toString(input$dados_basicos_area)
    if(is.null(input$idioma)){
      if(idioma$area=='1'){
        idioma$re_VPL_total <- paste("NPV (",idioma$moeda,"/ha)",sep = "")
        idioma$re_VPL_infinito_total <- paste("Infinite NPV (",idioma$moeda,"/ha)",sep = "")
        idioma$re_RLPE_total <- paste("EAA (",idioma$moeda,"/ha)",sep = "")
        idioma$re_VET_total <- paste("LEV (",idioma$moeda,"/ha)",sep = "")
        idioma$re_TIR_total <- paste("IRR (","%)",sep = "")
        idioma$re_PAYBACK_total <- paste("Discounted Payback",sep = "")
      } else {
        idioma$re_VPL_total <- paste("NPV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_VPL_infinito_total <- paste("Infinite NPV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_RLPE_total <- paste("EAA (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_VET_total <- paste("LEV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
        idioma$re_TIR_total <- paste("IRR (","%)",sep = "")
        idioma$re_PAYBACK_total <- paste("Discounted Payback",sep = "")
      }
    } else {
      if(input$idioma=="EN-USA"){
        if(idioma$area=='1'){
        idioma$re_VPL_total <- paste("NPV (",idioma$moeda,"/ha)",sep = "")
        idioma$re_VPL_infinito_total <- paste("Infinite NPV (",idioma$moeda,"/ha)",sep = "")
        idioma$re_RLPE_total <- paste("EAA (",idioma$moeda,"/ha)",sep = "")
        idioma$re_VET_total <- paste("LEV (",idioma$moeda,"/ha)",sep = "")
        idioma$re_TIR_total <- paste("IRR (","%)",sep = "")
        idioma$re_PAYBACK_total <- paste("Discounted Payback",sep = "")
        } else {
          idioma$re_VPL_total <- paste("NPV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
          idioma$re_VPL_infinito_total <- paste("Infinite NPV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
          idioma$re_RLPE_total <- paste("EAA (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
          idioma$re_VET_total <- paste("LEV (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
          idioma$re_TIR_total <- paste("IRR (","%)",sep = "")
          idioma$re_PAYBACK_total <- paste("Discounted Payback",sep = "")
        }
      } else if(input$idioma=="PT-BR"){
        if(idioma$area=='1'){
        idioma$re_VPL_total <- paste("VPL (",idioma$moeda,"/ha)",sep = "")
        idioma$re_VPL_infinito_total <- paste("VPL Infinito (",idioma$moeda,"/ha)",sep = "")
        idioma$re_RLPE_total <- paste("AAE (",idioma$moeda,"/ha)",sep = "")
        idioma$re_VET_total <- paste("VET (",idioma$moeda,"/ha)",sep = "")
        idioma$re_TIR_total <- paste("TIR (","%)",sep = "")
        idioma$re_PAYBACK_total <- paste("Payback com desconto",sep = "")
        } else {
          idioma$re_VPL_total <- paste("VPL (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
          idioma$re_VPL_infinito_total <- paste("VPL Infinito (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
          idioma$re_RLPE_total <- paste("AAE (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
          idioma$re_VET_total <- paste("VET (",idioma$moeda,"/",idioma$area,"ha)",sep = "")
          idioma$re_TIR_total <- paste("TIR (","%)",sep = "")
          idioma$re_PAYBACK_total <- paste("Payback com desconto",sep = "")
      }
      }
    }
    
    if(is.na(input$dados_basicos_area) | is.null(input$dados_basicos_area) | input$dados_basicos_area=="NA"){
      updateNumericInput(session = session,inputId = "dados_basicos_area",value = "0",min = "0")
    }
    if(input$dados_basicos_area>0 & input$dados_basicos_area!=1 & !is.na(input$dados_basicos_area)){
    output$ui_dados_basicos_valor_terra<- renderUI({
      div(
        numericInput(inputId = "dados_basicos_valor_terra_total", label = paste(idioma$valor_da_terra," (",idioma$moeda,"/",idioma$area,"ha):",sep = ""), value = "0", min = "0"),
        shinyjs::disabled(numericInput(inputId = "dados_basicos_valor_terra", label = paste(idioma$valor_da_terra," (",idioma$moeda,"/","ha):",sep = ""), value = "0", min = "0"))
        )
    })
    } else if(input$dados_basicos_area==1 & !is.na(input$dados_basicos_area)) {
      output$ui_dados_basicos_valor_terra<- renderUI({
        div(
          numericInput(inputId = "dados_basicos_valor_terra_total", label = paste("Value of Land (",idioma$moeda,"/",idioma$area," ha):",sep = ""), value = "0", min = "0"),
          shinyjs::hidden(numericInput(inputId = "dados_basicos_valor_terra", label = paste(idioma$valor_da_terra," (",idioma$moeda,"/","ha):",sep = ""), value = "0", min = "0"))
        )
      })
    }else {
      output$ui_dados_basicos_valor_terra<- renderUI({})
    }
    updateNumericInput(session = session,inputId = "dados_basicos_valor_terra_total",label = paste(idioma$valor_da_terra," (",idioma$moeda,"/",idioma$area,"ha):",sep = ""),value = input$dados_basicos_valor_terra_total,min = "0")
    updateNumericInput(session = session,inputId = "dados_basicos_valor_terra",label = paste(idioma$valor_da_terra," (",idioma$moeda,"/","ha):",sep = ""),value = (input$dados_basicos_valor_terra_total/input$dados_basicos_area),min = "0")
    if(area_automatica==FALSE){
    updateCheckboxInput(session = session,inputId = "check_arquivo_rotacao_silvicultural",value = FALSE)
    }
    area_automatica<<-FALSE
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
        target = 'cell', disable = list(columns = c(0, 2, 3,4))), rownames = FALSE,extensions = "Buttons",colnames= c(idioma$rs_ano,idioma$rs_volume,idioma$rs_IMA,idioma$rs_ICA),
        options = list(paging = TRUE,    ## paginate the output
                       scrollX = TRUE,   ## enable scrolling on X axis
                       dom = 'lBrtip',
                       lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                       scrollY = TRUE,
                       buttons =list('copy', 'print', list(extend = 'collection',buttons = list(list(extend = 'csv', filename = "Spreadsheet_EdenTree_Silvicultural_Rotation_CSV",title = NULL),list(extend = 'excel', filename = "Spreadsheet_EdenTree_Silvicultural_Rotation_EXCEL",title = NULL)),text = 'Download'))
                       
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

    if(is.numeric(x) == TRUE && x>0 & input$dados_basicos_area>0 | is.double(x) == TRUE && x>0 & input$dados_basicos_area>0){
      dados_tb_prod <<- DT::editData(dados_tb_prod, input$tabela_volume_cell_edit, 'tabela_volume', rownames = FALSE)
      if(i==1 && j==2){
        dados_tb_prod[i,j]<<- dados_tb_prod[i,j]/as.numeric(input$dados_basicos_area) #Volume dividido pela area
        dados_tb_prod[i,j+2]<<-as.numeric(dados_tb_prod[i,j])
        dados_tb_prod[i,j+1]<<-as.numeric(dados_tb_prod[i,j])/as.numeric(dados_tb_prod[i,j-1])###  IMA = Volume/Idade
      } else {
        dados_tb_prod[i,j-1]<<- i
        dados_tb_prod[i,j]<<- dados_tb_prod[i,j]/as.numeric(input$dados_basicos_area) #Volume dividido pela area
        dados_tb_prod[i,j+2]<<-as.numeric(dados_tb_prod[i,j])-as.numeric(dados_tb_prod[i-1,2])
        dados_tb_prod[i,j+1]<<-as.numeric(dados_tb_prod[i,j])/as.numeric(dados_tb_prod[i,j-1])###  IMA = Volume/Idade
      }
      if(nrow(dados_tb_prod)<as.numeric(range$horizonte) && j==2){
        dados_tb_prod <- rbind(dados_tb_prod, c(i+1,0,0,0))

      }
      dados_tabela$producao <<- dados_tb_prod
    } else {
      if(input$dados_basicos_area<=0){
        #aviso area
        showNotification(idioma$aviso_area,type = "error")
      } else {
        showNotification(idioma$aviso_volume_invalido,type = "error")
      }
      output$tabela_volume <- DT::renderDT(round(dados_tabela$producao,4), selection = 'none', editable = list(
        target = 'cell', disable = list(columns = c(0, 2, 3,4))), rownames = FALSE,extensions = "Buttons",colnames= c(idioma$rs_ano,idioma$rs_volume,idioma$rs_IMA,idioma$rs_ICA),
        options = list(paging = TRUE,    ## paginate the output
                       scrollX = TRUE,   ## enable scrolling on X axis
                       scrollY = TRUE,
                       lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                       dom = 'lBrtip',
                       buttons = list('copy', 'print', list( extend = 'collection',buttons = list(list(extend = 'csv', filename = "Spreadsheet_EdenTree_Silvicultural_Rotation_CSV",title = NULL),list(extend = 'excel', filename = "Spreadsheet_EdenTree_Silvicultural_Rotation_EXCEL",title = NULL)),text = 'Download'))

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


      #Verifica arquivo - EN-USA
      if(verifica_colunas_nomes[1]=="Year" & verifica_colunas_nomes[2]=="Volume"){
        verifica_colunas_nomes[1]<-"Ano"
        verifica_colunas_nomes[2]<-"Volume"
        colnames(analisa_colunas) <- c("Ano","Volume")
      }

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
          if(input$dados_basicos_area<=0){
            showNotification(idioma$aviso_area_2,type = "warning")
            area_automatica<<-TRUE
            updateNumericInput(session = session,inputId = "dados_basicos_area",value = "1",min = "0")
          } else {
            area_automatica<<-FALSE
            analisa_colunas[["Volume"]]<-(analisa_colunas[["Volume"]]/as.numeric(input$dados_basicos_area))
          }
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
      return(div(icon("file-arrow-up",lib = "font-awesome"),idioma$rs_label_status,":",icon("spinner",lib = "font-awesome")," ",idioma$rs_label_status_aguardando))
    } else if (values_inputs$file_input_arquivo_state=='uploaded') {
      return(div(icon("file-arrow-up",lib = "font-awesome"),idioma$rs_label_status,":",icon("check",lib = "font-awesome")," ",idioma$rs_label_status_valido))
    } else if (values_inputs$file_input_arquivo_state=='reset') {
      return(div(icon("file-arrow-up",lib = "font-awesome"),idioma$rs_label_status,":",icon("xmark",lib = "font-awesome")," ",idioma$rs_label_status_invalido,p(icon("triangle-exclamation",lib = "font-awesome"),idioma$rs_label_status_invalido_aviso)))
    }
  })
  output$icon_file_input_status <- renderUI({
    status_icon_file_input()
  })
observeEvent(input$check_arquivo_rotacao_silvicultural,{
  if(input$dados_basicos_area<=0 & input$check_arquivo_rotacao_silvicultural==TRUE){
    #aviso area
    showNotification(idioma$aviso_area,type = "error")
    updateCheckboxInput(session = session,inputId = "check_arquivo_rotacao_silvicultural",value = FALSE)
  } else {
  output$resultado_rotacao_silvicultural<- renderUI({})
  output$resultado_rotacao_economica<- renderUI({})
  bs4Dash::updateBox("rotacao_economica",action = "remove")
  bs4Dash::updateBox("caixa",action = "remove")
  if(input$check_arquivo_rotacao_silvicultural==TRUE){
    shinyjs::disable(id="dados_basicos_horizonte")
    dados_tb_prod <<- data.frame(Ano = 1, Volume = 0) %>% dplyr::mutate(IMA = 0) %>% dplyr::mutate(ICA =  0)
    dados_tabela$producao<<-dados_tb_prod
    output$tabela_volume <- DT::renderDT(round(dados_tabela$producao,4), selection = 'none', editable = FALSE, rownames = FALSE,extensions = "Buttons",colnames= c(idioma$rs_ano,idioma$rs_volume,idioma$rs_IMA,idioma$rs_ICA),
      options = list(paging = TRUE,    ## paginate the output
                     scrollX = TRUE,   ## enable scrolling on X axis
                     scrollY = TRUE,
                     lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                     dom = 'lBrtip',
                     buttons =list('copy', 'print',list( extend = 'collection',buttons = list(list(extend = 'csv', filename = "Spreadsheet_EdenTree_Silvicultural_Rotation_CSV",title = NULL),list(extend = 'excel', filename = "Spreadsheet_EdenTree_Silvicultural_Rotation_EXCEL",title = NULL)),text = 'Download'))
      )
    )

    output$arquivo_rotacao_silvicultural<- renderUI({
    div(
      bs4Dash::box(
        width = 12,
        title = idioma$rs_label_arquivo,
        status = "primary",
        collapsible = F,
        collapsed = F,
      fileInput(inputId = "arquivo_rs", label = uiOutput("icon_file_input_status"),buttonLabel = idioma$btn_arquivo, placeholder = idioma$placeholder_arquivo, accept = "xlsx")
      )
      )
    })
    output$modelo_arquivo_rotacao_silvicultural<- renderUI({
      div(
      bs4Dash::box(
        width = 12,
        title =  downloadLink("baixar_planilha_modelo", label = div(icon("cloud-arrow-down",lib = "font-awesome"),idioma$rs_label_modelo)),
        status = "primary",
        collapsible = F,
        collapsed = F,
        DT::datatable(data.frame(
          Ano = c("4", "5"),
          Volume = c(132.78 * input$dados_basicos_area, 
                     206.38 * input$dados_basicos_area) |>
            format(nsmall = 2) |>
            gsub("\\.", ",", x = _) |>
            as.character()
        ),colnames= c(idioma$rs_ano,paste(idioma$rs_volume_2," (m³/",idioma$area,"ha)",sep = "")),
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
    output$tabela_volume <- DT::renderDT(round(dados_tabela$producao,4), selection = 'none',colnames= c(idioma$rs_ano,idioma$rs_volume,idioma$rs_IMA,idioma$rs_ICA) ,editable = list(
      target = 'cell', disable = list(columns = c(0, 2, 3,4))), rownames = FALSE,extensions = "Buttons",
      options = list(paging = TRUE,    ## paginate the output
                     scrollX = TRUE,   ## enable scrolling on X axis
                     scrollY = TRUE,   ## enable scrolling on Y axis
                     lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                     dom = 'lBrtip',
                     buttons=list('copy', 'print', list(extend = 'collection',buttons = list(list(extend = 'csv', filename = "Spreadsheet_EdenTree_Silvicultural_Rotation_CSV",title = NULL),list(extend = 'excel', filename = "Spreadsheet_EdenTree_Silvicultural_Rotation_EXCEL",title = NULL)),text = 'Download'))

      )
    )
  }
}})
#Download Planilha Modelo
output$baixar_planilha_modelo<- downloadHandler(
  filename = function() {
    paste("Spreadsheet_Example_EdenTree.xlsx", sep="")
  },
  content = function(file) {
    file.copy(file.path("inst/app/www/PLANILHA_MODELO/",idioma$rs_arquivo_exemplo,"/Spreadsheet_Example_EdenTree.xlsx"),file)
  }
)
#Ano da Rotacao Silvicultural
  observeEvent(dados_tabela$producao,{
    resultado_ima_rs<-0
    for (cont in 1:nrow(dados_tabela$producao)) {
     if(as.numeric(dados_tabela$producao[cont,"IMA"])==max(as.numeric(dados_tabela$producao[,"IMA"])) && resultado_ima_rs<as.numeric(dados_tabela$producao[cont,"IMA"])){
       resultado_ima_rs<-as.numeric(dados_tabela$producao[cont,"IMA"])
       resultado_ano_rs<-paste(idioma$rs_ano," ",as.numeric(dados_tabela$producao[cont,"Ano"]),sep = "")
     }
    }
    if(all(dados_tabela$producao["Volume"]==0, na.rm = TRUE)==FALSE){
    output$resultado_rotacao_silvicultural<- renderUI({
      bs4Dash::blockQuote(paste(idioma$label_rotacao_silvicultural,":",toString(resultado_ano_rs),sep = ""),color = "primary")
    }
    )
    }

  })
#Ano da Rotacao Economica
  observeEvent(dados_tabela$metricas_economicas,{
    resultado_vpl_re<- 0
    resultado_ano_re<-paste(idioma$re_ano," ",as.numeric(0),sep = "")
    verifica_vpl_negativo<-FALSE
    #View(dados_tabela$metricas_economicas)
    for (cont in 2:nrow(dados_tabela$metricas_economicas)) {
      if(as.numeric(dados_tabela$metricas_economicas[cont,"VPL_Infinito"])==max(as.numeric(dados_tabela$metricas_economicas[,"VPL_Infinito"]),na.rm = TRUE) && all(as.numeric(dados_tabela$metricas_economicas[,"VPL_Infinito"])==as.numeric(dados_tabela$metricas_economicas[cont,"VPL_Infinito"]),na.rm = TRUE)==FALSE){
        resultado_vpl_re<-as.numeric(dados_tabela$metricas_economicas[cont,"VPL_Infinito"])
        resultado_ano_re<-paste(idioma$re_ano," ",as.numeric(dados_tabela$metricas_economicas[cont,"Ano"]),sep = "")
      }
    }
    if(all(dados_tabela$metricas_economicas["VPL_Infinito"]==0, na.rm = TRUE)==FALSE){
      output$resultado_rotacao_economica<- renderUI({
        bs4Dash::blockQuote(paste(idioma$label_rotacao_economica,": ",toString(resultado_ano_re),sep = ""),color = "primary")
      }
      )
    }

  })
  #Grafico Producao - Ima - ICA
  output$grafico_prod<- renderPlot({
  ggplot2::ggplot(data = dados_tabela$producao)+
    {if(all(dados_tabela$producao["Volume"]==0, na.rm = TRUE)==FALSE && nrow(dados_tabela$producao)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = Volume,color="Produção (m³/ha)"),na.rm=TRUE)}+
    ggplot2:: scale_color_manual(values = c("Produção (m³/ha)" = "darkred"))+
    ggplot2::xlab(idioma$rs_ano)+
    ggplot2::ylab(idioma$rs_producao)+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "none",plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),axis.title = ggplot2::element_text(size=18),axis.text = ggplot2::element_text(size=15))+
    ggpubr::grids(axis = c("xy"), color = "grey92", size = 1, linetype = "solid")
  })
  output$grafico_ima_ica<-renderPlot({
    ggplot2::ggplot(data = dados_tabela$producao)+
      {if(all(dados_tabela$producao["Volume"]==0, na.rm = TRUE)==FALSE && nrow(dados_tabela$producao)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = IMA,color=idioma$rs_IMA),na.rm=TRUE)}+
      {if(all(dados_tabela$producao["Volume"]==0, na.rm = TRUE)==FALSE && nrow(dados_tabela$producao)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = ICA,color=idioma$rs_ICA),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "", values = c("darkblue","darkgreen"))+
      ggplot2::xlab(idioma$rs_ano)+
      ggplot2::ylab(idioma$rs_metricas)+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),legend.direction = "vertical",legend.position = "bottom",legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=18),axis.text = ggplot2::element_text(size=15))+
      ggpubr::grids(axis = c("xy"), color = "grey92", size = 1, linetype = "solid")
  })
  output$grafico_rotacao_economica_vpl_ha<- renderPlot({
    ggplot2::ggplot(data = dados_tabela$metricas_economicas[c("Ano","VPL")])+
      {if(all(dados_tabela$metricas_economicas["VPL"]==0, na.rm = TRUE)==FALSE && nrow(dados_tabela$metricas_economicas)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = VPL,color=idioma$re_VPL ),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "", values = c( "orange"))+
      ggplot2::xlab(idioma$re_ano)+
      ggplot2::ylab(idioma$re_VPL)+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),legend.direction = "vertical",legend.position = "none",legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=18),axis.text = ggplot2::element_text(size=15))+
      ggpubr::grids(axis = c("xy"), color = "grey92", size = 1, linetype = "solid")
  })
  output$grafico_rotacao_economica_vpl_infinito_ha<- renderPlot({
    ggplot2::ggplot(data = dados_tabela$metricas_economicas[c("Ano","VPL_Infinito")])+
      {if(all(dados_tabela$metricas_economicas["VPL_Infinito"]==0, na.rm = TRUE)==FALSE  && nrow(dados_tabela$metricas_economicas)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = VPL_Infinito,color=idioma$re_VPL_infinito),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "", values = c("purple"))+
      ggplot2::xlab(idioma$re_ano)+
      ggplot2::ylab(idioma$re_VPL_infinito)+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),legend.direction = "vertical",legend.position = "none",legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=18),axis.text = ggplot2::element_text(size=15))+
      ggpubr::grids(axis = c("xy"), color = "grey92", size = 1, linetype = "solid")
  })
  output$grafico_rotacao_economica_rlpe_ha<- renderPlot({
    ggplot2::ggplot(data = dados_tabela$metricas_economicas[c("Ano","RLPE")])+
      {if(all(dados_tabela$metricas_economicas["RLPE"]==0, na.rm = TRUE)==FALSE  && nrow(dados_tabela$metricas_economicas)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = RLPE,color=idioma$re_RLPE),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "", values = c("magenta"))+
      ggplot2::xlab(idioma$re_ano)+
      ggplot2::ylab(idioma$re_RLPE)+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),legend.direction = "vertical",legend.position = "none",legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=18),axis.text = ggplot2::element_text(size=15))+
      ggpubr::grids(axis = c("xy"), color = "grey92", size = 1, linetype = "solid")
  })
  output$grafico_rotacao_economica_vet_ha<- renderPlot({
    ggplot2::ggplot(data = dados_tabela$metricas_economicas[c("Ano","VET")])+
      {if(all(dados_tabela$metricas_economicas["VET"]==0, na.rm = TRUE)==FALSE  && nrow(dados_tabela$metricas_economicas)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = VET,color= idioma$re_VET ),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "", values = c("darkcyan"))+
      ggplot2::xlab(idioma$re_ano)+
      ggplot2::ylab(idioma$re_VET)+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),legend.direction = "vertical",legend.position = "none",legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=18),axis.text = ggplot2::element_text(size=15))+
      ggpubr::grids(axis = c("xy"), color = "grey92", size = 1, linetype = "solid")
  })
  output$grafico_rotacao_economica_tir<- renderPlot({
    ggplot2::ggplot(data = dados_tabela$metricas_economicas[c("Ano","TIR")])+
      {if(all(dados_tabela$metricas_economicas["TIR"]==0, na.rm = TRUE)==FALSE  && nrow(dados_tabela$metricas_economicas)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = TIR,color= idioma$re_TIR),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "", values = c("yellow"))+
      ggplot2::xlab(idioma$re_ano)+
      ggplot2::ylab(idioma$re_TIR)+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),legend.direction = "vertical",legend.position = "none",legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=18),axis.text = ggplot2::element_text(size=15))+
      ggpubr::grids(axis = c("xy"), color = "grey92", size = 1, linetype = "solid")
  })
  #PARA AREA TOTAL
  output$grafico_rotacao_economica_vpl<- renderPlot({
    ggplot2::ggplot(data = dados_tabela$metricas_economicas_total[c("Ano","VPL")])+
      {if(all(dados_tabela$metricas_economicas_total["VPL"]==0, na.rm = TRUE)==FALSE && nrow(dados_tabela$metricas_economicas_total)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = VPL,color=idioma$re_VPL_total ),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "", values = c( "orange"))+
      ggplot2::xlab(idioma$re_ano)+
      ggplot2::ylab(idioma$re_VPL_total)+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),legend.direction = "vertical",legend.position = "none",legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=18),axis.text = ggplot2::element_text(size=15))+
      ggpubr::grids(axis = c("xy"), color = "grey92", size = 1, linetype = "solid")
  })
  output$grafico_rotacao_economica_vpl_infinito<- renderPlot({
    ggplot2::ggplot(data = dados_tabela$metricas_economicas_total[c("Ano","VPL_Infinito")])+
      {if(all(dados_tabela$metricas_economicas_total["VPL_Infinito"]==0, na.rm = TRUE)==FALSE  && nrow(dados_tabela$metricas_economicas_total)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = VPL_Infinito,color=idioma$re_VPL_infinito_total),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "", values = c("purple"))+
      ggplot2::xlab(idioma$re_ano)+
      ggplot2::ylab(idioma$re_VPL_infinito_total)+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),legend.direction = "vertical",legend.position = "none",legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=18),axis.text = ggplot2::element_text(size=15))+
      ggpubr::grids(axis = c("xy"), color = "grey92", size = 1, linetype = "solid")
  })
  output$grafico_rotacao_economica_rlpe<- renderPlot({
    ggplot2::ggplot(data = dados_tabela$metricas_economicas_total[c("Ano","RLPE")])+
      {if(all(dados_tabela$metricas_economicas_total["RLPE"]==0, na.rm = TRUE)==FALSE  && nrow(dados_tabela$metricas_economicas_total)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = RLPE,color=idioma$re_RLPE_total),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "", values = c("magenta"))+
      ggplot2::xlab(idioma$re_ano)+
      ggplot2::ylab(idioma$re_RLPE_total)+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),legend.direction = "vertical",legend.position = "none",legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=18),axis.text = ggplot2::element_text(size=15))+
      ggpubr::grids(axis = c("xy"), color = "grey92", size = 1, linetype = "solid")
  })
  output$grafico_rotacao_economica_vet<- renderPlot({
    ggplot2::ggplot(data = dados_tabela$metricas_economicas_total[c("Ano","VET")])+
      {if(all(dados_tabela$metricas_economicas_total["VET"]==0, na.rm = TRUE)==FALSE  && nrow(dados_tabela$metricas_economicas_total)>1)ggplot2::geom_line(size=1.5,mapping = ggplot2::aes(x= Ano, y = VET,color= idioma$re_VET_total ),na.rm=TRUE)}+
      ggplot2:: scale_color_manual(name = "", values = c("darkcyan"))+
      ggplot2::xlab(idioma$re_ano)+
      ggplot2::ylab(idioma$re_VET_total)+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 1, 0, 1), "cm"),legend.direction = "vertical",legend.position = "none",legend.title = ggplot2::element_text(size=18),legend.text=ggplot2::element_text(size=15),axis.title = ggplot2::element_text(size=18),axis.text = ggplot2::element_text(size=15))+
      ggpubr::grids(axis = c("xy"), color = "grey92", size = 1, linetype = "solid")
  })
  output$dados_basicos <- renderUI({
    fluidRow(
    bs4Dash::box(id="dados_basicos",
                 title = idioma$label_dados,
                 collapsible = F,
                 collapsed = F,
                 status = "primary",
                 width = 12,
                 dropdownMenu = bs4Dash::boxDropdown(
                   div(style="text-align:center;",
                       bs4Dash::boxDropdownItem(actionLink(inputId ="download_relatorio",label = "Download Report"), id = "dropdownItem_download_relatorio"),
                   ),icon = icon("cloud-arrow-down",lib = "font-awesome")),
                 fluidRow(
                   column(width = 4,
                          textInput(inputId = "dados_basicos_nome_projeto", label = "Project Name:")
                   ),
                   column(width = 4,
                         numericInput(inputId = "dados_basicos_horizonte", label = "Planning Horizon (years):", value = "1", min = "1")
                   ),
                   column(width = 4,
                          selectInput(inputId = "dados_basicos_moeda", label = "Currency ($):", choices = sort(unique(isocurrency$currency_code)), selected = "USD")
                   )
                 ),
                 fluidRow(
                   column(width = 4,
                          numericInput(inputId = "dados_basicos_taxa", label = "Rate (%):", value = "0", min = "0", max = "100")
                   ),
                   column(width = 4,
                          numericInput(inputId = "dados_basicos_area", label = "Area (ha):", value = "0", min = "0")
                   ),
                   column(width = 4,
                          uiOutput("ui_dados_basicos_valor_terra")
                   )
                 )
    )
    )
  })
   output$rotacao_silvicultural <- renderUI({
    fluidRow(

      bs4Dash::box(id="rotacao_silvicultural",
                   title = idioma$label_rotacao_silvicultural,
                   footer = div(
                     fluidRow(column(width = 12,
                                     checkboxInput(inputId = "check_arquivo_rotacao_silvicultural",label = idioma$rs_adicionar_arquivo)
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
                            plotOutput("grafico_prod")
                            )
                            ),
                            fluidRow(column(width = 12,
                                            plotOutput("grafico_ima_ica")
                            )
                            ),
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
                   title = idioma$label_caixa,
                   collapsible = F,
                   collapsed = F,
                   status = "primary",
                   width = 12,
                   fluidRow(
                     column(width = 4,
                            textInput(inputId = "caixa_atividade", label = idioma$fc_label_atividade)
                            ),
                     column(width = 4,
                            numericInput(inputId = "caixa_valor", label = idioma$fc_label_valor, value = 0, min = 0)
                     ),
                     column(width = 4,
                            #Alterar choices nos ifs, devido diferenca de moeda em cada pais
                            radioButtons(inputId = "caixa_unidade", label = idioma$fc_label_unidade, choices = c(paste(idioma$moeda,"/",idioma$area,"ha",sep = ""),paste(idioma$moeda,"/m³",sep = "")))
                     )

                   ),
                   fluidRow(
                     column(width = 12,
                     hr(),
                            checkboxInput(inputId = "caixa_custo_terra", label = idioma$fc_add_custo_terra),
                     hr()
                     )
                     ),
                   fluidRow(
                     column(width = 6,
                            actionButton(width = "100%", inputId = "btn_caixa_atv", label = idioma$btn_criar_atividade),
                            ),
                     column(width = 6,
                            actionButton(width = "100%", inputId = "btn_reset_caixa", label = idioma$btn_reiniciar),
                     )
                   ),

                   lapply(0:range$horizonte,function(cont) {
                   fluidRow(
                     column(width = 12,
                     sortable::bucket_list(
                       header = paste(idioma$fc_ano," ",cont,sep = ""),
                       group_name = paste("bucket_list_group_",cont,sep = ""),
                       orientation = "horizontal",
                       sortable::add_rank_list(
                         text = idioma$fc_atividades,
                         labels = cx[[paste("atv_",cont,sep = "")]],
                         input_id = paste("caixa_atividades_",cont,sep = "")
                       ),

                         sortable::add_rank_list(
                           text = idioma$fc_custos_independentes,
                           labels = cx[[paste("custo_independente_volume_",cont,sep = "")]],
                           input_id = paste("caixa_custos_independentes_volume_",cont,sep = "")
                         ),
                       sortable::add_rank_list(
                         text = idioma$fc_custos_dependentes,
                         labels = cx$custo_dependente_volume,
                         input_id = paste("caixa_custos_dependentes_volume_",cont,sep = "")
                       ),

                          sortable::add_rank_list(
                                 text =  idioma$fc_receitas,
                                 labels = cx$receita,
                                 input_id = paste("caixa_receitas_",cont,sep = "")
                               )


                     )
                     )
                   )
                   }),
                   fluidRow(
                     column(width = 12,
                            actionButton(width = "100%", inputId = "btn_caixa_gerar", label = idioma$btn_gerar),
                     )
                   )
                   )
    )
  })
 
  output$rotacao_economica<- renderUI({
    if(!is.null(input$dados_basicos_area)){
    if(input$dados_basicos_area>1){
      fluidRow(
        bs4Dash::box(id="rotacao_economica",
                     title = idioma$label_rotacao_economica,
                     collapsible = F,
                     collapsed = F,
                     status = "primary",
                     width = 12,
                     fluidRow(
                       column(width = 12,
                              DT::DTOutput("tabela_rotacao_economica")
                       )),
                     fluidRow(
                       column(4,
                              plotOutput("grafico_rotacao_economica_vpl_ha"),
                              plotOutput("grafico_rotacao_economica_vpl"),
                              plotOutput("grafico_rotacao_economica_vpl_infinito_ha"),
                              plotOutput("grafico_rotacao_economica_vpl_infinito")
                       ),
                       column(4,
                              plotOutput("grafico_rotacao_economica_rlpe_ha"),
                              plotOutput("grafico_rotacao_economica_rlpe"),
                              plotOutput("grafico_rotacao_economica_vet_ha"),
                              plotOutput("grafico_rotacao_economica_vet")
                       ),
                       column(4,
                              plotOutput("grafico_rotacao_economica_tir"),
                              fluidRow(column(width = 12,
                                              uiOutput("resultado_rotacao_economica")))
                       )
                     )
        )
      )
    } else if (input$dados_basicos_area==1){
      fluidRow(
        bs4Dash::box(id="rotacao_economica",
                     title = idioma$label_rotacao_economica,
                     collapsible = F,
                     collapsed = F,
                     status = "primary",
                     width = 12,
                     fluidRow(
                       column(width = 12,
                              DT::DTOutput("tabela_rotacao_economica")
                       )),
                     fluidRow(
                       column(4,
                              plotOutput("grafico_rotacao_economica_vpl_ha"),
                              plotOutput("grafico_rotacao_economica_vpl_infinito_ha"),
                       ),
                       column(4,
                              plotOutput("grafico_rotacao_economica_rlpe_ha"),
                              plotOutput("grafico_rotacao_economica_vet_ha"),
                       ),
                       column(4,
                              plotOutput("grafico_rotacao_economica_tir"),
                              fluidRow(column(width = 12,
                                              uiOutput("resultado_rotacao_economica")))
                       )
                     )
        )
      )
    }
  }
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
    showNotification(idioma$aviso_taxa,type = "error")
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
  
#####CORTE
  #CODIGO ANTIGO
  #CALCULO DO TOTAL
  MFLUXO[,5]=MFLUXO[,2]-MFLUXO[,3]
  #CALCULANDO O VPL / RLPE
  VPL<<-c()
  RLPE<<-c()
  VPLinf<<-c()
  VET<<-c()
  TIR<<-c()
  PAYBACK_DESCONTADO<<-rep(" ", nrow(MFLUXO))
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
        TIR[cont] <<- NA
        PAYBACK_DESCONTADO[cont] <<- " "
      } else {
        RLPE[cont] <<- (VPL[cont]*(input$dados_basicos_taxa/100)*(1+(input$dados_basicos_taxa/100))^(cont-1))/(((1+(input$dados_basicos_taxa/100))^(cont-1))-1)
        VPLinf[cont] <<- RLPE[cont]/(as.numeric(input$dados_basicos_taxa)/100)
        VET[cont] <<- VPLinf[cont] + as.numeric(input$dados_basicos_valor_terra)
        TIR[cont]<<- tryCatch(
          round((FinCal::irr(c(MFLUXO[1:(cont-1), 3]*-1,((MFLUXO[cont, 3]*-1)+MFLUXO[cont, 2]+(MFLUXO[cont, 4]*-1)))) * 100),4), 
          error = function(e) NA
        )
        if(VPLinf[cont] >= 0){
          PAYBACK_DESCONTADO[cont] <<- "✔"
        } else {
          PAYBACK_DESCONTADO[cont] <<- " "
        }
      }
      
    }
    
    dados_tabela$metricas_economicas<<- data.frame("Ano" = 0:range$horizonte, "VPL" = VPL,"VPL_Infinito" = VPLinf,"RLPE" = RLPE, "VET" = VET, "TIR" = TIR)
    dados_tabela$metricas_economicas<<-round(dados_tabela$metricas_economicas,4)
    dados_tabela$metricas_economicas_total<<-as.data.frame(cbind(dados_tabela$metricas_economicas["Ano"],dados_tabela$metricas_economicas[c("VPL","VPL_Infinito","RLPE","VET")]*as.numeric(input$dados_basicos_area)))
    dados_tabela$metricas_economicas<<-as.data.frame(cbind(dados_tabela$metricas_economicas,PAYBACK_DESCONTADO))
    if(input$dados_basicos_area<=1){
    output$tabela_rotacao_economica <- DT::renderDT(dados_tabela$metricas_economicas, selection = 'none', colnames= c(idioma$re_ano,idioma$re_VPL,idioma$re_VPL_infinito,idioma$re_RLPE ,idioma$re_VET,idioma$re_TIR,idioma$re_PAYBACK ), rownames = FALSE,extensions = "Buttons",
                                                    options = list(paging = TRUE,    ## paginate the output
                                                                   scrollX = TRUE,   ## enable scrolling on X axis
                                                                   scrollY = TRUE,
                                                                   lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                                                                   dom = 'lBrtip',
                                                                   buttons =list('copy', 'print', list( extend = 'collection',buttons = list(list(extend = 'csv', filename = "Spreadsheet_EdenTree_Economic_Rotation_CSV",title = NULL),list(extend = 'excel', filename = "Spreadsheet_EdenTree_Economic_Rotation_EXCEL",title = NULL)),text = 'Download'))
                                                                   
                                                    )
    )} else {
      output$tabela_rotacao_economica <- DT::renderDT(as.data.frame(cbind(dados_tabela$metricas_economicas,dados_tabela$metricas_economicas_total[c("VPL","VPL_Infinito","RLPE","VET")])), selection = 'none', colnames= c(idioma$re_ano,idioma$re_VPL,idioma$re_VPL_infinito,idioma$re_RLPE ,idioma$re_VET,idioma$re_TIR,idioma$re_PAYBACK,idioma$re_VPL_total,idioma$re_VPL_infinito_total,idioma$re_RLPE_total,idioma$re_VET_total), rownames = FALSE,extensions = "Buttons",
                                                      options = list(paging = TRUE,    ## paginate the output
                                                                     scrollX = TRUE,   ## enable scrolling on X axis
                                                                     scrollY = TRUE,
                                                                     lengthMenu = list(c(25, 50,100, -1), c('25', '50','100' ,'Todas')),
                                                                     dom = 'lBrtip',
                                                                     buttons =list('copy', 'print', list( extend = 'collection',buttons = list(list(extend = 'csv', filename = "Spreadsheet_EdenTree_Economic_Rotation_CSV",title = NULL),list(extend = 'excel', filename = "Spreadsheet_EdenTree_Economic_Rotation_EXCEL",title = NULL)),text = 'Download'))
                                                                     
                                                      )
      )
    }
  }
####CORTE
if(all(dados_tabela$metricas_economicas[,"VPL"]==0)){
  output$resultado_rotacao_economica<- renderUI({})
}

})


  #Rodape
output$render_Rodape_Tutorial <- renderUI({
  p(style = "font-weight: bold;margin: 0px;",actionLink(inputId = "modal_tutorial",label = paste("•",idioma$label_tutorial,sep = "")))
})
observeEvent(input$modal_tutorial, {
  showModal(modalDialog(
    size = "xl",
    title = div(icon("person-chalkboard", lib = "font-awesome"), idioma$tutorial_title),
    easyClose = TRUE,
    footer = modalButton(toString(idioma$btn_fechar)),
    bs4Dash::bs4Accordion(
      id = "Accordion_Tutorial",
      bs4Dash::bs4AccordionItem(
        title = idioma$tutorial_1_Dados,
        status = "primary",
        solidHeader = TRUE,
        collapsed = FALSE,
        bs4Dash::blockQuote(
          p(idioma$tutorial_1_Dados_1_1),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/13","_",idioma$tutorial_img,".png", sep = ""))),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_1_Dados_1_2),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/15","_",idioma$tutorial_img,".png", sep = ""))),
          p(idioma$tutorial_1_Dados_1_2_note),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_1_Dados_1_3),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/14","_",idioma$tutorial_img,".png", sep = ""))),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_1_Dados_1_4),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/20","_",idioma$tutorial_img,".png", sep = ""))),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_1_Dados_1_5),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/16","_",idioma$tutorial_img,".png", sep = ""))),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_1_Dados_1_6, icon("cloud-arrow-down", lib = "font-awesome")),
          color = "primary"
        )
      ),
      bs4Dash::bs4AccordionItem(
        title = idioma$tutorial_2_Rotacao_Silvicultural,
        status = "primary",
        solidHeader = TRUE,
        bs4Dash::blockQuote(
          p(idioma$tutorial_2_Rotacao_Silvicultural_2_1),
          bs4Dash::blockQuote(
            p(idioma$tutorial_2_Rotacao_Silvicultural_2_1_1),
            div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/2","_",idioma$tutorial_img,".png", sep = ""))),
            color = "primary"
          ),
          bs4Dash::blockQuote(
            p(idioma$tutorial_2_Rotacao_Silvicultural_2_1_2),
            div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/4","_",idioma$tutorial_img,".png", sep = ""))),
            p(idioma$tutorial_2_Rotacao_Silvicultural_2_1_2_note_1),
            div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/5","_",idioma$tutorial_img,".png", sep = ""))),
            p(idioma$tutorial_2_Rotacao_Silvicultural_2_1_2_note_2),
            color = "primary"
          ),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_2_Rotacao_Silvicultural_2_2),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/11","_",idioma$tutorial_img,".png", sep = ""))),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/12","_",idioma$tutorial_img,".png", sep = ""))),
          color = "primary"
        )
      ),
      bs4Dash::bs4AccordionItem(
        title = idioma$tutorial_3_Caixa,
        status = "primary",
        solidHeader = TRUE,
        bs4Dash::blockQuote(
          p(idioma$tutorial_3_Caixa_3_1),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 100px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/17","_",idioma$tutorial_img,".png", sep = ""))),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_3_Caixa_3_2),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 100px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/18","_",idioma$tutorial_img,".png", sep = ""))),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_3_Caixa_3_3),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 100px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/19","_",idioma$tutorial_img,".png", sep = ""))),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_3_Caixa_3_4),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 20px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/7","_",idioma$tutorial_img,".png", sep = ""))),
          p(idioma$tutorial_3_Caixa_3_4_1),
          p(idioma$tutorial_3_Caixa_3_4_2),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_3_Caixa_3_5),
          p(idioma$tutorial_3_Caixa_3_5_note_1),
          p(idioma$tutorial_3_Caixa_3_5_note_2),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_3_Caixa_3_6),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/8","_",idioma$tutorial_img,".png", sep = ""))),
          p(idioma$tutorial_3_Caixa_3_6_note),
          color = "primary"
        ),
        bs4Dash::blockQuote(
          p(idioma$tutorial_3_Caixa_3_7),
          color = "primary"
        )
      ),
      bs4Dash::bs4AccordionItem(
        title = idioma$tutorial_4_Rotacao_Economica,
        status = "primary",
        solidHeader = TRUE,
        bs4Dash::blockQuote(
          p(idioma$tutorial_4_Rotacao_Economica_4_1),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/9","_",idioma$tutorial_img,".png", sep = ""))),
          div(align = "center", img(class = "responsive_img_tutorial_tabs", style = "max-height: 500px;", src = paste("www/TUTORIAL/", idioma$tutorial_img, "/10","_",idioma$tutorial_img,".png", sep = ""))),
          color = "primary"
        )
      )
    )
  ))
})


output$render_Rodape_Sobre_Nos <- renderUI({
  p(style = "font-weight: bold;margin: 0px;",actionLink(inputId = "modal_sobre_nos",label = paste("•",toString(idioma$label_sobre_nos),sep="")))
})
observeEvent(input$modal_sobre_nos,{
  showModal(modalDialog(
    size = "xl",
    title = div(icon("address-card",lib = "font-awesome"),toString(idioma$label_sobre_nos)) ,
    easyClose = TRUE,
    footer = modalButton(toString(idioma$btn_fechar)),
    div(align="center",
        bs4Dash::blockQuote(
          h1(idioma$label_sobre_eden_tree),
          div(
            fluidRow(
              p(idioma$sobre_eden_tree)
            ),
          )
          , color= "primary"),
        bs4Dash::blockQuote(
          h1(idioma$equipe),
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
                     bs4Dash::blockQuote(style="text-align:left;",h5(HTML("<a href='https://www.escavador.com/sobre/6194151/lucas-rezende-gomide' target='_blank'>Dr. Lucas Rezende Gomide</a>")),p("•E-mail: lucasgomide@ufla.br"),color = "primary")
              )
            ),
            fluidRow(
              #height de cada linha 300px para medio,grande
              #height de cada linha 315px para pequeno
              # HTML(
              #   '<iframe width="100%" height="350px" src="www/LINKEDIN/linkedin_users.html" frameborder="0" scrolling="yes"></iframe>'
              # )
              
              column(width = 4,
                     bs4Dash::blockQuote(style="text-align:left;",h5(HTML("<a href='https://www.linkedin.com/in/marcelo-vitor-chaves-888681284' target='_blank'>Marcelo Vitor Gualberto Santos Chaves</a>")),p("•E-mail: marcelo160102@gmail.com"),color = "primary")
              ),
              column(width = 4,
                     bs4Dash::blockQuote(style="text-align:left;",h5(HTML("<a href='https://www.linkedin.com/in/marcelo-pauletti-919302200/' target='_blank'>Marcelo Lourençoni Pauletti</a>")),p("•E-mail: lourenconi.marcelo@icloud.com"),color = "primary")
              )
            )
          )
          , color= "primary"),
        bs4Dash::blockQuote(
          h5(icon("file-lines"),HTML(paste(sep = "","<a href='https://www.revistaarvore.ufv.br/rarv/article/view/263865' target='_blank'>",idioma$artigo,"</a>")))
          , color= "primary")
        )
  ))
})
  output$render_Rodape_Direitos_Ano <- renderText({
    paste("©", format(Sys.Date(), "%Y")," - ",idioma$label_direitos)
  })
  output$render_Rodape_Versao_Codename <- renderUI({
    actionLink(inputId = "modal_codename",label = "AppVersion:1.0 - Codename: openeyes")
  })
  observeEvent(input$modal_codename,{
    showModal(modalDialog(
      title = div(icon("code",lib = "font-awesome"),"openeyes") ,
      easyClose = TRUE,
      size="xl",
      footer = modalButton(toString(idioma$btn_fechar)),
      bs4Dash::box(width = 12,
                   title = div(icon("eye",lib = "font-awesome"),idioma$label_easteregg),
                   status = "primary",
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   HTML(
                     '<iframe width="100%" height="500" src="https://www.youtube.com/embed/Iymkk85ELxw?si=kF-fmBPe65FAlpqx" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'
                       )
      )
    ))
  })
  #Idioma
  output$render_Rodape_Idioma <- renderUI({
    p(style = "font-weight: bold;margin: 0px;",actionLink(inputId = "modal_idioma",label = paste("•",toString(idioma$label_idioma),sep = "")))
  })
  observeEvent(input$modal_idioma,{
    showModal(modalDialog(
      title = div(icon("flag",lib = "font-awesome"),idioma$label_idioma),
      easyClose = TRUE,
      size="s",
      footer = NULL,
      selectInput("idioma",label = NULL, choices = c("EN-USA","PT-BR"), selected = input$idioma)
    ))
  })
}

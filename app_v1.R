
library(shiny)
library(vroom)
library(waiter)
library(gt)
library(dplyr)
library(tidyr)
library(shinyWidgets)
library(markdown)
library(rio)
library(stringr)
library(scholar)
library(purrr)

# funciones preliminares --------------------------------------------------

valores <- function(temp2, rubrica, catalogo_perfiles) {
  library(dplyr)
  library(stringr)
  library(tidyr)
  
  #-----------------------------
  # 1. Normalizar nombres de variables del formulario
  #-----------------------------
  datos <- temp2 |>
    rename(
      antiguedad      = `Antigüedad impartiendo esta asignatura en esta licenciatura`,
      antiguedad_otro = `Experiencia impartiendo asignaturas similares en otros programas de licenciatura`,
      papime_resp     = `Responsable de proyectos PAPIME en la enseñanza de esta asignatura o asignatura afín`,
      papime_part     = `Participación en proyectos PAPIME en la enseñanza de esta asignatura o asignatura afín`,
      cursos          = `Número de Cursos de Actualización Docente con duración superior a 20 horas en los últimos 5 años`,
      inv_resp        = `Proyectos de Investigación con financiamiento en el campo de conocimiento de la asignatura, de los que ha sido Responsable Técnico o Coordinador:`,
      inv_part        = `Proyectos de Investigación con financiamiento en el campo de conocimiento de la asignatura, de los que ha sido Participante:`,
      cons_resp       = `Proyectos de Servicio o Consultoría en el campo de conocimiento de la asignatura, de los que ha sido Responsable Técnico o Coordinador:`,
      cons_part       = `Proyectos de Servicio o Consultoría en el campo de conocimiento de la asignatura, de los que ha sido Participante:`,
      productos       = `Productos académicos, técnicos o profesionales relevantes para la asignatura`,
      perfil_form     = `Perfil de formación profesional`
    )
  
  # si tu asignatura es "0507 ECONOMIA ECOLOGICA", extrae la clave numérica
  datos <- datos |>
    mutate(
      clv_materia = str_extract(asignatura, "^[0-9]{3,4}")
    )
  
  #-----------------------------
  # 2. Preparar rúbrica compilada por tipo
  #-----------------------------
  rub_comp <- rubrica
  
  rub_docencia   <- rub_comp |> filter(tipo == "docencia")
  rub_antiguedad <- rub_comp |> filter(tipo == "antiguedad")
  rub_experiencia<- rub_comp |> filter(tipo == "experiencia")
  rub_perfiles   <- rub_comp |> filter(tipo == "perfiles")
  
  # función auxiliar para mapear una variable a la rúbrica
  mapear_rubrica <- function(df, rub, var_nombre, sufijo) {
    var_sym <- rlang::sym(var_nombre)
    rub_var <- rub |> filter(variable == var_nombre) |> 
      select(valor, puntos)
    
    df |>
      left_join(rub_var, by = c(!!var_sym := "valor")) |>
      rename(!!paste0("PTS_", sufijo) := puntos)
  }
  
  #-----------------------------
  # 3. Módulo de pertinencia (perfiles)
  #-----------------------------
  # 3.1. Catalogo de perfiles en formato largo
  cat_long <- catalogo_perfiles |>
    # ajusta nombres si difieren
    rename(clv_materia = clv_materia) |>
    pivot_longer(
      cols = c(FMT, BQ, MS, CE, H, SS, AF, IDT, I),
      names_to  = "area_abbr",
      values_to = "nivel"
    ) |>
    mutate(
      nivel = if_else(is.na(nivel) | nivel == "", "INS", nivel)
    )
  
  # 3.2. Separar áreas del perfil del profesor y mapear a abreviaturas
  datos <- datos |>
    mutate(
      area1 = str_trim(str_split_fixed(perfil_form, ",", 2)[,1]),
      area2 = str_trim(str_split_fixed(perfil_form, ",", 2)[,2]),
      area1_abbr = dplyr::recode(
        area1,
        "Físico-Matemáticas y Ciencias de la Tierra" = "FMT",
        "Biología y Química"                         = "BQ",
        "Medicina y Ciencias de la Salud"            = "MS",
        "Ciencias de la Conducta y la Educación"     = "CE",
        "Humanidades"                                = "H",
        "Ciencias Sociales"                          = "SS",
        "Agricultura, Agropecuarias, Forestales, y de Ecosistemas" = "AF",
        "Ingenierías y Desarrollo Tecnológico"       = "IDT",
        "Interdisciplinaria"                         = "I",
        .default = NA_character_
      ),
      area2_abbr = dplyr::recode(
        area2,
        "Físico-Matemáticas y Ciencias de la Tierra" = "FMT",
        "Biología y Química"                         = "BQ",
        "Medicina y Ciencias de la Salud"            = "MS",
        "Ciencias de la Conducta y la Educación"     = "CE",
        "Humanidades"                                = "H",
        "Ciencias Sociales"                          = "SS",
        "Agricultura, Agropecuarias, Forestales, y de Ecosistemas" = "AF",
        "Ingenierías y Desarrollo Tecnológico"       = "IDT",
        "Interdisciplinaria"                         = "I",
        .default = NA_character_
      )
    )
  
  # 3.3. Cruzar áreas del profesor con el catálogo y obtener el mejor nivel
  pertinencia <- datos |>
    select(ID_solicitud, profesor, asignatura, clv_materia,
           area1_abbr, area2_abbr) |>
    pivot_longer(
      cols = c(area1_abbr, area2_abbr),
      names_to  = "which",
      values_to = "area_abbr",
      values_drop_na = TRUE
    ) |>
    left_join(cat_long, by = c("clv_materia", "area_abbr")) |>
    mutate(
      nivel = if_else(is.na(nivel) | nivel == "", "INS", nivel),
      nivel = factor(
        nivel,
        levels = c("INS", "PAR", "MED", "ALT", "DIR"),
        ordered = TRUE
      )
    ) |>
    group_by(ID_solicitud, profesor, asignatura) |>
    summarise(
      nivel_max = as.character(max(nivel)),
      .groups = "drop"
    )
  
  datos <- datos |>
    left_join(pertinencia,
              by = c("ID_solicitud", "profesor", "asignatura")) |>
    mutate(
      nivel_max = replace_na(nivel_max, "INS")
    ) |>
    left_join(
      rub_perfiles |> select(valor, puntos),
      by = c("nivel_max" = "valor")
    ) |>
    rename(PTS_PERTINENCIA = puntos)
  
  #-----------------------------
  # 4. Aplicar rúbrica a docencia, antigüedad y experiencia
  #-----------------------------
  # Docencia: nombramiento, adscripcion, estudios, papime_resp, papime_part, cursos
  datos <- datos |>
    mapear_rubrica(rub_docencia, "nombramiento",   "NOMBRAMIENTO") |>
    mapear_rubrica(rub_docencia, "adscripcion",    "ADSCRIPCION") |>
    mapear_rubrica(rub_docencia, "estudios",       "ESTUDIOS") |>
    mapear_rubrica(rub_docencia, "papime_resp",    "PAPIME_RESP") |>
    mapear_rubrica(rub_docencia, "papime_part",    "PAPIME_PART") |>
    mapear_rubrica(rub_docencia, "cursos",         "CURSOS")
  
  datos <- datos |>
    mutate(
      PTS_DOCENCIA = rowSums(across(starts_with("PTS_") &
                                      c("PTS_NOMBRAMIENTO",
                                        "PTS_ADSCRIPCION",
                                        "PTS_ESTUDIOS",
                                        "PTS_PAPIME_RESP",
                                        "PTS_PAPIME_PART",
                                        "PTS_CURSOS")),
                             na.rm = TRUE)
    )
  
  # Antigüedad: asignatura y otras
  datos <- datos |>
    mapear_rubrica(rub_antiguedad, "antiguedad",      "ANTIGUEDAD") |>
    mapear_rubrica(rub_antiguedad, "antiguedad_otro", "ANTIGUEDAD_OTRO") |>
    mutate(
      PTS_ANTIGUEDAD = rowSums(across(c(PTS_ANTIGUEDAD, PTS_ANTIGUEDAD_OTRO)),
                               na.rm = TRUE)
    )
  
  # Experiencia: inv_resp, inv_part, cons_resp, cons_part, productos
  datos <- datos |>
    mapear_rubrica(rub_experiencia, "inv_resp",   "INV_RESP") |>
    mapear_rubrica(rub_experiencia, "inv_part",   "INV_PART") |>
    mapear_rubrica(rub_experiencia, "cons_resp",  "CONS_RESP") |>
    mapear_rubrica(rub_experiencia, "cons_part",  "CONS_PART") |>
    mapear_rubrica(rub_experiencia, "productos",  "PRODUCTOS") |>
    mutate(
      PTS_EXPERIENCIA = rowSums(across(c(PTS_INV_RESP,
                                         PTS_INV_PART,
                                         PTS_CONS_RESP,
                                         PTS_CONS_PART,
                                         PTS_PRODUCTOS)),
                                na.rm = TRUE)
    )
  
  #-----------------------------
  # 5. Puntaje total por profesor (sin ponderar horas todavía)
  #-----------------------------
  datos <- datos |>
    mutate(
      PTS_CONDICION = 0,  # si ya no usas condición/h-index, lo dejas en 0 o lo eliminas
      PTS_PROFESOR  = PTS_CONDICION +
        PTS_ANTIGUEDAD +
        PTS_DOCENCIA +
        PTS_EXPERIENCIA +
        PTS_PERTINENCIA
    )
  
  datos
}


nomina <- function(x){
  adjudicados <- x
  yy <- tibble(
    "Semestre/Grupo" = str_remove(adjudicados$semestre_grupo, "LCA, semestre "),
    "Asignatura" = adjudicados$asignatura,
    "Profesor(a)" = adjudicados$profesor,
    "estudios" = adjudicados$estudios,
    "Nombramiento" = factor(adjudicados$nombramiento,
                            levels = c("UNAM - Profesor",
                                       "Profesor de Asignatura",
                                       "UNAM -  Investigador",
                                       "Profesor Honorífico",
                                       "UNAM - Técnico Académico"
                            ),
                            labels = c("Profesor Tiempo Completo",
                                       "Profesor de Asignatura",
                                       "Investigador Tiempo Completo",
                                       "Profesor Honorífico",
                                       "Técnico Académico")),
    "RFC" = adjudicados$rfc,
    "UNAM" = as.character(adjudicados$num_unam),
    "correo1" = adjudicados$correo_unam,
    "correo2" = adjudicados$correo_pers,
    "HT" = as.numeric(adjudicados$horas_t),
    "HP" = as.numeric(adjudicados$horas_p)
  )
  
  xx <- yy |> 
    group_by(`Semestre/Grupo`, Asignatura, `Profesor(a)`) |> 
    mutate("Carga Horaria" = sum(HT, HP),
           "Horas Pagadas"= if_else(Nombramiento == "Profesor de Asignatura", 
                                    true = sum(HT, HP),
                                    false = 0),
           "correo" = if_else(str_detect(correo1, pattern = "unam.mx"),
                              correo1,
                              correo2)
    ) |> 
    relocate(correo, .after = UNAM) |> 
    relocate(`Carga Horaria`, .before = HT) |> 
    select(-c(correo1, correo2)) |> 
    pivot_longer(cols = estudios:correo, names_to = "info", values_to = "Datos") |> 
    select(-c(info)) |> 
    relocate(Datos, .after = `Profesor(a)`)
  return(xx)

}
# User interface ----------------------------------------------------------

ui <- fluidPage(
  waiter::use_waiter(),
  titlePanel("Preselección de postulaciones a docencia ENES Mérida"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(p(strong("1. Preparación de datos")),
                fileInput("file", "Cargar formulario", accept = ".xlsx", multiple = TRUE),
                actionButton("tidy", "Ordenar solicitudes")
      ),
      wellPanel(p(strong("2. Aplicación de rúbrica")),
                fileInput("file2", label = "Cargar rúbrica", accept = ".xlsx"),
                fileInput("file_cat", label = "Cargar catálogo", accept = ".xlsx"),
                actionButton("rubrica", "Aplicar rúbrica"),
                actionButton("total", "Preseleccionar")
      ),
      wellPanel(p(strong("3. Datos para nómina")),
                fileInput("file3", label = "Cargar validaciones", accept = ".csv"),
                actionButton("adjudicar", "Adjudicados"),
                actionButton("nomina", "Datos para nómina")
      ),
      wellPanel(p(strong("4. Asignaturas desiertas")),
                fileInput("file4", label = "Cargar catálogo de la licenciatura", accept = ".xlsx"),
                actionButton("desierto", "Identificar desiertas")
      ),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Info", 
                 includeMarkdown("intro.md")),
        tabPanel("Postulaciones ordenadas", 
                 gt_output("tabla1"),
                 downloadButton("descarga0",label = "Descarga")),
        tabPanel("Postulaciones valoradas en detalle",
                 gt_output("tabla2"),
                 downloadButton("descarga1",label = "Descarga")),
        tabPanel("Preselecciones por validar",
                 gt_output("tabla3"),
                 downloadButton("descarga2", label = "Descarga")),
        tabPanel("Adjudicados",
                 gt_output("tabla4"),
                 downloadButton("descarga3", label = "Descarga")),
        tabPanel("Nómina",
                 gt_output("tabla5"),
                 downloadButton("descarga4", label = "Descarga")),
        tabPanel("Desiertas",
                 gt_output("tabla6"),
                 downloadButton("descarga5", label = "Descarga"))
      )
    )
  )
)



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 1. Cargar formulario sin procesar
  temp1 <- reactive({
    infile <- input$file
    import(infile$datapath, col_types = "text")
  })
  
  # 2. Modificar formulario (sin h-index)
  temp2 <- eventReactive(input$tidy, {
    withProgress(message = 'Ordenando solicitudes...', value = 0, {
      temp1() |>
        mutate(ID_solicitud = as.character(1:nrow(temp1()))) |>
        relocate(ID_solicitud, .before = `Marca temporal`) |>
        # ajusta nombres de columnas según tu archivo actual
        rename(
          marca          = `Marca temporal`,
          semestre_grupo = `Por favor, indica el semestre y grupo en el que solicitas impartir la asignatura`,
          profesor       = `Nombre completo (sin grados académicos ni abreviaturas)`
        ) |>
        # aquí va tu lógica de pivot_longer/pivot_wider original,
        # pero sin columnas de Google Scholar
        pivot_longer(
          cols = starts_with("Elige la asignatura"),
          names_to  = "variable",
          values_to = "asignatura"
        ) |>
        drop_na(asignatura) |>
        group_by(marca, semestre_grupo, variable) |>
        mutate(fila = row_number()) |>
        ungroup()
    })
  })
  
  # 3. Cargar rúbrica
  rubrica <- reactive({
    infile2 <- input$file2
    import(infile2$datapath, sheet = "compilada")
  })
  
  # 4. Cargar catálogo (perfiles)
  catalogo <- reactive({
    infile_cat <- input$file_cat
    import(infile_cat$datapath, sheet = "perfiles")
  })
  
  # 5. Aplicar rúbrica + pertinencia
  temp4 <- eventReactive(input$rubrica, {
    temp3 <- valores(
      temp2   = temp2(),
      rubrica = rubrica(),
      catalogo_perfiles = catalogo()
    )
    
    temp3 |>
      group_by(ID_solicitud, profesor) |>
      # PTS_PROFESOR ya viene de valores()
      relocate(semestre_grupo, .before = ID_solicitud) |>
      relocate(asignatura, .after = semestre_grupo) |>
      arrange(semestre_grupo, asignatura, ID_solicitud) |>
      select(-c(marca, fila))
  })
  
  # 6. Estimación puntos de cada solicitud (ponderado por horas)
  temp5 <- eventReactive(input$total, {
    temp4() |>
      mutate(
        horas_tot_prof = as.numeric(horas_t) + as.numeric(horas_p)
      ) |>
      group_by(semestre_grupo, asignatura, ID_solicitud, responsable) |>
      summarise(
        total = ifelse(
          all(is.na(horas_tot_prof)) | sum(horas_tot_prof, na.rm = TRUE) == 0,
          mean(PTS_PROFESOR, na.rm = TRUE),
          stats::weighted.mean(PTS_PROFESOR, w = horas_tot_prof, na.rm = TRUE)
        ),
        .groups = "drop"
      ) |>
      group_by(semestre_grupo, asignatura) |>
      arrange(semestre_grupo, asignatura, desc(total)) |>
      mutate(
        adjudicacion = if_else(total == max(total), "VALIDAR POR CA", "CONDICIONADA"),
        diferencia   = if_else(adjudicacion == "VALIDAR POR CA", 0, max(total) - total)
      ) |>
      ungroup()
  })
  
  # 7. Datos nómina (igual que antes)
  temp6 <- reactive({
    infile3 <- input$file3
    import(infile3$datapath, 
           col_types = c("text","text","text","text","numeric","text","numeric"))
  })
  
  temp7 <- eventReactive(input$adjudicar, {
    temp2() |>
      semi_join(temp6() |> filter(adjudicacion == "VALIDADA")) |>
      relocate(semestre_grupo, .before = ID_solicitud) |>
      relocate(asignatura, .after = semestre_grupo) |>
      arrange(semestre_grupo, asignatura, ID_solicitud) |>
      select(ID_solicitud, semestre_grupo, asignatura, profesor, nombramiento, adscripcion, estudios,
             rfc, curp, num_unam, correo_unam, correo_pers, telf, horas_t, horas_p)
  })
  
  temp8 <- eventReactive(input$nomina, {
    nomina(temp7())
  })
  
  # 8. Desiertas (igual que antes)
  temp9 <- reactive({
    infile4 <- input$file4
    import(infile4$datapath)
  })
  
  temp10 <- eventReactive(input$desierto, {
    anti_join(temp9(), temp7())
  })
  
  #----------------- outputs (puedes mantener renderTable o pasar a gt) -------------
  output$tabla1 <- renderTable({ temp2() })
  output$tabla2 <- renderTable({ temp4() })
  output$tabla3 <- renderTable({ temp5() })
  output$tabla4 <- renderTable({ temp7() })
  output$tabla5 <- renderTable({ temp8() })
  output$tabla6 <- renderTable({ temp10() })
  
  # descargas (ajustadas a los nuevos objetos)
  output$descarga0 <- downloadHandler(
    filename = "temp2.csv",
    content = function(file) {
      vroom::vroom_write(temp2(), file , delim = ",", bom = TRUE)
    }
  )
  
  output$descarga1 <- downloadHandler(
    filename = "valoracion_profesores.csv",
    content = function(file) {
      vroom::vroom_write(temp4(), file , delim = ",", bom = TRUE)
    }
  )
  
  output$descarga2 <- downloadHandler(
    filename = "valoracion_solicitudes.csv",
    content = function(file) {
      vroom::vroom_write(temp5(), file , delim = ",", bom = TRUE)
    }
  )
  
  output$descarga3 <- downloadHandler(
    filename = "datos_adjudicados.csv",
    content = function(file) {
      vroom::vroom_write(temp7(), file , delim = ",", bom = TRUE)
    }
  )
  
  output$descarga4 <- downloadHandler(
    filename = "datos_nomina.csv",
    content = function(file) {
      vroom::vroom_write(temp8(), file , delim = ",", bom = TRUE)
    }
  )
  
  output$descarga5 <- downloadHandler(
    filename = "asignaturas_desiertas.csv",
    content = function(file) {
      vroom::vroom_write(temp10(), file , delim = ",", bom = TRUE)
    }
  )
}

# Aplicacion --------------------------------------------------------------


shinyApp(ui = ui, server = server)

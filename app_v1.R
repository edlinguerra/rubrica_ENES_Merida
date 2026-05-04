
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

profile <- safely(get_profile)
hindex <- function(x){
  id <- profile(x)
  if(is.null(id$result)){
    return(0)
  }
  if(is.na(id$result)){
    return(0)
  }
  else{return(id$result$h_index)
  }
}

valores <- function(temp2, rubrica){
  cond <- rubrica %>% 
    filter(tipo == "condicion")
  
 # esq <- rubrica %>% 
 #  filter(tipo == "esquema")
  
  ant <- rubrica %>% 
    filter(tipo == "antiguedad")
  
  docen <- rubrica %>% 
    filter(tipo == "docencia")
  
  exp <- rubrica %>% 
    filter(tipo == "experiencia")
  
  
  temp3 <- temp2 %>% 
    mutate(pts_nombramiento = rep(NA, nrow(temp2)),
           pts_adscripcion = rep(NA, nrow(temp2)),
           pts_estudios = rep(NA, nrow(temp2)))
  
  #nombramiento
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(cond)){
      if (temp3$nombramiento[i] == cond$valor[l]){
        temp3$pts_nombramiento[i] <- cond$puntos[l]
      }
    }}
  
  #adscripción
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(cond)){
      if (temp3$adscripcion[i] == cond$valor[l]){
        temp3$pts_adscripcion[i] <- cond$puntos[l]
      }
    }}
  
  for (i in 1:nrow(temp3)){
    if (is.na(temp3$pts_adscripcion[i])){
      temp3$pts_adscripcion[i] <- 5
    }
  }
  
  #estudios
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(cond)){
      if (temp3$estudios[i] == cond$valor[l]){
        temp3$pts_estudios[i] <- cond$puntos[l]
      }
    }}
  
  for (i in 1:nrow(temp3)){
    if (is.na(temp3$pts_estudios[i])){
      temp3$pts_estudios[i] <- 1 
    }
  }
  
  temp3 <- temp3 %>%
    group_by(marca,profesor)  %>%
    mutate(PTS_CONDICION = sum(pts_nombramiento,pts_adscripcion, pts_estudios)) %>%
    ungroup()
  
  ##Esquema de enseñanza
  # temp3 <- temp3 %>% 
  #   mutate(pts_esquema = rep(NA, nrow(temp3)))
  # 
  # for (i in 1:nrow(temp3)){
  #   for (l in 1:nrow(esq)){
  #     if (temp3$esquema[i] == esq$valor[l]){
  #       temp3$pts_esquema[i] <- esq$puntos[l]
  #     }
  #   }}
  # 
  # temp3 <- temp3 %>% 
  #   group_by(marca,profesor) %>%
  #   mutate(PTS_ESQUEMA = pts_esquema) %>%
  #   ungroup()
  
  ## Antigüedad impartiendo la asignatura
  temp3 <- temp3 %>% 
    mutate(pts_a_lic = rep(NA, nrow(temp3)),
           pts_a_otra = rep(NA, nrow(temp3)))
  
  #En la licenciatura
  anta <- ant %>%
    filter(variable == "antiguedad_lca")
  
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(anta)){
      if (temp3$antiguedad_lca[i] == anta$valor[l]){
        temp3$pts_a_lic[i] <- anta$puntos[l]
      }
    }}
  
  #En otra licenciatura
  antb <- ant %>%
    filter(variable == "antiguedad_otro")
  
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(antb)){
      if (temp3$antiguedad_otro[i] == antb$valor[l]){
        temp3$pts_a_otra[i] <- antb$puntos[l]
      }
    }}
  
  temp3 <- temp3 %>% 
    group_by(marca,profesor) %>%
    mutate(PTS_ANTIGUEDAD = sum(pts_a_lic,pts_a_otra)) %>%
    ungroup()
  
  ## Elementos del CV relacionados con la actividad docente
  temp3 <- temp3 %>% 
    mutate(pts_rpapime = rep(NA, nrow(temp3)),
           pts_ppapime = rep(NA, nrow(temp3)),
           pts_cursos = rep(NA, nrow(temp3)))
  
  #Responsabilidad en PAPIME o equivalente
  docena <- docen %>% 
    filter(variable == "papime_resp")
  
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(docena)){
      if (temp3$papime_resp[i] == docena$valor[l]){
        temp3$pts_rpapime[i] <-  docena$puntos[l]
      }
    }
  }
  
  #Participación en PAPIME o equivalente
  docenb <- docen %>% 
    filter(variable == "papime_part")
  
  
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(docenb)){
      if (temp3$papime_part[i] == docenb$valor[l]){
        temp3$pts_ppapime[i] <-  docenb$puntos[l]
      }
    }
  }      
  
  #cursos de mejoramiento docente
  docenc <- docen %>% 
    filter(variable == "cursos")
  
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(docenc)){
      if (temp3$cursos[i] == docenc$valor[l]){
        temp3$pts_cursos[i] <-  docenc$puntos[l]
      }
    }
  }   
  
  temp3 <- temp3 %>% 
    group_by(marca,profesor) %>%
    mutate(PTS_DOCENCIA = sum(pts_rpapime, pts_ppapime, pts_cursos)) %>%
    ungroup()
  
  ## Elementos del CV relacionados con la actividad profesional
  temp3 <- temp3 %>% 
    mutate(pts_inv_r = rep(NA, nrow(temp3)),
           pts_inv_p = rep(NA, nrow(temp3)),
           pts_con_r = rep(NA, nrow(temp3)),
           pts_con_p = rep(NA, nrow(temp3)),
           pts_prod = rep(NA, nrow(temp3)),
           pts_hindex = rep(NA, nrow(temp3)))
  
  #Responsabilidad en proyectos de investigación financiados
  expa <- exp %>%
    filter(variable == "inv_resp")
  
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(expa)){
      if (temp3$inv_resp[i] == expa$valor[l]){
        temp3$pts_inv_r[i] <- expa$puntos[l]
      }
    }}
  
  #Participación en proyectos de investigación financiados
  expb <- exp %>%
    filter(variable == "inv_part")
  
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(expb)){
      if (temp3$inv_part[i] == expb$valor[l]){
        temp3$pts_inv_p[i] <- expb$puntos[l]
      }
    }}
  
  #Responsabilidad en proyectos de consultoría o servicio técnico
  expc <- exp %>%
    filter(variable == "cons_resp")
  
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(expc)){
      if (temp3$cons_resp[i] == expc$valor[l]){
        temp3$pts_con_r[i] <- expc$puntos[l]
      }
    }}
  
  #Participación en proyectos de consultoría o servicio técnico
  expd <- exp %>%
    filter(variable == "cons_part")
  
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(expd)){
      if (temp3$cons_part[i] == expd$valor[l]){
        temp3$pts_con_p[i] <- expd$puntos[l]
      }
    }}
  
  #Número de productos académicos (artículos arbitrados, patentes, libros, etc.)
  expe <- exp %>%
    filter(variable == "productos")
  
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(expe)){
      if (temp3$productos[i] == expe$valor[l]){
        temp3$pts_prod[i] <- expe$puntos[l]
      }
    }}
  
  #Impacto académico en investigación basado en h-index de Google Scholar
  expf <- exp %>%
    filter(variable == "h_index") %>% 
    mutate(valor = as.numeric(valor))
  
  for (i in 1:nrow(temp3)){
    for (l in 1:nrow(expf)){
      if (temp3$h_index[i] == 0){
        temp3$pts_hindex[i] <- 0
      }
      if (between(temp3$h_index[i], 1, 4)){
        temp3$pts_hindex[i] <- 0.5
      } 
      if(between(temp3$h_index[i], 5, 9)){
        temp3$pts_hindex[i] <- 1
      }
      if(between(temp3$h_index[i], 10, 14)){
        temp3$pts_hindex[i] <- 1.5
      }
      if(between(temp3$h_index[i], 15, 19)){
        temp3$pts_hindex[i] <- 2
      }
      if(between(temp3$h_index[i], 20, 500)){
        temp3$pts_hindex[i] <- 2.5
      }
    }
  }
  
  temp3 <- temp3 %>% 
    group_by(marca,profesor) %>%
    mutate(PTS_EXPERIENCIA = sum(pts_inv_r,
                                 pts_inv_p,
                                 pts_con_r,
                                 pts_con_p,
                                 pts_prod,
                                 pts_hindex,
                                 na.rm = TRUE)) %>%
    ungroup()
  return(temp3)
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
  
  xx <- yy %>% 
    group_by(`Semestre/Grupo`, Asignatura, `Profesor(a)`) %>% 
    mutate("Carga Horaria" = sum(HT, HP),
           "Horas Pagadas"= if_else(Nombramiento == "Profesor de Asignatura", 
                                    true = sum(HT, HP),
                                    false = 0),
           "correo" = if_else(str_detect(correo1, pattern = "unam.mx"),
                              correo1,
                              correo2)
    ) %>% 
    relocate(correo, .after = UNAM) %>% 
    relocate(`Carga Horaria`, .before = HT) %>% 
    select(-c(correo1, correo2)) %>% 
    pivot_longer(cols = estudios:correo, names_to = "info", values_to = "Datos") %>% 
    select(-c(info)) %>% 
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
  
#cargar formulario sin procesar
  temp1 <- reactive({
    infile <- input$file
    import(infile$datapath, col_types = "text")
    })

#Modificar formulario  
  temp2 <- eventReactive(input$tidy, {
    #barra de progreso
    withProgress(message = 'buscando h-index, espere...', value = 0, {
    # Organización de formulario y búsqueda de h-index
      temp1() %>%
        mutate(ID_solicitud = as.character(1:nrow(temp1()))) %>%
        relocate(ID_solicitud, .before = marca) %>%
        pivot_longer(cols = asignatura...5:sum(length(temp1()),1), names_to = "variable") %>%
        mutate(variable = str_remove(string = variable, pattern = "\\...*")) %>%
        mutate(variable = str_remove_all(string = variable,
                                         pattern = c("_1"="",
                                                     "_2"="",
                                                     "_3"=""))) %>% 
        filter(variable != "2do_prof", variable != "3er_prof") %>%
        drop_na() %>%
        group_by(marca, semestre_grupo, variable) %>%
        mutate(fila = row_number()) %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        ungroup() %>%
        #mutate(id_gs = str_remove(string = id_gs, pattern = ".*\\=")) %>%
        mutate(id_gs = str_remove(string = id_gs, pattern = ".*user=")) %>%
        group_by(ID_solicitud, profesor) %>%
        mutate(h_index = hindex(id_gs)) %>%
        #mutate(h_index = NA) %>%
        mutate(h_index = replace_na(h_index, 0)) %>%
        ungroup() %>%
        relocate(h_index, .after = id_gs) %>%
        fill(asignatura)
     #incProgress()
      
    })
  })

#Cargar rúbrica
  rubrica <- reactive({
    infile2 <- input$file2
    import(infile2$datapath, sheet = "compilada")
  })

  
#Aplicar rúbrica
temp4 <- eventReactive(input$rubrica,{
  temp3 <- valores(temp2 = temp2(), rubrica = rubrica())
    temp3 %>%
    group_by(ID_solicitud,profesor) %>% 
    mutate(PTS_PROFESOR = sum(PTS_CONDICION,
                              PTS_ANTIGUEDAD,
                              #PTS_ESQUEMA,
                              PTS_DOCENCIA,
                              PTS_EXPERIENCIA)) %>% 
    relocate(semestre_grupo, .before = ID_solicitud) %>% 
    relocate(asignatura, .after = semestre_grupo) %>% 
    arrange(semestre_grupo, asignatura, ID_solicitud) %>% 
    select(-c(marca, fila, solicitud, rfc, curp, correo_unam, correo_pers, telf, id_gs))
    })

#Estimación puntos de cada solicitud por semestre y grupo
temp5 <- eventReactive(input$total,{
  temp4() %>% 
  group_by(semestre_grupo, asignatura, ID_solicitud, responsable) %>% 
  summarise(total = mean(PTS_PROFESOR)) %>%
  ungroup() %>% 
  group_by(semestre_grupo, asignatura) %>% 
  arrange(semestre_grupo, asignatura, desc(total)) %>% 
  mutate(adjudicacion = if_else(total == max(total), "VALIDAR POR CA", "CONDICIONADA")) %>% 
  mutate(diferencia = if_else(adjudicacion == "VALIDAR POR CA", 0, max(total)-total)) %>% 
  ungroup()
})

#Datos nómina

temp6 <- reactive({
  infile3 <- input$file3
  import(infile3$datapath, 
         col_types = c("text",
                       "text",
                       "text",
                       "text",
                       "numeric",
                       "text",
                       "numeric"))
})


temp7 <- eventReactive(input$adjudicar,{
  temp2() %>% 
    semi_join(temp6() %>% 
                filter(adjudicacion == "VALIDADA")) %>% 
    relocate(semestre_grupo, .before = ID_solicitud) %>%
    relocate(asignatura, .after = semestre_grupo) %>%
    arrange(semestre_grupo, asignatura, ID_solicitud) %>%
    select(ID_solicitud,semestre_grupo,asignatura,profesor,nombramiento,adscripcion,estudios,
           rfc,curp,num_unam,correo_unam,correo_pers,telf,horas_t,horas_p)
  
})
  

temp8 <- eventReactive(input$nomina,{
  nomina(temp7())
})  
  
  
#Desiertas

temp9 <- reactive({
  infile4 <- input$file4
  import(infile4$datapath)
})

temp10 <- eventReactive(input$desierto,{
  anti_join(temp9(), temp7())
})

##########    outputs  
output$tabla1 <- renderTable({
  temp2()
})

output$tabla2 <- renderTable({
  temp4()
})

output$tabla3 <- renderTable({
  temp5()
})

output$tabla4 <- renderTable({
  temp7()
})

output$tabla5 <- renderTable({
  temp8()
})

output$tabla6 <- renderTable({
  temp10()
})

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

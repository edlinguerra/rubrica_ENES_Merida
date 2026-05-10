# Paquetes requeridos
library(shiny)
library(vroom)
library(waiter)
library(DT)
library(dplyr)
library(tidyr)
library(shinyWidgets)
library(markdown)
library(rio)
library(stringr)
library(scholar)
library(purrr)
library(janitor)
library(openxlsx)
library(officer)
library(flextable)

# funciones preliminares --------------------------------------------------

normalizar_productos <- function(x) {
  
  x_chr <- as.character(x)
  x_chr <- stringr::str_squish(x_chr)
  x_chr <- stringr::str_to_lower(x_chr)
  x_chr <- chartr("áéíóú", "aeiou", x_chr)
  
  dplyr::case_when(
    is.na(x_chr) | x_chr == "" ~ NA_real_,
    stringr::str_detect(x_chr, "11\\s*o\\s*mas") ~ 11,
    stringr::str_detect(x_chr, "11\\+") ~ 11,
    TRUE ~ suppressWarnings(as.numeric(stringr::str_extract(x_chr, "[0-9]+")))
  )
}

leer_formulario <- function(df_raw) {
  
  # ============================================================
  # 1. LEER CSV (desactivado, solo para probar fuera de la app)
  # ============================================================
  
  #df_raw <- readr::read_csv(path_csv, show_col_types = FALSE)
  
  # ============================================================
  # 2. RENOMBRAR COLUMNAS USANDO PATRONES
  # ============================================================
  
  # Detectar todas las columnas que empiezan con "Elige la asignatura"
  cols_asignaturas <- grep("^Elige la asignatura", names(df_raw), value = TRUE)
  
  # Crear nombres nuevos: asignatura_1, asignatura_2, ...
  nuevos_nombres <- paste0("asignatura_", seq_along(cols_asignaturas))
  
  df <- df_raw |>
    rename(
      # Datos generales
      marca              = matches("^Marca temporal"),
      correo_respuesta   = matches("^Dirección de correo electrónico"),
      aplica_convocatoria= matches("^¿Quieres aplicar a la Convocatoria"),
      semestre_grupo     = matches("^Por favor, indica el semestre"),
      
      # Asignaturas
      !!!setNames(cols_asignaturas, nuevos_nombres),
      
      # PDFs generales
      pdf_solicitud      = matches("^Anexar solicitud"),
      pdf_plan_instr     = matches("^Anexar el plan instruccional"),
      pdf_practicas_campo= matches("^Anexar resumen de prácticas de campo"),
      pdf_practicas_lab  = matches("^Anexar resumen de prácticas de laboratorio"),
      
      # Profesor 1
      profesor_p1        = matches("Nombre completo.*\\.\\.\\.15$"),
      rfc_p1             = matches("RFC.*\\.\\.\\.16$"),
      curp_p1            = matches("CURP.*\\.\\.\\.17$"),
      num_trabajador_p1  = matches("Número de trabajador.*\\.\\.\\.18$"),
      correo_unam_p1     = matches("Correo UNAM.*\\.\\.\\.19$"),
      correo_personal_p1 = matches("Correo personal.*\\.\\.\\.20$"),
      telefono_p1        = matches("Número de teléfono.*\\.\\.\\.21$"),
      nombramiento_p1    = matches("Nombramiento.*\\.\\.\\.22$"),
      adscripcion_p1     = matches("Adscripción.*\\.\\.\\.23$"),
      horas_teoricas_p1  = matches("horas teóricas.*\\.\\.\\.24$"),
      horas_practicas_p1 = matches("horas prácticas.*\\.\\.\\.25$"),
      nivel_estudios_p1  = matches("Último nivel de estudios.*\\.\\.\\.26$"),
      perfil_form_p1     = matches("Perfil de formación profesional.*\\.\\.\\.27$"),
      antiguedad_p1      = matches("Antigüedad impartiendo.*\\.\\.\\.28$"),
      experiencia_similar_p1 = matches("Experiencia impartiendo.*\\.\\.\\.29$"),
      papime_resp_p1     = matches("Responsable de proyectos PAPIME.*\\.\\.\\.30$"),
      papime_part_p1     = matches("Participación en proyectos PAPIME.*\\.\\.\\.31$"),
      cursos_p1          = matches("Cursos de Actualización.*\\.\\.\\.32$"),
      inv_resp_p1        = matches("Responsable Técnico.*\\.\\.\\.33$"),
      inv_part_p1        = matches("Participante.*\\.\\.\\.34$"),
      cons_resp_p1       = matches("Consultoría.*\\.\\.\\.35$"),
      cons_part_p1       = matches("Consultoría.*\\.\\.\\.36$"),
      productos_p1       = matches("Productos académicos.*\\.\\.\\.37$"),
      pdf_cv_p1          = matches("Resumen curricular.*\\.\\.\\.38$"),
      
      # Profesor 2
      agrega_p2          = matches("^¿Agregará a un 2do profesor"),
      profesor_p2        = matches("Nombre completo.*\\.\\.\\.40$"),
      rfc_p2             = matches("RFC.*\\.\\.\\.41$"),
      curp_p2            = matches("CURP.*\\.\\.\\.42$"),
      num_trabajador_p2  = matches("Número de trabajador.*\\.\\.\\.43$"),
      correo_unam_p2     = matches("Correo UNAM.*\\.\\.\\.44$"),
      correo_personal_p2 = matches("Correo personal.*\\.\\.\\.45$"),
      telefono_p2        = matches("Número de teléfono.*\\.\\.\\.46$"),
      nombramiento_p2    = matches("Nombramiento.*\\.\\.\\.47$"),
      adscripcion_p2     = matches("Adscripción.*\\.\\.\\.48$"),
      horas_teoricas_p2  = matches("horas teóricas.*\\.\\.\\.49$"),
      horas_practicas_p2 = matches("horas prácticas.*\\.\\.\\.50$"),
      nivel_estudios_p2  = matches("Último nivel de estudios.*\\.\\.\\.51$"),
      perfil_form_p2     = matches("Perfil de formación profesional.*\\.\\.\\.52$"),
      antiguedad_p2      = matches("Antigüedad impartiendo.*\\.\\.\\.53$"),
      experiencia_similar_p2 = matches("Experiencia impartiendo.*\\.\\.\\.54$"),
      papime_resp_p2     = matches("PAPIME.*\\.\\.\\.55$"),
      papime_part_p2     = matches("PAPIME.*\\.\\.\\.56$"),
      cursos_p2          = matches("Cursos.*\\.\\.\\.57$"),
      inv_resp_p2        = matches("Responsable Técnico.*\\.\\.\\.58$"),
      inv_part_p2        = matches("Participante.*\\.\\.\\.59$"),
      cons_resp_p2       = matches("Consultoría.*\\.\\.\\.60$"),
      cons_part_p2       = matches("Consultoría.*\\.\\.\\.61$"),
      productos_p2       = matches("Productos.*\\.\\.\\.62$"),
      pdf_cv_p2          = matches("Resumen curricular.*\\.\\.\\.63$"),
      
      # Profesor 3
      agrega_p3          = matches("^¿Agregará a un tercer profesor"),
      profesor_p3        = matches("Nombre completo.*\\.\\.\\.65$"),
      rfc_p3             = matches("RFC.*\\.\\.\\.66$"),
      curp_p3            = matches("CURP.*\\.\\.\\.67$"),
      num_trabajador_p3  = matches("Número de trabajador.*\\.\\.\\.68$"),
      correo_unam_p3     = matches("Correo UNAM.*\\.\\.\\.69$"),
      correo_personal_p3 = matches("Correo personal.*\\.\\.\\.70$"),
      telefono_p3        = matches("Número de teléfono.*\\.\\.\\.71$"),
      nombramiento_p3    = matches("Nombramiento.*\\.\\.\\.72$"),
      adscripcion_p3     = matches("Adscripción.*\\.\\.\\.73$"),
      horas_teoricas_p3  = matches("horas teóricas.*\\.\\.\\.74$"),
      horas_practicas_p3 = matches("horas prácticas.*\\.\\.\\.75$"),
      nivel_estudios_p3  = matches("Último nivel de estudios.*\\.\\.\\.76$"),
      perfil_form_p3     = matches("Perfil de formación profesional.*\\.\\.\\.77$"),
      antiguedad_p3      = matches("Antigüedad impartiendo.*\\.\\.\\.78$"),
      experiencia_similar_p3 = matches("Experiencia impartiendo.*\\.\\.\\.79$"),
      papime_resp_p3     = matches("PAPIME.*\\.\\.\\.80$"),
      papime_part_p3     = matches("PAPIME.*\\.\\.\\.81$"),
      cursos_p3          = matches("Cursos.*\\.\\.\\.82$"),
      inv_resp_p3        = matches("Responsable Técnico.*\\.\\.\\.83$"),
      inv_part_p3        = matches("Participante.*\\.\\.\\.84$"),
      cons_resp_p3       = matches("Consultoría.*\\.\\.\\.85$"),
      cons_part_p3       = matches("Consultoría.*\\.\\.\\.86$"),
      productos_p3       = matches("Productos.*\\.\\.\\.87$"),
      pdf_cv_p3          = matches("Resumen curricular.*\\.\\.\\.88$")
    )
  
  # ============================================================
  # 3. TIPOS: fecha
  # ============================================================
  
  df <- df |>
    mutate(
      marca = lubridate::dmy_hms(marca, tz = "America/Merida"))
  
  # ============================================================
  # 4. PIVOTEAR ASIGNATURAS
  # ============================================================
  
  df <- df |>
    pivot_longer(
      cols = starts_with("asignatura_"),
      names_to = "variable",
      values_to = "asignatura"
    ) |>
    drop_na(asignatura) |>
    mutate(
      clv_materia = str_extract(asignatura, "^[0-9]{3,4}"),
      id_solicitud = as.character(row_number())
    )|>
    relocate(c(id_solicitud, variable, clv_materia, asignatura), .after = marca) 
  
  # ------------------------------------------------------------
  # 5. Pasar profesores p1, p2, p3 de formato ancho a largo
  # ------------------------------------------------------------
  
  # Identificar columnas de profesores
  cols_profesores <- grep("_p[123]$", names(df), value = TRUE)
  
  # Separar horas de columnas textuales
  cols_horas <- grep("horas_(teoricas|practicas)_p[123]$", names(df), value = TRUE)
  cols_texto <- setdiff(cols_profesores, cols_horas)
  
  # Homogeneizar tipos
  df <- df |>
    mutate(across(all_of(cols_texto), as.character)) |>
    mutate(across(all_of(cols_horas), ~as.numeric(.x)))
  
  # Pivotear profesores
  df <- df |>
    pivot_longer(
      cols = all_of(cols_profesores),
      names_to = c(".value", "profesor_id"),
      names_pattern = "^(.*)_p([123])$"
    ) |>
    mutate(
      profesor_id = paste0("p", profesor_id),
      clv_materia = str_extract(asignatura, "^[0-9]{3,4}")
    ) |>
    filter(!is.na(profesor), str_trim(profesor) != "")
  
  df <- df |>
    mutate(
      profesor = str_to_upper(profesor),
      productos_join = normalizar_productos(productos)
    )
  
  return(df)
}

valores <- function(temp2 = NULL, df = NULL, rubrica, catalogo_perfiles) {
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(janitor)
  
  # ============================================================
  # 0. NORMALIZAR ENTRADA
  # ============================================================
  # Esta versión acepta dos formas de entrada:
  # 1) temp2: salida del server, con profesores en columnas *_p1, *_p2, *_p3
  # 2) df: datos ya pivoteados a formato largo por profesor
  
  if (is.null(df)) {
    
    if (is.null(temp2)) {
      stop("Debes proporcionar `temp2` o `df` a la función valores().")
    }
    
    datos <- temp2
    
  } else {
    
    datos <- df
  }
  
  # ------------------------------------------------------------
  # 0B. Asegurar clave de materia
  # ------------------------------------------------------------
  
  datos <- datos |>
    dplyr::mutate(
      clv_materia = stringr::str_extract(asignatura, "^[0-9]{3,4}")
    )
  
  # ------------------------------------------------------------
  # 0C. Si los profesores siguen en formato ancho, pasarlos a largo
  # ------------------------------------------------------------
  
  if (!all(c("profesor", "profesor_id") %in% names(datos))) {
    
    cols_profesores <- grep("_p[123]$", names(datos), value = TRUE)
    
    if (length(cols_profesores) == 0) {
      stop(
        "No encuentro columnas de profesores con sufijos `_p1`, `_p2` o `_p3`. ",
        "Revisa la salida de `temp2()`."
      )
    }
    
    datos <- datos |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(cols_profesores),
          ~ dplyr::na_if(stringr::str_squish(as.character(.x)), "")
        )
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(cols_profesores),
        names_to = c(".value", "profesor_id"),
        names_pattern = "^(.*)_p([123])$"
      ) |>
      dplyr::mutate(
        profesor_id = paste0("p", profesor_id),
        clv_materia = stringr::str_extract(asignatura, "^[0-9]{3,4}")
      ) |>
      dplyr::filter(
        !is.na(profesor),
        stringr::str_trim(profesor) != ""
      )
  }
  
  # ------------------------------------------------------------
  # 0D. Crear variable auxiliar para productos
  # ------------------------------------------------------------
  
  if ("productos" %in% names(datos) && !"productos_join" %in% names(datos)) {
    datos <- datos |>
      dplyr::mutate(
        productos_join = normalizar_productos(productos)
      )
  }
  
  # ============================================================
  # 1. PREPARAR RÚBRICA
  # ============================================================
  
  rub_doc <- rubrica |> filter(tipo == "docencia")
  rub_ant <- rubrica |> filter(tipo == "antiguedad")
  rub_exp <- rubrica |> filter(tipo == "experiencia")
  rub_per <- rubrica |> filter(tipo == "perfiles")
  
  # ============================================================
  # 2. PREPARAR CATÁLOGO DE PERFILES
  # ============================================================
  
  cat_long <- catalogo_perfiles |>
    janitor::clean_names() |>
    tidyr::pivot_longer(
      cols = dplyr::any_of(c("fmt", "bq", "ms", "ce", "h", "ss", "af", "idt", "i")),
      names_to = "area_abbr",
      values_to = "nivel"
    ) |>
    dplyr::mutate(
      nivel = dplyr::if_else(is.na(nivel) | nivel == "", "INS", nivel),
      clave = stringr::str_extract(as.character(clave), "[0-9]{3,4}"),
      area_abbr = toupper(area_abbr)
    )
  
  # ============================================================
  # 3. PROCESAR PERFIL PROFESIONAL DEL PROFESOR
  # ============================================================
  
  datos <- datos |>
    mutate(
      perfil_form = replace_na(perfil_form, ""), 
      area1 = str_trim(str_split_fixed(perfil_form, ",", 2)[,1]),
      area2 = str_trim(str_split_fixed(perfil_form, ",", 2)[,2]),
      area1 = na_if(area1, ""),
      area2 = na_if(area2, ""),
      
      area1_abbr = recode(area1,
                          "Físico-Matemáticas y Ciencias de la Tierra" = "FMT",
                          "Biología y Química" = "BQ",
                          "Medicina y Ciencias de la Salud" = "MS",
                          "Ciencias de la Conducta y la Educación" = "CE",
                          "Humanidades" = "H",
                          "Ciencias Sociales" = "SS",
                          "Agricultura, Agropecuarias, Forestales, y de Ecosistemas" = "AF",
                          "Ingenierías y Desarrollo Tecnológico" = "IDT",
                          "Interdisciplinaria" = "I",
                          .default = NA_character_
      ),
      
      area2_abbr = recode(area2,
                          "Físico-Matemáticas y Ciencias de la Tierra" = "FMT",
                          "Biología y Química" = "BQ",
                          "Medicina y Ciencias de la Salud" = "MS",
                          "Ciencias de la Conducta y la Educación" = "CE",
                          "Humanidades" = "H",
                          "Ciencias Sociales" = "SS",
                          "Agricultura, Agropecuarias, Forestales, y de Ecosistemas" = "AF",
                          "Ingenierías y Desarrollo Tecnológico" = "IDT",
                          "Interdisciplinaria" = "I",
                          .default = NA_character_
      )
    )
  
  # ============================================================
  # 4. CALCULAR PERTINENCIA
  # ============================================================
  
  pertinencia <- datos |>
    select(id_solicitud, profesor_id, profesor, asignatura, clv_materia,
           area1_abbr, area2_abbr) |>
    pivot_longer(
      cols = c(area1_abbr, area2_abbr),
      names_to = "which",
      values_to = "area_abbr",
      values_drop_na = TRUE
    ) |>
    left_join(cat_long, by = c("clv_materia" = "clave", "area_abbr")) |>
    mutate(
      nivel = if_else(is.na(nivel), "INS", nivel),
      nivel = factor(nivel, levels = c("INS","PAR","MED","ALT","DIR"), ordered = TRUE)
    ) |>
    group_by(id_solicitud, profesor_id, profesor, asignatura) |>
    summarise(nivel_max = as.character(max(nivel)), .groups = "drop")
  
  datos <- datos |>
    left_join(pertinencia,
              by = c("id_solicitud", "profesor_id", "profesor","asignatura")) |> 
    mutate(nivel_max = replace_na(nivel_max, "INS")) |> 
    left_join(
      rub_per |> select(valor, puntos),
      by = c("nivel_max" = "valor")
    ) |> 
    rename(PTS_PERTINENCIA = puntos)
  
  # ============================================================
  # 5. APLICAR RÚBRICA
  # ============================================================
  
  mapear <- function(df, rub, col_df, var_rub, sufijo){
    
    rub_filtrada <- rub |>
      dplyr::filter(.data$variable %in% var_rub) |>
      dplyr::select(valor, puntos) |>
      dplyr::mutate(
        valor = stringr::str_squish(as.character(valor)),
        puntos = as.numeric(puntos)
      ) |>
      dplyr::distinct(valor, .keep_all = TRUE)
    
    df |>
      dplyr::mutate(
        "{col_df}" := stringr::str_squish(as.character(.data[[col_df]]))
      ) |>
      dplyr::left_join(
        rub_filtrada,
        by = stats::setNames("valor", col_df)
      ) |>
      dplyr::rename(!!paste0("PTS_", sufijo) := puntos)
  }
  
  mapear_productos <- function(df, rub, sufijo = "PRODUCTOS") {
    
    rub_filtrada <- rub |>
      filter(variable == "productos") |>
      mutate(
        valor_join = suppressWarnings(as.numeric(valor)),
        puntos = as.numeric(puntos)
      ) |>
      filter(!is.na(valor_join)) |>
      select(valor_join, puntos)
    
    df |>
      mutate(productos_join = normalizar_productos(productos)) |>
      left_join(rub_filtrada,
                by = c("productos_join" = "valor_join")) |>
      rename(!!paste0("PTS_", sufijo) := puntos)
  }
  
  datos <- datos |>
    mapear(
      rub = rub_doc,
      col_df = "nombramiento",
      var_rub = c("Nombramiento", "nombramiento"),
      sufijo = "NOMBRAMIENTO"
    ) |>
    mapear(
      rub = rub_doc,
      col_df = "adscripcion",
      var_rub = c("Adscripcion", "Adscripción", "adscripcion"),
      sufijo = "ADSCRIPCION"
    ) |>
    mapear(
      rub = rub_doc,
      col_df = "nivel_estudios",
      var_rub = c("Estudios", "Último nivel de estudios", "nivel_estudios", "estudios"),
      sufijo = "ESTUDIOS"
    ) |>
    mapear(
      rub = rub_doc,
      col_df = "papime_resp",
      var_rub = "papime_resp",
      sufijo = "PAPIME_RESP"
    ) |>
    mapear(
      rub = rub_doc,
      col_df = "papime_part",
      var_rub = "papime_part",
      sufijo = "PAPIME_PART"
    ) |>
    mapear(
      rub = rub_doc,
      col_df = "cursos",
      var_rub = "cursos",
      sufijo = "CURSOS"
    ) |>
    mapear(
      rub = rub_ant,
      col_df = "antiguedad",
      var_rub = "antiguedad",
      sufijo = "ANTIGUEDAD"
    ) |>
    mapear(
      rub = rub_ant,
      col_df = "experiencia_similar",
      var_rub = c("antiguedad_otro", "experiencia_similar"),
      sufijo = "ANTIG_OTRO"
    ) |>
    mapear(
      rub = rub_exp,
      col_df = "inv_resp",
      var_rub = "inv_resp",
      sufijo = "INV_RESP"
    ) |>
    mapear(
      rub = rub_exp,
      col_df = "inv_part",
      var_rub = "inv_part",
      sufijo = "INV_PART"
    ) |>
    mapear(
      rub = rub_exp,
      col_df = "cons_resp",
      var_rub = "cons_resp",
      sufijo = "CONS_RESP"
    ) |>
    mapear(
      rub = rub_exp,
      col_df = "cons_part",
      var_rub = "cons_part",
      sufijo = "CONS_PART"
    ) |>
    mapear_productos(
      rub = rub_exp,
      sufijo = "PRODUCTOS"
    )
  
  # ============================================================
  # 6. PUNTAJE POR PROFESOR
  # ============================================================
  
  datos <- datos |>
    mutate(
      PTS_DOCENCIA =
        rowSums(across(c(PTS_NOMBRAMIENTO, PTS_ADSCRIPCION, PTS_ESTUDIOS,
                         PTS_PAPIME_RESP, PTS_PAPIME_PART, PTS_CURSOS)),
                na.rm = TRUE),
      PTS_ANTIGUEDAD =
        rowSums(across(c(PTS_ANTIGUEDAD, PTS_ANTIG_OTRO)), na.rm = TRUE),
      PTS_EXPERIENCIA =
        rowSums(across(c(PTS_INV_RESP, PTS_INV_PART,
                         PTS_CONS_RESP, PTS_CONS_PART,
                         PTS_PRODUCTOS)), na.rm = TRUE),
      PTS_PROFESOR =
        PTS_DOCENCIA + PTS_ANTIGUEDAD + PTS_EXPERIENCIA + PTS_PERTINENCIA
    )
  
  # ============================================================
  # 7. PONDERACIÓN POR HORAS
  # ============================================================
  
  datos <- datos |>
    mutate(
      horas_teoricas = as.numeric(horas_teoricas),
      horas_practicas = as.numeric(horas_practicas),
      HORAS_PROFESOR = replace_na(horas_teoricas,0) +
        replace_na(horas_practicas,0)
    ) |>
    group_by(id_solicitud, asignatura) |>
    mutate(
      HORAS_TOTALES_ASIG = sum(HORAS_PROFESOR, na.rm = TRUE),
      PESO_PROFESOR = if_else(HORAS_TOTALES_ASIG > 0,
                              HORAS_PROFESOR / HORAS_TOTALES_ASIG,
                              0),
      PTS_PROFESOR_POND = PTS_PROFESOR * PESO_PROFESOR
    ) |>
    ungroup()
  
  # ============================================================
  # 8. PUNTAJE FINAL POR SOLICITUD
  # ============================================================
  
  temp_solicitud <- datos |>
    group_by(id_solicitud, asignatura, semestre_grupo) |>
    summarise(
      PTS_SOLICITUD = sum(PTS_PROFESOR_POND, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ============================================================
  # 9. SALIDA
  # ============================================================
  
  list(
    por_profesor  = datos,
    por_solicitud = temp_solicitud
  )
}

nomina <- function(x) {
  
  adjudicados <- x
  
  # ------------------------------------------------------------
  # Validación mínima de columnas esperadas
  # ------------------------------------------------------------
  
  cols_necesarias <- c(
    "semestre_grupo",
    "asignatura",
    "profesor",
    "nivel_estudios",
    "nombramiento",
    "rfc",
    "num_trabajador",
    "correo_unam",
    "correo_personal",
    "horas_teoricas",
    "horas_practicas"
  )
  
  cols_faltantes <- setdiff(cols_necesarias, names(adjudicados))
  
  if (length(cols_faltantes) > 0) {
    stop(
      "Faltan columnas necesarias para generar la nómina: ",
      paste(cols_faltantes, collapse = ", ")
    )
  }
  
  # ------------------------------------------------------------
  # Helper para convertir horas a numérico
  # ------------------------------------------------------------
  
  to_num <- function(x) {
    suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
  }
  
  # ------------------------------------------------------------
  # Tabla base
  # ------------------------------------------------------------
  
  yy <- tibble(
    "Semestre/Grupo" = str_remove(
      as.character(adjudicados$semestre_grupo),
      "LCA, semestre\\s*"
    ),
    "Asignatura" = adjudicados$asignatura,
    "Profesor(a)" = adjudicados$profesor,
    "estudios" = adjudicados$nivel_estudios,
    "Nombramiento" = recode(
      as.character(adjudicados$nombramiento),
      "UNAM - Profesor" = "Profesor Tiempo Completo",
      "Profesor de Asignatura" = "Profesor de Asignatura",
      "UNAM - Investigador" = "Investigador Tiempo Completo",
      "Profesor Honorífico" = "Profesor Honorífico",
      "UNAM - Técnico Académico" = "Técnico Académico",
      .default = as.character(adjudicados$nombramiento)
    ),
    "RFC" = adjudicados$rfc,
    "UNAM" = as.character(adjudicados$num_trabajador),
    "correo1" = adjudicados$correo_unam,
    "correo2" = adjudicados$correo_personal,
    "HT" = to_num(adjudicados$horas_teoricas),
    "HP" = to_num(adjudicados$horas_practicas)
  )
  
  # ------------------------------------------------------------
  # Formato requerido para nómina
  # ------------------------------------------------------------
  
  xx <- yy |>
    group_by(`Semestre/Grupo`, Asignatura, `Profesor(a)`) |>
    mutate(
      `Carga Horaria` = sum(HT + HP, na.rm = TRUE),
      `Horas Pagadas` = if_else(
        Nombramiento == "Profesor de Asignatura",
        true = `Carga Horaria`,
        false = 0
      ),
      correo = if_else(
        str_detect(
          coalesce(correo1, ""),
          regex("unam\\.mx", ignore_case = TRUE)
        ),
        correo1,
        correo2
      )
    ) |>
    ungroup() |>
    relocate(correo, .after = UNAM) |>
    relocate(`Carga Horaria`, .before = HT) |>
    select(-c(correo1, correo2)) |>
    pivot_longer(
      cols = estudios:correo,
      names_to = "info",
      values_to = "Datos"
    ) |>
    select(-info) |>
    relocate(Datos, .after = `Profesor(a)`)
  
  return(xx)
}

nomina_compacta <- function(x) {
  
  adjudicados <- x
  
  # ------------------------------------------------------------
  # Validación mínima
  # ------------------------------------------------------------
  
  cols_necesarias <- c(
    "semestre_grupo",
    "asignatura",
    "profesor",
    "nivel_estudios",
    "nombramiento",
    "rfc",
    "num_trabajador",
    "correo_unam",
    "correo_personal",
    "horas_teoricas",
    "horas_practicas"
  )
  
  cols_faltantes <- setdiff(cols_necesarias, names(adjudicados))
  
  if (length(cols_faltantes) > 0) {
    stop(
      "Faltan columnas necesarias para generar el documento de nómina: ",
      paste(cols_faltantes, collapse = ", ")
    )
  }
  
  # ------------------------------------------------------------
  # Helpers
  # ------------------------------------------------------------
  
  to_num <- function(x) {
    suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
  }
  
  limpiar_txt <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x[x %in% c("", "NA", "NaN", "NULL")] <- NA_character_
    x
  }
  
  colapsar_datos <- function(x) {
    x <- limpiar_txt(x)
    x <- x[!is.na(x)]
    paste(x, collapse = "\n")
  }
  
  recodificar_nombramiento <- function(x) {
    dplyr::recode(
      limpiar_txt(x),
      "UNAM - Profesor" = "Profesor Tiempo Completo",
      "Profesor de Asignatura" = "Profesor de Asignatura",
      "UNAM -  Investigador" = "Investigador Tiempo Completo",
      "UNAM - Investigador" = "Investigador Tiempo Completo",
      "Profesor Honorífico" = "Profesor Honorífico",
      "UNAM - Técnico Académico" = "Técnico Académico",
      .default = limpiar_txt(x)
    )
  }
  
  # ------------------------------------------------------------
  # Construir tabla compacta
  # ------------------------------------------------------------
  
  tabla <- adjudicados |>
    dplyr::mutate(
      semestre_grupo_out = stringr::str_remove(
        as.character(semestre_grupo),
        "LCA, semestre\\s*"
      ),
      asignatura_out = limpiar_txt(asignatura),
      profesor_out = limpiar_txt(profesor),
      estudios_out = limpiar_txt(nivel_estudios),
      nombramiento_out = recodificar_nombramiento(nombramiento),
      rfc_out = limpiar_txt(rfc),
      unam_out = limpiar_txt(num_trabajador),
      correo_unam_out = limpiar_txt(correo_unam),
      correo_personal_out = limpiar_txt(correo_personal),
      correo_out = dplyr::if_else(
        stringr::str_detect(
          dplyr::coalesce(correo_unam_out, ""),
          stringr::regex("unam\\.mx", ignore_case = TRUE)
        ),
        correo_unam_out,
        correo_personal_out
      ),
      HT = tidyr::replace_na(to_num(horas_teoricas), 0),
      HP = tidyr::replace_na(to_num(horas_practicas), 0)
    ) |>
    dplyr::group_by(
      semestre_grupo_out,
      asignatura_out,
      profesor_out,
      estudios_out,
      nombramiento_out,
      rfc_out,
      unam_out,
      correo_out
    ) |>
    dplyr::summarise(
      HT = sum(HT, na.rm = TRUE),
      HP = sum(HP, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `Carga Horaria` = HT + HP,
      `Horas Pagadas` = dplyr::if_else(
        nombramiento_out == "Profesor de Asignatura",
        `Carga Horaria`,
        0
      ),
      Datos = purrr::pmap_chr(
        list(
          estudios_out,
          nombramiento_out,
          rfc_out,
          unam_out,
          correo_out
        ),
        ~ colapsar_datos(c(...))
      )
    ) |>
    dplyr::transmute(
      `Semestre/Grupo` = semestre_grupo_out,
      Asignatura = asignatura_out,
      `Profesor(a)` = profesor_out,
      Datos = Datos,
      `Carga Horaria` = `Carga Horaria`,
      HT = HT,
      HP = HP,
      `Horas Pagadas` = `Horas Pagadas`
    ) |>
    dplyr::arrange(`Semestre/Grupo`, Asignatura, `Profesor(a)`)
  
  return(tabla)
}

crear_docx_nomina <- function(adjudicados, file) {
  
  tabla <- nomina_compacta(adjudicados)
  
  # ------------------------------------------------------------
  # Documento Word en orientación horizontal
  # ------------------------------------------------------------
  
  doc <- officer::read_docx()
  
  seccion_horizontal <- officer::prop_section(
    page_size = officer::page_size(orient = "landscape"),
    page_margins = officer::page_mar(
      top = 0.5,
      bottom = 0.5,
      left = 0.5,
      right = 0.5
    )
  )
  
  doc <- officer::body_set_default_section(
    doc,
    value = seccion_horizontal
  )
  
  doc <- officer::body_add_par(
    doc,
    "Datos para nómina",
    style = "heading 1"
  )
  
  doc <- officer::body_add_par(
    doc,
    paste0("Generado el ", format(Sys.Date(), "%d/%m/%Y")),
    style = "Normal"
  )
  
  doc <- officer::body_add_par(doc, "", style = "Normal")
  
  # ------------------------------------------------------------
  # Si no hay adjudicados
  # ------------------------------------------------------------
  
  if (nrow(tabla) == 0) {
    
    doc <- officer::body_add_par(
      doc,
      "No hay registros adjudicados para generar la nómina.",
      style = "Normal"
    )
    
    print(doc, target = file)
    return(invisible(file))
  }
  
  # ------------------------------------------------------------
  # Tabla Word
  # ------------------------------------------------------------
  
  ft <- flextable::flextable(tabla)
  
  filas_azules <- seq(2, nrow(tabla), by = 2)
  
  ft <- ft |>
    flextable::theme_vanilla() |>
    flextable::bg(i = filas_azules, bg = "#D9EAF7", part = "body") |>
    flextable::fontsize(size = 8, part = "all") |>
    flextable::fontsize(size = 8, part = "header") |>
    flextable::bold(part = "header") |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(
      j = c("Carga Horaria", "HT", "HP", "Horas Pagadas"),
      align = "center",
      part = "body"
    ) |>
    flextable::align(
      j = c("Semestre/Grupo", "Asignatura", "Profesor(a)", "Datos"),
      align = "left",
      part = "body"
    ) |>
    flextable::valign(valign = "top", part = "all") |>
    flextable::padding(padding = 3, part = "all") |>
    flextable::line_spacing(space = 1, part = "all") |>
    flextable::width(j = "Semestre/Grupo", width = 0.8) |>
    flextable::width(j = "Asignatura", width = 2.2) |>
    flextable::width(j = "Profesor(a)", width = 1.8) |>
    flextable::width(j = "Datos", width = 3.1) |>
    flextable::width(j = "Carga Horaria", width = 0.8) |>
    flextable::width(j = "HT", width = 0.5) |>
    flextable::width(j = "HP", width = 0.5) |>
    flextable::width(j = "Horas Pagadas", width = 0.8) |>
    flextable::set_table_properties(
      layout = "fixed",
      width = 1
    )
  
  ft <- ft |>
    flextable::border_outer(
      border = officer::fp_border(color = "#000000", width = 0.75),
      part = "all"
    ) |>
    flextable::border_inner_h(
      border = officer::fp_border(color = "#BFBFBF", width = 0.5),
      part = "all"
    ) |>
    flextable::border_inner_v(
      border = officer::fp_border(color = "#BFBFBF", width = 0.5),
      part = "all"
    )
  
  doc <- flextable::body_add_flextable(doc, ft)
  
  print(doc, target = file)
  
  invisible(file)
}

# ------------------------------------------------------------
# Formato para las tablas
# ------------------------------------------------------------

tabla_dt <- function(data, titulo = NULL, page_length = 10) {
  
  DT::datatable(
    data,
    rownames = FALSE,
    filter = "top",
    caption = if (!is.null(titulo)) {
      htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: bold; font-size: 16px; padding-bottom: 8px;",
        titulo
      )
    },
    options = list(
      pageLength = page_length,
      lengthMenu = list(
        c(10, 25, 50, 100, -1),
        c("10", "25", "50", "100", "Todos")
      ),
      scrollX = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      searching = TRUE,
      language = list(
        search = "Buscar:",
        lengthMenu = "Mostrar _MENU_ registros",
        info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
        infoEmpty = "Mostrando 0 a 0 de 0 registros",
        infoFiltered = "(filtrado de _MAX_ registros totales)",
        zeroRecords = "No se encontraron registros coincidentes",
        paginate = list(
          first = "Primero",
          previous = "Anterior",
          `next` = "Siguiente",
          last = "Último"
        )
      )
    )
  )
}

# User interface ----------------------------------------------------------

ui <- fluidPage(
  waiter::use_waiter(),
  
  tags$head(
    tags$style(HTML("
      .app-header {
        width: 100%;
        margin-bottom: 25px;
        padding: 10px 0 20px 0;
        border-bottom: 1px solid #e5e5e5;
      }

      .header-logos {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 25px;
        flex-wrap: wrap;
      }

      .header-logo-475 {
        max-width: 17%;
        height: auto;
      }

      .header-logo-enes {
        max-width: 17%;
        height: auto;
      }

      .app-title {
        margin-top: 18px;
        font-size: 26px;
        font-weight: 600;
        color: #002b5c;
        line-height: 1.25;
      }

      @media (max-width: 768px) {
        .header-logos {
          justify-content: center;
        }

        .header-logo-475,
        .header-logo-enes {
          max-width: 90%;
        }

        .app-title {
          text-align: center;
          font-size: 22px;
        }
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(p(strong("1. Preparación de datos")),
                fileInput("file", "Cargar formulario", accept = ".csv"),
                actionButton("tidy", "Ordenar solicitudes")
      ),
      wellPanel(
        p(strong("2. Aplicación de rúbrica")),
        fileInput("file2", label = "Cargar rúbrica", accept = ".xlsx"),
        fileInput("file_cat", label = "Cargar catálogo", accept = ".xlsx"),
        actionButton("rubrica", "Aplicar rúbrica"),
        actionButton("total", "Preseleccionar")
      ),
      wellPanel(
        p(strong("3. Datos para nómina")),
        fileInput("file3", label = "Cargar validaciones", accept = ".csv"),
        actionButton("adjudicar", "Adjudicados"),
        actionButton("nomina", "Datos para nómina")
      ),
      wellPanel(
        p(strong("4. Asignaturas desiertas")),
        #fileInput("file4", label = "Cargar catálogo de la licenciatura", accept = ".xlsx"),
        actionButton("desierto", "Identificar desiertas")
      )
    ),
    
    mainPanel(
      
      div(
        class = "app-header",
        div(
          class = "header-logos",
          tags$img(
            src = "475_fondo_blanco.jpeg",
            class = "header-logo-475",
            alt = "475 aniversario Universidad de México"
          ),
          tags$img(
            src = "ENES_Merida.jpg.jpeg",
            class = "header-logo-enes",
            alt = "UNAM ENES Mérida"
          )
        ),
        div(
          class = "app-title",
          "Preselección de postulaciones a docencia ENES Mérida"
        )
      ),
      
      tabsetPanel(
        tabPanel(
          "Info", 
          includeMarkdown("intro.md")
        ),
        tabPanel(
          "Postulaciones ordenadas", 
          DTOutput("tabla1"),
          downloadButton("descarga0", label = "Descarga")
        ),
        tabPanel(
          "Postulaciones valoradas en detalle",
          DTOutput("tabla2"),
          downloadButton("descarga1", label = "Descarga")
        ),
        tabPanel(
          "Preselecciones por validar",
          DTOutput("tabla3"),
          downloadButton("descarga2", label = "Descarga")
        ),
        tabPanel(
          "Adjudicados",
          DTOutput("tabla4"),
          downloadButton("descarga3", label = "Descarga")
        ),
        tabPanel(
          "Nómina",
          DTOutput("tabla5"),
          br(),
          # downloadButton("descarga4", label = "Descargar CSV"),
          downloadButton("descarga4_docx", label = "Descargar Word")
        ),
        tabPanel(
          "Desiertas",
          DTOutput("tabla6"),
          downloadButton("descarga5", label = "Descarga")
        )
      )
    )
  )
)



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 1. Cargar formulario sin procesar
  temp1 <- reactive({
    infile <- input$file
    req(infile)
    
    raw_text <- readr::read_file(infile$datapath)
    
    readr::read_csv(
      raw_text,
      col_types = readr::cols(.default = "c")
    )
  })
  
  
  # 2. Modificar formulario
  temp2 <- eventReactive(input$tidy, {
    withProgress(message = 'Ordenando solicitudes...', value = 0, {
     leer_formulario(temp1())
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
  
  # 5. Aplicar rúbrica + pertinencia -----------------------------------------
  # valores() devuelve una lista:
  #   $por_profesor
  #   $por_solicitud
  
  valoracion <- eventReactive(input$rubrica, {
    
    req(temp2())
    req(rubrica())
    req(catalogo())
    
    withProgress(message = "Aplicando rúbrica y calculando puntajes...", value = 0, {
      
      incProgress(0.3, detail = "Evaluando profesores")
      
      out <- valores(
        df = temp2(),
        rubrica = rubrica(),
        catalogo_perfiles = catalogo()
      )
      
      incProgress(0.7, detail = "Preparando resultados")
      
      out
    })
  })
  
  
  # 5A. Valoración por profesor ----------------------------------------------
  # Esta tabla usa directamente valores()$por_profesor.
  # Aquí solo se reordenan columnas y se eliminan columnas auxiliares.
  
  temp4 <- reactive({
    
    req(valoracion())
    
    valoracion()$por_profesor |>
      dplyr::ungroup() |>
      dplyr::relocate(semestre_grupo, .before = id_solicitud) |>
      dplyr::relocate(asignatura, .after = semestre_grupo) |>
      dplyr::arrange(
        semestre_grupo,
        asignatura,
        id_solicitud,
        profesor_id
      ) |>
      dplyr::select(
        -dplyr::any_of(c(
          "marca",
          "fila",
          "num_asignatura"
        ))
      )
  })
  
  
  # 6. Estimación/adjudicación de cada solicitud ------------------------------

  temp5 <- eventReactive(input$total, {
    
    req(valoracion())
    
    valoracion()$por_solicitud |>
      dplyr::ungroup() |>
      
      # 1. Insertar columna vacía para el CA
      dplyr::mutate(
        Pts_opinionCA = NA_real_
      ) |>
      
      # 2. total = PTS_SOLICITUD (sin sumar opinión del CA)
      dplyr::mutate(
        total = PTS_SOLICITUD
      ) |>
      
      # 3. Ordenar y adjudicar como antes
      dplyr::group_by(semestre_grupo, asignatura) |>
      dplyr::arrange(
        semestre_grupo,
        asignatura,
        dplyr::desc(total),
        .by_group = TRUE
      ) |>
      dplyr::mutate(
        puntaje_max = if (all(is.na(total))) {
          NA_real_
        } else {
          max(total, na.rm = TRUE)
        },
        n_max = sum(total == puntaje_max, na.rm = TRUE),
        adjudicacion = dplyr::case_when(
          is.na(total) ~ "REVISAR",
          total == puntaje_max & n_max == 1 ~ "VALIDAR POR CA",
          total == puntaje_max & n_max > 1 ~ "EMPATE - VALIDAR POR CA",
          TRUE ~ "CONDICIONADA"
        ),
        diferencia = dplyr::if_else(
          adjudicacion == "VALIDAR POR CA",
          0,
          puntaje_max - total
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-puntaje_max)
  })
  
  # 7. <- nómina (igual que antes)
  temp6 <- reactive({
    infile3 <- input$file3
    import(infile3$datapath, 
           col_types = c("text","text","text","text","numeric","text","numeric"))
  })
  
  temp7 <- eventReactive(input$adjudicar, {
    
    req(temp4())
    req(temp6())
    
    adjudicaciones_validas <- temp6() |>
      janitor::clean_names() |>
      mutate(
        id_solicitud = as.character(id_solicitud),
        semestre_grupo = as.character(semestre_grupo),
        asignatura = as.character(asignatura)
      ) |>
      filter(adjudicacion == "VALIDADA")
    
    temp4() |>
      mutate(
        id_solicitud = as.character(id_solicitud),
        semestre_grupo = as.character(semestre_grupo),
        asignatura = as.character(asignatura)
      ) |>
      semi_join(
        adjudicaciones_validas,
        by = c("id_solicitud", "semestre_grupo", "asignatura")
      ) |>
      relocate(semestre_grupo, .before = id_solicitud) |>
      relocate(asignatura, .after = semestre_grupo) |>
      arrange(semestre_grupo, asignatura, id_solicitud, profesor_id) |>
      select(
        any_of(c(
          "id_solicitud",
          "semestre_grupo",
          "asignatura",
          "profesor_id",
          "profesor",
          "nombramiento",
          "adscripcion",
          "nivel_estudios",
          "rfc",
          "curp",
          "num_trabajador",
          "correo_unam",
          "correo_personal",
          "telefono",
          "horas_teoricas",
          "horas_practicas",
          "PTS_PROFESOR",
          "PTS_PROFESOR_POND"
        ))
      )
  })
  
  temp8 <- eventReactive(input$nomina, {
    nomina(temp7())
  })
  
  # 8. Desiertas -------------------------------------------------------------
  temp9 <- reactive({
    
    req(input$file_cat)
    
    infile_cat <- input$file_cat
    
    import(infile_cat$datapath, sheet = "catalogo") |>
      janitor::clean_names() |>
      mutate(
        semestre_grupo = stringr::str_squish(as.character(semestre_grupo)),
        asignatura = stringr::str_squish(as.character(asignatura))
      ) |>
      distinct(semestre_grupo, asignatura, .keep_all = TRUE)
  })
  
  # temp9 <- reactive({
  #   
  #   req(input$file4)
  #   
  #   infile4 <- input$file4
  #   
  #   import(infile4$datapath) |>
  #     janitor::clean_names() |>
  #     mutate(
  #       semestre_grupo = stringr::str_squish(as.character(semestre_grupo)),
  #       asignatura = stringr::str_squish(as.character(asignatura))
  #     ) |>
  #     distinct(semestre_grupo, asignatura, .keep_all = TRUE)
  # })
  
  temp10 <- eventReactive(input$desierto, {
    
    req(temp9())
    req(temp7())
    
    catalogo_licenciatura <- temp9() |>
      mutate(
        semestre_grupo = stringr::str_squish(as.character(semestre_grupo)),
        asignatura = stringr::str_squish(as.character(asignatura))
      )
    
    asignaturas_adjudicadas <- temp7() |>
      mutate(
        semestre_grupo = stringr::str_squish(as.character(semestre_grupo)),
        asignatura = stringr::str_squish(as.character(asignatura))
      ) |>
      distinct(semestre_grupo, asignatura)
    
    anti_join(
      catalogo_licenciatura,
      asignaturas_adjudicadas,
      by = c("semestre_grupo", "asignatura")
    )
  })
  
  # temp10 <- eventReactive(input$desierto, {
  #   
  #   req(temp9())
  #   req(temp7())
  #   
  #   catalogo_licenciatura <- temp9() |>
  #     mutate(
  #       semestre_grupo = stringr::str_squish(as.character(semestre_grupo)),
  #       asignatura = stringr::str_squish(as.character(asignatura))
  #     )
  #   
  #   asignaturas_adjudicadas <- temp7() |>
  #     mutate(
  #       semestre_grupo = stringr::str_squish(as.character(semestre_grupo)),
  #       asignatura = stringr::str_squish(as.character(asignatura))
  #     ) |>
  #     distinct(semestre_grupo, asignatura)
  #   
  #   anti_join(
  #     catalogo_licenciatura,
  #     asignaturas_adjudicadas,
  #     by = c("semestre_grupo", "asignatura")
  #   )
  # })
  
  #----------------- outputs DT ----------------------------------------------------
  
  output$tabla1 <- DT::renderDT({
    req(temp2())
    tabla_dt(temp2(), "Postulaciones ordenadas", page_length = 10)
  }, server = TRUE)
  
  output$tabla2 <- DT::renderDT({
    req(temp4())
    tabla_dt(temp4(), "Postulaciones valoradas en detalle", page_length = 10)
  }, server = TRUE)
  
  output$tabla3 <- DT::renderDT({
    req(temp5())
    tabla_dt(temp5(), "Preselecciones por validar", page_length = 10)
  }, server = TRUE)
  
  output$tabla4 <- DT::renderDT({
    req(temp7())
    tabla_dt(temp7(), "Adjudicados", page_length = 10)
  }, server = TRUE)
  
  output$tabla5 <- DT::renderDT({
    req(temp8())
    tabla_dt(temp8(), "Datos para nómina", page_length = 10)
  }, server = TRUE)
  
  output$tabla6 <- DT::renderDT({
    req(temp10())
    tabla_dt(temp10(), "Asignaturas desiertas", page_length = 10)
  }, server = TRUE)
  
  # descargas (ajustadas a los nuevos objetos)
  output$descarga0 <- downloadHandler(
    filename = "convocatorias.csv",
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
  
  output$descarga4_docx <- downloadHandler(
    filename = function() {
      paste0("datos_nomina_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content = function(file) {
      
      req(temp7())
      
      tryCatch(
        {
          crear_docx_nomina(
            adjudicados = temp7(),
            file = file
          )
          
          if (!file.exists(file)) {
            stop("La función crear_docx_nomina() terminó, pero no creó el archivo esperado.")
          }
          
          if (file.info(file)$size == 0) {
            stop("La función crear_docx_nomina() creó un archivo vacío.")
          }
        },
        error = function(e) {
          message("ERROR al generar el DOCX de nómina: ", conditionMessage(e))
          showNotification(
            paste("Error al generar el Word:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
          stop(e)
        }
      )
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

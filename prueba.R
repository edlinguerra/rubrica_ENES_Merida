# Prueba

# carga un formulario, rubrica y catálogo

#Aplica las líneas de la función valores para detectar errores e incosistencias con los archivos

temp1 <- rio::import("./data/Convocatoria_Docente_ LCA-2027-1 (respuestas).xlsx", col_types = "text") 
  
rubrica <- rio::import("./data/Rubricas_2027-1.xlsx", sheet = "compilada")
  
catalogo_perfiles <- rio::import("./data/CATALOGO_LCA-ENES-MERIDA.xlsx", sheet = "perfiles")

temp2 <- temp1 |> 
  mutate(id_solicitud = as.character(1:nrow(temp1))) |>
  relocate(id_solicitud, .before = `Marca temporal`) |>
  # ajusta nombres de columnas según tu archivo actual
  dplyr::rename(
    marca = `Marca temporal`,
    correo_respuesta = `Dirección de correo electrónico`,
    aplica_convocatoria = `¿Quieres aplicar a la Convocatoria?`,
    semestre_grupo = `Por favor, indica el semestre y grupo en el que solicitas impartir la asignatura`,
    asignatura_1 = `Elige la asignatura`,
    asignatura_2 = `Elige la asignatura 2`,
    asignatura_3 = `Elige la asignatura 3`,
    asignatura_4 = `Elige la asignatura 4`,
    asignatura_5 = `Elige la asignatura 5`,
    asignatura_6 = `Elige la asignatura 6`,
    pdf_solicitud = `Anexar solicitud de la asignatura en formato PDF dirigida al Dr. David Romero, Coordinador de la Licenciatura en Ciencias Ambientales de la ENES Mérida.`,
    pdf_plan_instr = `Anexar el plan instruccional de la asignatura definiendo los objetivos,  la planeación del programa a impartir (en caso de plantear modificaciones con respecto al plan original),  estrategia de enseñanza, criterios de evaluación y bibliografía sugerida sobre el contenido de la asignatura.`,
    pdf_practicas_campo = `Anexar resumen de prácticas de campo`,
    pdf_practicas_lab = `Anexar resumen de prácticas de laboratorio`,
    
    profesor_p1 = `Nombre completo (sin grados académicos ni abreviaturas)`,
    rfc_p1 = `RFC`,
    curp_p1 = `CURP`,
    num_trabajador_p1 = `Número de trabajador UNAM (utilice 0 si no aplica)`,
    correo_unam_p1 = `Correo UNAM (Use el personal en caso de no contar)`,
    correo_personal_p1 = `Correo personal`,
    telefono_p1 = `Número de teléfono`,
    nombramiento_p1 = `Nombramiento`,
    adscripcion_p1 = `Adscripción`,
    horas_teoricas_p1 = `Carga horaria semanal, horas teóricas`,
    horas_practicas_p1 = `Carga horaria semanal, horas prácticas (elegir 0 si en el temario de la asignatura se indica como tipo teórica)`,
    nivel_estudios_p1 = `Último nivel de estudios`,
    perfil_form_p1 = `Perfil de formación profesional`,
    antiguedad_p1 = `Antigüedad impartiendo esta asignatura en esta licenciatura`,
    experiencia_similar_p1 = `Experiencia impartiendo asignaturas similares en otros programas de licenciatura`,
    papime_resp_p1 = `Responsable de proyectos PAPIME en la enseñanza de esta asignatura o asignatura afín`,
    papime_part_p1 = `Participación en proyectos PAPIME en la enseñanza de esta asignatura o asignatura afín`,
    cursos_p1 = `Número de Cursos de Actualización Docente con duración superior a 20 horas en los últimos 5 años`,
    inv_resp_p1 = `Proyectos de Investigación con financiamiento en el campo de conocimiento de la asignatura, de los que ha sido Responsable Técnico o Coordinador:`,
    inv_part_p1 = `Proyectos de Investigación con financiamiento en el campo de conocimiento de la asignatura, de los que ha sido Participante:`,
    cons_resp_p1 = `Proyectos de Servicio o Consultoría en el campo de conocimiento de la asignatura, de los que ha sido Responsable Técnico o Coordinador:`,
    cons_part_p1 = `Proyectos de Servicio o Consultoría en el campo de conocimiento de la asignatura, de los que ha sido Participante:`,
    productos_p1 = `Productos académicos, técnicos o profesionales relevantes para la asignatura`,
    pdf_cv_p1 = `Resumen curricular actualizado en PDF`,
    
    agrega_p2 = `¿Agregará a un 2do profesor?`,
    profesor_p2 = `Nombre completo (sin grados académicos ni abreviaturas) 2`,
    rfc_p2 = `RFC 2`,
    curp_p2 = `CURP 2`,
    num_trabajador_p2 = `Número de trabajador UNAM (utilice 0 si no aplica) 2`,
    correo_unam_p2 = `Correo UNAM (Use el personal en caso de no contar) 2`,
    correo_personal_p2 = `Correo personal 2`,
    telefono_p2 = `Número de teléfono 2`,
    nombramiento_p2 = `Nombramiento 2`,
    adscripcion_p2 = `Adscripción 2`,
    horas_teoricas_p2 = `Carga horaria semanal, horas teóricas 2`,
    horas_practicas_p2 = `Carga horaria semanal, horas prácticas (elegir 0 si en el temario de la asignatura se indica como tipo teórica) 2`,
    nivel_estudios_p2 = `Último nivel de estudios 2`,
    perfil_form_p2 = `Perfil de formación profesional 2`,
    antiguedad_p2 = `Antigüedad impartiendo esta asignatura en esta licenciatura 2`,
    experiencia_similar_p2 = `Experiencia impartiendo asignaturas similares en otros programas de licenciatura 2`,
    papime_resp_p2 = `Responsable de proyectos PAPIME en la enseñanza de esta asignatura o asignatura afín 2`,
    papime_part_p2 = `Participación en proyectos PAPIME en la enseñanza de esta asignatura o asignatura afín 2`,
    cursos_p2 = `Número de Cursos de Actualización Docente con duración superior a 20 horas en los últimos 5 años 2`,
    inv_resp_p2 = `Proyectos de Investigación con financiamiento en el campo de conocimiento de la asignatura, de los que ha sido Responsable Técnico o Coordinador: 2`,
    inv_part_p2 = `Proyectos de Investigación con financiamiento en el campo de conocimiento de la asignatura, de los que ha sido Participante: 2`,
    cons_resp_p2 = `Proyectos de Servicio o Consultoría en el campo de conocimiento de la asignatura, de los que ha sido Responsable Técnico o Coordinador: 2`,
    cons_part_p2 = `Proyectos de Servicio o Consultoría en el campo de conocimiento de la asignatura, de los que ha sido Participante: 2`,
    productos_p2 = `Productos académicos, técnicos o profesionales relevantes para la asignatura  2`,
    pdf_cv_p2 = `Resumen curricular actualizado en PDF 2`,
    
    agrega_p3 = `¿Agregará a un tercer profesor?`,
    profesor_p3 = `Nombre completo (sin grados académicos ni abreviaturas) 3`,
    rfc_p3 = `RFC 3`,
    curp_p3 = `CURP 3`,
    num_trabajador_p3 = `Número de trabajador UNAM (utilice 0 si no aplica) 3`,
    correo_unam_p3 = `Correo UNAM (Use el personal en caso de no contar) 3`,
    correo_personal_p3 = `Correo personal 3`,
    telefono_p3 = `Número de teléfono 3`,
    nombramiento_p3 = `Nombramiento 3`,
    adscripcion_p3 = `Adscripción 3`,
    horas_teoricas_p3 = `Carga horaria semanal, horas teóricas 3`,
    horas_practicas_p3 = `Carga horaria semanal, horas prácticas (elegir 0 si en el temario de la asignatura se indica como tipo teórica) 3`,
    nivel_estudios_p3 = `Último nivel de estudios 3`,
    perfil_form_p3 = `Perfil de formación profesional 3`,
    antiguedad_p3 = `Antigüedad impartiendo esta asignatura en esta licenciatura 3`,
    experiencia_similar_p3 = `Experiencia impartiendo asignaturas similares en otros programas de licenciatura 3`,
    papime_resp_p3 = `Responsable de proyectos PAPIME en la enseñanza de esta asignatura o asignatura afín 3`,
    papime_part_p3 = `Participación en proyectos PAPIME en la enseñanza de esta asignatura o asignatura afín 3`,
    cursos_p3 = `Número de Cursos de Actualización Docente con duración superior a 20 horas en los últimos 5 años 3`,
    inv_resp_p3 = `Proyectos de Investigación con financiamiento en el campo de conocimiento de la asignatura, de los que ha sido Responsable Técnico o Coordinador: 3`,
    inv_part_p3 = `Proyectos de Investigación con financiamiento en el campo de conocimiento de la asignatura, de los que ha sido Participante: 3`,
    cons_resp_p3 = `Proyectos de Servicio o Consultoría en el campo de conocimiento de la asignatura, de los que ha sido Responsable Técnico o Coordinador: 3`,
    cons_part_p3 = `Proyectos de Servicio o Consultoría en el campo de conocimiento de la asignatura, de los que ha sido Participante: 3`,
    productos_p3 = `Productos académicos, técnicos o profesionales relevantes para la asignatura  3`,
    pdf_cv_p3 = `Resumen curricular actualizado en PDF 3`
  ) |>
  pivot_longer(
    cols = starts_with("asignatura_"),
    names_to  = "variable",
    values_to = "asignatura"
  ) |>
  drop_na(asignatura) |>
  group_by(marca, semestre_grupo, variable) |>
  mutate(fila = row_number()) |>
  ungroup()


# ============================================================
# 0. NORMALIZAR NOMBRES DEL FORMULARIO
# ============================================================

datos <- temp2 |>
  mutate(
    clv_materia = str_extract(asignatura, "^[0-9]{3,4}")
  )

# ------------------------------------------------------------
# 0A. Función para normalizar productos de experiencia a 11 o más
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 0B. Pasar profesores p1, p2, p3 de formato ancho a largo
# ------------------------------------------------------------

cols_profesores <- grep("_p[123]$", names(datos), value = TRUE)

datos <- datos |>
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

datos <- datos |>
  mutate(
    productos_join = normalizar_productos(productos)
  )

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
  clean_names() |>
  pivot_longer(
    cols = any_of(c("fmt", "bq", "ms", "ce", "h", "ss", "af", "idt", "i")),
    names_to = "area_abbr",
    values_to = "nivel"
  ) |>
  mutate(
    nivel = if_else(is.na(nivel) | nivel == "", "INS", nivel),
    clave = sub("^.", "", clave),
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
# 5. APLICAR RÚBRICA A TODAS LAS VARIABLES
# ============================================================

mapear <- function(df, rub, col_df, var_rub, sufijo){
  rub_filtrada <- rub |>
    filter(.data$variable %in% var_rub) |>
    select(valor, puntos) |>
    mutate(puntos = as.numeric(puntos))
  
  df |>
    left_join(
      rub_filtrada,
      by = stats::setNames("valor", col_df)
    ) |>
    rename(!!paste0("PTS_", sufijo) := puntos)
}

mapear_productos <- function(df, rub, sufijo = "PRODUCTOS") {
  
  rub_filtrada <- rub |>
    dplyr::filter(.data$variable == "productos") |>
    dplyr::select(valor, puntos) |>
    dplyr::mutate(
      valor_join = suppressWarnings(as.numeric(as.character(valor))),
      puntos = as.numeric(puntos)
    ) |>
    dplyr::filter(!is.na(valor_join)) |>
    dplyr::select(valor_join, puntos)
  
  df |>
    dplyr::left_join(
      rub_filtrada,
      by = c("productos_join" = "valor_join")
    ) |>
    dplyr::rename(!!paste0("PTS_", sufijo) := puntos)
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
# 6. PUNTAJE POR PROFESOR (sin ponderar)
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
# 7. PONDERACIÓN POR HORAS (CORRECTA)
# ============================================================

to_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
}

datos <- datos |>
  mutate(
    horas_teoricas = replace_na(to_num(horas_teoricas), 0),
    horas_practicas = replace_na(to_num(horas_practicas), 0),
    HORAS_PROFESOR = horas_teoricas + horas_practicas
  ) |>
  group_by(id_solicitud, asignatura) |>
  mutate(
    HORAS_TOTALES_ASIG = sum(HORAS_PROFESOR, na.rm = TRUE),
    PESO_PROFESOR = if_else(
      HORAS_TOTALES_ASIG > 0,
      HORAS_PROFESOR / HORAS_TOTALES_ASIG,
      0
    ),
    PTS_PROFESOR_POND = PTS_PROFESOR * PESO_PROFESOR
  ) |>
  ungroup()


# ============================================================
# 8. PUNTAJE FINAL DE LA SOLICITUD
# ============================================================

temp_solicitud <- datos |>
  group_by(id_solicitud, asignatura, semestre_grupo) |>
  summarise(
    PTS_SOLICITUD = sum(PTS_PROFESOR_POND, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 9. DEVOLVER AMBOS NIVELES
# ============================================================

valoracion <- list(
  por_profesor  = datos,
  por_solicitud = temp_solicitud
)

# ============================================================
# 10. VALORACION POR PROFESOR
# ============================================================

temp4 <- valoracion$por_profesor |>
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

# ============================================================
# 10. VALORACION POR SOLICITUD
# ============================================================

temp5 <- valoracion$por_solicitud |>
  dplyr::ungroup() |>
  dplyr::mutate(
    total = PTS_SOLICITUD
  ) |>
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

# ============================================================
# 11. FUNCIÓN PARA CALCULO NOMINA
# ============================================================

nomina <- function(x) {
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  
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
      "UNAM -  Investigador" = "Investigador Tiempo Completo",
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

# ============================================================
# 12. LEE ADJUDICACIONES VALIDAS Y MANDA A NOMINA
# ============================================================


temp6 <- import("./data/valoracion_solicitudes.csv", col_types = c("text","text","text","text","numeric","text","numeric"))

adjudicaciones_validas <- temp6 |> 
  janitor::clean_names() |>
  mutate(
    id_solicitud = as.character(id_solicitud),
    semestre_grupo = as.character(semestre_grupo),
    asignatura = as.character(asignatura)
  ) |>
  filter(adjudicacion == "VALIDADA")

temp7 <- temp4 |>
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

temp8 <- nomina(temp7)

# ============================================================
# 13. PREPARA LISTA ASIGNATURAS DESIERTAS
# ============================================================

temp9 <- import("./data/Descargas/desiertas.xlsx")|>
  janitor::clean_names() |>
  mutate(
    semestre_grupo = stringr::str_squish(as.character(semestre_grupo)),
    asignatura = stringr::str_squish(as.character(asignatura))
  ) |>
  distinct(semestre_grupo, asignatura, .keep_all = TRUE)

catalogo_licenciatura <- temp9 |>
  mutate(
    semestre_grupo = stringr::str_squish(as.character(semestre_grupo)),
    asignatura = stringr::str_squish(as.character(asignatura))
  )

asignaturas_adjudicadas <- temp7 |>
  mutate(
    semestre_grupo = stringr::str_squish(as.character(semestre_grupo)),
    asignatura = stringr::str_squish(as.character(asignatura))
  ) |>
  distinct(semestre_grupo, asignatura)

temp10 <- anti_join(
  catalogo_licenciatura,
  asignaturas_adjudicadas,
  by = c("semestre_grupo", "asignatura")
  )



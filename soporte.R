# Librerías ----
pacman::p_load(tidyverse, funModeling, googlesheets4, gargle, gt, 
               extrafont, scales, ggalt, kableExtra, wordcloud, networkD3,
               data.table, ggeconodist)


# Datos --------

## 2021 ----

# Encuesta
kiwi21 <- read_sheet("1nhpqDWuJoWhiVj0rIaV51-SdfnSxTpbV3Vcd3iYmyTw") 


# Tipo de cambio 2021
tc <- read_sheet("194DbwO2TNmYkWU5Ru1m6UxuOCfXxuRlFcrWrP_JzGv8") %>% 
  select(pais, tipo_cambio)

## 2020 ----
kiwi20 <- read_sheet("1aeuu9dVfN42EjyvbmhEcsf0ilSz2DiXU-0MpnF896ss")
tipo_cambio <- read_sheet("1tEc4-_gXJi4lJ_Bj_ysleC-O01jiVOumOQCxGQ1tShM") %>% 
  select(pais, tipo_cambio)


# Configuraciones generales ----

options(scipen = 999)   # Modifica la visualización de los ejes numérico a valores nominales

loadfonts(quiet = TRUE) # Permite cargar en R otros tipos de fuentes.

# Estilo limpio sin líneas de fondo
estilo <- theme(panel.grid = element_blank(),
                plot.background = element_rect(fill = "#FBFCFC"),
                panel.background = element_blank(),
                text = element_text(family = "Roboto"))

# Estilo limpio con líneas de referencia verticales en gris claro
estilov <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.x = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto"))

# Estilo limpio con líneas de referencia horizontales en gris claro
estiloh <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.y = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto"))

genero <- c("#8624F5", "#1FC3AA", "#FFD129", "#75838F") #Violeta - Verde - Amarillo - Gris
genero3 <- c("#8624F5","#FFD129", "#1FC3AA")

colores <-  c("#8624F5", "#1FC3AA")

azul <- "#344D7E"
verde <-  "#4A9FC7"
rosa1 <- "#B95192"
rosa2 <- "#EE5777"
naranja <- "#FF764C"
amarillo <- "#FFA600"
gris <- "#75838F"
lila <- "#755395"
rojo <- "#943126"

col4 <- c(azul, lila, rosa1, rosa2)
col5 <- c(azul, lila, rosa1, rosa2, naranja)
col6 <- c(azul, lila, rosa1, rosa2, naranja, amarillo)

# Creo un objeto con un texto que se va a repetir mucho a lo largo del análisis
fuente <- "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam 2021"

# Creo objetos para formatear las etiquetas numéricas de los ejes x e y
eje_x_n <- scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))

eje_y_n <- scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))


# Funciones ----

# nullToNA <- function(x) {
#   x[sapply(x, is.null)] <- NA
#   return(x)
# }

# Limpieza datos 2021 -----

# Añade columna de id
kiwi21$id <- rep(1:nrow(kiwi21))

kiwi21 <- kiwi21 %>% 
  select(id, everything(), -"¿Querés contestar más preguntas?...30",
         -"¿Querés contestar más preguntas?...39", 
         -"¿Querés contestar más preguntas?...47",
         - Comentarios)


## Limpieza nombres de columnas ----
limpios <- make.names(colnames(kiwi21))
colnames(kiwi21) <- limpios

names(kiwi21)

kiwi21 <- kiwi21 %>% 
  rename(
    fecha = Marca.temporal,
    genero = Identidad.de.Género,
    edad = Edad,
    nivel_formacion = Máximo.nivel.de.formación,
    carrera_grado = X.Qué.carrera.de.grado.estudiaste.,
    tipo_universidad = X.En.qué.tipo.de.universidad.estudiaste.tu.carrera.de.grado.,
    pais = País.en.el.que.trabajas,
    provincia = Provincia.donde.trabajas,
    trabajo = Trabajo,
    rubro = Rubro.de.la.empresa,
    dotacion = X.Cuántos.empleados.tiene.la.empresa.,
    origen_capital = Origen.del.capital,
    dotacion_rh = X.Cuántas.personas.integran.el.área.de.RRHH.,
    puesto = X.En.qué.puesto.trabajás.,
    tipo_contratacion = Tipo.de.contratación,
    jornada = X.Cómo.es.tu.jornada.laboral.,
    funcion = X.Cuál.es.tu.función.principal.en.RRHH.,
    personas_a_cargo = X.Cuántas.personas.tenés.a.cargo...poné.0.si.no.tenés.gente.a.cargo..,
    anios_en_empresa = X.Hace.cuántos.años.trabajas.en.la.empresa.donde.estás...0.para.menos.de.un.año.,
    anios_en_puesto = X.Hace.cuántos.años.estás.en.tu.puesto.actual...0.para.menos.de.un.año.,
    anios_experiencia = X.Cuántos.años.de.experiencia.tenés.en.RRHH.,
    sueldo_bruto = X.Cuál.es.tu.remuneración.BRUTA.MENSUAL.en.tu.moneda.local...antes.de.impuestos.y.deducciones.,
    beneficios = X.Qué.beneficios.tenés.,
    bono = X.Recibís.bonos.,
    ajuste = X.Tuviste.ajustes.por.inflación.en.2021.,
    ajuste_porcentaje = X.Cuál.fue.el.porcentaje.de.aumento.acumulado.que.tuviste.en.2021.,
    ajuste_mes = Mes.del.último.ajuste,
    otros_proyectos = X.Trabajás.en.proyectos.independientes.además.de.tu.empleo.,
    erp = X.Qué.sistema.de.gestión.de.RRHH.usan.en.tu.empresa.,
    nombre_area = X.Cómo.se.llama.el.área.en.tu.empresa.,
    idioma_exigencia = X.Te.exigieron.saber.un.idioma.extranjero..inglés..portugués..etc...para.entrar.a.trabajar.en.tu.empresa.,
    idioma_porcentaje = X.Qué.porcentaje.del.tiempo.usas.el.idioma.extranjero.en.tu.puesto.actual.,
    satisfaccion = X.Qué.tan.satisfecho.estás.con.tu.empresa.,
    busqueda = X.Estás.buscando.trabajo.,
    beneficios_expectativa = X.Qué.beneficios.te.gustaría.tener.,
    pregunta_bizarra = X.Cuál.es.la.pregunta.más.bizarra.que.te.han.hecho.has.hecho.en.una.entrevista.,
    rh_una_palabra = Definí.a.RRHH.con.una.sola.palabra,
    diversidad_sexual = X.Te.identificás.como.LGBTIQ...lesbiana..gay..bisexual..transexual..otra.minoría.sexual..,
    libertad_ser = En.tu.empresa.puedes.ser.como.realmente.eres..por.ej...expresar.abiertamente.tu.personalidad..tu.identidad.de.género..orientación.sexual..etc...,
    diversidad_management = X.Qué.porcentaje.aproximado.del.management.de.tu.empresa.son.mujeres...Entiéndase.posiciones.de.Jefatura..de.Gerencia..o.de.Dirección.,
    lenguaje_inclusivo = Es.importante.incorporar.el.lenguaje.inclusivo.en.la.organización.,
    discapacidad = X.Tenés.alguna.discapacidad.,
    linea_segura = X.En.tu.organización.existe.una.línea.segura.o.políticas.definidas.para.actuar.frente.a.situaciones.de.acoso.o.discriminación.,
    sufrio_acoso = X.Sufriste.alguna.situación.de.acoso..abuso.o.de.discriminación.en.algún.trabajo.,
    teletrabajo = X.Estás.trabajando.desde.tu.casa.,
    retorno_plan = X.Cómo.están.planeando.el.regreso.a.las.oficinas.,
    retorno_mes = X.En.qué.mes.está.planificado.el.retorno.de.la.mayoría.de.los.empleados.,
    retorno_decision = X.Cómo.fue.la.decisión.del.esquema.de.teletrabajo.a.realizar.,
    retorno_dias = Cantidad.de.días.que.se.aplicará.teletrabajo,
    modalidad_ingresos = Las.nuevas.contrataciones...bajo.qué.modalidad.ingresaron.,
    retorno_satisfaccion = X.Qué.tan.satisfecho.estás.con.la.decisión.de.retornar.a.la.oficina.,
    valoracion_gestion_pandemia = X.Cómo.valorarías.la.gestión.de.tu.empresa.durante.la.pandemia.del.COVID.,
    registro_fiscal = X.Cómo.estás.registrado.a.fiscalmente.,
    anios_freelance = X.Hace.cuántos.años.trabajás.como.freelance.,
    lugar_trabajo = X.Dónde.trabajás.habitualmente...sin.considerar.la.coyuntura.por.COVID.19.,
    exporta = X.Exportás.tus.servicios.,
    medio_pago_exterior = Si.exportás.servicios...a.través.de.qué.medios.de.pago.recibís.los.pagos.del.exterior.,
    cuotas = X.Aceptás.pagos.en.cuotas.,
    colaboracion_freelance = X.Trabajás.con.otros.freelancers.de.tu.mismo.rubro.,
    servicio_busqueda = X.Tu.servicio.principal.está.relacionado.con.búsqueda.y.selección.,
    busqueda_it = X.Te.dedicás.principalmente.a.realizar.búsquedas.de.IT.Tecnología.,
    trabajo_a_riesgo =X.Trabajás.a.riesgo.,
    coeficiente = X.Cuál.es.el.coeficiente.que.cobrás.por.tus.servicios.,
    base_coeficiente = El.coeficiente.lo.calculás.sobre.,
    garantia = X.Ofrecés.garantía.,
    servicio_principal = X.Cuál.es.el.servicio.principal.que.brindas...si.brindás.más.de.un.servicio..elegí.el.que.más.ingresos.genere.,
    valor_hora = X.Cuál.es.el.valor.hora.promedio.que.ofrecés...moneda.local.
)

# Limpieza datos 2020 ------
limpios <- make.names(colnames(kiwi20))
colnames(kiwi20) <- limpios

rm(limpios)

kiwi20 <- kiwi20 %>% 
  select(-X.Querés.contestar.más.preguntas....31, 
         -X.Querés.contestar.más.preguntas....42) %>% 
  rename(genero = Género,
         genero_diverso = X.Te.identificás.como.LGBT...lesbiana..gay..bisexual..transexual..otra.minoría.sexual..,
         edad = Edad,
         discapacidad = X.Tenés.alguna.discapacidad.,
         nivel_formacion = `Máximo.nivel.de.formación`,
         carrera_grado = X.Qué.carrera.de.grado.estudiaste.,
         tipo_universidad = X.En.qué.tipo.de.universidad.estudiaste.tu.carrera.de.grado.,
         pais = País.en.el.que.trabajas,
         provincia = Provincia.donde.trabajas,
         trabajo = Trabajo,
         rubro = Rubro.de.la.empresa,
         dotacion = X.Cuántos.empleados.tiene.la.empresa.,
         origen_capital = Origen.del.capital,
         dotacion_rh = X.Cuántas.personas.integran.el.área.de.RRHH.,
         puesto = X.En.qué.puesto.trabajás.,
         tipo_contratacion = Tipo.de.contratación,
         funcion_rh = X.Cuál.es.tu.función.principal.en.RRHH.,
         personas_a_cargo = "X.Cuántas.personas.tenés.a.cargo...poné.0.si.no.tenés.gente.a.cargo.",
         anios_en_empresa = "X.Hace.cuántos.años.trabajas.en.la.empresa.donde.estás...0.para.menos.de.un.año.",
         anios_en_puesto = "X.Hace.cuántos.años.estás.en.tu.puesto.actual...0.para.menos.de.un.año.",
         anios_experiencia = X.Cuántos.años.de.experiencia.tenés.en.RRHH.,
         sueldo_bruto = X.Cuál.es.tu.remuneración.BRUTA.MENSUAL.en.tu.moneda.local...antes.de.impuestos.y.deducciones.,
         beneficios = X.Qué.beneficios.tenés.,
         bono = X.Recibís.bonos.,
         ajuste = X.Tuviste.ajustes.por.inflación.en.2020.,
         ajuste_porcentaje = X.Cuál.fue.el.porcentaje.de.aumento.acumulado.que.tuviste.en.2020.,
         ajuste_mes = Mes.del.último.ajuste,
         otros_proyectos = X.Trabajás.en.proyectos.independientes.además.de.tu.empleo.,
         erp = X.Qué.sistema.de.gestión.de.RRHH.usan.en.tu.empresa.,
         nombre_area = X.Cómo.se.llama.el.área.en.tu.empresa.,
         mate = X.Se.podía.tomar.mate.en.las.oficinas.de.tu.empresa...antes.del.COVID.19.,
         idioma_exigencia = X.Te.exigieron.saber.un.idioma.extranjero..inglés..portugués..etc...para.entrar.a.trabajar.en.tu.empresa.,
         idioma_porcentaje = X.Qué.porcentaje.del.tiempo.usas.el.idioma.extranjero.en.tu.puesto.actual.,
         contactos_linkedin = "X.Cuántos.contactos.tenés.en.LinkedIn...poné.0.si.no.tenés.cuenta.de.LinkedIn.",
         satisfaccion = X.Qué.tan.satisfecho.estás.con.tu.empresa.,
         busqueda = X.Estás.buscando.trabajo.,
         beneficios_expectativa = X.Qué.beneficios.te.gustaría.tener.,
         rh_una_palabra = Definí.a.RRHH.con.una.sola.palabra,
         pregunta_bizarra = X.Cuál.es.la.pregunta.más.bizarra.que.te.han.hecho.has.hecho.en.una.entrevista.,
         teletrabajo = X.Estás.trabajando.desde.tu.casa.,
         elementos = X.Qué.elementos.te.proveyó.la.empresa.para.que.puedas.trabajar.desde.tu.casa.,
         valoracion_gestion_empresa = X.Cómo.valorarías.la.gestión.de.tu.empresa.en.este.nuevo.contexto.,
         registro_fiscal = X.Cómo.estás.registrado.a.fiscalmente.,
         anios_freelance = X.Hace.cuántos.años.trabajás.como.freelance.,
         lugar_trabajo = X.Dónde.trabajás.habitualmente...sin.considerar.la.coyuntura.por.COVID.19.,
         exporta = X.Exportás.tus.servicios.,
         medio_pago_exterior = Si.exportás.servicios...a.través.de.qué.medios.de.pago.recibís.los.pagos.del.exterior.,
         cuotas = X.Aceptás.pagos.en.cuotas.,
         colaboracion_freelance = X.Trabajás.con.otros.freelancers.de.tu.mismo.rubro.,
         servicio_busqueda = X.Tu.servicio.principal.está.relacionado.con.búsqueda.y.selección.,
         busqueda_it = X.Te.dedicás.principalmente.a.realizar.búsquedas.de.IT.Tecnología.,
         trabajo_a_riesgo =X.Trabajás.a.riesgo.,
         coeficiente = X.Cuál.es.el.coeficiente.que.cobrás.por.tus.servicios.,
         base_coeficiente = El.coeficiente.lo.calculás.sobre.,
         garantia = X.Ofrecés.garantía.,
         servicio_principal = X.Cuál.es.el.servicio.principal.que.brindas...si.brindás.más.de.un.servicio..elegí.el.que.más.ingresos.genere.,
         valor_hora = X.Cuál.es.el.valor.hora.promedio.que.ofrecés...moneda.local.)


# Base de empleados en relación de dependencia
rh20 <- kiwi20 %>% 
  filter(trabajo == "Relación de Dependencia", 
         funcion_rh != "Ninguno",
         funcion_rh != "No me desempeño en el área de RRHH.",
         funcion_rh != "Trabajo en el área de Sistemas.",
         funcion_rh != "No trabajo en el area",
         funcion_rh != "No trabajo en recursos humanos",
         funcion_rh != "No trabajo en Rrhh",
         funcion_rh != "Trabajo en Administración y Finanzas",
         funcion_rh != "Aaa",
         funcion_rh != "IT") %>% 
  mutate(sueldo_bruto = as.numeric(unlist(sueldo_bruto)),
         puesto = factor(puesto))

rh20 <- rh20 %>% 
  filter(puesto != "Juzgado Civil y Comercial",
         puesto != "Programador",
         puesto != "Cuidado",
         puesto != "Asesor",
         puesto != "Jefe de Proyecto") %>% 
  mutate(puesto = str_trim(puesto, side = "both"), # Elimina espacios vacíos
         puesto = fct_collapse(puesto, "Gerente" = "Superintendente"),
         puesto = fct_collapse(puesto, "Director" = "Director ( escalafón municipal)"),
         puesto = fct_collapse(puesto, "HRBP" = c("Senior Consultoría", "specialist", "especialista",
                                                  "Especialista en selección IT", "Recruiter")),
         puesto = fct_collapse(puesto, "Responsable" = c("Coordinación", "Coordinador de Payroll",
                                                         "Encargado", "Supervisor")),
         puesto = fct_collapse(puesto, "Administrativo" = c("Asistente", "Asistente RRHH", "Aux", 
                                                            "Capacitador", "Consultor Ejecutivo",
                                                            "consultor jr")),
         puesto = fct_collapse(puesto, "Analista" = c("Asesoramiento", "Consultor", "Generalista", 
                                                      "Reclutadora", "Selectora", "Senior"))) %>% 
  select(Marca.temporal:valoracion_gestion_empresa)

# Pasa los campos de lista a numéricos
rh20 <- rh20 %>% 
  mutate(anios_en_empresa  = as.numeric(unlist(anios_en_empresa)),
         anios_en_puesto   = as.numeric(unlist(anios_en_puesto)),
         anios_experiencia = as.numeric(unlist(anios_experiencia)),
         ajuste_porcentaje = as.numeric(unlist(ajuste_porcentaje)))

rh20 <- rh20 %>% 
  select(genero, nivel_formacion, pais, rubro, dotacion, origen_capital, puesto,
         funcion_rh, anios_experiencia, tipo_contratacion, sueldo_bruto, satisfaccion,
         busqueda)

rh20 <- unnest(data = rh20, cols = c(anios_experiencia), keep_empty = TRUE)


# Corregir orden de puestos y simplificar género

rh20 <- rh20 %>% 
  mutate(puesto = factor(puesto, levels = c("Pasante", "Administrativo", "Analista",
                                            "HRBP", "Responsable","Jefe",
                                            "Gerente", "Director")),
         genero = fct_recode(genero, "Género Diverso" = "Género diverso (género diverso / género fluido /otras minorías)"))

rh20 <- rh20 %>% 
  filter(nivel_formacion != "Secundario en curso") %>% 
  mutate(nivel_formacion = fct_collapse(nivel_formacion, "Universitario completo" = c("Maestría abandonada"),
                                        "Secundario completo" = c("Terciario abandonado", "Terciario en curso", 
                                                                  "Universitario abandonado"),
                                        "Maestría completa" = c("Doctorado en curso")))


# Comparación de sueldos en dólares
rh20 <- rh20 %>% 
  left_join(tipo_cambio, by = "pais") %>% 
  mutate(multiplicador = if_else(tipo_contratacion == "Part time", 1.5, 1),
         sueldo_ft = sueldo_bruto * multiplicador,    # Hace la equivalencia de un sueldo part time a full time
         sueldo_dolar = sueldo_ft/tipo_cambio,  # Convierto los sueldos a dólares
         cuenta = 1)


# Guarda csv rh20
write_delim(rh20, file = "data/rh_2020.csv",
            delim = ";")


## Generacion dataframes 2021 ----  
### Freelancers ----
freelo21 <- kiwi21 %>% 
  filter(trabajo == "Freelance")

# Eliminar columnas vacias
freelo21 <- freelo21 %>% 
  select(where (~ !all(is.na(.x))), # Elimina columnas con valores NA
         -anios_experiencia,        # Eliminación manual de columnas
         -sueldo_bruto,
         -ajuste_porcentaje,
         -pregunta_bizarra)


freelo21$coeficiente

freelo21$coeficiente[[4]] <- 1.5
freelo21$coeficiente[[40]] <- 1.2

# Reemplazar valores NULL con NA
freelo21$coeficiente[[2]] <- NA
freelo21$coeficiente[[8]] <- NA
freelo21$coeficiente[[9]] <- NA
freelo21$coeficiente[[11]] <- NA
freelo21$coeficiente[[13]] <- NA
freelo21$coeficiente[[16]] <- NA
freelo21$coeficiente[[19]] <- NA
freelo21$coeficiente[[25]] <- NA
freelo21$coeficiente[[27]] <- NA
freelo21$coeficiente[[29]] <- NA
freelo21$coeficiente[[37]] <- NA
freelo21$coeficiente[[38]] <- NA
freelo21$coeficiente[[39]] <- NA
freelo21$coeficiente[[41]] <- NA
freelo21$coeficiente[[46]] <- NA
freelo21$coeficiente[[47]] <- NA
freelo21$coeficiente[[49]] <- NA

freelo21 <- freelo21 %>% 
  mutate(medio_pago_exterior = as.character(unlist(medio_pago_exterior)),
         coeficiente = as.numeric(unlist(coeficiente)))

glimpse(freelo21)

# Guardar el archivo de freelancers
write_delim(freelo21, file = "data/freelancers_2021.csv",
            delim = ";")

### Relación de dependencia ----
rh21 <- kiwi21 %>% 
  filter(trabajo != "Freelance") %>% 
  select("id":"valoracion_gestion_pandemia")

names(rh21)
glimpse(rh21)

rh21 <- rh21 %>% 
  mutate(anios_experiencia = as.numeric(unlist(anios_experiencia)),
         ajuste_porcentaje = as.numeric(unlist(ajuste_porcentaje)),
         sueldo_bruto = as.numeric(unlist(sueldo_bruto)))


unique(rh21$puesto)
unique(rh21$funcion)




rh21 <- rh21 %>% 
  filter(!puesto %in% c("Inspección de calidad", "Desarrollador", "-"),
         !funcion %in% c("Salud y Seguridad", "No trabajo en rrll", "Customer", 
                         "Data scientist", "No trabajo en RRHH"))


# EDA 2021 ----
status(rh21)

## Limpieza variable genero ----
rh21 <- rh21 %>% 
  mutate(genero = fct_collapse(genero, "Mujer cis" = c("Mujer cis", "Mujer", "mujer",
                                                       "Mujer heterosexual"),
                               "Hombre cis" = c("Hombre cis", "Hombre", 
                                                "Hombre heterosexual")),
         genero = factor(genero, levels = c("Mujer cis", "Hombre cis",
                                            "Gay", "Prefiero no responder")))

## Limpieza variable carrera_grado ----
unique(rh21$carrera_grado)

rh21 <- rh21 %>% 
  mutate(carrera_grado = fct_collapse(carrera_grado,
                                      "Relaciones Públicas" = c("Lic. en Relaciones Públicas",
                                                                "Lic. en Relacione Publicas",
                                                                "Relaciones Públicas e Institucionales"),
                                      "Psicología" = c("Psicología", "Psicología Social"),
                                      "Ingeniería Comercial" = c("Ingeniería Comercial", "INGENIERA COMERCIAL",
                                                                 "Ingenieria comercial", "Ingeniera Comercial"),
                                      "Ciencias de la Educación" = c("Ciencias de la Educacion", "Educación"),
                                      "Comunicación" = c("Comunicación", "Comunicación Social", 
                                                         "Ciencias de la Comunicación",
                                                         "Soy Licenciada en Ciencias Comunicación y Diplomada en RRHH"),
                                      "Sistemas" = c("Ingeniería de Sistemas", "Tecnologías de la Información")))
unique(rh21$carrera_grado)

rh21 <- rh21 %>% 
  mutate(carrera_grado = fct_lump_min(carrera_grado,           # Agrupa carreras con un mínimo de casos
                                      min = 4, 
                                      other_level = "Otras"))

## Limpieza columna puesto ----
unique(rh21$puesto)

rh21 <- rh21 %>% 
  mutate(puesto = str_trim(puesto, side = "both"),
         puesto = fct_collapse(puesto,
                               "Gerente" = "Superintendente",
                               "Analista" = c("Analista", "Talent Acquisition", "talent",
                                              "Senior Recruiter","Recruiter",
                                              "Analista semi senior", "Consultor", 
                                              "Reclutador", "Sourcer Specialist", "Sourcer (Recruiter)",
                                              "Tech Recruiter", "Representante"),
                               "Administrativo" = c("Administrativo", "Técnico", "Payroll Assistant",
                                                    "Asistente", "Auxiliar",
                                                    "El cargo es Asistente de CH, pero leo adelante Comunicación Interna, RSE, Capacitacion"),
                               "HRBP" = c("HRBP", "Especialista de selección por un lado (única persona en estas tareas) y HRBP de 2 equipos por otro"),
                               "Responsable" = c("Responsable", "Supervisor", "coordinación", "Coordinadora"
                                                 ))) 

## Limpieza columna funcion ----
unique(rh21$funcion)

rh21 %>% 
  count(funcion, sort = T)

rh21 <- rh21 %>% 
  mutate(funcion = fct_collapse(funcion,
                                "Reclutamiento" = c("Reclutamiento y selección", "Selección & Desarrollo",
                                                    "Candidate Experience"),
                                "Generalista" = c("Administración de RRHH, Comunicación Interna, Rexlutamientl dr Selección, Relaciones Laborales y RSE",
                                                  "Todos los anteriores", "Servicios Generales", "De todo un poco por ser un equipo chico",
                                                  "todas las anteriores", "Gestión de los equipos de TH", "Deberia poder marcarse mas de una opción aquí",
                                                  "Todas las anteriores", "RH y Gestión ( RH, RL, SO, CERTIFICACIONES", 
                                                  "Todos los departamentos", "ADP, comunicación interna y Reclutamiento y selección",
                                                  "Todo lo que tenga que ver con RR.HH", "Varias funciones",
                                                  "Todas las áreas integrales de RH", "de todo un poco..!",
                                                  "Analista gestión de RRHH", "Todo menos payroll"),
                                "Administración de Personal" = c("Administración de personal", "Asistente administrativo"),
                                "Payroll" = c("Payroll / Liquidación de sueldos", "Coordinación de Procesos de Liquidación",
                                              "Administración personal, liquidación de sueldos"), 
                                "Capacitación y Desarrollo" = c("Capacitación y desarrollo", "Generalista soft", 
                                                                "Desarrollo de talento"),
                                "Diseño Organizacional" = c("Diseño organizacional", "Formulación de proyectos",
                                                            "Gestión Estrategica", "Cultura y Change Management"),
                                "People Analytics" = c("People analytics", "HRIS", "Analista de datos - Operaciones"), 
                                "Comunicación Interna" = c("Comunicación interna", "Comunicación, Capacitación y RSE"),
                                "Clima & Cultura" = c("Clima y cultura", "People Experience")
                                )) 

## Limpieza columna tipo_contratacion ----
unique(rh21$tipo_contratacion)

rh21 <- rh21 %>% 
  mutate(tipo_contratacion = fct_collapse(tipo_contratacion,
                                          "Indeterminado" = c("Indeterminado", "Por tiempo indefinido",
                                                              "Indefinido, no sé si sea lo mismo a indeterminado",
                                                              "Permanente", "Prestación de servicios indeterminado (El Salvador)",
                                                              "Plata permanente", "Indefinido", "De planta"
                                                              ))) 

## Limpieza nivel_formación ----

# Agrupar niveles de formación

unique(rh21$nivel_formacion)

rh21 %>% 
  mutate(nivel_formacion = fct_collapse(nivel_formacion,
                                        "Universitario completo" = c("Diplomado de posgrado en curso",
                                                                     "Maestría abandonada", "Maestría en curso",
                                                                     "Diplomado de posgrado abandonado"),
                                        "Secundario completo" = c("Universitario en curso", "Universitario abandonado",
                                                                  "Terciario abandonado", "Terciario en curso"))) %>% 
  group_by(nivel_formacion) %>% 
  tally(sort = T)


## Añadir columna sueldo dolar ----
rh21 %>% 
  group_by(jornada, tipo_contratacion) %>% 
  tally()

rh21 <- rh21 %>% 
  left_join(tc, by = "pais")

rh21 <- rh21 %>% 
  mutate(multiplicador = if_else(tipo_contratacion == "Part time", 1.5, 1),
         sueldo_ft = sueldo_bruto * multiplicador,    # Hace la equivalencia de un sueldo part time a full time
         sueldo_dolar = sueldo_ft/tipo_cambio,  # Convierto los sueldos a dólares
         cuenta = 1)

write_delim(rh21, file = "data/rh_2021.csv",
            delim = ";")   

rh21 %>% 
  count(rubro, sort = T)


# Educación ----
carreras <- kiwi21 %>% 
  select(nivel_formacion, carrera_grado, tipo_universidad, trabajo, 
         sueldo_bruto, puesto, funcion, pais, genero) %>% 
  mutate(carrera_grado = factor(carrera_grado))


sort(unique(carreras$carrera_grado))

carreras %>% 
  count(carrera_grado, sort = T) %>% 
  mutate(prop = n/sum(n)) %>% 
  print(n = Inf)
  

carreras <- carreras %>% 
  mutate(carrera_grado = fct_collapse(carrera_grado, 
                                      "Comunicación Social" = c("Ciencias de la Comunicación", "Comunicación", "Comunicación social",
                                                                "Comunicación Social",
                                                                "Soy Licenciada en Ciencias Comunicación y Diplomada en RRHH"),
                                      "Ingenierías" = c("Ingeniera Comercial", 
                                                        "INGENIERA COMERCIAL",
                                                        "Ing. Gestión Empresarial", 
                                                        "Ingeniería",
                                                        "Ingenieria comercial", 
                                                        "Ingeniería Comercial",
                                                        "Ingeniería de Sistemas", "Ingeniería Industrial"),
                                      "Relaciones Públicas" = c("Lic. en Relacione Publicas",
                                                                "Lic. en Relaciones Públicas",
                                                                "Relaciones Públicas e Institucionales"), 
                                      "Psicología" = c("Psicología", "Psicología Social")),
         carrera_grado = fct_lump(carrera_grado, 
                                  prop = 0.02, 
                                  other_level = "Otros"),
         carrera_grado = factor(carrera_grado,
                                levels = c("RRHH / RRLL / RRTT", "Psicología", "Administración de Empresas", 
                                           "Contador Público", "Ingenierías","Comunicación Social", "Otros")))



carreras %>% 
  group_by(carrera_grado) %>% 
  summarise(cant = n()) %>% 
  mutate(prop = cant/sum(cant)) %>% 
  arrange(-cant)

## Niveles de formación ----

carreras %>% 
  count(nivel_formacion)

carreras %>% 
  mutate(nivel_formacion = fct_collapse(nivel_formacion,
                                        "Secundario completo" = c("Secundario completo", "Terciario en curso",
                                                                  "Terciario abandonado", "Universitario abandonado"),
                                        "Universitario completo" = c("Universitario completo", "Maestría abandonada", 
                                                                     "Diplomado de posgrado abandonado")),
         nivel_formacion = fct_recode(nivel_formacion, "Diplomado completo" = "Diplomado de posgrado completo",
                                      "Diplomado en curso" = "Diplomado de posgrado en curso")) %>%
  count(nivel_formacion)

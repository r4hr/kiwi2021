# Librerías ----
pacman::p_load(tidyverse, funModeling, googlesheets4, gargle, gt, 
               extrafont, scales, ggalt, kableExtra, wordcloud, networkD3,
               data.table)


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

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

# Limpieza datos 2021 -----

# Añade columna de id
kiwi21$id <- rep(1:nrow(kiwi21))

kiwi21 <- kiwi21 %>% 
  select(id, everything(), -"¿Querés contestar más preguntas?...30",
         -"¿Querés contestar más preguntas?...39", 
         -"¿Querés contestar más preguntas?...47",)


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
    valor_hora = X.Cuál.es.el.valor.hora.promedio.que.ofrecés...moneda.local.,
    comentarios = Comentarios)



## Generacion dataframes ----  
freelo21 <- kiwi21 %>% 
  filter(trabajo == "Freelance")

# Eliminar columnas vacias
freelo21 <- freelo21 %>% 
  select(where (~ !all(is.na(.x))), # Elimina columnas con valores NA
         -anios_experiencia,        # Eliminación manual de columnas
         -sueldo_bruto,
         -ajuste_porcentaje,
         -pregunta_bizarra,
         -comentarios)


freelo21$coeficiente

freelo21$coeficiente[[4]] <- 1.5
freelo21$coeficiente[[40]] <- 1.2

freelo21 <- lapply(freelo21, nullToNA)

freelo21 <- freelo21 %>% 
  mutate(medio_pago_exterior = as.character(unlist(medio_pago_exterior)),
         coeficiente = as.numeric(unlist(coeficiente)))

freelo21 <- rbindlist(freelo21)
glimpse(freelo21)


  
rh21 <- kiwi21 %>% 
  filter(trabajo != "Freelance") %>% 
  select("id":"valoracion_gestion_pandemia")

names(rh21)
glimpse(rh21)

rh21 <- rh21 %>% 
  mutate(anios_experiencia = as.numeric(unlist(anios_experiencia)),
         ajuste_porcentaje = as.numeric(unlist(ajuste_porcentaje)),
         sueldo_bruto = as.numeric(unlist(sueldo_bruto)),
         pregunta_bizarra = as.character(unlist(pregunta_bizarra)))



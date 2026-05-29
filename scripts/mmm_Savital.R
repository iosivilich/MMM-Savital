# Establece el directorio de trabajo en la raíz del proyecto antes de ejecutar.
# En RStudio: abre el archivo .Rproj o usa Session → Set Working Directory → To Project Directory.
# getwd()  # descomentar para verificar

library(readxl)
library(dplyr)
library(janitor)
library(lubridate)
library(tidyr)
library(Robyn)

# Los datos reales van en /data/ (carpeta excluida del repositorio).
# Para probar con datos sintéticos usa: read_csv("data_example/data_example.csv")
df <- read_excel("data/BASE.xlsx") %>% clean_names()

# Filtrar categorías capilares
categorias_capilares <- c(
  "LINEA DE SHAMPOO",
  "SHAMPOO FEMENINO",
  "TRATAMIENTOS PARA EL CABELLO",
  "BALSAMO - ACONDICIONADOR",
  "OTROS PRODUCTOS CAPILARES",
  "LINEA DE PRODUCTOS - COSMETICOS",
  "P C ASEO PERSONAL"
)

df <- df %>%
  filter(categoria %in% categorias_capilares)

# Limpieza final EXACTA
df <- df %>%
  mutate(
    fecha = as.Date(fecha, format = "%d/%m/%Y"),
    
    awareness = awareness %>%
      gsub(",", ".", .) %>%
      as.numeric(),
    
    top_of_mind = top_of_mind %>%
      gsub(",", ".", .) %>%
      as.numeric(),
    
    total_total_personas = total_total_personas %>%
      gsub(",", ".", .) %>%
      as.numeric(),
    
    medio = toupper(medio),
    marca = toupper(marca)
  )

#-------------------------------------------------------------------------------
# MMM1: TOM promedio del mercado

library(Robyn)

df_cat <- df %>%
  group_by(fecha = floor_date(fecha, "month")) %>%
  summarise(
    tv = sum(total_inversion[medio %in% c("TELEVISION NACIONAL","TV SUSCRIPCION","TELEVISION REGIONAL")], na.rm = TRUE),
    radio = sum(total_inversion[medio=="RADIO"], na.rm = TRUE),
    prensa = sum(total_inversion[medio %in% c("PRENSA","REVISTA","REVISTAS DE PRENSA")], na.rm = TRUE),
    exterior = sum(total_inversion[medio=="PUBLICIDAD EXTERIOR"], na.rm = TRUE),
    
    awareness = mean(awareness, na.rm=TRUE),
    tom = mean(top_of_mind, na.rm=TRUE),
    personas = sum(total_total_personas, na.rm=TRUE)
  ) %>%
  arrange(fecha)

InputCollect_cat <- robyn_inputs(
  dt_input = df_cat,
  date_var = "fecha",
  dep_var = "tom",
  dep_var_type = "conversion",
  
  paid_media_spends = c("tv","radio","prensa","exterior"),
  paid_media_vars   = c("tv","radio","prensa","exterior"),
  
  context_vars = c("awareness","personas"),
  
  adstock = "geometric"
)

# Definir hiperparámetros por medio
hyper_cat <- list(
  tv_alphas      = c(0.1, 3),
  tv_gammas      = c(0.3, 1),
  tv_thetas      = c(0.3, 0.7),
  
  radio_alphas   = c(0.1, 3),
  radio_gammas   = c(0.3, 1),
  radio_thetas   = c(0.1, 0.5),
  
  prensa_alphas  = c(0.1, 3),
  prensa_gammas  = c(0.3, 1),
  prensa_thetas  = c(0.1, 0.5),
  
  exterior_alphas = c(0.1, 3),
  exterior_gammas = c(0.3, 1),
  exterior_thetas = c(0.2, 0.6)
)

InputCollect_cat <- robyn_inputs(
  dt_input = df_cat,
  date_var = "fecha",
  dep_var = "tom",
  dep_var_type = "conversion",
  
  paid_media_spends = c("tv","radio","prensa","exterior"),
  paid_media_vars   = c("tv","radio","prensa","exterior"),
  
  context_vars = c("awareness","personas"),
  
  adstock = "geometric",
  
  hyperparameters = hyper_cat
)

OutputModels_cat <- robyn_run(
  InputCollect = InputCollect_cat,
  iterations = 1500,
  trials = 2,
  cores = 2
)

Result_cat <- robyn_outputs(
  InputCollect = InputCollect_cat,
  OutputModels = OutputModels_cat
)

#-------------------------------------------------------------------------------
#MMM 2: SAVITAL vs PANTENE PRO V + NUTRIBELA

marcas_directas <- c("SAVITAL", "PANTENE PRO V", "NUTRIBELA")

df_marca <- df %>%
  filter(marca %in% marcas_directas) %>%
  mutate(
    marca = toupper(marca),
    fecha = as.Date(fecha, format = "%d/%m/%Y")
  ) %>%
  group_by(fecha = floor_date(fecha, "month"), marca) %>%
  summarise(
    inversion_total = sum(total_inversion, na.rm = TRUE),   # combinamos todos los medios
    awareness = mean(awareness, na.rm = TRUE),
    tom = mean(top_of_mind, na.rm = TRUE),
    personas = sum(total_total_personas, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(marca = gsub(" ", "_", marca))

# 2) PIVOT WIDER: CADA MARCA SE VUELVE UNA COLUMNA
df_wide <- df_marca %>%
  pivot_wider(
    names_from = marca,
    values_from = c(inversion_total, awareness, tom, personas),
    values_fill = 0
  ) %>%
  arrange(fecha)

# 3) DEFINIR PAID MEDIA Y CONTEXT VARIABLES
# Columnas de inversión por marca (ya combinadas)
paid_cols <- grep("^inversion_total_", names(df_wide), value = TRUE)

# Contexto: awareness, TOM y personas de competidores
context_cols <- c(
  "awareness_PANTENE_PRO_V",
  "awareness_NUTRIBELA",
  "tom_PANTENE_PRO_V",
  "tom_NUTRIBELA",
  "personas_SAVITAL"
)

# 4) ELIMINAR CANALES CON TODA LA INVERSIÓN EN CERO
cols_zeros <- paid_cols[sapply(df_wide[, paid_cols], function(x) all(x == 0))]

df_wide2 <- df_wide %>% select(-all_of(cols_zeros))

paid_cols2 <- paid_cols[!paid_cols %in% cols_zeros]

# 5) CREAR HIPERPARÁMETROS POR MARCA (no por medio)
hyper_comp2 <- list()

for (col in paid_cols2) {
  
  # alphas, gammas y thetas iguales para todos (porque ya son por marca)
  hyper_comp2[[paste0(col, "_alphas")]] <- c(0.1, 3)
  hyper_comp2[[paste0(col, "_gammas")]] <- c(0.3, 1)
  hyper_comp2[[paste0(col, "_thetas")]] <- c(0.1, 0.5)
}

# 6) INPUTCOLLECT PARA MMM2 (Savital TOM como dependiente)
InputCollect_comp <- robyn_inputs(
  dt_input = df_wide2,
  date_var = "fecha",
  dep_var = "tom_SAVITAL",
  dep_var_type = "conversion",
  
  paid_media_spends = paid_cols2,
  paid_media_vars   = paid_cols2,
  
  context_vars = context_cols,
  
  adstock = "geometric",
  hyperparameters = hyper_comp2
)

# 7) CORRER ROBYN
OutputModels_comp <- robyn_run(
  InputCollect = InputCollect_comp,
  iterations = 1500,
  trials = 2,
  cores = 2
)

Result_comp <- robyn_outputs(
  InputCollect = InputCollect_comp,
  OutputModels = OutputModels_comp
)


#-------------------------------------------------------------------------------
# Librerías necesarias
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(writexl)
library(patchwork)   # para juntar plots (opcional)

# ---------------------------
# A. Datos base y columnas
# ---------------------------
# df_wide2  -> la tabla pivotada con inversiones por marca (creada en tu script anterior)
# paid_cols2 -> columnas de inversión por marca (creadas en tu script anterior)
# context_cols -> columnas de context vars que definiste
# Result_comp -> salida de robyn_outputs() para el MMM2

# Si alguno no existe, avisar
if (!exists("df_wide2")) stop("df_wide2 no existe. Corre la pestaña previa para construir df_wide2.")
if (!exists("paid_cols2")) stop("paid_cols2 no existe. Revisa la pestaña previa.")
if (!exists("Result_comp")) message("Result_comp no encontrado: las métricas derivadas de Robyn no estarán disponibles, pero se crearán gráficos de datos base.")


# ---------------------------
# 1) Gráfico: Inversión por marca en el tiempo
# ---------------------------
inv_long <- df_wide2 %>%
  select(fecha, all_of(paid_cols2)) %>%
  pivot_longer(-fecha, names_to = "canal", values_to = "inversion") %>%
  mutate(marca = gsub("^inversion_total_", "", canal))

p1 <- ggplot(inv_long, aes(x = fecha, y = inversion, color = marca)) +
  geom_line(size = 0.9) +
  geom_point(size = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Inversión por marca en el tiempo",
       x = "Fecha",
       y = "Inversión",
       color = "Marca") +
  theme_minimal(base_size = 12)


# ---------------------------
# 2) Gráfico: TOM Savital vs inversión total (serie)
# ---------------------------
# crear inversión total de mercado (opcional) y de Savital
df_plot2 <- df_wide2 %>%
  mutate(inv_SAVITAL = ifelse("inversion_total_SAVITAL" %in% names(.), inversion_total_SAVITAL, 0),
         inv_total_categoria = rowSums(select(., starts_with("inversion_total_")), na.rm = TRUE),
         tom_SAVITAL = ifelse("tom_SAVITAL" %in% names(.), tom_SAVITAL, NA))

p2 <- ggplot(df_plot2, aes(x = fecha)) +
  geom_line(aes(y = inv_SAVITAL, linetype = "Inv. Savital")) +
  geom_line(aes(y = inv_total_categoria, linetype = "Inv. Categoria")) +
  geom_line(aes(y = tom_SAVITAL * max(inv_total_categoria, na.rm = TRUE), linetype = "TOM Savital (scaled)")) + 
  scale_y_continuous(
    name = "Inversión (COP)", labels = scales::comma,
    sec.axis = sec_axis(~ . / max(df_plot2$inv_total_categoria, na.rm = TRUE), name = "TOM Savital (escala relativa)")
  ) +
  labs(title = "Inversión (Savital y categoría) y TOM Savital (escala relativa)",
       x = "Fecha", linetype = "") +
  theme_minimal(base_size = 12)


# ---------------------------
# 3) Pie: Share of Investment promedio (periodo completo)
# ---------------------------
soi <- inv_long %>%
  group_by(marca) %>%
  summarise(total_inv = sum(inversion, na.rm = TRUE)) %>%
  arrange(desc(total_inv)) %>%
  mutate(pct = total_inv / sum(total_inv))

p3 <- ggplot(soi, aes(x = "", y = pct, fill = marca)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Share of Investment (periodo)", x = NULL, y = NULL, fill = "Marca") +
  theme_void(base_size = 12)


# ---------------------------
# 4) (Aproximado) Share of Contribution y ROI
#    - Intento de extraer la contribución desde Result_comp si existe
#    - Si no existe, se calcula una aproximación via correlación (solo indicativo)
# ---------------------------
soc_df <- NULL
roi_df <- NULL

# Intentar extraer contributions / solutions desde Result_comp
try({
  # Robyn estructura común: Result_comp$plot$... OR Result_comp$result[['...']]
  # Intentamos lo típico: Result_comp$contribution / Result_comp$allSolutions
  if (!is.null(Result_comp$allSolutions)) {
    # tomar la primera solución pareto-optimal (si existe)
    sol <- Result_comp$allSolutions[[1]]
    # si hay object con 'contribution' o similar
    if (!is.null(sol$contribution)) {
      soc_df <- as.data.frame(sol$contribution) # formato variable, depende de Robyn
    }
  }
}, silent = TRUE)

# Si no se pudo, fallback: aproximación por correlación ponderada
if (is.null(soc_df)) {
  # correlación entre cada inv series y tom_SAVITAL
  inv_mat <- df_wide2 %>% select(all_of(paid_cols2))
  tom_vec <- df_wide2$tom_SAVITAL
  cors <- sapply(inv_mat, function(x) {
    if (all(x == 0) | all(is.na(tom_vec))) return(0)
    cor(x, tom_vec, use = "pairwise.complete.obs")
  })
  soc_df <- data.frame(
    canal = names(cors),
    corr = as.numeric(cors),
    total_inv = colSums(inv_mat, na.rm = TRUE)
  ) %>%
    mutate(
      contribution_score = abs(corr) * total_inv,
      pct_contribution = contribution_score / sum(contribution_score, na.rm = TRUE)
    ) %>%
    arrange(desc(pct_contribution))
}

p4 <- ggplot(soc_df, aes(x = reorder(canal, pct_contribution), y = pct_contribution, fill = canal)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Share of Contribution (aprox.)", x = "", y = "Share of contribution") +
  theme_minimal(base_size = 12)


# ---------------------------
# 5) Elasticidades Savital (aproximadas)
#    - Si Robyn trae elasticities en Result_comp -> usar
#    - Sino: simple elasticity = %Δtom / %Δspend (approx via regressions)
# ---------------------------
elas_df <- NULL
try({
  # intento de extraer desde Result_comp
  if (!is.null(Result_comp$elasticities)) {
    elas_df <- as.data.frame(Result_comp$elasticities)
  }
}, silent = TRUE)

if (is.null(elas_df)) {
  # ajuste: regress tom_SAVITAL ~ each inv (log-log) cuando sea posible
  elas <- sapply(paid_cols2, function(col) {
    x <- df_wide2[[col]]
    y <- df_wide2$tom_SAVITAL
    ok <- which(!is.na(x) & x > 0 & !is.na(y) & y > 0)
    if (length(ok) < 5) return(NA)
    mod <- lm(log(y[ok]) ~ log(x[ok]))
    coef(mod)[2]
  })
  elas_df <- data.frame(canal = names(elas), elasticity = as.numeric(elas))
}

p5 <- ggplot(elas_df, aes(x = reorder(canal, elasticity), y = elasticity, fill = canal)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Elasticidades aproximadas (Savital)", x = "", y = "Elasticidad (%Δtom / %Δspend)") +
  theme_minimal(base_size = 12)


# ---------------------------
# 6) Escenario de optimización de inversión (simple)
#    - Recomendación heurística: invertir más en los canales con mayor contribution per unit spend
#    - Se estima contribution per unit = pct_contribution / total_inv (apróx.)
# ---------------------------
scen <- soc_df %>%
  mutate(contrib_per_unit = pct_contribution / (total_inv + 1e-9)) %>%
  arrange(desc(contrib_per_unit)) %>%
  mutate(recommendation = case_when(
    row_number() == 1 ~ "Impulsar",
    row_number() <= 3 ~ "Mantener",
    TRUE ~ "Evaluar"
  ))

# gráfico de contrib_per_unit
p6 <- ggplot(scen, aes(x = reorder(canal, contrib_per_unit), y = contrib_per_unit, fill = canal)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Contribución por unidad de inversión (apróx.)", x = "", y = "Contrib per unit") +
  theme_minimal(base_size = 12)


# ---------------------------
# EXPORTAR CADA GRÁFICA POR SEPARADO
# ---------------------------

ggsave("plot_1_inversion_tiempo.png", plot = p1,
       width = 10, height = 6, dpi = 300)

ggsave("plot_2_inversion_vs_tom_savital.png", plot = p2,
       width = 10, height = 6, dpi = 300)

ggsave("plot_3_share_investment.png", plot = p3,
       width = 8, height = 8, dpi = 300)

ggsave("plot_4_share_contribution.png", plot = p4,
       width = 10, height = 6, dpi = 300)

ggsave("plot_5_elasticidades.png", plot = p5,
       width = 10, height = 6, dpi = 300)

ggsave("plot_6_contrib_por_unidad.png", plot = p6,
       width = 10, height = 6, dpi = 300)

message("TODAS LAS GRÁFICAS FUERON EXPORTADAS COMO PNG EN TU WORKING DIRECTORY.")

library(patchwork)

# Exportar p1 + p2
ggsave("slide_01_inversion_y_tom.png",
       plot = (p1 / p2),
       width = 10, height = 10, dpi = 300)

# Exportar p3 + p4
ggsave("slide_02_soi_y_soc.png",
       plot = (p3 | p4),
       width = 14, height = 7, dpi = 300)

# Exportar p5 + p6
ggsave("slide_03_elasticidades_y_contrib_unitaria.png",
       plot = (p5 / p6),
       width = 10, height = 10, dpi = 300)


# ---------------------------
# EXPORTAR a Excel: datos + métricas
# ---------------------------
# Construir hojas
sheets <- list(
  df_wide2 = df_wide2,
  investments_long = inv_long,
  soi = soi,
  soc_approx = soc_df,
  elasticities = elas_df,
  scenario = scen
)

# Si Result_comp tiene tablas útiles, añadirlas
try({
  if (!is.null(Result_comp$allSolutions)) sheets$robyn_solutions <- as.data.frame(Result_comp$allSolutions[[1]])
}, silent = TRUE)

# Escribir Excel
write_xlsx(sheets, path = "MMM2_Savital_results.xlsx")

message("Exportado a MMM2_Savital_results.xlsx en tu working directory.")

names(Result_comp)
names(Result_comp$xDecompAgg)
names(Result_comp$xDecompVecCollect)
names(Result_comp$mediaVecCollect)

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(writexl)
library(patchwork)

# ================================================================
# 1. IDENTIFICAR CANALES (ya sabemos cuáles son)
# ================================================================

channels <- c(
  "inversion_total_SAVITAL",
  "inversion_total_PANTENE_PRO_V",
  "inversion_total_NUTRIBELA"
)

# ================================================================
# 2. EXTRAER CONTRIBUCIÓN REAL POR CANAL (Robyn real)
# ================================================================

contrib_vec <- Result_comp$xDecompVecCollect %>%
  select(ds, all_of(channels))

contrib_totals <- contrib_vec %>%
  summarise(across(all_of(channels), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "canal", values_to = "contribution")

# ================================================================
# 3. SHARE OF CONTRIBUTION (REAL)
# ================================================================

soc <- contrib_totals %>%
  mutate(share = contribution / sum(contribution),
         marca = gsub("inversion_total_", "", canal))

# ================================================================
# 4. SHARE OF INVESTMENT (REAL)
# ================================================================

soi <- df_wide2 %>%
  select(fecha, all_of(channels)) %>%
  pivot_longer(-fecha, names_to = "canal", values_to = "inv") %>%
  group_by(canal) %>%
  summarise(total_inv = sum(inv, na.rm = TRUE)) %>%
  mutate(share = total_inv / sum(total_inv),
         marca = gsub("inversion_total_", "", canal))

# ================================================================
# 5. ROI REAL = Contribución / Inversión real
# ================================================================

roi_df <- soc %>%
  left_join(soi %>% select(canal, total_inv), by="canal") %>%
  mutate(ROI = contribution / total_inv)

# ================================================================
# 6. ELASTICIDADES REALES
# Robyn guarda elasticidades en mediaVecCollect como "type = 'elasticity'"
# ================================================================

elas_df <- Result_comp$mediaVecCollect %>%
  filter(type == "elasticity") %>%
  select(all_of(channels)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "canal", values_to="elasticity") %>%
  mutate(marca = gsub("inversion_total_", "", canal))

# ================================================================
# 7. GRÁFICOS ESTILO ACCELERATOR
# ================================================================

# --------------------- 7.1 Share of Investment ------------------
p_soi <- ggplot(soi, aes(x = reorder(marca, share), y = share, fill = marca)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(title="Share of Investment", x="", y="Share") +
  theme_minimal()

# --------------------- 7.2 Share of Contribution ----------------
p_soc <- ggplot(soc, aes(x = reorder(marca, share), y = share, fill = marca)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(title="Share of Contribution", x="", y="Share") +
  theme_minimal()

# --------------------- 7.3 ROI por marca ------------------------
p_roi <- ggplot(roi_df, aes(x = reorder(marca, ROI), y = ROI, fill = marca)) +
  geom_col() +
  coord_flip() +
  labs(title="ROI por marca", x="", y="ROI") +
  theme_minimal()

# --------------------- 7.4 Elasticidades -------------------------
p_elas <- ggplot(elas_df, aes(x = reorder(marca, elasticity), y = elasticity, fill = marca)) +
  geom_col() +
  coord_flip() +
  labs(title="Elasticidad (%)", x="", y="Elasticidad") +
  theme_minimal()

# --------------------- 7.5 Serie TOM vs inversión Savital -------
p_tom <- ggplot(df_wide2, aes(x=fecha)) +
  geom_line(aes(y=inversion_total_SAVITAL), color="blue", size=1) +
  geom_line(aes(y=tom_SAVITAL * max(inversion_total_SAVITAL, na.rm=TRUE)),
            color="red", size=1, linetype="dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./max(df_wide2$inversion_total_SAVITAL, na.rm=TRUE),
                                         name="TOM Savital (scaled)")) +
  labs(title="Inversión Savital vs TOM", x="", y="Inversión") +
  theme_minimal()

# --------------------- 7.6 Optimización básica -------------------
opt_df <- roi_df %>%
  mutate(recommendation = case_when(
    ROI == max(ROI, na.rm=TRUE) ~ "↑ Incrementar",
    ROI >= median(ROI, na.rm=TRUE) ~ "→ Mantener",
    TRUE ~ "↓ Revisar"
  ))

p_opt <- ggplot(opt_df, aes(x=reorder(marca, ROI), y=ROI, fill=recommendation)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("↑ Incrementar"="green","→ Mantener"="orange","↓ Revisar"="red")) +
  labs(title="Recomendación de inversión", x="", y="ROI") +
  theme_minimal()

# ================================================================
# 8. EXPORTACIÓN A EXCEL
# ================================================================

write_xlsx(
  list(
    contrib_por_periodo = contrib_vec,
    contrib_totales = contrib_totals,
    share_of_contribution = soc,
    share_of_investment = soi,
    ROI = roi_df,
    elasticidades = elas_df,
    optimizacion = opt_df,
    base_mensual = df_wide2
  ),
  "MMM2_Savital_Output_Final.xlsx"
)

message("Archivo exportado como MMM2_Savital_Output_Final.xlsx")


# ============================================
#  ELASTICIDADES — INTEGRACIÓN FINAL
# ============================================

library(dplyr)
library(ggplot2)
library(scales)
library(writexl)

# ----- 1) Cálculo robusto -----

elas_df <- sapply(paid_cols2, function(col) {
  x <- df_wide2[[col]]
  y <- df_wide2$tom_SAVITAL
  
  ok <- which(!is.na(x) & x > 0 & !is.na(y) & y > 0)
  if (length(ok) < 6) return(NA)
  
  mod <- lm(log(y[ok]) ~ log(x[ok]))
  coef(mod)[2]
})

elastic_final <- data.frame(
  canal = names(elas_df),
  marca = sub(".*_", "", names(elas_df)),
  elasticidad = round(as.numeric(elas_df), 3)
)

# Orden descendente
elastic_final <- elastic_final %>% arrange(desc(elasticidad))
p_elas <- ggplot(elastic_final, 
                 aes(x = reorder(canal, elasticidad), 
                     y = elasticidad, fill = marca)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = round(elasticidad, 2)), 
            hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  labs(title = "Elasticidades (log-log) – Savital MMM",
       x = "Canal",
       y = "Elasticidad (%Δ TOM / %Δ inversión)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face="bold"),
    legend.position = "bottom"
  )

interpretacion <- elastic_final %>%
  mutate(
    conclusion = case_when(
      elasticidad > 1 ~ "Alta respuesta: %Δ inversión genera %Δ TOM acelerado.",
      elasticidad > 0.3 ~ "Elasticidad media: contribuye, pero con retornos decrecientes.",
      elasticidad > 0   ~ "Efecto bajo: impacto marginal sobre el TOM.",
      TRUE              ~ "Elasticidad negativa: riesgo de saturación o ruido competitivo."
    )
  )
interpretacion

write_xlsx(
  list(
    "Elasticidades"      = elastic_final,
    "Interpretación"     = interpretacion,
    "Data_Base_Model"    = df_wide2
  ),
  "Elasticidades_MMM_Savital.xlsx"
)



p_soi
p_soc
p_elas
p_roi
p_tom
p_opt

(p_soi | p_soc) /
  (p_roi | p_elas) /
  (p_tom | p_opt)




#-------------------------------------------------------------------------------
#Gráficos MM1

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(writexl)
library(patchwork)

paid_cols_cat <- c("tv","radio","prensa","exterior")
context_cols_cat <- c("awareness","personas")

decomp_cat <- Result_cat$xDecompVecCollect %>%
  mutate(fecha = as.Date(ds)) %>%
  select(fecha, depVarHat, all_of(paid_cols_cat))

spend_df <- df_cat %>%
  summarise(across(all_of(paid_cols_cat), sum, na.rm=TRUE)) %>%
  pivot_longer(everything(), names_to="canal", values_to="spend")

p_soi_cat <- ggplot(spend_df, aes(x = reorder(canal, spend), y = spend/1e6, fill = canal)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title="Share of Investment – MMM1", y="Inversión total (MM COP)", x="") +
  theme_minimal(base_size=12)
soc_df <- decomp_cat %>%
  summarise(across(all_of(paid_cols_cat), sum, na.rm=TRUE)) %>%
  pivot_longer(everything(), names_to="canal", values_to="contribution")

p_soc_cat <- ggplot(soc_df, aes(x = reorder(canal, contribution), y = contribution, fill = canal)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title="Share of Contribution – MMM1", y="Contribución al TOM", x="") +
  theme_minimal(base_size=12)
roi_cat <- spend_df %>%
  left_join(soc_df, by="canal") %>%
  mutate(roi = contribution / spend)

p_roi_cat <- ggplot(roi_cat, aes(x=reorder(canal, roi), y=roi, fill=canal)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title="ROI por medio – MMM1", y="ROI (Contribución / Inversión)", x="") +
  theme_minimal(base_size=12)
elas_cat <- sapply(paid_cols_cat, function(col){
  x <- df_cat[[col]]
  y <- df_cat$tom
  
  ok <- which(x > 0 & y > 0)
  if (length(ok) < 6) return(NA)
  
  coef(lm(log(y[ok]) ~ log(x[ok])))[2]
})

elastic_cat <- data.frame(
  canal = names(elas_cat),
  elasticidad = round(as.numeric(elas_cat), 3)
)

p_elas_cat <- ggplot(elastic_cat, aes(x=reorder(canal, elasticidad), y=elasticidad, fill=canal)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title="Elasticidades (log-log) – MMM1", x="", y="Elasticidad TOM vs inversión") +
  theme_minimal(base_size=12)
df_cat2 <- df_cat %>%
  mutate(inversion_total = tv + radio + prensa + exterior)

p_tom_cat <- ggplot(df_cat2, aes(x=inversion_total, y=tom)) +
  geom_point(size=3, alpha=0.8) +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="TOM vs inversión total – MMM1",
       x="Inversión mensual total",
       y="Top of Mind promedio del mercado") +
  theme_minimal(base_size=12)
dashboard_mmm1 <- 
  (p_soi_cat | p_soc_cat) /
  (p_roi_cat | p_elas_cat) /
  p_tom_cat

dashboard_mmm1
write_xlsx(
  list(
    "Contribuciones" = soc_df,
    "Inversiones" = spend_df,
    "ROI" = roi_cat,
    "Elasticidades" = elastic_cat,
    "Base_MMM1" = df_cat
  ),
  "MMM1_Report.xlsx"
)



# MMM1
Result_cat$xDecompAgg %>%
  select(rsq_train, nrmse_train, mape, decomp.rssd) %>%
  distinct()
# MMM2
Result_comp$xDecompAgg %>%
  select(rsq_train, nrmse_train, mape, decomp.rssd, rsq_val, nrmse_val) %>%
  distinct()
library(Robyn)

Result_comp <- robyn_outputs(
  InputCollect = InputCollect_comp,
  OutputModels = OutputModels_comp
)



library(writexl)
write_xlsx(Result_comp$xDecompAgg, "MMM2_indicadores_calidad.xlsx")
write_xlsx(Result_cat$xDecompAgg, "MMM1_indicadores_calidad.xlsx")


decomp <- Result_cat$xDecompAgg

media_vars <- c("tv", "radio", "prensa", "exterior")

df_mmm1 <- decomp %>% 
  filter(rn %in% media_vars) %>%
  select(canal = rn, spend = total_spend, contribution = xDecompAgg) %>%
  mutate(
    soi = spend / sum(spend),
    soc = contribution / sum(contribution),
    roi = contribution / spend
  )

df_soi_soc <- df_mmm1 %>%
  mutate(
    soi_pct = round(soi * 100, 1),
    soc_pct = round(soc * 100, 1)
  ) %>%
  select(canal, soi_pct, soc_pct)


ggplot(df_mmm1, aes(x = reorder(canal, roi), y = roi, fill = canal)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "ROI por Medio – Mercado Hair Care",
    x = "",
    y = "ROI (Contribution / Spend)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df_mmm1, aes(x = canal, y = contribution, fill = canal)) +
  geom_col() +
  labs(
    title = "Contribución al TOM del Mercado",
    x = "",
    y = "Contribución total"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

df_soi_soc

df_mmm1 <- Result_cat$xDecompAgg %>%
  filter(rn %in% c("tv", "radio", "prensa", "exterior")) %>%
  group_by(canal = rn) %>%
  summarise(
    spend = sum(total_spend, na.rm = TRUE),
    contribution = sum(xDecompAgg, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    soi = spend / sum(spend),
    soc = contribution / sum(contribution),
    soi_pct = round(soi * 100, 1),
    soc_pct = round(soc * 100, 1)
  )
df_mmm1

library(ggplot2)
df_plot <- df_mmm1 %>%
  mutate(
    soi_pct = soi * 100,
    soc_pct = soc * 100
  ) %>%
  select(canal, soi_pct, soc_pct) %>%
  tidyr::pivot_longer(cols = c("soi_pct", "soc_pct"),
                      names_to = "tipo", values_to = "valor")

ggplot(df_plot, aes(x = canal, y = valor, fill = tipo)) +
  geom_col(position = "dodge") +
  labs(
    title = "SOI vs SOC por Medio – Mercado Hair Care",
    x = "",
    y = "% del total"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(
    values = c("soi_pct" = "#1f77b4", "soc_pct" = "#ff7f0e"),
    labels = c("SOC (Share of Contribution)", "SOI (Share of Investment)")
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

df_gap <- df_mmm1 %>%
  mutate(
    gap = soc - soi,
    gap_pct = gap * 100
  )

ggplot(df_gap, aes(x = reorder(canal, gap_pct), y = gap_pct, fill = gap_pct > 0)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Diferencia SOC – SOI por Medio",
    x = "",
    y = "Puntos porcentuales"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("TRUE" = "#2ca02c", "FALSE" = "#d62728")) +
  theme_minimal() +
  theme(legend.position = "none")

library(ggplot2)

df_soi <- df_mmm1 %>%
  mutate(soi_pct = soi * 100)

ggplot(df_soi, aes(x = reorder(canal, soi_pct), y = soi_pct, fill = canal)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = paste0(round(soi_pct, 1), "%")),
            hjust = -0.1, size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, max(df_soi$soi_pct) * 1.15)) +
  labs(
    title = "Share of Investment (SOI) por Medio – Mercado Hair Care",
    x = "",
    y = "% del total invertido"
  ) +
  theme_minimal()


library(ggplot2)

df_comp <- Result_comp$xDecompAgg %>%
  filter(rn %in% c("inversion_total_SAVITAL",
                   "inversion_total_PANTENE_PRO_V",
                   "inversion_total_NUTRIBELA")) %>%
  mutate(
    marca = gsub("inversion_total_", "", rn)
  )

ggplot(df_comp, aes(x = reorder(marca, xDecompAgg), y = xDecompAgg, fill = marca)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Contribución total al TOM según inversión",
    x = "",
    y = "Contribución"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

df_roi <- df_comp %>%
  mutate(roi = xDecompAgg / total_spend)

ggplot(df_roi, aes(x = reorder(marca, roi), y = roi, fill = marca)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "ROI por Medio – Savital",
    x = "",
    y = "ROI"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

df_media <- Result_comp$xDecompAgg %>%
  filter(grepl("SAVITAL", rn))

ggplot(df_media, aes(x = reorder(rn, roi_mean), y = roi_mean)) +
  geom_col() +
  coord_flip()

ggplot(Result_comp$xDecompAgg, aes(x = reorder(rn, coef), y = coef)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Impacto relativo de cada driver en TOM Savital",
    x = "",
    y = "Coeficiente"
  ) +
  theme_minimal()

df_bubbles <- Result_comp$xDecompAgg %>%
  filter(rn %in% c("inversion_total_SAVITAL",
                   "inversion_total_PANTENE_PRO_V",
                   "inversion_total_NUTRIBELA")) %>%
  mutate(
    marca = gsub("inversion_total_", "", rn),
    roi = xDecompAgg / total_spend
  )

ggplot(df_bubbles, aes(x = total_spend, y = xDecompAgg, size = roi, label = marca)) +
  geom_point(alpha = 0.6) +
  geom_text(vjust = -1) +
  scale_size(range = c(5,15)) +
  labs(
    title = "Eficiencia comparada: Spend vs Contribution",
    x = "Inversión total",
    y = "Contribución al TOM"
  ) +
  theme_minimal()

library(ggplot2)
library(dplyr)

df_sc <- Result_comp$xDecompAgg %>%
  filter(rn %in% c("inversion_total_SAVITAL",
                   "inversion_total_PANTENE_PRO_V",
                   "inversion_total_NUTRIBELA")) %>%
  mutate(
    marca = gsub("inversion_total_", "", rn),
    marca = gsub("_", " ", marca),
    roi = xDecompAgg / total_spend
  ) %>%
  select(marca, spend = total_spend, contrib = xDecompAgg)

df_long <- df_sc %>%
  tidyr::pivot_longer(cols = c(spend, contrib),
                      names_to = "tipo",
                      values_to = "valor")

ggplot(df_long, aes(x = marca, y = valor, fill = tipo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Inversión vs Contribución al TOM",
    x = "",
    y = "Valor"
  ) +
  theme_minimal()

ggplot(df_sc, aes(x = reorder(marca, roi), y = roi, fill = marca)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "ROI por Marca – Eficiencia en TOM",
    x = "",
    y = "ROI"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df_sc, aes(x = spend, y = contrib, label = marca)) +
  geom_point(size = 6) +
  geom_text(vjust = -1) +
  labs(
    title = "Inversión vs Contribución al TOM (3 Competidores)",
    x = "Inversión total",
    y = "Contribución total"
  ) +
  theme_minimal()

ggplot(df_sc, aes(x = reorder(marca, contrib), y = contrib, fill = marca)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Contribución al TOM por Marca",
    x = "",
    y = "Contribución"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df_sc, aes(x = reorder(marca, roi), y = roi, fill = marca)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "ROI por Marca – Eficiencia en TOM",
    x = "",
    y = "ROI"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

names(df_sc)
df_sc <- df_sc %>% 
  mutate(roi = contribution / spend)

df_sc_sum <- df_sc %>%
  group_by(marca) %>%
  summarise(
    spend = sum(spend, na.rm = TRUE),
    contribution = sum(contribution, na.rm = TRUE)
  ) %>%
  mutate(roi = contribution / spend)
ggplot(df_sc_sum, aes(x = reorder(marca, roi), y = roi, fill = marca)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "ROI por Marca – Eficiencia en TOM",
    x = "",
    y = "ROI"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
glimpse(df_sc_sum)

library(ggplot2)
library(dplyr)

ggplot(df_sc_sum, aes(x = reorder(marca, roi), y = roi, fill = marca)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "ROI por Marca – Eficiencia en Generación de TOM",
    x = "",
    y = "ROI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  )
df_sc_sum <- df_sc_sum %>%
  mutate(
    soi = spend / sum(spend),
    soc = contribution / sum(contribution)
  )
ggplot(df_sc_sum, aes(x = soi, y = soc, label = marca)) +
  geom_point(size = 5) +
  geom_text(vjust = -0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = "SOI vs SOC por Marca",
    x = "Share of Investment (SOI)",
    y = "Share of Contribution (SOC)"
  ) +
  theme_minimal()

df_long <- df_sc_sum %>%
  select(marca, soi, soc) %>%
  pivot_longer(cols = c(soi, soc), names_to = "tipo", values_to = "valor")

ggplot(df_long, aes(x = marca, y = valor, fill = tipo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Comparación SOI vs SOC por Marca",
    x = "",
    y = "Participación"
  ) +
  theme_minimal()
df_sc_sum <- df_sc_sum %>%
  mutate(
    soi = spend / sum(spend),
    soc = contribution / sum(contribution)
  )
df_long <- df_sc_sum %>%
  select(marca, soi, soc) %>%
  pivot_longer(cols = c(soi, soc),
               names_to = "metric",
               values_to = "value")
ggplot(df_long, aes(x = marca, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "SOI vs SOC por Marca",
    x = "",
    y = "Porcentaje",
    fill = ""
  ) +
  theme_minimal(base_size = 13)

# Extraer la curva de respuesta de Savital
response_curves <- Result_comp$OutputModels$resultHypParam$response

# Filtrar solo Savital
sav_curve <- response_curves %>%
  filter(channel == "inversion_total_SAVITAL")

# --- 1) Extraer fila de SAVITAL en xDecompAgg ---
row_sav <- Result_comp$xDecompAgg %>% 
  filter(rn == "inversion_total_SAVITAL")

coef_sav <- row_sav$coef
gamma_sav <- row_sav$mean_carryover  # adstock geométrico
alpha_sav <- 1                       # Robyn lo usa para hill; si no existe, asumimos 1

spend_seq <- seq(
  from = 0,
  to   = max(df_wide2$inversion_total_SAVITAL, na.rm = TRUE) * 2,
  length.out = 100
)
adstock_seq <- spend_seq * gamma_sav
response_seq <- coef_sav * (adstock_seq / (1 + adstock_seq))
df_curve <- data.frame(
  spend = spend_seq,
  response = response_seq
)

length(spend_seq)
length(adstock_seq)
length(response_seq)
length(coef_sav)
length(gamma_sav)
str(coef_sav)
str(gamma_sav)


# --- 1) Filtrar solo la solución top para SAVITAL ---
row_sav <- Result_comp$xDecompAgg %>% 
  filter(rn == "inversion_total_SAVITAL", top_sol == TRUE)

coef_sav <- row_sav$coef
gamma_sav <- row_sav$mean_carryover  # adstock geométrico

# --- 2) Crear secuencia de inversión simulada ---
spend_seq <- seq(
  from = 0,
  to = max(df_wide2$inversion_total_SAVITAL, na.rm = TRUE) * 2,
  length.out = 100
)

# --- 3) Adstock geométrico (curva estática) ---
adstock_seq <- spend_seq * gamma_sav

# --- 4) Saturación tipo Hill con alpha = 1 ---
response_seq <- coef_sav * (adstock_seq / (1 + adstock_seq))

# --- 5) Crear data frame ---
df_curve <- data.frame(
  spend = spend_seq,
  response = response_seq
)

# --- 6) Graficar ---
library(ggplot2)

ggplot(df_curve, aes(x = spend, y = response)) +
  geom_line(size = 1.2) +
  labs(
    title = "Curva de respuesta – Savital (MMM2)",
    x = "Inversión",
    y = "Contribución estimada al TOM"
  ) +
  theme_minimal(base_size = 14)


names(Result_comp$plots)
str(Result_comp, max.level = 2)
names(Result_comp)
list.files(Result_comp$plot_folder, recursive = TRUE)

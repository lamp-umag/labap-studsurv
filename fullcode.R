## 0 Package management --------
required_packages <- c("dplyr", "ggplot2", "psych", "lavaan", "tidyr", 
                       "ggbeeswarm", "rstatix", "gridExtra", "grid", "semPlot")

# Check and install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all packages
lapply(required_packages, library, character.only = TRUE)

## 1 setup ---------
# Load necessary library
library(dplyr)

# Load the dataset
file_path <- "cuestionario.csv"
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Add participant ID (pidn) as the first column
data <- data %>%
  mutate(pidn = row_number())

# Reorder `pidn` to be the first column
data <- data %>%
  select(pidn, everything())

# Rename columns
names(data) <- c(
  "pidn", "tstp", "plev", "psex", "pgen", "pest", "psne",
  "reg1", "reg2", "reg3", "lab1", "lab2", "lab3"
)

# Recode `plev` as numbers
data <- data %>%
  mutate(
    plev = case_when(
      plev == "Pre-escolar" ~ 1,
      plev == "1ero a 4to básico" ~ 2,
      plev == "5to a 8to básico" ~ 3,
      plev == "Educación Media" ~ 4,
      plev == "Básica" ~ NA_real_
    )
  )

# Recode psex to 1-2-3 (original coding retained)
data <- data %>%
  mutate(
    psex = case_when(
      psex == "Masculino" ~ 1, # Male
      psex == "Femenino" ~ 2,  # Female
      psex == "Otro" ~ 3       # Other
    )
  )

# Create psed (sex dichotomized)
data <- data %>%
  mutate(
    psed = case_when(
      psex == 1 ~ 1, # Male
      TRUE ~ 0       # Female and Other
    )
  )

# Relocate psed to be immediately after psex
data <- data %>%
  relocate(psed, .after = psex)

# View the updated data structure
str(data)

# Recode pgen to numeric values
data <- data %>%
  mutate(
    pgen = case_when(
      pgen == "Masculino" ~ 1, # Male
      pgen == "Femenino" ~ 2,  # Female
      pgen == "Otro" ~ 3       # Other
    )
  )


# Create `pge2` for gender identity
data <- data %>%
  mutate(
    pge2 = case_when(
      psex == 1 & pgen == 1 ~ 1, # Male cis
      psex == 2 & pgen == 2 ~ 2, # Female cis
      TRUE ~ 3 # Other or non-cisgender identities
    )
  )

# Reorder columns to place `pge2` after `pgen`
data <- data %>%
  relocate(pge2, .after = pgen)
# Standardize school names in `pest`
school_mapping <- c(
  "Instituto Don Bosco" = "Don Bosco",
  "Escuela villa las nieves" = "Villa las Nieves",
  "Escuela Villa las Nieves " = "Villa las Nieves",
  "Instituto Don Bosco " = "Don Bosco",
  "Instituto Don bosco" = "Don Bosco",
  "Centro educacional sembrado vidas " = "Sembrando Vidas",
  "Centro educacional sembrando vidas " = "Sembrando Vidas",
  "Instituto don Bosco " = "Don Bosco",
  "Escuela Libertador Bernardo O'higgins " = "Bernardo O'Higgins",
  "Escuela bernardo ohiggins " = "Bernardo O'Higgins",
  "Escuela bernardo ohggins" = "Bernardo O'Higgins",
  "Bernardo O'Higgins " = "Bernardo O'Higgins",
  "Escuela Patagonia" = "Patagonia",
  "Colegio Luterano" = "Luterano",
  "Colegio Luterano " = "Luterano",
  "Colegio Charles darwin" = "Charles Darwin",
  "Colegio Charles Darwin" = "Charles Darwin",
  "Charles Darwin " = "Charles Darwin",
  "Villa las Nieves " = "Villa las Nieves",
  "Colegio Francés " = "Francés",
  "Escuela Villa las Nieves" = "Villa las Nieves",
  "Escuela Bernardo O'higgings " = "Bernardo O'Higgins",
  "Escuela libertador Bernardo O'Higgins " = "Bernardo O'Higgins",
  "Escuela libertador Bernardo O'Higgins" = "Bernardo O'Higgins",
  "Escuela Patagonia " = "Patagonia",
  "Colegio luterano" = "Luterano",
  "Colegio Punta Arenas " = "Punta Arenas",
  "Aleman" = "Alemán",
  "Ds Punta arenas" = "Alemán",
  "Aleman " = "Alemán",
  "Colegio alemán de Punta Arenas " = "Alemán",
  "Alemán" = "Alemán",
  "Colegio Contardi" = "Contardi",
  "Colegio Contardi " = "Contardi",
  "INSAFA" = "Insafa",
  "Colegio Bernardo O’higgins" = "Bernardo O'Higgins",
  "Colegio Punta Arenas" = "Punta Arenas",
  "Escuela Bernardo O'Higgins " = "Bernardo O'Higgins",
  "Colegio Alemán " = "Alemán",
  "Colegio alemán " = "Alemán",
  "Colegio aleman" = "Alemán",
  "Británico " = "Británico",
  "Colegio Británico " = "Británico",
  "The british school" = "The British School",
  "The British School " = "The British School",
  "The British school" = "The British School",
  "Educación Adventista de Punta Arenas" = "Adventista",
  "Liceo adventista de punta arenas " = "Adventista",
  "Liceo adventista Punta Arenas " = "Adventista",
  "Adventista " = "Adventista",
  "Insuco" = "Insuco",
  "INSUCO " = "Insuco",
  "INSUCO" = "Insuco",
  "Centro Educativo Bambú" = "Bambú",
  "Centro educativo Bambú" = "Bambú",
  "Liceo adventista " = "Adventista"
)

data <- data %>%
  mutate(pest = recode(pest, !!!school_mapping))


# Standardize Likert scale responses
likert_map <- c(
  "Muy de acuerdo" = 5,
  "De acuerdo" = 4,
  "Neutral" = 3,
  "Ni acuerdo ni en desacuerdo" = 3,
  "En desacuerdo" = 2,
  "Muy en desacuerdo" = 1
)

data <- data %>%
  mutate(across(c(reg1, reg2, reg3, lab1, lab2, lab3), ~ likert_map[.], .names = "{.col}"))

# Standardize dichotomous variables
dichotomous_map <- c("Sí" = 1, "No" = 0)

data <- data %>%
  mutate(psne = dichotomous_map[psne])

# Remove rows where 'pest' is empty
data <- data %>%
  filter(!is.na(pest) & pest != "")

# Save the cleaned dataset
output_file <- "odf.csv"
write.csv(data, output_file, row.names = FALSE)

cat("Data cleaned and saved to", output_file)

## 2 reliability and composite scores -------
# Load necessary libraries
library(psych)

# Reverse code lab2 and lab3
data <- data %>%
  mutate(
    lab2_rev = 6 - lab2, # Reverse code (assuming Likert scale is 1 to 5)
    lab3_rev = 6 - lab3  # Reverse code
  )

# Compute Cronbach's alpha and omega for reg items
reg_items <- data %>% select(reg1, reg2, reg3)
reg_alpha <- psych::alpha(reg_items)
cat("Cronbach's alpha for reg items:", round(reg_alpha$total$raw_alpha, 3), "\n")
reg_omega <- psych::omega(reg_items)
cat("McDonald's omega for reg items:", round(reg_omega$omega.tot, 3), "\n")

# Compute average composite score for reg
data <- data %>%
  mutate(regc = rowMeans(reg_items, na.rm = TRUE))

# Compute Cronbach's alpha and omega for lab items
lab_items <- data %>% select(lab1, lab2_rev, lab3_rev)
lab_alpha <- psych::alpha(lab_items)
cat("Cronbach's alpha for lab items (reverse coded considered):", round(lab_alpha$total$raw_alpha, 3), "\n")
lab_omega <- psych::omega(lab_items)
cat("McDonald's omega for lab items (reverse coded considered):", round(lab_omega$omega.tot, 3), "\n")

# Compute average composite score for lab
data <- data %>%
  mutate(labc = rowMeans(lab_items, na.rm = TRUE))

# Print head of data to verify new columns
head(data)

# Calculate Pearson correlation between regc and labc
avg_correlation <- cor(data$regc, data$labc, use = "complete.obs", method = "pearson")

# Print the correlation
cat("Correlation between regc and labc:", round(avg_correlation, 3), "\n")

## 3 cfa and factor scores -------
# Load necessary library
library(lavaan)

# Define the CFA model
model <- '
  reg =~ reg1 + reg2 + reg3
  lab =~ lab1 + lab2 + lab3
'

# Fit the CFA model with ordinal variables
fit <- cfa(model, data = data, ordered = c("reg1", "reg2", "reg3", "lab1", "lab2", "lab3"))

# Summarize the results
summary(fit, fit.measures = TRUE, standardized = TRUE)

# nice semplot

semPaths(fit, what="col", whatLabels = "std",
         edge.label.cex = .8,nCharEdges=0,nCharNodes = 0,
         structural=F,optimizeLatRes=F,
         #style="mx",residuals = T,#residScale = 4,
         layout="tree", rotation=2,
         curvePivot = T,exoVar = F,intercepts = F,thresholds = F,
         sizeLat = 7,sizeLat2 = 7,
         sizeMan = 4,sizeMan2 = 1.75,
         edge.width=4)
# Add fit indices to plot
fitind <- round(fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr")), 3)
title(sub = sprintf("CFI = %.3f   TLI = %.3f   RMSEA = %.3f   SRMR = %.3f", 
                    fitind["cfi"], fitind["tli"], fitind["rmsea"], fitind["srmr"]),
      line = 1)

# Extract factor scores and add to the dataframe
factor_scores <- lavPredict(fit)
data <- data %>%
  mutate(
    regf = factor_scores[, "reg"],
    labf = factor_scores[, "lab"]
  )



# Calculate Pearson correlation between regf and labf
factor_correlation <- cor(data$regf, data$labf, use = "complete.obs", method = "pearson")

# Print the correlation
cat("Correlation between regf and labf:", round(factor_correlation, 3), "\n")


## 4 nice desc plots------
#plots
library(ggplot2)

ggplot(data, aes(x = regc, y = labc)) +
  geom_jitter(color = "forestgreen", alpha = 0.7, size = 3, width = 0.1, height = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = "purple", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Scatterplot of Composite Scores (regc vs labc) with Jitter",
    x = "regc (Composite Score for reg Items)",
    y = "labc (Composite Score for lab Items)"
  ) +
  theme(text = element_text(size = 12))


# Plot histogram for 'regf'
ggplot(data, aes(x = regf)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of regf", x = "regf Factor Score", y = "Frequency") +
  theme(text = element_text(size = 12))

# Plot histogram for 'labf'
ggplot(data, aes(x = labf)) +
  geom_histogram(binwidth = 0.5, fill = "darkorange", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of labf", x = "labf Factor Score", y = "Frequency") +
  theme(text = element_text(size = 12))


#
ggplot(data, aes(x = regf, y = labf)) +
  geom_jitter(color = "forestgreen", alpha = 0.7, size = 3, width = 0.1, height = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = "purple", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Scatterplot of Factor Scores (regf vs labf) with Jitter",
    x = "regc (Factor Score for reg Items)",
    y = "labc (Factor Score for lab Items)"
  ) +
  theme(text = element_text(size = 12))


## 5 Group comparisons and visualization --------
# Function to run and report t-tests
run_t_test <- function(data, dv, group, group_name, dv_name) {
  test <- t.test(data[[dv]] ~ data[[group]])
  cat(sprintf("\nt-test for %s by %s:\n", dv_name, group_name))
  cat(sprintf("t = %.3f, df = %.2f, p = %.3f\n", 
              test$statistic, test$parameter, test$p.value))
  cat(sprintf("Mean difference = %.3f\n", diff(test$estimate)))
  return(test)
}

# T-tests for special education needs (psne)
# Composite scores
t_psne_regc <- run_t_test(data, "regc", "psne", "Special Ed. Needs", "Regional Care Attitudes (Composite)")
t_psne_labc <- run_t_test(data, "labc", "psne", "Special Ed. Needs", "Lab Experience (Composite)")

# Factor scores
t_psne_regf <- run_t_test(data, "regf", "psne", "Special Ed. Needs", "Regional Care Attitudes (Factor)")
t_psne_labf <- run_t_test(data, "labf", "psne", "Special Ed. Needs", "Lab Experience (Factor)")

# ANOVA for education level (plev) and gender identity (pge2)
run_anova <- function(data, dv, group, group_name, dv_name) {
  model <- aov(data[[dv]] ~ factor(data[[group]]))
  cat(sprintf("\nANOVA for %s by %s:\n", dv_name, group_name))
  print(summary(model))
  return(model)
}

# Run ANOVAs for composite scores
anova_plev_regc <- run_anova(data, "regc", "plev", "Education Level", "Regional Care Attitudes (Composite)")
anova_pge2_regc <- run_anova(data, "regc", "pge2", "Gender Identity", "Regional Care Attitudes (Composite)")
anova_plev_labc <- run_anova(data, "labc", "plev", "Education Level", "Lab Experience (Composite)")
anova_pge2_labc <- run_anova(data, "labc", "pge2", "Gender Identity", "Lab Experience (Composite)")

# Run ANOVAs for factor scores
anova_plev_regf <- run_anova(data, "regf", "plev", "Education Level", "Regional Care Attitudes (Factor)")
anova_pge2_regf <- run_anova(data, "regf", "pge2", "Gender Identity", "Regional Care Attitudes (Factor)")
anova_plev_labf <- run_anova(data, "labf", "plev", "Education Level", "Lab Experience (Factor)")
anova_pge2_labf <- run_anova(data, "labf", "pge2", "Gender Identity", "Lab Experience (Factor)")

## 6 comparison plots --------

# Helper function for consistent limits including jitter space
get_consistent_limits <- function(data, vars) {
  all_values <- unlist(data[vars])
  min_val <- floor(min(all_values, na.rm = TRUE)) - 0.2
  max_val <- ceiling(max(all_values, na.rm = TRUE)) + 0.2
  return(c(min_val, max_val))
}

# Main boxplot function
create_boxplot <- function(data, x, y, title, xlab, ylab, y_limits = NULL) {
  # Calculate n for each group and add to labels
  n_counts <- table(data[[x]])
  
  # Factor level labeling with counts
  if(x == "plev") {
    labels <- c("Pre-escolar", "1-4 básico", "5-8 básico", "Ed. Media")
    labels_with_n <- paste0(labels, "\n(n=", n_counts[1:4], ")")
    data[[x]] <- factor(data[[x]], 
                        levels = 1:4,
                        labels = labels_with_n)
  } else if(x == "pge2") {
    labels <- c("Hombre Cis", "Mujer Cis", "Otro/No-Cis")
    labels_with_n <- paste0(labels, "\n(n=", n_counts[1:3], ")")
    data[[x]] <- factor(data[[x]], 
                        levels = 1:3,
                        labels = labels_with_n)
  } else if(x == "psne") {
    labels <- c("No", "Sí")
    labels_with_n <- paste0(labels, "\n(n=", n_counts[1:2], ")")
    data[[x]] <- factor(data[[x]], 
                        levels = 0:1,
                        labels = labels_with_n)
  }
  
  p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y))) +
    geom_hline(yintercept = seq(floor(min(data[[y]])), 
                                ceiling(max(data[[y]])), 
                                by = 0.5),
               color = "gray90", linewidth = 0.2) +
    geom_boxplot(fill = "lightblue", 
                 alpha = 0.5,
                 outlier.shape = NA,
                 color = "gray30") +
    geom_jitter(position = position_jitter(width = 0.2, height = 0.2),
                alpha = 0.4, 
                size = 1.5,
                color = "navy") +
    stat_summary(fun.data = mean_cl_normal, 
                 geom = "pointrange",
                 color = "#FF9999",
                 size = 0.8,
                 position = position_nudge(x = 0.3)) +
    theme_minimal() +
    labs(title = title, 
         x = xlab, 
         y = ylab) +
    theme(
      plot.title = element_text(size = 11, face = "bold", margin = margin(b = 10)),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
    )
  
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  return(p)
}

# Create plots for composite scores
comp_limits <- get_consistent_limits(data, c("regc", "labc"))

p1 <- create_boxplot(data, "plev", "regc",
                     "Actitudes Regional por Nivel Educativo",
                     "Nivel Educativo", "Puntaje Compuesto",
                     y_limits = comp_limits)

p2 <- create_boxplot(data, "pge2", "regc",
                     "Actitudes Regional por Identidad de Género",
                     "Identidad de Género", "Puntaje Compuesto",
                     y_limits = comp_limits)

p3 <- create_boxplot(data, "psne", "regc",
                     "Actitudes Regional por NEE",
                     "NEE", "Puntaje Compuesto",
                     y_limits = comp_limits)

# Combine composite score plots
composite_plot <- p1 + p2 + p3 + 
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Comparación de Actitudes hacia el Cuidado Regional (Puntajes Compuestos)",
    theme = theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
  )

print(composite_plot)

# Create plots for factor scores
factor_limits <- get_consistent_limits(data, c("regf", "labf"))

p4 <- create_boxplot(data, "plev", "regf",
                     "Actitudes Regional por Nivel Educativo",
                     "Nivel Educativo", "Puntaje Factorial",
                     y_limits = factor_limits)

p5 <- create_boxplot(data, "pge2", "regf",
                     "Actitudes Regional por Identidad de Género",
                     "Identidad de Género", "Puntaje Factorial",
                     y_limits = factor_limits)

p6 <- create_boxplot(data, "psne", "regf",
                     "Actitudes Regional por NEE",
                     "NEE", "Puntaje Factorial",
                     y_limits = factor_limits)

# Combine factor score plots
factor_plot <- p4 + p5 + p6 + 
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Comparación de Actitudes hacia el Cuidado Regional (Puntajes Factoriales)",
    theme = theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
  )

print(factor_plot)

# Create plots for lab experience composite scores
p7 <- create_boxplot(data, "plev", "labc",
                     "Experiencia Lab por Nivel Educativo",
                     "Nivel Educativo", "Puntaje Compuesto",
                     y_limits = comp_limits)

p8 <- create_boxplot(data, "pge2", "labc",
                     "Experiencia Lab por Identidad de Género",
                     "Identidad de Género", "Puntaje Compuesto",
                     y_limits = comp_limits)

p9 <- create_boxplot(data, "psne", "labc",
                     "Experiencia Lab por NEE",
                     "NEE", "Puntaje Compuesto",
                     y_limits = comp_limits)

# Combine lab composite score plots
lab_composite_plot <- p7 + p8 + p9 + 
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Comparación de Experiencias en el Laboratorio (Puntajes Compuestos)",
    theme = theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
  )

print(lab_composite_plot)

# Create plots for lab experience factor scores
p10 <- create_boxplot(data, "plev", "labf",
                      "Experiencia Lab por Nivel Educativo",
                      "Nivel Educativo", "Puntaje Factorial",
                      y_limits = factor_limits)

p11 <- create_boxplot(data, "pge2", "labf",
                      "Experiencia Lab por Identidad de Género",
                      "Identidad de Género", "Puntaje Factorial",
                      y_limits = factor_limits)

p12 <- create_boxplot(data, "psne", "labf",
                      "Experiencia Lab por NEE",
                      "NEE", "Puntaje Factorial",
                      y_limits = factor_limits)

# Combine lab factor score plots
lab_factor_plot <- p10 + p11 + p12 + 
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Comparación de Experiencias en el Laboratorio (Puntajes Factoriales)",
    theme = theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
  )

print(lab_factor_plot)

## 7 make pdfs for plots --------------------

# For histograms
pdf("hist_regf.pdf", width = 8, height = 6)
ggplot(data, aes(x = regf)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of regf", x = "regf Factor Score", y = "Frequency") +
  theme(text = element_text(size = 12))
dev.off()

pdf("hist_labf.pdf", width = 8, height = 6)
ggplot(data, aes(x = labf)) +
  geom_histogram(binwidth = 0.5, fill = "darkorange", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of labf", x = "labf Factor Score", y = "Frequency") +
  theme(text = element_text(size = 12))
dev.off()

# For scatter plots
pdf("scat_comp.pdf", width = 8, height = 6)
ggplot(data, aes(x = regc, y = labc)) +
  geom_jitter(color = "forestgreen", alpha = 0.7, size = 3, width = 0.1, height = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = "purple", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Scatterplot of Composite Scores (regc vs labc) with Jitter",
    x = "regc (Composite Score for reg Items)",
    y = "labc (Composite Score for lab Items)"
  ) +
  theme(text = element_text(size = 12))
dev.off()

pdf("scat_fact.pdf", width = 8, height = 6)
ggplot(data, aes(x = regf, y = labf)) +
  geom_jitter(color = "forestgreen", alpha = 0.7, size = 3, width = 0.1, height = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = "purple", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Scatterplot of Factor Scores (regf vs labf) with Jitter",
    x = "regf (Factor Score for reg Items)",
    y = "labf (Factor Score for lab Items)"
  ) +
  theme(text = element_text(size = 12))
dev.off()

# For comparison plots
pdf("comp_regc.pdf", width = 15, height = 6)
print(composite_plot)
dev.off()

pdf("comp_regf.pdf", width = 15, height = 6)
print(factor_plot)
dev.off()

pdf("comp_labc.pdf", width = 15, height = 6)
print(lab_composite_plot)
dev.off()

pdf("comp_labf.pdf", width = 15, height = 6)
print(lab_factor_plot)
dev.off()






# Create nice semPlot
pdf("plot_cfa_.pdf", width = 10, height = 8)
semPaths(fit, what="col", whatLabels = "std",
         edge.label.cex = .8,nCharEdges=0,nCharNodes = 0,
         structural=F,optimizeLatRes=F,
         #style="mx",residuals = T,#residScale = 4,
         layout="tree", rotation=2,
         curvePivot = T,exoVar = F,intercepts = F,thresholds = F,
         sizeLat = 7,sizeLat2 = 7,
         sizeMan = 4,sizeMan2 = 1.75,
         edge.width=4)


# Add fit indices to plot
fitind <- round(fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr")), 3)
title(sub = sprintf("CFI = %.3f   TLI = %.3f   RMSEA = %.3f   SRMR = %.3f", 
                    fitind["cfi"], fitind["tli"], fitind["rmsea"], fitind["srmr"]),
      line = 1)
dev.off()

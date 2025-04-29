# Install and load the meta package
#install.packages("meta")


# Load required package
library(meta)

# Forest plot for TAK-003
tak003_data <- data.frame(
  Study = c(
    "Green et al. 2023",
    "Green et al. 2023",
    "Tricou et al. 2023",
    "Tricou et al. 2020",
    "Sirivichayakul et al. 2022",
    "Biswal et al. 2021",
    "Biswal et al. 2021",
    "Green et al. 2023",
    "Tricou et al. 2020"
  ),
  Category = c(
    "Vaccine Efficacy:",
    "Vaccine Efficacy:",
    "Vaccine Efficacy:",
    "Immunogenicity:",
    "Immunogenicity:",
    "Immunogenicity:",
    "Immunogenicity:",
    "Safety:",
    "Safety:"
  ),
  Group = c(
    "Confirmed dengue",
    "Hosp dengue",
    "Symptomatic dengue",
    "Day120",
    "Month36",
    "Month4",
    "Month9",
    "SAE",
    "Severe solicited sys AE"
  ),
  AgeGroup = c(
    "4 to 16 years",
    "4 to 16 years",
    "18 years+",
    "2 to 17 years",
    "1 to 45 years",
    "2 to 17 years",
    "2 to 17 years",
    "4 to 16 years",
    "2 to 17 years"
  ),
  StudyPeriod = c(
    "7 months",
    "7 months",
    "15 months",
    "3 months",
    "53 months",
    "14 months",
    "14 months",
    "7 months",
    "3 months"
  ),
  event.e = c(236, 97, 6, 251, 306, 299, 257, round(0.05 * 13340), round(0.013 * 264)),
  n.e     = c(12177, 12177, 201, 264, 360, 300, 300, 13340, 264),
  event.c = c(round(0.10 * 6080), round(0.10 * 6080), round(0.10 * 199),
              round(0.50 * 132), round(0.45 * 360), round(0.10 * 100), 8,
              round(0.059 * 6687), 0),
  n.c     = c(6080, 6080, 199, 132, 360, 100, 100, 6687, 132),
  stringsAsFactors = FALSE
)

# Remove rows with NA values
tak003_data <- na.omit(tak003_data)

# Run meta-analysis using metabin
meta_tak003 <- metabin(
  event.e = tak003_data$event.e,
  n.e     = tak003_data$n.e,
  event.c = tak003_data$event.c,
  n.c     = tak003_data$n.c,
  studlab = tak003_data$Study,
  data    = tak003_data,
  sm      = "RR",
  byvar   = tak003_data$Category,
  comb.fixed  = FALSE,
  comb.random = TRUE,
  method.tau = "DL",
  print.byvar = FALSE
  )

# Open a PDF device to save the plot
pdf("/home/shweta/Downloads/forest_plot_TAK003_final.pdf", width = 15, height = 10)

# Plot the forest plot with enhanced formatting
forest(meta_tak003,
       xlab        = "Risk Ratio (Vaccine vs Placebo)",
       leftcols    = c("studlab", "Group", "AgeGroup", "StudyPeriod", "event.e", "n.e", "event.c", "n.c"),
       leftlabs    = c("Study", "Group", "Age Group", "Study Period", "Vax Events", "Vax N", "Placebo Events", "Placebo N"),
       rightcols   = c("effect", "ci"),
       rightlabs   = c("Risk Ratio", "95% CI"),
       col.by      = "black",
       print.I2    = TRUE,
       print.tau2  = TRUE,
       smlab       = "",
       cex         = 0.8,
       colgap.forest = "1cm", spacing = 1.5,
       header = TRUE
)

# Close the PDF device
dev.off()




######################################################################################################################################



###########################################################################################################################################

### Forest plot for CYD-TDV


library(meta)

# Original data with effect sizes and CIs
df <- data.frame(
  Study = c(
    "de Moraes et al. 2022", "de Moraes et al. 2022", "de Moraes et al. 2022", "de Moraes et al. 2022",
    "Reynales et al. 2020", "Reynales et al. 2020", "Reynales et al. 2020", "Reynales et al. 2020",
    "Luhm et al. 2023", "Luhm et al. 2023"
  ),
  Category = c(
    "Vaccine Efficacy:", "Vaccine Efficacy:", "Vaccine Efficacy:", "Vaccine Efficacy:",
    "Vaccine Efficacy:", "Vaccine Efficacy:", "Vaccine Efficacy:", "Safety:",
    "Vaccine Efficacy:", "Vaccine Efficacy:"
  ),
  Group = c(
    "Any serotype", "DENV‑1", "DENV‑2", "DENV‑4",
    "Symptomatic VCD", "Hospitalized VCD", "Severe hospitalized VCD", "SAE",
    "Probable dengue", "Lab‑confirmed dengue"
  ),
  AgeGroup = c(
    rep("9 to 44 years", 4),
    rep("9 to 16 years", 4),
    rep("15 to 44 years", 2)
  ),
  StudyPeriod = c(
    rep("39 months", 4),
    rep("81 months", 4),
    rep("28 months", 2)
  ),
  TE = c(
    0.111, 0.333, -0.567, 0.933,
    0.675, 0.166, 0.154, 0.9262,
    0.337, 0.201
  ),
  lower = c(
    -0.190, -0.050, -1.422, 0.477,
    0.583, 0.090, 0.040, 0.8252,
    0.325, 0.171
  ),
  upper = c(
    0.336, 0.576, -0.050, 0.992,
    0.747, 0.290, 0.500, 1.0396,
    0.349, 0.229
  ),
  stringsAsFactors = FALSE
)

# Run meta-analysis using metagen with the pre-calculated effect sizes
meta_dengue <- metagen(
  TE = df$TE,
  lower = df$lower,
  upper = df$upper,
  studlab = df$Study,
  data = df,
  sm = "GEN",
  byvar = df$Category,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL",
  print.byvar = FALSE
)

# Open a PDF device to save the plot
pdf("/home/shweta/Downloads/forest_plot_CYD_TDV_final.pdf", width = 12, height = 8)

# Plot the forest plot with enhanced formatting to match your previous plot
forest(meta_dengue,
       xlab = "Risk Ratio",
       leftcols = c("studlab", "Group", "AgeGroup", "StudyPeriod"),
       leftlabs = c("Study", "Group", "Age Group", "Study Period"),
       rightcols = c("effect", "ci"),
       rightlabs = c("Risk Ratio", "95% CI"),
       col.by = "black",
       print.I2 = TRUE,
       print.tau2 = TRUE,
       smlab = "",
       cex = 0.8,
       colgap.forest = "1.5cm", spacing = 1.5,
       header = TRUE
)

# Close the PDF device
dev.off()




######################################################################################################################################



###########################################################################################################################################



### Updated Forest plot for TV005 (and TV003 phase I)

library(meta)

# ——— build the TV005/TV003 comparative dataset ———
tv005_data <- data.frame(
  Study       = c(
    # CHIM efficacy
    "Pierce KK et al. 2024", "Pierce KK et al. 2024",
    # Bangladesh RCT safety
    "Walsh MC et al. 2024",  "Walsh MC et al. 2024",  "Walsh MC et al. 2024",
    # Russell Phase I safety
    "Russell KL et al. 2022","Russell KL et al. 2022"
  ),
  Category    = c(
    # CHIM efficacy
    "Vaccine Efficacy:",   "Vaccine Efficacy:",
    # Bangladesh RCT safety
    "Safety:",                "Safety:",              
    "Safety:",
    # Russell Phase I safety
    "Safety:",         "Safety:"
  ),
  Group    = c(
    # CHIM efficacy
    "Viremia (DENV-2/3)",   "Rash (DENV-2/3)",
    # Bangladesh RCT safety
    "Rash AE",                "Fever AE",              
    "Arthralgia AE",
    # Russell Phase I safety
    "Any AE (TV003)",         "Any AE (TV005)"
  ),
  AgeGroup    = c(
    "18 to 49 years","18 to 49 years",
    "1 to 50 years",  "1 to 50 years",  "1 to 50 years",
    "18 to 50 years", "18 to 50 years"
  ),
  StudyPeriod = c(
    "6 months","6 months",
    "12 months", "12 months", "12 months",
    "23 months", "23 months"
  ),
  event.e     = c(
    # CHIM events (vaccine)
    0, 0,
    # Bang RCT events (vaccine)
    37, 7, 7,
    # Russ Phase I events (vaccine)
    70, 67
  ),
  n.e         = c(
    21, 30,
    144,144,108,
    80,  80
  ),
  event.c     = c(
    # CHIM events (placebo)
    21,17,
    # Bang RCT events (placebo)
    6, 0, 0,
    # Russ Phase I events (placebo)
    26, 26
  ),
  n.c         = c(
    21, 20,
    48, 48, 48,
    40, 40
  ),
  stringsAsFactors = FALSE
)

# meta-analysis of risk ratios, random-effects by Category
meta_tv005 <- metabin(
  event.e    = event.e,
  n.e        = n.e,
  event.c    = event.c,
  n.c        = n.c,
  studlab    = Study,
  data       = tv005_data,
  sm         = "RR",
  byvar      = Category,
  comb.fixed = FALSE,
  comb.random= TRUE,
  method.tau = "DL",
  print.byvar= FALSE
)


# forest plot
pdf("~/Downloads/forest_plot_TV005_final.pdf", width=16, height=8)
forest(meta_tv005,
       xlab         = "Risk Ratio (Vaccine vs Placebo)",
       leftcols     = c("studlab", "Group","AgeGroup","StudyPeriod","event.e","n.e","event.c","n.c"),
       leftlabs     = c("Study", "Group","Age Group","Study Period","Vax Events","Vax N","Placebo Events","Placebo N"),
       rightcols    = c("effect","ci"),
       rightlabs    = c("Risk Ratio","95% CI"),
       col.by       = "black",
       print.I2     = TRUE,
       print.tau2   = TRUE,
       smlab        = "",
       cex          = 0.8,
       colgap.forest= "1cm",
       spacing      = 1.5,
       header       = TRUE
)

dev.off()

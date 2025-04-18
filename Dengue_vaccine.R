# Install and load the meta package
#install.packages("meta")


# Load required package
library(meta)

# Create the dataset with additional columns for Age Group and Study Period
tak003_data <- data.frame(
  Study = c(
    "Green et al. 2023",
    "Green et al. 2023",
    "Tricou et al. 2020",
    "Tricou et al. 2023",
    "Sirivichayakul et al. 2022",
    "Biswal et al. 2021",
    "Biswal et al. 2021",
    "Green et al. 2023",
    "Tricou et al. 2023",
    "Biswal et al. 2021"
  ),
  Category = c(
    "VE: Confirmed dengue",
    "VE: Hosp dengue",
    "VE: Symptomatic dengue",
    "Immunogenicity: Day120",
    "Immunogenicity",
    "Immunogenicity: Month4",
    "Immunogenicity: Month9",
    "Safety: SAE",
    "Safety: Severe solicited sys AE",
    "Safety: Medically-attended AE"
  ),
  AgeGroup = c(
    "4 to 16 years",
    "4 to 16 years",
    "2 to 17 years",
    "2 to 17 years",
    "2 to 17 years",
    "1 to 11 years",
    "1 to 11 years",
    "4 to 16 years",
    "2 to 17 years",
    "1 to 11 years"
  ),
  StudyPeriod = c(
    "36 months",
    "36 months",
    "48 months",
    "48 months",
    "48 months",
    "36 months",
    "36 months",
    "36 months",
    "48 months",
    "36 months"
  ),
  event.e = c(236, 97, 6, 251, 306, 299, NA, round(0.05 * 13340), round(0.013 * 462), 1),
  n.e     = c(12177, 12177, 201, 264, 360, 300, NA, 13340, 462, 200),
  event.c = c(round(0.10 * 6080), round(0.10 * 6080), round(0.10 * 199),
              round(0.50 * 132), round(0.45 * 360), round(0.10 * 100), NA,
              round(0.059 * 6687), 0, 1),
  n.c     = c(6080, 6080, 199, 132, 360, 100, NA, 6687, 461, 200),
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
pdf("/home/shweta/Downloads/forest_plot_TAK003.pdf", width = 15, height = 10)

# Plot the forest plot with enhanced formatting
forest(meta_tak003,
       xlab        = "Risk Ratio (Vaccine vs Placebo)",
       leftcols    = c("studlab", "AgeGroup", "StudyPeriod", "event.e", "n.e", "event.c", "n.c"),
       leftlabs    = c("Study", "Age Group", "Study Period", "Vax Events", "Vax N", "Placebo Events", "Placebo N"),
       rightcols   = c("effect", "ci"),
       rightlabs   = c("Risk Ratio", "95% CI"),
       col.by      = "black",
       print.I2    = TRUE,
       print.tau2  = TRUE,
       smlab       = "",
       cex         = 0.8,
       colgap.forest = "1cm"
)

# Close the PDF device
dev.off()




##################################################


#install.packages("forestploter")  # Install forestploter if not already installed
#install.packages("grid")          # Install grid package if not already installed
library(forestploter)
library(grid)

cyd_tdv_data <- data.frame(
  Study = c("de Moraes et al. 2022", "Coronel-Martínez et al. 2021", "Reynales et al. 2020", "Luhm et al. 2023"),
  AgeGroup = c("15–44 years", "9–50 years", "9–16 years", "15–44 years"),
  StudyPeriod = c("Sep 2016 - Dec 2019", "Not specified", "Jun 2011 - Mar 2018", "Aug 2016 – Dec 2018"),
  VE = c(0.111, NA, 0.675, 0.337),
  CI_low = c(-0.190, NA, 0.583, 0.325),
  CI_high = c(0.336, NA, 0.747, 0.349)
)

cyd_tdv_data$CI <- ifelse(
  is.na(cyd_tdv_data$VE),
  "Not Available",
  paste0("[", cyd_tdv_data$CI_low, "; ", cyd_tdv_data$CI_high, "]")
)

tabletext <- cyd_tdv_data[, c("Study", "AgeGroup", "StudyPeriod", "VE", "CI")]

# Define the theme for the forest plot
tm <- forest_theme(base_size = 10,
                   refline_gp = gpar(col = "red"),
                   footnote_gp = gpar(col = "#636363", fontface = "italic"))

# Generate the forest plot
p <- forest(tabletext,
            est = cyd_tdv_data$VE,
            lower = cyd_tdv_data$CI_low,
            upper = cyd_tdv_data$CI_high,
            ci_column = 4,
            ref_line = 0,
            xlim = c(-0.5, 1),
            ticks_at = seq(-0.5, 1, 0.25),
            arrow_lab = c("Placebo Better", "Vaccine Better"),
            xlab = "Vaccine Efficacy",
            title = "Forest Plot of CYD-TDV Vaccine Studies",
            theme = tm)

# Print the plot
plot(p)




########################################


library(forestplot)
library(meta)

# Original data
df <- data.frame(
  StudyID     = c(
    rep("de Moraes et al. 2022", 4),
    rep("Reynales et al. 2020", 4),
    rep("Luhm et al. 2023", 2)
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
  Category = c(
    rep("VE", 4),
    rep("VE", 3), "Safety",
    rep("VE", 2)
  ),
  Outcome = c(
    "Any serotype", "DENV‑1", "DENV‑2", "DENV‑4",
    "Symptomatic VCD", "Hospitalized VCD", "Severe hospitalized VCD",
    "SAE",
    "Probable dengue", "Lab‑confirmed dengue"
  ),
  EffectSize = c(
    0.111, 0.333, -0.567, 0.933,
    0.675, 0.166, 0.154,
    0.9262,
    0.337, 0.201
  ),
  LowerCI = c(
    -0.190, -0.050, -1.422, 0.477,
    0.583, 0.090, 0.040,
    0.8252,
    0.325, 0.171
  ),
  UpperCI = c(
    0.336, 0.576, -0.050, 0.992,
    0.747, 0.290, 0.500,
    1.0396,
    0.349, 0.229
  ),
  stringsAsFactors = FALSE
)

# Meta-analysis (excluding safety)
eff_df <- df[df$Category != "Safety", ]
meta_res <- metagen(
  TE = eff_df$EffectSize,
  lower = eff_df$LowerCI,
  upper = eff_df$UpperCI,
  sm = "RR",
  method.tau = "REML"
)

# Create summary row for meta-analysis results
summary_row <- data.frame(
  StudyID = "",
  AgeGroup = "",
  StudyPeriod = "",
  Category = "Summary",
  Outcome = "Random-effects model",
  EffectSize = meta_res$TE.random,
  LowerCI = meta_res$lower.random,
  UpperCI = meta_res$upper.random,
  stringsAsFactors = FALSE
)

# Append summary row to the data
df <- rbind(df, summary_row)

# Construct table for forestplot
tabletext <- cbind(
  c("", 
    ifelse(df$Category == "Summary",
           "Random-effects model",
           paste0(df$Category, ": ", df$Outcome, "\n", df$StudyID)
    )),
  c("Age Group", df$AgeGroup),
  c("Study Period", df$StudyPeriod),
  c("Risk Ratio", sprintf("%.2f", df$EffectSize)),
  c("95% CI", sprintf("%.2f to %.2f", df$LowerCI, df$UpperCI))
)

# Create vector for styling - only header row and summary row should be bold
is_summary <- c(FALSE, rep(FALSE, nrow(df)-1), TRUE)
is_header <- c(TRUE, rep(FALSE, nrow(df)))

# Plot values (shift by 1 for header row)
mean_vals <- c(NA, df$EffectSize)
lower_vals <- c(NA, df$LowerCI)
upper_vals <- c(NA, df$UpperCI)

# Create font styling - bold only for header and summary row
# Create a vector of fontfaces with "bold" only for header and summary rows
fontfaces <- rep("plain", nrow(tabletext))
fontfaces[1] <- "bold"  # Header row
fontfaces[nrow(tabletext)] <- "bold"  # Summary row

fp_txt <- fpTxtGp(
  label = gpar(fontface = fontfaces),
  ticks = gpar(fontsize = 8),
  xlab = gpar(fontsize = 10),
  title = gpar(fontsize = 12, fontface = "bold")
)

# Save plot as PDF
pdf("/home/shweta/Downloads/forest_plot_MetaAnalysis.pdf", width = 15, height = 10)

# Plot the forest plot with enhanced formatting
forestplot(
  labeltext = tabletext,
  mean = mean_vals,
  lower = lower_vals,
  upper = upper_vals,
  zero = 1,
  xlab = "Risk Ratio", 
  is.summary = is_summary,
  txt_gp = fp_txt,
  hrzl_lines = list("1" = gpar(lty = 1), "2" = gpar(lty = 2)),
  col = fpColors(box = "black", lines = "darkgray", summary = "darkblue"),
  graph.pos = 5,
  fn.ci_norm = fpDrawNormalCI
  # Add meta-analysis statistics to the plot
  # legend = list(
  #   paste0("Random effects model (I² = ", 
  #          formatC(meta_res$I2, digits=1, format="f"), "%, ",
  #          "τ² = ", formatC(meta_res$tau2, digits=3, format="f"), ")")
  # ),
  # legend_args = fpLegend(
  #   pos = list(x = 0.5, y = 0.05),
  #   gp = gpar(col = "darkblue")
  # ),
  # new_page = TRUE
)

# Add meta-analysis stats as a footnote
grid.text(
  paste0("Random effects model: I² = ", 
         formatC(meta_res$I2, digits = 1, format = "f"), "%, ",
         "τ² = ", formatC(meta_res$tau2, digits = 3, format = "f")),
  x = unit(0.5, "npc"),
  y = unit(0.05, "npc"),
  gp = gpar(col = "darkblue", fontsize = 10)
)


# Close the PDF device
dev.off()

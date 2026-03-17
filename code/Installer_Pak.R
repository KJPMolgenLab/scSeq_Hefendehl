system("/usr/local/lib/conda/bin/mamba install -y -c conda-forge openjdk glpk mpfr pandoc pari")

Sys.setenv(LD_LIBRARY_PATH = paste("/usr/local/lib/conda/lib", Sys.getenv("LD_LIBRARY_PATH"), sep = ":"))
# Install R packages via pak (handles CRAN, Bioconductor, and GitHub)
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://r-lib.github.io/p/pak/stable/")
}

# ── CRAN packages ────────────────────────────────────────────────────────────
cran_pkgs <- c(
  "workflowr",
  "tidyverse",
  "data.table",
  "DT",
  "xlsx",
  "RCurl",
  "kableExtra",
  "RColorBrewer",
  "ggplot2",
  "ggpubr",
  "ggside",
  "ggstatsplot",
  "viridis",
  "Seurat",
  "harmony",
  "clustree",
  "pheatmap",
  "gprofiler2",
  "plotly",
  "brms",
  "compareGroups",
  "lm.beta",
  "chisq.posthoc.test",
  "WGCNA",
  "igraph"
)

# ── Bioconductor packages ────────────────────────────────────────────────────
bioc_pkgs <- c(
  "AnnotationHub",
  "org.Mm.eg.db",
  "SingleR",
  "SingleCellExperiment",
  "slingshot",
  "EnhancedVolcano",
  "limma",
  "DESeq2"
)

# ── Extra packages ───────────────────────────────────────────────────────────
extra_pkgs <- c(
  "scCustomize"
)

# ── Install ──────────────────────────────────────────────────────────────────
message("========================================")
message(" Installing CRAN packages")
message("========================================")
pak::pak(cran_pkgs)

message("========================================")
message(" Installing Bioconductor packages")
message("========================================")
pak::pak(paste0("bioc::", bioc_pkgs))

message("========================================")
message(" Installing extra packages")
message("========================================")
pak::pak(extra_pkgs)

message("\n✓ All packages installed successfully.")

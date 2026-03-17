#!/usr/bin/bash
# Install R packages via mamba (conda-forge + bioconda channels)

MAMBA=${MAMBA_EXE:-mamba}

# Channels
CHANNELS="-c conda-forge -c bioconda"

# ── CRAN packages ────────────────────────────────────────────────────────────
CRAN_PKGS=(
    r-workflowr
    r-tidyverse
    r-data.table
    r-dt
    r-xlsx
    r-rcurl
    r-kableextra
    r-rcolorbrewer
    r-ggplot2
    r-ggpubr
    r-ggside
    r-ggstatsplot
    r-viridis
    r-seurat
    r-harmony
    r-clustree
    r-pheatmap
    r-gprofiler2
    r-plotly
    r-brms
    r-comparegroups
    r-lm.beta
    r-chisq.posthoc.test
    r-wgcna
)

# ── Bioconductor packages ────────────────────────────────────────────────────
BIOC_PKGS=(
    bioconductor-annotationhub
    bioconductor-org.mm.eg.db
    bioconductor-singler
    bioconductor-singlecellexperiment
    bioconductor-trajectoryutils
    bioconductor-slingshot
    bioconductor-enhancedvolcano
    bioconductor-limma
    bioconductor-deseq2
)

# ── GitHub / conda-forge only packages ──────────────────────────────────────
# scCustomize is available on conda-forge
EXTRA_PKGS=(
    r-sccustomize
)

echo "========================================"
echo " Installing CRAN packages"
echo "========================================"
$MAMBA install --yes $CHANNELS "${CRAN_PKGS[@]}"

echo "========================================"
echo " Installing Bioconductor packages"
echo "========================================"
$MAMBA install --yes $CHANNELS "${BIOC_PKGS[@]}"

echo "========================================"
echo " Installing extra packages"
echo "========================================"
$MAMBA install --yes $CHANNELS "${EXTRA_PKGS[@]}"

echo ""
echo "✓ All packages installed successfully."

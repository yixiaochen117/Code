#!/usr/bin/env bash
# Idempotent setup for this economics coursework / replication repository.
#
# The repo contains two kinds of analysis code:
#   * R scripts using the lfe, plm, lmtest and sandwich packages
#   * A Python Jupyter notebook using pandas, statsmodels, matplotlib, etc.
#
# There are no long-running services, so all setup lives here in `install`.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CRAN_MIRROR="https://cloud.r-project.org"

echo "==> Installing system packages (R, build tools, precompiled R packages, Python tooling)"
export DEBIAN_FRONTEND=noninteractive
sudo apt-get update -y
sudo apt-get install -y --no-install-recommends \
  r-base \
  r-base-dev \
  gfortran \
  r-cran-matrix \
  r-cran-plm \
  r-cran-lmtest \
  r-cran-sandwich \
  python3 \
  python3-pip \
  python3-dev

echo "==> Ensuring CRAN-only R packages are installed (lfe)"
# lfe is not packaged for apt, so compile it from CRAN only when missing.
# Runs under sudo because the R site-library (/usr/local/lib/R/site-library) is
# only writable by root.
sudo Rscript -e 'pkgs <- c("lfe"); missing <- pkgs[!(pkgs %in% rownames(installed.packages()))]; if (length(missing)) install.packages(missing, repos="'"$CRAN_MIRROR"'") else cat("lfe already installed\n")'

echo "==> Verifying R packages load"
Rscript -e 'for (p in c("lfe","plm","lmtest","sandwich")) { suppressMessages(library(p, character.only=TRUE)); cat(p, as.character(packageVersion(p)), "OK\n") }'

echo "==> Installing Python packages (system interpreter)"
# Ubuntu marks the system Python as externally managed (PEP 668); this is a
# disposable agent VM, so installing globally keeps `python3`/`jupyter` usable
# without any virtualenv activation step.
sudo pip install --break-system-packages -r "$REPO_ROOT/.cursor/requirements.txt"

echo "==> Registering Jupyter kernels"
sudo python3 -m ipykernel install --name python3 --display-name "Python 3" >/dev/null
# The checked-in notebook was authored against a conda "base" kernel; register
# that name too so `jupyter nbconvert --execute` runs the notebook unchanged.
sudo python3 -m ipykernel install --name conda-base-py --display-name "Python 3 (base)" >/dev/null

echo "==> Verifying Python packages import"
python3 - <<'PY'
import pandas as pd, numpy, statsmodels, matplotlib, openpyxl, tabulate
print("pandas", pd.__version__, "- all Python packages import OK")
PY

echo "==> Environment setup complete."

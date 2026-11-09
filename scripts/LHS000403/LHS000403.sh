#!/bin/bash
#SBATCH --job-name=LHS000403
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=12:00:00
#SBATCH --output=out/LHS000403/LHS000403_%A_%a.out
#SBATCH --error=out/LHS000403/LHS000403_%A_%a.err

# Imputation array wrapper for LHS000403: one IMPL_ subset per task with standard
# mice (m = 5, maxit = 50, ~80 min per subset). Each task loads only its pre-split
# subset (LHS000402_split.R); subsets already imputed are skipped.
# Submit with: sbatch --array=1-500 scripts/LHS000403/LHS000403.sh

module load r/4.5.0

task=${SLURM_ARRAY_TASK_ID}
if [ -f data/derived/LHS000403/LHS000403_${task}.rda ]; then
  echo "subset ${task} skipped"; exit 0
fi
start=$(date +%s)
timeout 11h Rscript scripts/LHS000403/LHS000403.R ${task}
code=$?
elapsed=$(( $(date +%s) - start ))

if [ ${code} -eq 0 ]; then
  echo "subset ${task} done in ${elapsed}s"
else
  echo "subset ${task} FAILED (exit ${code}) after ${elapsed}s"
fi
exit ${code}

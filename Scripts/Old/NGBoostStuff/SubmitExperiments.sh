#!/bin/bash
# submit_experiments.sh
# Submit all experimental variations

# Parse command line arguments
SELECTED_EXPERIMENTS="$1"  # Optional: specific experiments to run
RUN_MODE="${2:-parallel}"   # parallel, sequential, or single

echo "=== GAM Experiment Pipeline ==="
echo "Run mode: $RUN_MODE"
echo "Timestamp: $(date)"

# Create base directories
mkdir -p Experiments
mkdir -p Logs/Experiments

# Setup experiments if not already done
if [ ! -f "Experiments/ExperimentMatrix.qs" ]; then
    echo "Setting up experiment matrix..."
    Rscript -e "source('ExperimentConfig.R'); setup_all_experiments()"
fi

# Get list of experiments
if [ -z "$SELECTED_EXPERIMENTS" ]; then
    # Get all experiments
    EXPERIMENTS=$(Rscript -e "
        library(qs)
        exp_matrix <- qread('Experiments/NonLinearModeling/ExperimentMatrix.qs')
        cat(paste(exp_matrix\$experiment_id, collapse=' '))
    ")
else
    EXPERIMENTS="$SELECTED_EXPERIMENTS"
fi

echo "Experiments to run: $EXPERIMENTS"

# Function to submit a single experiment
submit_experiment() {
    local exp_id=$1
    echo "Submitting experiment: $exp_id"
    
    # Get experiment configuration for resource requirements
    local mem_gb=$(Rscript -e "
        library(qs)
        config <- qread('Experiments/NonLinearModeling/$exp_id/config.qs')
        cat(config\$memory_gb)
    ")
    
    local runtime_hours=$(Rscript -e "
        library(qs)
        config <- qread('Experiments/NonLinearModeling/$exp_id/config.qs')
        cat(config\$expected_runtime_hours)
    ")
    
    local cores=$(Rscript -e "
        library(qs)
        config <- qread('Experiments/NonLinearModeling/$exp_id/config.qs')
        cat(config\$cores_needed)
    ")
    
    # Submit stage 1 with appropriate resources
    local job_id=$(sbatch --parsable \
        --job-name="gam_${exp_id}_stage1" \
        --output="Experiments/NonLinearModeling/${exp_id}/logs/stage1_%j.out" \
        --error="Experiments/NonLinearModeling/${exp_id}/logs/stage1_%j.err" \
        --time="${runtime_hours}:00:00" \
        --nodes=1 \
        --ntasks=1 \
        --cpus-per-task=$cores \
        --mem="${mem_gb}G" \
        --export=EXPERIMENT_ID=$exp_id \
        job_scripts/experiment_stage1.sh)
    
    echo "  Stage 1 submitted: Job ID $job_id"
    
    # Submit stage 2 with dependency
    local job_id2=$(sbatch --parsable \
        --dependency=afterok:$job_id \
        --job-name="gam_${exp_id}_stage2" \
        --output="Experiments/NonLinearModeling/${exp_id}/logs/stage2_%j.out" \
        --error="Experiments/NonLinearModeling/${exp_id}/logs/stage2_%j.err" \
        --time="${runtime_hours}:00:00" \
        --nodes=1 \
        --ntasks=1 \
        --cpus-per-task=$cores \
        --mem="${mem_gb}G" \
        --export=EXPERIMENT_ID=$exp_id \
        job_scripts/experiment_stage2.sh)
    
    echo "  Stage 2 submitted: Job ID $job_id2"
    
    # Submit stage 3 with dependency
    local job_id3=$(sbatch --parsable \
        --dependency=afterok:$job_id2 \
        --job-name="gam_${exp_id}_stage3" \
        --output="Experiments/NonLinearModeling/${exp_id}/logs/stage3_%j.out" \
        --error="Experiments/NonLinearModeling/${exp_id}/logs/stage3_%j.err" \
        --time="${runtime_hours}:00:00" \
        --nodes=1 \
        --ntasks=1 \
        --cpus-per-task=$cores \
        --mem="${mem_gb}G" \
        --export=EXPERIMENT_ID=$exp_id \
        job_scripts/experiment_stage3.sh)
    
    echo "  Stage 3 submitted: Job ID $job_id3"
    
    # Return the final job ID for tracking
    echo $job_id3
}

# Submit experiments based on run mode
case $RUN_MODE in
    "parallel")
        echo "Submitting all experiments in parallel..."
        FINAL_JOBS=()
        for exp in $EXPERIMENTS; do
            final_job=$(submit_experiment $exp)
            FINAL_JOBS+=($final_job)
        done
        
        # Submit summary job when all complete
        DEPENDENCY_LIST=$(IFS=:; echo "${FINAL_JOBS[*]}")
        sbatch --dependency=afterany:$DEPENDENCY_LIST \
               --job-name=gam_experiment_summary \
               --output=Logs/Experiments/NonLinearModeling/summary_%j.out \
               --time=00:30:00 \
               --mem=4G \
               job_scripts/experiment_summary.sh
        ;;
        
    "sequential")
        echo "Submitting experiments sequentially..."
        PREV_JOB=""
        for exp in $EXPERIMENTS; do
            if [ -n "$PREV_JOB" ]; then
                # Add dependency on previous experiment
                final_job=$(sbatch --dependency=afterany:$PREV_JOB \
                    --parsable --export=EXPERIMENT_ID=$exp \
                    job_scripts/experiment_full_pipeline.sh)
            else
                final_job=$(submit_experiment $exp)
            fi
            PREV_JOB=$final_job
        done
        ;;
        
    "single")
        if [ $(echo $EXPERIMENTS | wc -w) -gt 1 ]; then
            echo "ERROR: Single mode requires exactly one experiment"
            exit 1
        fi
        submit_experiment $EXPERIMENTS
        ;;
esac

echo ""
echo "Pipeline submitted successfully!"
echo "Monitor with: squeue -u $USER"
echo "Results will be in: experiments/[experiment_id]/results/"
echo ""
echo "Example monitoring commands:"
echo "  tail -f experiments/gam_none_single_year_2016/logs/stage1_*.out"
echo "  ls experiments/*/results/"
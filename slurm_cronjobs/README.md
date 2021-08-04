# run as cronjob via slurm

make a fresh copy of `post_echam.r` and/or `plot_echam.r`, modify the respective namelists and `slurm_cronjob.run` in the same directory and submit with
```
sbatch slurm_cronjob.run
```


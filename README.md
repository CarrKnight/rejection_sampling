# rejection_sampling
The code repository for the paper "Rejection sampling for complex fishery models facing limited data"

# Bio-economic model

The whole model is coded, run and analyzed in a single R script: `bioeconomic_model.R`. 

# ABM

The plotting instructions are in the file `タチウオ.Rmd`, the inputs for POSEIDON are in the `slice3` folder but:

1. You need to extract the `outputs.zip` file to get access to the accepted simulation runs (future projections and so on)
2. You need to download the zipped scenario files from [here](https://www.dropbox.com/sh/xp77ketbhbcyoko/AACSuBl6psDX-TfkyQV9FOXna?dl=0) and place them in the `slice3/scenarios` folder (unzipping its contents as well) to get access to every POSEIDON input (accepted or not). This is a pain but unfortunately zipped this comes at about 2GB of data and that's too much for github.

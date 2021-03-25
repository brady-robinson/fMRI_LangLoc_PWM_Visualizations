# Steps to conduct a similar analysis for UDel QLab:

1) Understand the layout and logic of BLAST analyses. To do this you will need to understand how FSL, an MRI analysis software, outputs results and you will need to read about how this relates to previous analyses done for BLAST. More on FSL can be found by googling "Andy's Brain Book". More on BLAST directory organization can be found here: https://github.com/juliagoolia28/qlab/blob/mri_task_design/experiments/sl-fmri-expt/mri_task_design.md

2) Navigate to "firstlevelanalysis" directory in "blast" on sylvian server. "fsl" should be in the path.

3) In the directory from (2) you will see directories that contain the raw analyses done for this specific analysis in "brady_analysis_early_2021". In that directory, you will find additional information in the README.

4) To recreate these analyses in child data, for example, the goal is to extract "mean activation", or beta weights, for each contrast of interest for each child.

5) Step one to do this for child data is to average across runs for a given contrast in FSL. You can learn how to set up such a contrast by looking at the first or second level analysis sections of Andy's Brain Book.

6) Once the relevant analyses have been done with FSL, Terri Scott's Matlab scripts were used to extract raw mean activation from cope files, using language localizer parcels and phonological working memory parcels as masks. If an error appears that alerts to input vectors being of different lengths, make sure the masks are in the same space as the cope/zstat files.

7) The outputs from Terri Scott's scripts are what's in this Github repo. Use the R file as a starting point to analyze and visualize the mean activation data for a particular ROI in a given mask.
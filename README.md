# MeTACAST: Target- and Context-aware Spatial Selection in VR

[Paper](https://doi.org/10.1109/TVCG.2023.3326517) | [Video](https://www.youtube.com/watch?v=R_WRfzgnOAM&t=1s "Video") | [MeTACAST Source code](https://github.com/LixiangZhao98/MeTACAST)

This is a repository for all user study data and analysis scripts used in the paper "[MeTACAST: Target- and Context-aware Spatial Selection in VR](https://doi.org/10.1109/TVCG.2023.3326517)", presented at [IEEE Visualization 2023](http://ieeevis.org/year/2023/welcome) and published in the journal [IEEE Transactions on Visualization and Computer Graphics](https://ieeexplore.ieee.org/xpl/RecentIssue.jsp?punumber=2945). If you use the results in new projects or use it in a different way we would appreciate a citation:

L. Zhao, T. Isenberg, F. Xie, H. -N. Liang and L. Yu, "MeTACAST: Target- and Context-Aware Spatial Selection in VR," in IEEE Transactions on Visualization and Computer Graphics, vol. 30, no. 1, pp. 480-494, Jan. 2024, doi: [10.1109/TVCG.2023.3326517](https://doi.org/10.1109/TVCG.2023.3326517); open-access versions are available at [arXiv](https://arxiv.org/abs/2308.03616).
You can also find the source code for MeTACAST: [https://github.com/LixiangZhao98/MeTACAST](https://github.com/LixiangZhao98/MeTACAST)

## BibTex

```
@article{Zhao:2024:MTC,
  author      = {Lixiang Zhao and Tobias Isenberg and Fuqi Xie and Hai-Ning Liang and Lingyun Yu},
  title       = {MeTACAST: Target- and Context-aware Spatial Selection in VR},
  journal     = {IEEE Transactions on Visualization and Computer Graphics},
  year        = {2024},
  volume      = {30},
  number      = {1},
  month       = jan,
  pages       = {480--494},
  doi         = {10.1109/TVCG.2023.3326517},
  shortdoi    = {10/gtnn25},
  doi_url     = {https://doi.org/10.1109/TVCG.2023.3326517},
  oa_hal_url  = {https://hal.science/hal-04196163},
  preprint    = {https://doi.org/10.48550/arXiv.2308.03616},
  osf_url     = {https://osf.io/dvj9n/},
  url         = {https://tobias.isenberg.cc/p/Zhao2024MTC},
  github_url  = {https://github.com/LixiangZhao98/MeTACAST},
  github_url2 = {https://github.com/LixiangZhao98/PointCloud-Visualization-Tool},
  github_url3 = {https://github.com/LixiangZhao98/MeTACAST-study},
  pdf         = {https://tobias.isenberg.cc/personal/papers/Zhao_2024_MTC.pdf},
  video       = {https://youtu.be/R_WRfzgnOAM},
}
```
## Project websites
* https://yulingyun.com/MeTACAST/


## Requirements

The R script contained within this repository requires, in addition to a [normal R installation](https://cran.r-project.org/), several packages including (potentially more):

* `boot`
* `PropCIs`
* `ggplot2`
* `reshape2`
* `glue`
* `dplyr`
* `ggpubr`

To install these required packages, run the following call from a command line: 
```
install.packages(c("boot", "PropCIs", "ggplot2", "reshape2", "glue", "dplyr","ggpubr"),repos="https://cran.rstudio.com")
```


## Running the R script

1. Clone this repository using 
```
git clone https://github.com/LixiangZhao98/MeTACAST-study.git
```
 or download the code as a ZIP file from the repository.

2. Change the Working Directory to `MeTACAST-study`. You can do this by
```
setwd('.../MeTACAST-study-master')
```
3. Run `confidenceIntervalMacros.R`, `analysis.R` and `Questionnaire.R` in sequence. You can do this by
```
source('confidenceIntervalMacros.R')
source('analysis.R')
source('Questionnaire.R')
```
To be noted that, `analysis.R` need to complete in one or two minutes since the large amount of the user data.
4. After the script completes, the figures (PDF format) will be generated in the folder `resultFiles/log` and `resultFiles/Questionnaire`. The new generated files will cover the files generated last time.

## Files produced

In the folder `resultFiles/log` and `resultFiles/Questionnaire` you should see the result PDFs. The following figures in [the paper](https://github.com/LixiangZhao98/asset/tree/master/Publications/Papers/MeTACAST.pdf) are corresponding to the PDFs.


* resultFiles/log/time_Dataset0_rep23barChartTime.pdf------Figure7a in the paper
* resultFiles/log/time_Dataset1_rep23barChartTime.pdf------Figure7b in the paper
* resultFiles/log/time_Dataset2_rep23barChartTime.pdf------Figure7c in the paper
* resultFiles/log/time_Dataset3_rep23barChartTime.pdf------Figure7d in the paper
* resultFiles/log/time_Dataset4_rep23barChartTime.pdf------Figure7e in the paper
* resultFiles/log/time_Dataset4_rep23barChartTimeDatasetsDifference.pdf------Figure8 in the paper
* resultFiles/log/error_Dataset4_rep23_barChartF1.pdf------Figure9top in the paper
* resultFiles/log/error_Dataset4_rep23_barChartMCC.pdf------Figure9bottom in the paper
* resultFiles/Questionnaire/tlx_task1to4.pdf------Figure10 in the paper
* resultFiles/Questionnaire/tlx_task5.pdf------Figure11 in the paper
* resultFiles/log/time_0123Datasets_rep23barChartTime.pdf------Figure14 in the paper
* resultFiles/log/time_0123Datasets_rep23barChartTimeDatasetsDifference.pdf------Figure15 in the paper
* resultFiles/log/error_Dataset(0-4)_rep23_Means_F1.xls, log/error_Dataset(0-4)_rep23_Means_MCC.xls, log/time_Dataset(0-4)_rep23Means_time.xls, log/time_Dataset(0-4)_rep23Ratios_time.xls------Table2 in the paper
* resultFiles/log/error_0123Datasets_rep23_Means_F1.xls, log/error_0123Datasets_rep23_Means_MCC.xls, log/time_0123Datasets_rep23Means_time.xls, log/time_0123Datasets_rep23Ratios_time.xls------Table3 in the paper
* resultFiles/log/error_0123Datasets_rep23_barChartF1.pdf------Figure16 in the paper
* resultFiles/log/error_0123Datasets_rep23_barChartMCC.pdf------Figure17 in the paper
* resultFiles/log/error_Dataset0_rep23_barChartF1.pdf------Figure18 in the paper
* resultFiles/log/error_Dataset0_rep23_barChartMCC.pdf------Figure19 in the paper
* resultFiles/log/error_Dataset1_rep23_barChartF1.pdf------Figure20 in the paper
* resultFiles/log/error_Dataset1_rep23_barChartMCC.pdf------Figure21 in the paper
* resultFiles/log/error_Dataset2_rep23_barChartF1.pdf------Figure22 in the paper
* resultFiles/log/error_Dataset2_rep23_barChartMCC.pdf------Figure23 in the paper
* resultFiles/log/error_Dataset3_rep23_barChartF1.pdf------Figure24 in the paper
* resultFiles/log/error_Dataset3_rep23_barChartMCC.pdf------Figure25 in the paper
* resultFiles/log/error_Dataset4_rep23_barChartF1.pdf------Figure26 in the paper
* resultFiles/log/error_Dataset4_rep23_barChartMCC.pdf------Figure27 in the paper
* resultFiles/Questionnaire/TechniqueRank_dataset.pdf------Figure28 in the paper

Fig. 29-43 were generated by executing the `analysis.R` and `Questionnaire.R` for VR experts (using VR once a week) and novices (less than once a week) seperately.
Other images from the paper (i.e., Fig. 1-6, Fig.12–13) were created with the tool [Adobe Illustrator](https://www.adobe.com/products/illustrator/free-trial-download.html), so we could not script them.

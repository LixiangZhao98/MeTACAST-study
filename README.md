# MeTACAST: Target- and Context-aware Spatial Selection in VR
This is a repository for all data and analysis scripts used in the paper "[MeTACAST: Target- and Context-aware Spatial Selection in VR](https://doi.org/10.1109/TVCG.2023.3326517)", presented at [IEEE Visualization 2023](http://ieeevis.org/year/2023/welcome) and published in the journal [IEEE Transactions on Visualization and Computer Graphics](https://ieeexplore.ieee.org/xpl/RecentIssue.jsp?punumber=2945). If you use the results in new projects or use it in a different way we would appreciate a citation:

L. Zhao, T. Isenberg, F. Xie, H. -N. Liang and L. Yu, "MeTACAST: Target- and Context-Aware Spatial Selection in VR," in IEEE Transactions on Visualization and Computer Graphics, vol. 30, no. 1, pp. 480-494, Jan. 2024, doi: [10.1109/TVCG.2023.3326517](https://doi.org/10.1109/TVCG.2023.3326517); open-access versions are available at [arXiv](https://arxiv.org/abs/2308.03616).
You can also find the code for MeTACAST: [https://github.com/LixiangZhao98/MeTACAST](https://github.com/LixiangZhao98/MeTACAST)

## BibTex

```
@article{zhao2023metacast,
  title={MeTACAST: Target-and Context-aware Spatial Selection in VR},
  author={Zhao, Lixiang and Isenberg, Tobias and Xie, Fuqi and Liang, Hai-Ning and Yu, Lingyun},
  journal={IEEE Transactions on Visualization and Computer Graphics},
  year={2023},
  publisher={IEEE}
}
```
## Project websites
* https://yulingyun.com/MeTACAST/

## Instructions for generating figures ([Graphics Replicability Stamp](https://www.replicabilitystamp.org/))
### Requirements

The R script contained within this repository requires, in addition to a [normal R installation](https://cran.r-project.org/), several packages including (potentially more):

* `plyr`
* `dplyr`
* `tidyr`
* `reshape2`
* `ggplot2`
* `propCIs`

To install these required packages, run the following call from a command line: `Rscript -e "install.packages(c('plyr', 'dplyr', 'tidyr', 'reshape2', 'ggplot2', 'propCIs'), repos='https://cran.rstudio.com')"`

If you encounter problem with Pandoc:
1. To check whether Pandoc was correctly installed: ``Rscript -e "rmarkdown::pandoc_exec()"``
2. To install Pandoc from its official website: https://pandoc.org/installing.html . If you use macOS, you can also use Homebrew to install it: ``brew install pandoc``

### Running the R script

1. Clone this repository using `https://github.com/LixiangZhao98/MeTACAST-study.git` or download the code as a ZIP file from the repository.


### Files produced

After the script completes, in the `` folder you should see the following figures from [the paper]() in PDF format.

* Fig. 4 
  * Figure4a-exp2_beauvis_bar.pdf
  * Figure4b-exp2_beauvis_pie.pdf
  * Figure4c-exp2_beauvis_map.pdf
  * Figure4d-exp2_beauvis_diff_bar.pdf
  * Figure4e-exp2_beauvis_diff_pie.pdf
  * Figure4f-exp2_beauvis_diff_map.pdf


We are unable to script the other images in our paper, as they are design samples created by participants or old visualization examples.

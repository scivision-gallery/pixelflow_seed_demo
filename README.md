# pixelflow_seed_demo
Demo notebooks for using  a pre-trained StarDist model as described in 'Automated extraction of pod phenotype data from micro-computed tomography' (Corcoran et al. 2023; https://www.frontiersin.org/articles/10.3389/fpls.2023.1120182/full), Scivision, and Pixelflow (https://github.com/alan-turing-institute/pixelflow) to extract location, size and shape data for seeds detected and segmented in 2D and 3D images.

Example 2D and 3D seed images and label masks used in these notebooks can be downloaded from zenodo (https://zenodo.org/record/8355920)

R code used to run valve sorting in this notebook is available from the [Scivision gallery github page](https://github.com/scivision-gallery/pixelflow_seed_demo/tree/main) seed (`'seedpod_2D_valve_lowess_single.R'`)

## How to run
1. Create a conda environment

`conda create -m stardist_seed python=3.10`
`conda activate stardist_seed`

2. Install dependencies

`pip install -e git+https://github.com/alan-turing-institute/pixelflow.git#egg=pixelflow`
`pip install git+https://github.com/alan-turing-institute/scivision.git@main`
`pip install rpy2==3.5.13`

3. Open Jupyter Notebook:

`pip install jupyter`
`jupyter notebook`


<p align="center">
    <img height="150" src="https://uni-muenster.sciebo.de/s/RwS9PA1dj1Nmivn/download">
</p>

# clevRvis: visualization techniques for clonal evolution

<p align="center">
    <img height="400" src="https://oup.silverchair-cdn.com/oup/backfile/Content_public/Journal/gigascience/12/10.1093_gigascience_giad020/2/giad020fig2.jpeg?Expires=1737632296&Signature=RQtaoEli-znzs-8cVaO4gjYCFj2dh2psTgkHZy4lfglDIiYtqirhWWwQa~w8uxlwuW3eR8Ns4E5O8tNKD-Q-~ehY3HdWikmzW0X5fImpptZ1VLjxQokNd6SHHEWJ7qBRBNZ7xZg-zqkeSReTfEpCCD8394LYL7uktX1Y87SVdy7sKdd-BTbF0hGll99u43-cWKMBIaNHzfhlYfPLkMbXv7JnuQdGE-NN~pPtlw59rAMRTZ392Ox11qyjYwKQ1Lhm4QHwq3~LRiZq9NBcV~RYRtPv9cKX5I8~2XqcrKefGzRi3bRvBz8pN9sbMR9WHSTF~78dyl9d~IsMkv37atk0aw__&Key-Pair-Id=APKAIE5G5CRDK6RD3PGA">
</p>

<b>Figure 1:</b> Visualization of clonal evolution using fish plots, comparing the approaches fishplot, timescape, and clevRvis. (A) Dataset 1, UPN02: branched dependent evolution can be visualized by all 3 tools. However, starting points of 2 clones are not correctly displayed using fishplot. (B) Dataset 1, UPN09: branched independent evolution can only be visualized by fishplot and clevRvis. The starting point of 1 clone is not correctly displayed using fishplot. (C) Dataset 4, patient 7: linear evolution based on a single time point can only be visualized by timescape and clevRvis.

<p align="center">
    <img height="500" src="https://oup.silverchair-cdn.com/oup/backfile/Content_public/Journal/gigascience/12/10.1093_gigascience_giad020/2/giad020fig3.jpeg?Expires=1737632292&Signature=Wb5OH3hWqmdJ5TSbrb3HAvJvg97alJxLhIu1~Cl3wXUWrIH7VnJ-Aw69TX3z1CBYyb8oCH6TIDqdWSXlBQxCmhfaAUxJgeaLju0lR1wKOuxWshk869Z0fG0qeyzvX4DTWvssTV1eQWUNYHJPVaWa47VumIG6ej1IlOSiV4jWvM1uR6Nk4IbJH~o0kYPkRCHSldnTw6RbQqY1n2f33yZzK~~yZeZbu72tSjmn~WYF8N9C0AZdjb7iNHhV4xPc4N9BSTwzDx-sbexubJuxljJNYhZiUPZ~CYQvucYIF2F-iLBlZhpLniRS1vBAScJf-jO0CxTorWfjN17DNhwVmmk7Rw__&Key-Pair-Id=APKAIE5G5CRDK6RD3PGA">
</p>

<b>Figure 2:</b> Visualization of clonal evolution using dolphin plots. (A) Dataset 1, UPN07: analysis of 6 time points vs. analysis of 2 time points and additional time point interpolation. (B) Dataset 1, UPN08: analysis of 12 measured time points vs. analysis of 2 measured time points, additional time point interpolation and therapy effect estimation. (C) Dataset 1, UPN02: analysis of 5 measured time points vs. analysis of 2 measured time points, additional time point interpolation and therapy effect estimation. (D) Dataset 2, patient 2: analysis of 4 measured time points. (E) Dataset 2, patient 2: analysis of 4 measured time points and additional therapy effect estimation. Note: for all plots, time point interpolation is enabled to improve visualization of newly developing clones prior to the first measured time point.


<p align="center">
    <img height="550" src="https://oup.silverchair-cdn.com/oup/backfile/Content_public/Journal/gigascience/12/10.1093_gigascience_giad020/2/giad020fig4.jpeg?Expires=1737632288&Signature=Qfw5euXcKHcAX7iIUUoP9ZOO-~mfOy61opM4SAzUC6pNiCianVPdRwhfZdvOnkrM1o7J6kq7yx9HzU6kJI2~7kVcnnZrULjXQalwPXJxJIQogwD0QDB-ucGQcrEhpil-EpyudZqyhDSAKEsmE2acoQdnCAJhWXA4EazJJ3UclWzBI5CjAfyueYRgQn87iGSWmcwMtmEWh~v7fDpW7unoDhlDHBMOXmv2U0niNtIKN8v0CoTG4R6Y80Joehsq-1xCnGNfrdTBbFPYtGKpovOVxZAknJZBQQwpxp~NZzsNvYIfXBpDA9NYKi6NDV2KdYbKkdRPmB~4BWENuLGBF20kTg__&Key-Pair-Id=APKAIE5G5CRDK6RD3PGA">
</p>

<b>Figure 3:</b> Visualization of clonal evolution using plaice plots vs. dolphin plots. (A) Dataset 1, UPN05: a hemizygous variant affects BCOR. (B) Dataset 2, patient 1: a biallelic event causes TP53 deficiency in clone 2. Clone 3 is characterized by additional TBC1D4 and UBA1 deficiency. (C) Dataset 3, UPN06 (enabled therapy effect estimation): a double-hit event affects TP53 (der(17)t(13;17)(q21;p12) + point mutation). Clonal evolution cannot be reconstructed uniquely. Version 1 assumes branched dependent evolution, leading to TP53 deficiency in 1 clone. Version 2 assumes linear clonal evolution, leaving at least 1 healthy allele of TP53 in each clone. (D) Dataset 4, patients 1 to 5: different biallelic events lead to TP53 deficiency in patients 2, 3, and 5. Note: for all plots, time point interpolation is enabled to improve visualization of newly developing clones prior to the first measured time point.




clevRvis provides an extensive set of visualization techniques for clonal evolution. Three types of plots are available: 1) shark plots (basic trees, showing the phylogeny and optionally the cancer cell fraction CCF); 2) dolphin plots (advanced visualization, showing the phylogeny and the development of CCFs over time); 3) plaice plots (novel visualization, showing the phylogeny, the development of CCFs and the development of remaining healthy alleles, influenced by bi-allelic events, over time). Moreover, the tool provides algorithms for fully automatic interpolation of time points and estimation of therapy effect to approximate a tumor's development in the presence of few measured time points, as well as exploring alternative trees.

## Requirements
To run clevRvis, you need R Version 4.1.0 or higher.

##  Installation
clevRvis is available at Bioconductor. The latest version can be installed via:

```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("clevRvis")
```


The latest version available at github can be installed via:

```
if (!requireNamespace("devtools", quietly=TRUE))
  install.packages("devtools")
devtools::install_github("sandmanns/clevRvis")
```

## Detailed documentation
For detailed documentation, please check out the manual and the vignette available within this repsitory or on the bioconductor website (https://bioconductor.org/packages/release/bioc/html/appreci8R.html).

In case of errors or feature requests, do not hesitate to open an issue or contact Sarah Sandmann (sarah.sandmann@uni-muenster.de).

##  Citation
Sandmann S, Inserte C, Varghese J. clevRvis: visualization techniques for clonal evolution. Gigascience. 2022;12:giad020. 
https://doi.org/10.1093/gigascience/giad020

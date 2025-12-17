---
title: "AD vs FTD"
subtitle: "Análisis descriptivo, modelos de regresión logística y permutaciones"
author: "Canziani, Verónica - Freilij, Tomás - Gromadzyn, Guido"
date: "17/12/2025"
lang: es

output:
  html_document:
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: true
    number_sections: true
    theme: flatly        # otro tema lindo
    highlight: pygments
    df_print: paged
    fig_width: 7
    fig_height: 5
    keep_md: true        # opcional, guarda el .md

fontsize: 11pt

---



## Carga de librerias


``` r
library(tidyverse)
library(gt)
library(kableExtra)
library(gtsummary)
library(corrplot)
library(car)
library(caret)
library(pROC)
```

## Carga de dato


``` r
library(readxl)
file_path <- "/home/guido/Downloads/CogED xDiag xVero.xlsx"
dataAD <- read_excel(file_path, sheet = "CogEd AD", range = "B1:R478")



dataCN <- read_excel(file_path, sheet = "CogEd CN", range = "A1:Q669")

dataFTD <- read_excel(file_path, sheet = "CogEd FTD", range = "A1:Q274")

cat("dataAD:",  dim(dataAD),  "\n")
```

```
## dataAD: 477 17
```

``` r
cat("dataCN:",  dim(dataCN),  "\n")
```

```
## dataCN: 668 17
```

``` r
cat("dataFTD:", dim(dataFTD), "\n")
```

```
## dataFTD: 273 17
```

## Limpieza de variables


``` r
data <- rbind(dataAD, dataCN, dataFTD)
data$clinical_diagnosis <- as.factor(data$clinical_diagnosis)
data$cog_tmt_a_err <- as.numeric(ifelse(data$cog_tmt_a_err=="completed", 0, data$cog_tmt_a_err))
data$cog_tmt_a_corr <- as.numeric(ifelse(data$cog_tmt_a_corr=="completed", 25, data$cog_tmt_a_corr))
data$cog_tmt_b_err <- as.numeric(ifelse(data$cog_tmt_b_err=="completed", 0, data$cog_tmt_b_err))
data$cog_tmt_b_corr <- as.numeric(ifelse(data$cog_tmt_b_corr=="completed", 25, data$cog_tmt_b_corr))
data$cog_digits_forward_span <- as.numeric(ifelse(data$cog_digits_forward_span=="completed", 8, data$cog_digits_forward_span))
data$cog_digits_backward_span <- as.numeric(ifelse(data$cog_digits_backward_span=="completed", 7, data$cog_digits_backward_span))
data$cog_digits_forward_total <- ifelse(data$cog_digits_forward_total>16, NA, data$cog_digits_forward_total)
data$cog_digits_backward_total <- ifelse(data$cog_digits_backward_total>14, NA, data$cog_digits_backward_total)
data$cog_tmt_a <- ifelse(data$cog_tmt_a>150, NA, data$cog_tmt_a)
data$cog_tmt_b <- ifelse(data$cog_tmt_b>300, NA, data$cog_tmt_b)
```


## Distribución de valores


``` r
plot_col_hist <- function(data, title,column) {
  hist(data[[column]],
       main = title,
       xlab = "Años",
       ylab = "Frecuencia",
       col = "lightblue",
       breaks = 20)
}

plot_col_hist(data,   "Años de educación","cog_ed")
```

![](EEAA_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
plot_col_hist(dataAD, "Años de educación AD","cog_ed")
```

![](EEAA_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

``` r
plot_col_hist(dataCN, "Años de educación CN","cog_ed")
```

![](EEAA_files/figure-html/unnamed-chunk-4-3.png)<!-- -->

``` r
plot_col_hist(dataFTD, "Años de educación FTD","cog_ed")
```

![](EEAA_files/figure-html/unnamed-chunk-4-4.png)<!-- -->


``` r
plot_col_hist(data, "cog_digits_forward_total","cog_digits_forward_total")
```

![](EEAA_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

``` r
plot_col_hist(data, "cog_digits_backward_total","cog_digits_backward_total")
```

![](EEAA_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

``` r
plot_col_hist(data, "mmse_total","mmse_total")
```

![](EEAA_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

``` r
plot_col_hist(data, "cog_category_animals","cog_category_animals")
```

![](EEAA_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

``` r
plot_col_hist(data, "cog_category_vegetables","cog_category_vegetables")
```

![](EEAA_files/figure-html/unnamed-chunk-5-5.png)<!-- -->

``` r
plot_col_hist(data, "cog_tmt_a","cog_tmt_a")
```

![](EEAA_files/figure-html/unnamed-chunk-5-6.png)<!-- -->

``` r
plot_col_hist(data, "cog_tmt_b","cog_tmt_b")
```

![](EEAA_files/figure-html/unnamed-chunk-5-7.png)<!-- -->



``` r
data <- data %>%
  select(-codMP, -id_paciente) %>%
  mutate(tests_no_hechos = rowSums(is.na(across(c(cog_digits_forward_total, cog_category_animals, cog_category_vegetables, cog_tmt_a, cog_tmt_b)))))
```

## Descarte de pacientes con 5 tests no hechos


``` r
data <- data %>%
  filter(tests_no_hechos < 5)
```



## Tabla 1 (considerando 3 categorias)


``` r
tabla1 <- 
  tbl_summary(
    data = data,
    by = clinical_diagnosis,          # para comparar grupos
    include = c(                      # 👈 incluimos solo estas variables
      cog_ed,
      cog_digits_forward_total,
      cog_digits_forward_span,
      cog_digits_backward_total,
      cog_digits_backward_span,
      mmse_total,
      cog_category_animals,
      cog_category_vegetables,
      cog_tmt_a,
      cog_tmt_a_corr,
      cog_tmt_a_err,
      cog_tmt_b,
      cog_tmt_b_corr,
      cog_tmt_b_err
    ),
    missing = "ifany",
    missing_text = "No realizado",
    label = list(
      cog_ed ~ "Años de educación",
      cog_digits_forward_total  ~ "Digits forward total",
      cog_digits_forward_span   ~ "Digits forward span",
      cog_digits_backward_total ~ "Digits backwards total",
      cog_digits_backward_span  ~ "Digits backwards span",
      mmse_total                ~ "Mini mental",
      cog_category_animals      ~ "Fluidez semántica (animales)",
      cog_category_vegetables   ~ "Fluidez semántica (vegetales)",
      cog_tmt_a       ~ "Atención sostenida (tiempo)",
      cog_tmt_a_corr  ~ "Atención sostenida (correctas)",
      cog_tmt_a_err   ~ "Atención sostenida (errores)",
      cog_tmt_b       ~ "Funciones ejecutivas (tiempo)",
      cog_tmt_b_corr  ~ "Funciones ejecutivas (correctas)",
      cog_tmt_b_err   ~ "Funciones ejecutivas (errores)"
    ),
    type = list(                     # forzamos todas como continuas
      cog_ed ~ "continuous",
      cog_digits_forward_total  ~ "continuous",
      cog_digits_forward_span   ~ "continuous",
      cog_digits_backward_total ~ "continuous",
      cog_digits_backward_span  ~ "continuous",
      mmse_total                ~ "continuous",
      cog_category_animals      ~ "continuous",
      cog_category_vegetables   ~ "continuous",
      cog_tmt_a       ~ "continuous",
      cog_tmt_a_corr  ~ "continuous",
      cog_tmt_a_err   ~ "continuous",
      cog_tmt_b       ~ "continuous",
      cog_tmt_b_corr  ~ "continuous",
      cog_tmt_b_err   ~ "continuous"
    ),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous()  ~ "kruskal.test",
      all_categorical() ~ "chisq.test"
    )
  ) %>%
  bold_labels()

tabla1
```

```{=html}
<div id="rnqkueqqko" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rnqkueqqko table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rnqkueqqko thead, #rnqkueqqko tbody, #rnqkueqqko tfoot, #rnqkueqqko tr, #rnqkueqqko td, #rnqkueqqko th {
  border-style: none;
}

#rnqkueqqko p {
  margin: 0;
  padding: 0;
}

#rnqkueqqko .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#rnqkueqqko .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rnqkueqqko .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rnqkueqqko .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rnqkueqqko .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rnqkueqqko .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rnqkueqqko .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rnqkueqqko .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#rnqkueqqko .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rnqkueqqko .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rnqkueqqko .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rnqkueqqko .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rnqkueqqko .gt_spanner_row {
  border-bottom-style: hidden;
}

#rnqkueqqko .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#rnqkueqqko .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rnqkueqqko .gt_from_md > :first-child {
  margin-top: 0;
}

#rnqkueqqko .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rnqkueqqko .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#rnqkueqqko .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#rnqkueqqko .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#rnqkueqqko .gt_row_group_first td {
  border-top-width: 2px;
}

#rnqkueqqko .gt_row_group_first th {
  border-top-width: 2px;
}

#rnqkueqqko .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnqkueqqko .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rnqkueqqko .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rnqkueqqko .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rnqkueqqko .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnqkueqqko .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rnqkueqqko .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rnqkueqqko .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rnqkueqqko .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rnqkueqqko .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rnqkueqqko .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnqkueqqko .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rnqkueqqko .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnqkueqqko .gt_left {
  text-align: left;
}

#rnqkueqqko .gt_center {
  text-align: center;
}

#rnqkueqqko .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rnqkueqqko .gt_font_normal {
  font-weight: normal;
}

#rnqkueqqko .gt_font_bold {
  font-weight: bold;
}

#rnqkueqqko .gt_font_italic {
  font-style: italic;
}

#rnqkueqqko .gt_super {
  font-size: 65%;
}

#rnqkueqqko .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rnqkueqqko .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rnqkueqqko .gt_indent_1 {
  text-indent: 5px;
}

#rnqkueqqko .gt_indent_2 {
  text-indent: 10px;
}

#rnqkueqqko .gt_indent_3 {
  text-indent: 15px;
}

#rnqkueqqko .gt_indent_4 {
  text-indent: 20px;
}

#rnqkueqqko .gt_indent_5 {
  text-indent: 25px;
}

#rnqkueqqko .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#rnqkueqqko div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>Overall</strong><br />
N = 1,106</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>AD</strong><br />
N = 409</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>CN</strong><br />
N = 494</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_3"><span class='gt_from_md'><strong>FTD</strong><br />
N = 203</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Años de educación</td>
<td headers="stat_0" class="gt_row gt_center">13.0 (6.0, 16.0)</td>
<td headers="stat_1" class="gt_row gt_center">12.0 (8.0, 16.0)</td>
<td headers="stat_2" class="gt_row gt_center">12.0 (5.0, 17.0)</td>
<td headers="stat_3" class="gt_row gt_center">14.0 (11.0, 16.0)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Digits forward total</td>
<td headers="stat_0" class="gt_row gt_center">5.00 (4.00, 7.00)</td>
<td headers="stat_1" class="gt_row gt_center">5.00 (3.00, 6.00)</td>
<td headers="stat_2" class="gt_row gt_center">5.00 (4.00, 7.00)</td>
<td headers="stat_3" class="gt_row gt_center">5.00 (3.00, 6.00)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">10</td>
<td headers="stat_1" class="gt_row gt_center">5</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">5</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Digits forward span</td>
<td headers="stat_0" class="gt_row gt_center">5.00 (4.00, 6.00)</td>
<td headers="stat_1" class="gt_row gt_center">5.00 (4.00, 6.00)</td>
<td headers="stat_2" class="gt_row gt_center">5.00 (4.00, 6.00)</td>
<td headers="stat_3" class="gt_row gt_center">5.00 (4.00, 6.00)</td>
<td headers="p.value" class="gt_row gt_center">0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">7</td>
<td headers="stat_1" class="gt_row gt_center">4</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">3</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Digits backwards total</td>
<td headers="stat_0" class="gt_row gt_center">4.00 (3.00, 5.00)</td>
<td headers="stat_1" class="gt_row gt_center">3.00 (2.00, 4.00)</td>
<td headers="stat_2" class="gt_row gt_center">4.00 (3.00, 6.00)</td>
<td headers="stat_3" class="gt_row gt_center">3.00 (2.00, 5.00)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">25</td>
<td headers="stat_1" class="gt_row gt_center">7</td>
<td headers="stat_2" class="gt_row gt_center">6</td>
<td headers="stat_3" class="gt_row gt_center">12</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Digits backwards span</td>
<td headers="stat_0" class="gt_row gt_center">3.00 (3.00, 4.00)</td>
<td headers="stat_1" class="gt_row gt_center">3.00 (2.00, 4.00)</td>
<td headers="stat_2" class="gt_row gt_center">4.00 (3.00, 5.00)</td>
<td headers="stat_3" class="gt_row gt_center">3.00 (2.00, 4.00)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">7</td>
<td headers="stat_1" class="gt_row gt_center">4</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">3</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Mini mental</td>
<td headers="stat_0" class="gt_row gt_center">24.0 (20.0, 28.0)</td>
<td headers="stat_1" class="gt_row gt_center">21.0 (18.0, 24.0)</td>
<td headers="stat_2" class="gt_row gt_center">28.0 (24.0, 29.0)</td>
<td headers="stat_3" class="gt_row gt_center">23.0 (17.0, 26.0)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">3</td>
<td headers="stat_1" class="gt_row gt_center">2</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="stat_3" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Fluidez semántica (animales)</td>
<td headers="stat_0" class="gt_row gt_center">14 (10, 20)</td>
<td headers="stat_1" class="gt_row gt_center">11 (8, 14)</td>
<td headers="stat_2" class="gt_row gt_center">19 (15, 23)</td>
<td headers="stat_3" class="gt_row gt_center">9 (5, 15)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">6</td>
<td headers="stat_1" class="gt_row gt_center">3</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">3</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Fluidez semántica (vegetales)</td>
<td headers="stat_0" class="gt_row gt_center">11 (6, 15)</td>
<td headers="stat_1" class="gt_row gt_center">7 (4, 10)</td>
<td headers="stat_2" class="gt_row gt_center">15 (12, 19)</td>
<td headers="stat_3" class="gt_row gt_center">6 (3, 10)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">19</td>
<td headers="stat_1" class="gt_row gt_center">8</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="stat_3" class="gt_row gt_center">10</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Atención sostenida (tiempo)</td>
<td headers="stat_0" class="gt_row gt_center">65 (40, 107)</td>
<td headers="stat_1" class="gt_row gt_center">88 (56, 147)</td>
<td headers="stat_2" class="gt_row gt_center">49 (33, 79)</td>
<td headers="stat_3" class="gt_row gt_center">76 (46, 122)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">64</td>
<td headers="stat_1" class="gt_row gt_center">36</td>
<td headers="stat_2" class="gt_row gt_center">4</td>
<td headers="stat_3" class="gt_row gt_center">24</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Atención sostenida (correctas)</td>
<td headers="stat_0" class="gt_row gt_center">24.0 (24.0, 24.0)</td>
<td headers="stat_1" class="gt_row gt_center">24.0 (24.0, 24.0)</td>
<td headers="stat_2" class="gt_row gt_center">24.0 (24.0, 24.0)</td>
<td headers="stat_3" class="gt_row gt_center">24.0 (24.0, 24.0)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">26</td>
<td headers="stat_1" class="gt_row gt_center">17</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">9</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Atención sostenida (errores)</td>
<td headers="stat_0" class="gt_row gt_center">0.00 (0.00, 0.00)</td>
<td headers="stat_1" class="gt_row gt_center">0.00 (0.00, 0.00)</td>
<td headers="stat_2" class="gt_row gt_center">0.00 (0.00, 0.00)</td>
<td headers="stat_3" class="gt_row gt_center">0.00 (0.00, 1.00)</td>
<td headers="p.value" class="gt_row gt_center">0.033</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">26</td>
<td headers="stat_1" class="gt_row gt_center">17</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="stat_3" class="gt_row gt_center">9</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Funciones ejecutivas (tiempo)</td>
<td headers="stat_0" class="gt_row gt_center">168 (92, 300)</td>
<td headers="stat_1" class="gt_row gt_center">300 (164, 300)</td>
<td headers="stat_2" class="gt_row gt_center">110 (71, 208)</td>
<td headers="stat_3" class="gt_row gt_center">195 (126, 300)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">285</td>
<td headers="stat_1" class="gt_row gt_center">136</td>
<td headers="stat_2" class="gt_row gt_center">83</td>
<td headers="stat_3" class="gt_row gt_center">66</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Funciones ejecutivas (correctas)</td>
<td headers="stat_0" class="gt_row gt_center">24 (23, 24)</td>
<td headers="stat_1" class="gt_row gt_center">24 (13, 25)</td>
<td headers="stat_2" class="gt_row gt_center">24 (24, 24)</td>
<td headers="stat_3" class="gt_row gt_center">24 (22, 25)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">53</td>
<td headers="stat_1" class="gt_row gt_center">35</td>
<td headers="stat_2" class="gt_row gt_center">1</td>
<td headers="stat_3" class="gt_row gt_center">17</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Funciones ejecutivas (errores)</td>
<td headers="stat_0" class="gt_row gt_center">0.00 (0.00, 2.00)</td>
<td headers="stat_1" class="gt_row gt_center">1.00 (0.00, 3.00)</td>
<td headers="stat_2" class="gt_row gt_center">0.00 (0.00, 1.00)</td>
<td headers="stat_3" class="gt_row gt_center">0.00 (0.00, 2.00)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">54</td>
<td headers="stat_1" class="gt_row gt_center">35</td>
<td headers="stat_2" class="gt_row gt_center">2</td>
<td headers="stat_3" class="gt_row gt_center">17</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  <tfoot>
    <tr class="gt_footnotes">
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>Median (Q1, Q3)</span></td>
    </tr>
    <tr class="gt_footnotes">
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>Kruskal-Wallis rank sum test</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

## Tabla 1 considerando AD vs FTD


``` r
data2 <- data %>%
  filter(clinical_diagnosis != "CN") 
data2$clinical_diagnosis <- droplevels(data2$clinical_diagnosis)

tabla1 <- 
  tbl_summary(
    data = data2,
    by = clinical_diagnosis,          # para comparar grupos
    include = c(                      # 👈 incluimos solo estas variables
      cog_ed,
      cog_digits_forward_total,
      cog_digits_forward_span,
      cog_digits_backward_total,
      cog_digits_backward_span,
      mmse_total,
      cog_category_animals,
      cog_category_vegetables,
      cog_tmt_a,
      cog_tmt_a_corr,
      cog_tmt_a_err,
      cog_tmt_b,
      cog_tmt_b_corr,
      cog_tmt_b_err
    ),
    missing = "ifany",
    missing_text = "No realizado",
    label = list(
      cog_ed ~ "Años de educación",
      cog_digits_forward_total  ~ "Digits forward total",
      cog_digits_forward_span   ~ "Digits forward span",
      cog_digits_backward_total ~ "Digits backwards total",
      cog_digits_backward_span  ~ "Digits backwards span",
      mmse_total                ~ "Mini mental",
      cog_category_animals      ~ "Fluidez semántica (animales)",
      cog_category_vegetables   ~ "Fluidez semántica (vegetales)",
      cog_tmt_a       ~ "Atención sostenida (tiempo)",
      cog_tmt_a_corr  ~ "Atención sostenida (correctas)",
      cog_tmt_a_err   ~ "Atención sostenida (errores)",
      cog_tmt_b       ~ "Funciones ejecutivas (tiempo)",
      cog_tmt_b_corr  ~ "Funciones ejecutivas (correctas)",
      cog_tmt_b_err   ~ "Funciones ejecutivas (errores)"
    ),
    type = list(                     # forzamos todas como continuas
      cog_ed ~ "continuous",
      cog_digits_forward_total  ~ "continuous",
      cog_digits_forward_span   ~ "continuous",
      cog_digits_backward_total ~ "continuous",
      cog_digits_backward_span  ~ "continuous",
      mmse_total                ~ "continuous",
      cog_category_animals      ~ "continuous",
      cog_category_vegetables   ~ "continuous",
      cog_tmt_a       ~ "continuous",
      cog_tmt_a_corr  ~ "continuous",
      cog_tmt_a_err   ~ "continuous",
      cog_tmt_b       ~ "continuous",
      cog_tmt_b_corr  ~ "continuous",
      cog_tmt_b_err   ~ "continuous"
    ),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous()  ~ "wilcox.test",
      all_categorical() ~ "chisq.test"
    )
  ) %>%
  bold_labels()

tabla1
```

```{=html}
<div id="cfwusqcbql" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#cfwusqcbql table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#cfwusqcbql thead, #cfwusqcbql tbody, #cfwusqcbql tfoot, #cfwusqcbql tr, #cfwusqcbql td, #cfwusqcbql th {
  border-style: none;
}

#cfwusqcbql p {
  margin: 0;
  padding: 0;
}

#cfwusqcbql .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#cfwusqcbql .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#cfwusqcbql .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#cfwusqcbql .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#cfwusqcbql .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cfwusqcbql .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cfwusqcbql .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cfwusqcbql .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#cfwusqcbql .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#cfwusqcbql .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cfwusqcbql .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cfwusqcbql .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#cfwusqcbql .gt_spanner_row {
  border-bottom-style: hidden;
}

#cfwusqcbql .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#cfwusqcbql .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#cfwusqcbql .gt_from_md > :first-child {
  margin-top: 0;
}

#cfwusqcbql .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cfwusqcbql .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#cfwusqcbql .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#cfwusqcbql .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#cfwusqcbql .gt_row_group_first td {
  border-top-width: 2px;
}

#cfwusqcbql .gt_row_group_first th {
  border-top-width: 2px;
}

#cfwusqcbql .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cfwusqcbql .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#cfwusqcbql .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#cfwusqcbql .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cfwusqcbql .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cfwusqcbql .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cfwusqcbql .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#cfwusqcbql .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cfwusqcbql .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cfwusqcbql .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cfwusqcbql .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cfwusqcbql .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cfwusqcbql .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cfwusqcbql .gt_left {
  text-align: left;
}

#cfwusqcbql .gt_center {
  text-align: center;
}

#cfwusqcbql .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cfwusqcbql .gt_font_normal {
  font-weight: normal;
}

#cfwusqcbql .gt_font_bold {
  font-weight: bold;
}

#cfwusqcbql .gt_font_italic {
  font-style: italic;
}

#cfwusqcbql .gt_super {
  font-size: 65%;
}

#cfwusqcbql .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#cfwusqcbql .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#cfwusqcbql .gt_indent_1 {
  text-indent: 5px;
}

#cfwusqcbql .gt_indent_2 {
  text-indent: 10px;
}

#cfwusqcbql .gt_indent_3 {
  text-indent: 15px;
}

#cfwusqcbql .gt_indent_4 {
  text-indent: 20px;
}

#cfwusqcbql .gt_indent_5 {
  text-indent: 25px;
}

#cfwusqcbql .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#cfwusqcbql div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>Overall</strong><br />
N = 612</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>AD</strong><br />
N = 409</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>FTD</strong><br />
N = 203</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Años de educación</td>
<td headers="stat_0" class="gt_row gt_center">13.5 (10.0, 16.0)</td>
<td headers="stat_1" class="gt_row gt_center">12.0 (8.0, 16.0)</td>
<td headers="stat_2" class="gt_row gt_center">14.0 (11.0, 16.0)</td>
<td headers="p.value" class="gt_row gt_center">0.006</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Digits forward total</td>
<td headers="stat_0" class="gt_row gt_center">5.00 (3.00, 6.00)</td>
<td headers="stat_1" class="gt_row gt_center">5.00 (3.00, 6.00)</td>
<td headers="stat_2" class="gt_row gt_center">5.00 (3.00, 6.00)</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">10</td>
<td headers="stat_1" class="gt_row gt_center">5</td>
<td headers="stat_2" class="gt_row gt_center">5</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Digits forward span</td>
<td headers="stat_0" class="gt_row gt_center">5.00 (4.00, 6.00)</td>
<td headers="stat_1" class="gt_row gt_center">5.00 (4.00, 6.00)</td>
<td headers="stat_2" class="gt_row gt_center">5.00 (4.00, 6.00)</td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">7</td>
<td headers="stat_1" class="gt_row gt_center">4</td>
<td headers="stat_2" class="gt_row gt_center">3</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Digits backwards total</td>
<td headers="stat_0" class="gt_row gt_center">3.00 (2.00, 4.00)</td>
<td headers="stat_1" class="gt_row gt_center">3.00 (2.00, 4.00)</td>
<td headers="stat_2" class="gt_row gt_center">3.00 (2.00, 5.00)</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">19</td>
<td headers="stat_1" class="gt_row gt_center">7</td>
<td headers="stat_2" class="gt_row gt_center">12</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Digits backwards span</td>
<td headers="stat_0" class="gt_row gt_center">3.00 (2.00, 4.00)</td>
<td headers="stat_1" class="gt_row gt_center">3.00 (2.00, 4.00)</td>
<td headers="stat_2" class="gt_row gt_center">3.00 (2.00, 4.00)</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">7</td>
<td headers="stat_1" class="gt_row gt_center">4</td>
<td headers="stat_2" class="gt_row gt_center">3</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Mini mental</td>
<td headers="stat_0" class="gt_row gt_center">21.0 (18.0, 25.0)</td>
<td headers="stat_1" class="gt_row gt_center">21.0 (18.0, 24.0)</td>
<td headers="stat_2" class="gt_row gt_center">23.0 (17.0, 26.0)</td>
<td headers="p.value" class="gt_row gt_center">0.004</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">2</td>
<td headers="stat_1" class="gt_row gt_center">2</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Fluidez semántica (animales)</td>
<td headers="stat_0" class="gt_row gt_center">10.0 (7.0, 14.0)</td>
<td headers="stat_1" class="gt_row gt_center">11.0 (8.0, 14.0)</td>
<td headers="stat_2" class="gt_row gt_center">9.0 (5.0, 15.0)</td>
<td headers="p.value" class="gt_row gt_center">0.005</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">6</td>
<td headers="stat_1" class="gt_row gt_center">3</td>
<td headers="stat_2" class="gt_row gt_center">3</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Fluidez semántica (vegetales)</td>
<td headers="stat_0" class="gt_row gt_center">6.0 (4.0, 10.0)</td>
<td headers="stat_1" class="gt_row gt_center">7.0 (4.0, 10.0)</td>
<td headers="stat_2" class="gt_row gt_center">6.0 (3.0, 10.0)</td>
<td headers="p.value" class="gt_row gt_center">0.066</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">18</td>
<td headers="stat_1" class="gt_row gt_center">8</td>
<td headers="stat_2" class="gt_row gt_center">10</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Atención sostenida (tiempo)</td>
<td headers="stat_0" class="gt_row gt_center">84 (51, 137)</td>
<td headers="stat_1" class="gt_row gt_center">88 (56, 147)</td>
<td headers="stat_2" class="gt_row gt_center">76 (46, 122)</td>
<td headers="p.value" class="gt_row gt_center">0.008</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">60</td>
<td headers="stat_1" class="gt_row gt_center">36</td>
<td headers="stat_2" class="gt_row gt_center">24</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Atención sostenida (correctas)</td>
<td headers="stat_0" class="gt_row gt_center">24.0 (24.0, 24.0)</td>
<td headers="stat_1" class="gt_row gt_center">24.0 (24.0, 24.0)</td>
<td headers="stat_2" class="gt_row gt_center">24.0 (24.0, 24.0)</td>
<td headers="p.value" class="gt_row gt_center">0.3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">26</td>
<td headers="stat_1" class="gt_row gt_center">17</td>
<td headers="stat_2" class="gt_row gt_center">9</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Atención sostenida (errores)</td>
<td headers="stat_0" class="gt_row gt_center">0.00 (0.00, 1.00)</td>
<td headers="stat_1" class="gt_row gt_center">0.00 (0.00, 0.00)</td>
<td headers="stat_2" class="gt_row gt_center">0.00 (0.00, 1.00)</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">26</td>
<td headers="stat_1" class="gt_row gt_center">17</td>
<td headers="stat_2" class="gt_row gt_center">9</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Funciones ejecutivas (tiempo)</td>
<td headers="stat_0" class="gt_row gt_center">269 (147, 300)</td>
<td headers="stat_1" class="gt_row gt_center">300 (164, 300)</td>
<td headers="stat_2" class="gt_row gt_center">195 (126, 300)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">202</td>
<td headers="stat_1" class="gt_row gt_center">136</td>
<td headers="stat_2" class="gt_row gt_center">66</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Funciones ejecutivas (correctas)</td>
<td headers="stat_0" class="gt_row gt_center">24 (14, 25)</td>
<td headers="stat_1" class="gt_row gt_center">24 (13, 25)</td>
<td headers="stat_2" class="gt_row gt_center">24 (22, 25)</td>
<td headers="p.value" class="gt_row gt_center">0.031</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">52</td>
<td headers="stat_1" class="gt_row gt_center">35</td>
<td headers="stat_2" class="gt_row gt_center">17</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Funciones ejecutivas (errores)</td>
<td headers="stat_0" class="gt_row gt_center">1.00 (0.00, 3.00)</td>
<td headers="stat_1" class="gt_row gt_center">1.00 (0.00, 3.00)</td>
<td headers="stat_2" class="gt_row gt_center">0.00 (0.00, 2.00)</td>
<td headers="p.value" class="gt_row gt_center">0.030</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No realizado</td>
<td headers="stat_0" class="gt_row gt_center">52</td>
<td headers="stat_1" class="gt_row gt_center">35</td>
<td headers="stat_2" class="gt_row gt_center">17</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  <tfoot>
    <tr class="gt_footnotes">
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>Median (Q1, Q3)</span></td>
    </tr>
    <tr class="gt_footnotes">
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>Wilcoxon rank sum test</span></td>
    </tr>
  </tfoot>
</table>
</div>
```



## Función auxiliar para correlaciones


``` r
plot_corr <- function(df, vars_to_keep, label_map, title = NULL) {
  
  # Filter only numeric columns listed in vars_to_keep
  data_num <- df %>% 
    select(all_of(vars_to_keep))
  
  # Compute correlation matrix
  mat_cor <- cor(data_num, use = "pairwise.complete.obs", method = "pearson")
  
  # Apply labels
  mat_cor_labeled <- mat_cor
  colnames(mat_cor_labeled) <- label_map[colnames(mat_cor_labeled)]
  rownames(mat_cor_labeled) <- label_map[rownames(mat_cor_labeled)]
  
  # Palette
  paleta <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(100)
  
  # Plot
  corrplot(mat_cor_labeled,
           method = "color",
           type = "upper",
           order = "hclust",
           tl.cex = 0.8,
           col = paleta,
           title = title,
           mar = c(0,0,2,0))
}
```


## Gráfico de correlaciones

``` r
data_num <- data %>% 
  select(where(is.numeric))

mat_cor <- cor(data_num, use = "pairwise.complete.obs", method = "pearson")

label_map <- c(
  mmse_total                = "MMSE",
  cog_digits_forward_total  = "DF total",
  cog_digits_backward_total = "DB total",
  cog_digits_forward_span   = "DF span",
  cog_digits_backward_span  = "DB span",
  cog_category_animals      = "Fluidez animales",
  cog_category_vegetables   = "Fluidez vegetales",
  cog_tmt_a       = "TMT-A (tiempo)",
  cog_tmt_a_corr  = "TMT-A (corr.)",
  cog_tmt_a_err   = "TMT-A (errores)",
  cog_tmt_b       = "TMT-B (tiempo)",
  cog_tmt_b_corr  = "TMT-B (corr.)",
  cog_tmt_b_err   = "TMT-B (errores)"
)

vars_to_keep <- names(label_map)

plot_corr(data,  vars_to_keep, label_map, title = "Correlaciones")
```

![](EEAA_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


## Gráfico de correlaciones (simplificado)


``` r
data_num <- data %>% 
  select(where(is.numeric))

mat_cor <- cor(data_num, use = "pairwise.complete.obs", method = "pearson")

label_map <- c(
  mmse_total                = "Desempeño Total (MMSE)",
  cog_digits_forward_total  = "Digits Forward",
  cog_digits_backward_total = "Digits Backward",
  cog_category_animals      = "Fluidez animales",
  cog_category_vegetables   = "Fluidez vegetales",
  cog_tmt_a       = "Atención sostenida",
  cog_tmt_b       = "Funciones Ejecutivas"
)


vars_to_keep <- names(label_map)

plot_corr(data,  vars_to_keep, label_map, title = "Correlaciones")
```

![](EEAA_files/figure-html/unnamed-chunk-12-1.png)<!-- -->



``` r
label_map <- c(
  mmse_total                = "Desempeño Total (MMSE)",
  cog_digits_forward_total  = "Digits Forward",
  cog_digits_backward_total = "Digits Backward",
  cog_category_animals      = "Fluidez animales",
  cog_category_vegetables   = "Fluidez vegetales",
  cog_tmt_a       = "Atención sostenida",
  cog_tmt_b       = "Funciones Ejecutivas"
)

vars_to_keep <- names(label_map)

plot_corr(dataAD,  vars_to_keep, label_map, title = "Correlaciones - AD")
```

![](EEAA_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

``` r
plot_corr(dataCN,  vars_to_keep, label_map, title = "Correlaciones - CN")
```

![](EEAA_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

``` r
plot_corr(dataFTD, vars_to_keep, label_map, title = "Correlaciones - FTD")
```

![](EEAA_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

``` r
label_map <- c(
  mmse_total                = "Desempeño Total (MMSE)",
  cog_digits_forward_total  = "Memoria",
  cog_category_animals      = "Lenguaje",
  cog_tmt_b       = "Funciones Ejecutivas - Atención"
)

vars_to_keep <- names(label_map)

plot_corr(dataAD,  vars_to_keep, label_map, title = "Correlaciones - AD")
```

![](EEAA_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

``` r
plot_corr(dataCN,  vars_to_keep, label_map, title = "Correlaciones - CN")
```

![](EEAA_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

``` r
plot_corr(dataFTD, vars_to_keep, label_map, title = "Correlaciones - FTD")
```

![](EEAA_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

``` r
# Columnas y labels
label_map <- c(
  mmse_total                = "Desempeño Total (MMSE)",
  cog_digits_forward_total  = "Memoria",
  cog_category_animals      = "Lenguaje",
  cog_tmt_a                 = "Atención",
  cog_tmt_b                 = "Funciones Ejecutivas - Atención"
)

vars_to_keep <- names(label_map)

# Graficar correlaciones originales
plot_corr(dataAD, vars_to_keep, label_map, title = "Correlaciones - AD (original)")
```

![](EEAA_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

``` r
# Número de permutaciones a mostrar
n_perm_show <- 5
set.seed(123) # para reproducibilidad

# Elegimos la columna a permutar
column_to_permute <- "cog_tmt_a"

# Generamos 5 permutaciones diferentes
for (i in 1:n_perm_show) {
  data_perm <- dataAD
  data_perm[[column_to_permute]] <- sample(data_perm[[column_to_permute]])
  
  # Graficar correlación de esta permutación
  plot_corr(data_perm, vars_to_keep, label_map, 
            title = paste("Correlaciones - AD (perm", i, ")"))
}
```

![](EEAA_files/figure-html/unnamed-chunk-15-2.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-15-3.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-15-4.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-15-5.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-15-6.png)<!-- -->


``` r
# Columnas y labels
label_map <- c(
  mmse_total                = "Desempeño Total (MMSE)",
  cog_digits_forward_total  = "Memoria",
  cog_category_animals      = "Lenguaje",
  cog_tmt_a                 = "Atención",
  cog_tmt_b                 = "Funciones Ejecutivas - Atención"
)

vars_to_keep <- names(label_map)

# Correlación original
plot_corr(dataAD, vars_to_keep, label_map,
          title = "Correlaciones - AD (original)")
```

![](EEAA_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

``` r
# Número de permutaciones a mostrar por par
n_perm_show <- 5
set.seed(123)

# Todas las combinaciones de pares
pairs <- combn(vars_to_keep, 2, simplify = FALSE)

# Loop por cada par de columnas
for (pair in pairs) {
  
  col_perm <- pair[1]
  col_ref  <- pair[2]
  
  for (i in seq_len(n_perm_show)) {
    
    data_perm <- dataAD
    data_perm[[col_perm]] <- sample(data_perm[[col_perm]])
    
    plot_corr(
      data_perm,
      vars_to_keep,
      label_map,
      title = paste0(
        "AD | permutando: ", label_map[col_perm],
        " vs ", label_map[col_ref],
        " (perm ", i, ")"
      )
    )
  }
}
```

![](EEAA_files/figure-html/unnamed-chunk-16-2.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-3.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-4.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-5.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-6.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-7.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-8.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-9.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-10.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-11.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-12.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-13.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-14.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-15.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-16.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-17.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-18.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-19.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-20.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-21.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-22.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-23.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-24.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-25.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-26.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-27.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-28.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-29.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-30.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-31.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-32.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-33.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-34.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-35.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-36.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-37.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-38.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-39.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-40.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-41.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-42.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-43.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-44.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-45.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-46.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-47.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-48.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-49.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-50.png)<!-- -->![](EEAA_files/figure-html/unnamed-chunk-16-51.png)<!-- -->




``` r
#Limpio la base de las variables de digits span

#todos los datos
data <- data %>%
  select(-cog_digits_backward_span, -cog_digits_forward_span, -tests_no_hechos)

#solamente AD y FTD
data2 <- data %>%
  filter(clinical_diagnosis != "CN") 
data2$clinical_diagnosis <- droplevels(data2$clinical_diagnosis)
```


``` r
vars <- colnames(data)
vars <- vars[2:13]
```


``` r
#Cambio los nombres de las variables
label_map <- c(
  cog_ed                   = "Nivel educativo",
  cog_digits_forward_total = "Memoria inmediata",
  cog_digits_backward_total= "Memoria de trabajo",
  mmse_total               = "MMSE",
  cog_category_animals     = "Fluidez (animales)",
  cog_category_vegetables  = "Fluidez (vegetales)",
  cog_tmt_a                = "Atención",
  cog_tmt_b                = "Funciones ejecutivas",
  cog_tmt_a_err            = "Errores de atención",
  cog_tmt_b_err            = "Errores ejecutivos",
  cog_tmt_a_corr           = "Correctas atención",
  cog_tmt_b_corr           = "Correctas ejecutivas"
)

vars <- label_map[vars]
vars_original <- names(label_map)


data <- data%>%
  rename_with(
    ~ ifelse(.x %in% names(label_map), label_map[.x], .x),
    .cols = all_of(vars_original)
  )

data2<- data2 %>%
rename_with(
    ~ ifelse(.x %in% names(label_map), label_map[.x], .x),
    .cols = all_of(vars_original)
  )
```

## Correlaciones entre variables post permutaciones


``` r
#Test de correlaciones luego de las permutaciones. Permuta en cada par de variables. N=10000. Luego ordena según cuán desviado está de la media el delta de correlación (o sea cuánto)

perm_corr_test <- function(data, x, y, n_perm = 10000, method = "spearman") {
  
  r_obs <- cor(data[[x]], data[[y]], method = method, use = "complete.obs")
  
  r_perm <- replicate(n_perm, {
    cor(
      sample(data[[x]]),
      data[[y]],
      method = method,
      use = "complete.obs"
    )
  })
  
  tibble(
    var_x = x,
    var_y = y,
    r_obs = r_obs,
    r_perm_mean = mean(r_perm),
    r_perm_sd = sd(r_perm),
    delta_r = r_obs - mean(r_perm),
    p_value = (sum(abs(r_perm) >= abs(r_obs)) + 1) / (n_perm + 1),
    z_perm = (r_obs - mean(r_perm)) / sd(r_perm)

  )
}



library(purrr)

results_all <- map_dfr(
  combn(vars, 2, simplify = FALSE),
  ~ perm_corr_test(data, .x[1], .x[2], n_perm = 10000)
)

results_all <- results_all |>
  dplyr::mutate(
    p_adj = p.adjust(p_value, method = "BH")
  ) 

results_all |>
  arrange(desc(abs(z_perm)))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["var_x"],"name":[1],"type":["chr"],"align":["left"]},{"label":["var_y"],"name":[2],"type":["chr"],"align":["left"]},{"label":["r_obs"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["r_perm_mean"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["r_perm_sd"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["delta_r"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["p_value"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["z_perm"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["p_adj"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"Fluidez (animales)","2":"Fluidez (vegetales)","3":"0.736949715","4":"-3.595543e-05","5":"0.03056738","6":"0.736985671","7":"0.00009999","8":"24.11020148","9":"0.0001346804"},{"1":"MMSE","2":"Fluidez (animales)","3":"0.661018057","4":"-1.994133e-04","5":"0.02992829","6":"0.661217470","7":"0.00009999","8":"22.09339221","9":"0.0001346804"},{"1":"MMSE","2":"Atención","3":"-0.679349242","4":"-3.264477e-04","5":"0.03126163","6":"-0.679022795","7":"0.00009999","8":"-21.72064620","9":"0.0001346804"},{"1":"Atención","2":"Funciones ejecutivas","3":"0.789535226","4":"-2.956954e-04","5":"0.03648380","6":"0.789830921","7":"0.00009999","8":"21.64880985","9":"0.0001346804"},{"1":"Memoria inmediata","2":"Memoria de trabajo","3":"0.613162867","4":"1.027201e-04","5":"0.03055963","6":"0.613060147","7":"0.00009999","8":"20.06111371","9":"0.0001346804"},{"1":"Memoria de trabajo","2":"Atención","3":"-0.614524876","4":"3.123460e-04","5":"0.03122501","6":"-0.614837222","7":"0.00009999","8":"-19.69053932","9":"0.0001346804"},{"1":"Errores ejecutivos","2":"Correctas ejecutivas","3":"-0.614797848","4":"2.068519e-04","5":"0.03144201","6":"-0.615004700","7":"0.00009999","8":"-19.55996823","9":"0.0001346804"},{"1":"Memoria de trabajo","2":"MMSE","3":"0.596366290","4":"4.084069e-04","5":"0.03059305","6":"0.595957883","7":"0.00009999","8":"19.48016988","9":"0.0001346804"},{"1":"MMSE","2":"Funciones ejecutivas","3":"-0.664277893","4":"4.897524e-04","5":"0.03470425","6":"-0.664767646","7":"0.00009999","8":"-19.15522125","9":"0.0001346804"},{"1":"Fluidez (animales)","2":"Funciones ejecutivas","3":"-0.620132876","4":"-3.181349e-04","5":"0.03462472","6":"-0.619814741","7":"0.00009999","8":"-17.90093226","9":"0.0001346804"},{"1":"Funciones ejecutivas","2":"Correctas ejecutivas","3":"-0.643421345","4":"3.010349e-05","5":"0.03613275","6":"-0.643451448","7":"0.00009999","8":"-17.80798391","9":"0.0001346804"},{"1":"Fluidez (animales)","2":"Atención","3":"-0.554630891","4":"-2.253906e-04","5":"0.03115418","6":"-0.554405500","7":"0.00009999","8":"-17.79554073","9":"0.0001346804"},{"1":"MMSE","2":"Fluidez (vegetales)","3":"0.534717610","4":"-6.703712e-05","5":"0.03077075","6":"0.534784647","7":"0.00009999","8":"17.37964458","9":"0.0001346804"},{"1":"Funciones ejecutivas","2":"Errores ejecutivos","3":"0.598979279","4":"5.629631e-04","5":"0.03592060","6":"0.598416316","7":"0.00009999","8":"16.65941775","9":"0.0001346804"},{"1":"Memoria de trabajo","2":"Funciones ejecutivas","3":"-0.565898039","4":"-3.426550e-04","5":"0.03571908","6":"-0.565555384","7":"0.00009999","8":"-15.83342669","9":"0.0001346804"},{"1":"Memoria de trabajo","2":"Fluidez (animales)","3":"0.478889958","4":"1.362494e-04","5":"0.03059080","6":"0.478753709","7":"0.00009999","8":"15.65025343","9":"0.0001346804"},{"1":"Memoria inmediata","2":"Atención","3":"-0.479625129","4":"9.997686e-06","5":"0.03091117","6":"-0.479635126","7":"0.00009999","8":"-15.51656398","9":"0.0001346804"},{"1":"Atención","2":"Correctas atención","3":"-0.486142563","4":"1.844214e-04","5":"0.03142578","6":"-0.486326984","7":"0.00009999","8":"-15.47541486","9":"0.0001346804"},{"1":"Nivel educativo","2":"Atención","3":"-0.478590575","4":"2.672308e-04","5":"0.03108124","6":"-0.478857806","7":"0.00009999","8":"-15.40664858","9":"0.0001346804"},{"1":"Nivel educativo","2":"Memoria inmediata","3":"0.453178870","4":"-2.687279e-04","5":"0.03013298","6":"0.453447597","7":"0.00009999","8":"15.04821385","9":"0.0001346804"},{"1":"Memoria inmediata","2":"MMSE","3":"0.449695704","4":"2.233098e-04","5":"0.03061381","6":"0.449472394","7":"0.00009999","8":"14.68201373","9":"0.0001346804"},{"1":"Nivel educativo","2":"Memoria de trabajo","3":"0.419670371","4":"-4.333118e-04","5":"0.03053467","6":"0.420103683","7":"0.00009999","8":"13.75825119","9":"0.0001346804"},{"1":"Nivel educativo","2":"Funciones ejecutivas","3":"-0.473491906","4":"5.973234e-05","5":"0.03465845","6":"-0.473551638","7":"0.00009999","8":"-13.66338309","9":"0.0001346804"},{"1":"Fluidez (vegetales)","2":"Atención","3":"-0.407086147","4":"4.894017e-04","5":"0.03128986","6":"-0.407575549","7":"0.00009999","8":"-13.02580082","9":"0.0001346804"},{"1":"Memoria inmediata","2":"Funciones ejecutivas","3":"-0.457682282","4":"3.640339e-05","5":"0.03567168","6":"-0.457718686","7":"0.00009999","8":"-12.83142920","9":"0.0001346804"},{"1":"Fluidez (vegetales)","2":"Funciones ejecutivas","3":"-0.447567464","4":"3.991983e-04","5":"0.03509164","6":"-0.447966663","7":"0.00009999","8":"-12.76562518","9":"0.0001346804"},{"1":"Atención","2":"Errores de atención","3":"0.388691776","4":"-2.209831e-04","5":"0.03113386","6":"0.388912759","7":"0.00009999","8":"12.49163371","9":"0.0001346804"},{"1":"Memoria de trabajo","2":"Fluidez (vegetales)","3":"0.367039032","4":"-2.295946e-04","5":"0.03057051","6":"0.367268626","7":"0.00009999","8":"12.01382110","9":"0.0001346804"},{"1":"Memoria inmediata","2":"Fluidez (animales)","3":"0.342769948","4":"-5.248868e-04","5":"0.03041973","6":"0.343294835","7":"0.00009999","8":"11.28526961","9":"0.0001346804"},{"1":"Nivel educativo","2":"MMSE","3":"0.332959219","4":"-1.898590e-04","5":"0.03002663","6":"0.333149078","7":"0.00009999","8":"11.09511999","9":"0.0001346804"},{"1":"Errores de atención","2":"Correctas atención","3":"-0.334195849","4":"3.711445e-04","5":"0.03082573","6":"-0.334566994","7":"0.00009999","8":"-10.85349875","9":"0.0001346804"},{"1":"MMSE","2":"Errores de atención","3":"-0.247841066","4":"8.680997e-05","5":"0.03038687","6":"-0.247927876","7":"0.00009999","8":"-8.15904715","9":"0.0001346804"},{"1":"Memoria de trabajo","2":"Errores de atención","3":"-0.246096153","4":"-1.752861e-04","5":"0.03086552","6":"-0.245920867","7":"0.00009999","8":"-7.96749546","9":"0.0001346804"},{"1":"Memoria inmediata","2":"Errores de atención","3":"-0.226855604","4":"-6.990903e-05","5":"0.03042397","6":"-0.226785695","7":"0.00009999","8":"-7.45417843","9":"0.0001346804"},{"1":"Nivel educativo","2":"Errores de atención","3":"-0.207013936","4":"4.616005e-05","5":"0.03046389","6":"-0.207060096","7":"0.00009999","8":"-6.79690196","9":"0.0001346804"},{"1":"Memoria inmediata","2":"Fluidez (vegetales)","3":"0.205431473","4":"5.218754e-06","5":"0.03063765","6":"0.205426254","7":"0.00009999","8":"6.70502592","9":"0.0001346804"},{"1":"Correctas atención","2":"Funciones ejecutivas","3":"-0.231398845","4":"1.383103e-04","5":"0.03534109","6":"-0.231537155","7":"0.00009999","8":"-6.55150077","9":"0.0001346804"},{"1":"Errores de atención","2":"Funciones ejecutivas","3":"0.221092183","4":"-1.845914e-05","5":"0.03514700","6":"0.221110642","7":"0.00009999","8":"6.29102444","9":"0.0001346804"},{"1":"Memoria de trabajo","2":"Correctas atención","3":"0.186452079","4":"2.038783e-04","5":"0.03070371","6":"0.186248200","7":"0.00009999","8":"6.06598335","9":"0.0001346804"},{"1":"Nivel educativo","2":"Fluidez (animales)","3":"0.181762755","4":"2.933458e-04","5":"0.03005498","6":"0.181469410","7":"0.00009999","8":"6.03791475","9":"0.0001346804"},{"1":"Fluidez (animales)","2":"Errores de atención","3":"-0.178878061","4":"8.261552e-05","5":"0.03017230","6":"-0.178960677","7":"0.00009999","8":"-5.93129079","9":"0.0001346804"},{"1":"MMSE","2":"Correctas atención","3":"0.180092172","4":"2.140594e-04","5":"0.03075172","6":"0.179878112","7":"0.00009999","8":"5.84936659","9":"0.0001346804"},{"1":"Fluidez (animales)","2":"Correctas atención","3":"0.163728179","4":"1.054009e-04","5":"0.03032835","6":"0.163622779","7":"0.00009999","8":"5.39504315","9":"0.0001346804"},{"1":"Fluidez (vegetales)","2":"Correctas atención","3":"0.162159330","4":"1.696107e-04","5":"0.03046177","6":"0.161989719","7":"0.00009999","8":"5.31780427","9":"0.0001346804"},{"1":"Nivel educativo","2":"Correctas ejecutivas","3":"-0.156687684","4":"3.024569e-04","5":"0.03065238","6":"-0.156990141","7":"0.00009999","8":"-5.12162936","9":"0.0001346804"},{"1":"Memoria inmediata","2":"Correctas ejecutivas","3":"-0.145246113","4":"-2.149261e-04","5":"0.03045080","6":"-0.145031187","7":"0.00009999","8":"-4.76280450","9":"0.0001346804"},{"1":"Memoria de trabajo","2":"Correctas ejecutivas","3":"-0.147040626","4":"3.298336e-04","5":"0.03136261","6":"-0.147370459","7":"0.00009999","8":"-4.69892153","9":"0.0001346804"},{"1":"Memoria inmediata","2":"Correctas atención","3":"0.141674340","4":"2.598067e-04","5":"0.03068658","6":"0.141414534","7":"0.00009999","8":"4.60835135","9":"0.0001346804"},{"1":"MMSE","2":"Correctas ejecutivas","3":"-0.130481362","4":"5.620878e-05","5":"0.03043988","6":"-0.130537571","7":"0.00029997","8":"-4.28837305","9":"0.0003959604"},{"1":"Fluidez (vegetales)","2":"Errores de atención","3":"-0.126076325","4":"-3.740592e-04","5":"0.03092571","6":"-0.125702266","7":"0.00009999","8":"-4.06465256","9":"0.0001346804"},{"1":"Nivel educativo","2":"Correctas atención","3":"0.085375139","4":"1.278343e-04","5":"0.03009049","6":"0.085247304","7":"0.00569943","8":"2.83303183","9":"0.0073757330"},{"1":"Errores de atención","2":"Correctas ejecutivas","3":"0.077188160","4":"2.890953e-04","5":"0.03099041","6":"0.076899065","7":"0.01239876","8":"2.48138280","9":"0.0157368878"},{"1":"Fluidez (animales)","2":"Errores ejecutivos","3":"-0.073160679","4":"3.415624e-04","5":"0.03044419","6":"-0.073502241","7":"0.01679832","8":"-2.41432762","9":"0.0209186629"},{"1":"Atención","2":"Correctas ejecutivas","3":"0.061325786","4":"-4.580437e-05","5":"0.03162831","6":"0.061371591","7":"0.05419458","8":"1.94040026","9":"0.0651534847"},{"1":"Fluidez (vegetales)","2":"Errores ejecutivos","3":"-0.059834781","4":"-7.400705e-05","5":"0.03111459","6":"-0.059760774","7":"0.05429457","8":"-1.92066723","9":"0.0651534847"},{"1":"Nivel educativo","2":"Errores ejecutivos","3":"-0.053034086","4":"1.053724e-04","5":"0.03101425","6":"-0.053139459","7":"0.08529147","8":"-1.71338868","9":"0.1005220906"},{"1":"Fluidez (vegetales)","2":"Correctas ejecutivas","3":"0.048776716","4":"3.676631e-04","5":"0.03117702","6":"0.048409053","7":"0.12018798","8":"1.55271580","9":"0.1391650309"},{"1":"Correctas atención","2":"Correctas ejecutivas","3":"0.043771477","4":"-4.061863e-05","5":"0.03138300","6":"0.043812096","7":"0.16368363","8":"1.39604560","9":"0.1862606843"},{"1":"Nivel educativo","2":"Fluidez (vegetales)","3":"-0.029677808","4":"-2.189223e-05","5":"0.03041645","6":"-0.029655916","7":"0.32296770","8":"-0.97499588","9":"0.3612859053"},{"1":"Atención","2":"Errores ejecutivos","3":"0.030322021","4":"8.047605e-05","5":"0.03184730","6":"0.030241545","7":"0.33936606","8":"0.94957949","9":"0.3733026697"},{"1":"Correctas atención","2":"Errores ejecutivos","3":"0.018243900","4":"-5.246098e-04","5":"0.03103925","6":"0.018768510","7":"0.55594441","8":"0.60467022","9":"0.5918117866"},{"1":"Memoria inmediata","2":"Errores ejecutivos","3":"-0.018563375","4":"-5.171962e-04","5":"0.03080639","6":"-0.018046179","7":"0.54524548","8":"-0.58579329","9":"0.5899377275"},{"1":"Fluidez (animales)","2":"Correctas ejecutivas","3":"0.009517829","4":"5.907283e-04","5":"0.03127975","6":"0.008927100","7":"0.76322368","8":"0.28539550","9":"0.7995676623"},{"1":"Errores de atención","2":"Errores ejecutivos","3":"-0.008376443","4":"-1.779620e-04","5":"0.03099449","6":"-0.008198481","7":"0.78832117","8":"-0.26451412","9":"0.8129562044"},{"1":"Memoria de trabajo","2":"Errores ejecutivos","3":"-0.006744864","4":"2.080372e-04","5":"0.03166539","6":"-0.006952901","7":"0.82911709","8":"-0.21957417","9":"0.8418727358"},{"1":"MMSE","2":"Errores ejecutivos","3":"-0.002494467","4":"4.913586e-05","5":"0.03065966","6":"-0.002543602","7":"0.93320668","8":"-0.08296251","9":"0.9332066793"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
## "Score" de colinealidad


``` r
#Ahora esto lo que arma es un score de fuerza de colinealidad sumando los valores absolutos de cada delta r para cada permutación. Con lo cual es un valor más general sobre colinealidad de esa variable.

colinearity_score <- function(data, x, vars, n_perm = 10000, method = "spearman") {
  
  others <- setdiff(vars, x)
  
  deltas <- sapply(others, function(y) {
    
    r_obs <- cor(data[[x]], data[[y]], method = method, use = "complete.obs")
    
    r_perm <- replicate(
      n_perm,
      cor(sample(data[[x]]), data[[y]], method = method, use = "complete.obs")
    )
    
    r_obs - mean(r_perm)
  })
  
  tibble::tibble(
    var = x,
    colinearity_strength = sum(abs(deltas)),   # fuerza total
    mean_delta = mean(abs(deltas)),             # promedio
    max_delta = max(abs(deltas)),               # peor caso
    n_links = length(deltas)                    # cuántas variables compara
  )
}

library(purrr)
library(dplyr)

colinearity_tbl <- map_dfr(
  vars,
  ~ colinearity_score(
      data  = data,
      x     = .x,
      vars  = vars,
      n_perm = 10000   # o 5000 / 10000 si querés
    )
)

colinearity_tbl %>%
  arrange(desc(colinearity_strength))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["var"],"name":[1],"type":["chr"],"align":["left"]},{"label":["colinearity_strength"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["mean_delta"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["max_delta"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["n_links"],"name":[5],"type":["int"],"align":["right"]}],"data":[{"1":"Funciones ejecutivas","2":"5.715380","3":"0.5195800","4":"0.7893108","5":"11"},{"1":"Atención","2":"4.970976","3":"0.4519069","4":"0.7889484","5":"11"},{"1":"MMSE","2":"4.480097","3":"0.4072815","4":"0.6796830","5":"11"},{"1":"Memoria de trabajo","2":"4.242342","3":"0.3856674","4":"0.6146705","5":"11"},{"1":"Fluidez (animales)","2":"4.002465","3":"0.3638605","4":"0.7368670","5":"11"},{"1":"Memoria inmediata","2":"3.533722","3":"0.3212475","4":"0.6133629","5":"11"},{"1":"Fluidez (vegetales)","2":"3.125047","3":"0.2840952","4":"0.7375634","5":"11"},{"1":"Nivel educativo","2":"2.871727","3":"0.2610661","4":"0.4781254","5":"11"},{"1":"Errores de atención","2":"2.262078","3":"0.2056434","4":"0.3885538","5":"11"},{"1":"Correctas ejecutivas","2":"2.079542","3":"0.1890493","4":"0.6434465","5":"11"},{"1":"Correctas atención","2":"2.033190","3":"0.1848354","4":"0.4863067","5":"11"},{"1":"Errores ejecutivos","2":"1.483615","3":"0.1348741","4":"0.6146349","5":"11"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
### AD vs FTD


``` r
#Ahora hago lo mismo pero solamente con la data AD + FTD

results_all <- map_dfr(
  combn(vars, 2, simplify = FALSE),
  ~ perm_corr_test(data2, .x[1], .x[2], n_perm = 10000)
)

results_all <- results_all |>
  dplyr::mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )

results_all |>
  arrange(desc(abs(z_perm)))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["var_x"],"name":[1],"type":["chr"],"align":["left"]},{"label":["var_y"],"name":[2],"type":["chr"],"align":["left"]},{"label":["r_obs"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["r_perm_mean"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["r_perm_sd"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["delta_r"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["p_value"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["z_perm"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["p_adj"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"Fluidez (animales)","2":"Fluidez (vegetales)","3":"0.6334520390","4":"-2.939282e-04","5":"0.04090159","6":"0.6337459672","7":"0.00009999","8":"15.49441093","9":"0.0001609595"},{"1":"Errores ejecutivos","2":"Correctas ejecutivas","3":"-0.6570616535","4":"-5.903139e-05","5":"0.04427874","6":"-0.6570026221","7":"0.00009999","8":"-14.83787946","9":"0.0001609595"},{"1":"Atención","2":"Correctas atención","3":"-0.6131856318","4":"-4.087931e-04","5":"0.04352302","6":"-0.6127768388","7":"0.00009999","8":"-14.07937154","9":"0.0001609595"},{"1":"Funciones ejecutivas","2":"Correctas ejecutivas","3":"-0.7130178622","4":"-1.040868e-04","5":"0.05163383","6":"-0.7129137754","7":"0.00009999","8":"-13.80710586","9":"0.0001609595"},{"1":"Atención","2":"Funciones ejecutivas","3":"0.7074697696","4":"3.140506e-04","5":"0.05245158","6":"0.7071557190","7":"0.00009999","8":"13.48206811","9":"0.0001609595"},{"1":"Memoria inmediata","2":"Memoria de trabajo","3":"0.5352381285","4":"8.419164e-05","5":"0.04089506","6":"0.5351539368","7":"0.00009999","8":"13.08602899","9":"0.0001609595"},{"1":"MMSE","2":"Fluidez (animales)","3":"0.5255670955","4":"4.884751e-04","5":"0.04068370","6":"0.5250786204","7":"0.00009999","8":"12.90636213","9":"0.0001609595"},{"1":"MMSE","2":"Atención","3":"-0.5421863309","4":"5.272295e-05","5":"0.04254519","6":"-0.5422390538","7":"0.00009999","8":"-12.74501365","9":"0.0001609595"},{"1":"Memoria de trabajo","2":"Atención","3":"-0.5328968585","4":"2.492351e-04","5":"0.04298188","6":"-0.5331460935","7":"0.00009999","8":"-12.40397339","9":"0.0001609595"},{"1":"Memoria de trabajo","2":"MMSE","3":"0.4894126918","4":"1.890517e-04","5":"0.04137867","6":"0.4892236401","7":"0.00009999","8":"11.82308853","9":"0.0001609595"},{"1":"MMSE","2":"Fluidez (vegetales)","3":"0.4215841926","4":"4.253138e-05","5":"0.04086167","6":"0.4215416612","7":"0.00009999","8":"10.31630967","9":"0.0001609595"},{"1":"Atención","2":"Errores de atención","3":"0.4257655110","4":"3.183370e-04","5":"0.04347075","6":"0.4254471740","7":"0.00009999","8":"9.78697583","9":"0.0001609595"},{"1":"MMSE","2":"Funciones ejecutivas","3":"-0.4632416425","4":"8.348107e-05","5":"0.04921585","6":"-0.4633251236","7":"0.00009999","8":"-9.41414393","9":"0.0001609595"},{"1":"Memoria de trabajo","2":"Fluidez (animales)","3":"0.3902873064","4":"7.253458e-05","5":"0.04167871","6":"0.3902147718","7":"0.00009999","8":"9.36244943","9":"0.0001609595"},{"1":"Fluidez (animales)","2":"Atención","3":"-0.3846411500","4":"5.015848e-04","5":"0.04306498","6":"-0.3851427348","7":"0.00009999","8":"-8.94329307","9":"0.0001609595"},{"1":"Errores de atención","2":"Correctas atención","3":"-0.3771561066","4":"1.988563e-04","5":"0.04226737","6":"-0.3773549629","7":"0.00009999","8":"-8.92780899","9":"0.0001609595"},{"1":"Funciones ejecutivas","2":"Errores ejecutivos","3":"0.4529256713","4":"7.889232e-04","5":"0.05167349","6":"0.4521367480","7":"0.00009999","8":"8.74987787","9":"0.0001609595"},{"1":"Memoria de trabajo","2":"Funciones ejecutivas","3":"-0.4306414566","4":"-1.654835e-04","5":"0.04973566","6":"-0.4304759732","7":"0.00009999","8":"-8.65527816","9":"0.0001609595"},{"1":"Memoria inmediata","2":"MMSE","3":"0.3459879697","4":"2.090092e-05","5":"0.04049932","6":"0.3459670688","7":"0.00009999","8":"8.54253971","9":"0.0001609595"},{"1":"Memoria inmediata","2":"Atención","3":"-0.3626507375","4":"3.784564e-04","5":"0.04258682","6":"-0.3630291938","7":"0.00009999","8":"-8.52444946","9":"0.0001609595"},{"1":"Fluidez (animales)","2":"Funciones ejecutivas","3":"-0.4228613555","4":"-2.614849e-05","5":"0.04997071","6":"-0.4228352070","7":"0.00009999","8":"-8.46166129","9":"0.0001609595"},{"1":"Nivel educativo","2":"Memoria inmediata","3":"0.3374632877","4":"8.377366e-05","5":"0.04091364","6":"0.3373795140","7":"0.00009999","8":"8.24613868","9":"0.0001609595"},{"1":"Memoria de trabajo","2":"Fluidez (vegetales)","3":"0.3323089906","4":"-6.661537e-05","5":"0.04153796","6":"0.3323756060","7":"0.00009999","8":"8.00173068","9":"0.0001609595"},{"1":"Nivel educativo","2":"Atención","3":"-0.3389661108","4":"-6.022408e-04","5":"0.04338637","6":"-0.3383638700","7":"0.00009999","8":"-7.79885225","9":"0.0001609595"},{"1":"Fluidez (vegetales)","2":"Atención","3":"-0.3176541584","4":"-8.534652e-04","5":"0.04328500","6":"-0.3168006932","7":"0.00009999","8":"-7.31894793","9":"0.0001609595"},{"1":"Memoria inmediata","2":"Funciones ejecutivas","3":"-0.3449536036","4":"9.835364e-05","5":"0.04920603","6":"-0.3450519573","7":"0.00009999","8":"-7.01239176","9":"0.0001609595"},{"1":"Nivel educativo","2":"Funciones ejecutivas","3":"-0.3457957623","4":"5.299054e-04","5":"0.04971413","6":"-0.3463256678","7":"0.00009999","8":"-6.96634215","9":"0.0001609595"},{"1":"Memoria inmediata","2":"Fluidez (animales)","3":"0.2799445933","4":"2.676585e-04","5":"0.04130030","6":"0.2796769347","7":"0.00009999","8":"6.77179021","9":"0.0001609595"},{"1":"Nivel educativo","2":"Memoria de trabajo","3":"0.2795007290","4":"2.729006e-04","5":"0.04164759","6":"0.2792278284","7":"0.00009999","8":"6.70453710","9":"0.0001609595"},{"1":"Fluidez (vegetales)","2":"Funciones ejecutivas","3":"-0.2974545317","4":"5.465595e-04","5":"0.05056100","6":"-0.2980010911","7":"0.00009999","8":"-5.89389239","9":"0.0001609595"},{"1":"MMSE","2":"Errores ejecutivos","3":"0.2279493543","4":"5.242656e-04","5":"0.04244797","6":"0.2274250887","7":"0.00009999","8":"5.35773826","9":"0.0001609595"},{"1":"Correctas atención","2":"Funciones ejecutivas","3":"-0.2694018811","4":"-8.354580e-05","5":"0.05085556","6":"-0.2693183353","7":"0.00009999","8":"-5.29575037","9":"0.0001609595"},{"1":"Errores de atención","2":"Funciones ejecutivas","3":"0.2590687589","4":"-4.435710e-04","5":"0.05076252","6":"0.2595123299","7":"0.00009999","8":"5.11228187","9":"0.0001609595"},{"1":"Memoria de trabajo","2":"Errores de atención","3":"-0.2133439298","4":"-3.640297e-04","5":"0.04190784","6":"-0.2129799002","7":"0.00009999","8":"-5.08210168","9":"0.0001609595"},{"1":"Memoria inmediata","2":"Fluidez (vegetales)","3":"0.2087923881","4":"-9.104326e-05","5":"0.04167264","6":"0.2088834313","7":"0.00009999","8":"5.01248349","9":"0.0001609595"},{"1":"Nivel educativo","2":"MMSE","3":"0.2002780068","4":"-4.606395e-04","5":"0.04020808","6":"0.2007386463","7":"0.00009999","8":"4.99249480","9":"0.0001609595"},{"1":"MMSE","2":"Correctas ejecutivas","3":"-0.2036608697","4":"6.172237e-04","5":"0.04216605","6":"-0.2042780934","7":"0.00009999","8":"-4.84461053","9":"0.0001609595"},{"1":"Memoria de trabajo","2":"Correctas atención","3":"0.1941402760","4":"-6.503187e-04","5":"0.04195708","6":"0.1947905947","7":"0.00019998","8":"4.64261584","9":"0.0002999700"},{"1":"MMSE","2":"Errores de atención","3":"-0.1902311260","4":"1.170609e-04","5":"0.04134104","6":"-0.1903481868","7":"0.00009999","8":"-4.60434006","9":"0.0001609595"},{"1":"Memoria de trabajo","2":"Errores ejecutivos","3":"0.1970912234","4":"2.622544e-04","5":"0.04305433","6":"0.1968289689","7":"0.00009999","8":"4.57164199","9":"0.0001609595"},{"1":"Memoria inmediata","2":"Errores de atención","3":"-0.1805244740","4":"1.889621e-04","5":"0.04142089","6":"-0.1807134360","7":"0.00009999","8":"-4.36285693","9":"0.0001609595"},{"1":"Memoria de trabajo","2":"Correctas ejecutivas","3":"-0.1650290907","4":"1.950052e-04","5":"0.04301797","6":"-0.1652240959","7":"0.00019998","8":"-3.84081581","9":"0.0002999700"},{"1":"Nivel educativo","2":"Errores de atención","3":"-0.1553049197","4":"8.466760e-04","5":"0.04157965","6":"-0.1561515958","7":"0.00039996","8":"-3.75548137","9":"0.0005616460"},{"1":"Fluidez (animales)","2":"Errores ejecutivos","3":"0.1579053868","4":"2.363816e-04","5":"0.04220017","6":"0.1576690052","7":"0.00019998","8":"3.73621767","9":"0.0002999700"},{"1":"Fluidez (animales)","2":"Errores de atención","3":"-0.1526607079","4":"2.683729e-04","5":"0.04125712","6":"-0.1529290808","7":"0.00009999","8":"-3.70673193","9":"0.0001609595"},{"1":"Memoria inmediata","2":"Correctas atención","3":"0.1472281129","4":"-8.770713e-05","5":"0.04170505","6":"0.1473158201","7":"0.00039996","8":"3.53232590","9":"0.0005616460"},{"1":"MMSE","2":"Correctas atención","3":"0.1408021496","4":"-1.346090e-04","5":"0.04154944","6":"0.1409367586","7":"0.00029997","8":"3.39202560","9":"0.0004399560"},{"1":"Atención","2":"Errores ejecutivos","3":"-0.1335220130","4":"4.502886e-06","5":"0.04450614","6":"-0.1335265159","7":"0.00319968","8":"-3.00018187","9":"0.0043995600"},{"1":"Fluidez (vegetales)","2":"Errores ejecutivos","3":"0.1278573532","4":"-1.685565e-04","5":"0.04298085","6":"0.1280259097","7":"0.00389961","8":"2.97867347","9":"0.0052525360"},{"1":"Memoria inmediata","2":"Errores ejecutivos","3":"0.1207158118","4":"-3.589107e-04","5":"0.04270195","6":"0.1210747225","7":"0.00589941","8":"2.83534409","9":"0.0076345307"},{"1":"Nivel educativo","2":"Correctas atención","3":"0.1145242285","4":"3.649578e-04","5":"0.04081702","6":"0.1141592707","7":"0.00569943","8":"2.79685474","9":"0.0075232477"},{"1":"Fluidez (vegetales)","2":"Errores de atención","3":"-0.1146202363","4":"-6.003318e-04","5":"0.04222876","6":"-0.1140199046","7":"0.00649935","8":"-2.70005361","9":"0.0082491751"},{"1":"Memoria inmediata","2":"Correctas ejecutivas","3":"-0.1134334420","4":"2.957410e-04","5":"0.04263874","6":"-0.1137291830","7":"0.00719928","8":"-2.66727376","9":"0.0089651412"},{"1":"Nivel educativo","2":"Fluidez (animales)","3":"0.0861211262","4":"2.484238e-04","5":"0.04053445","6":"0.0858727024","7":"0.03259674","8":"2.11851142","9":"0.0398404604"},{"1":"Nivel educativo","2":"Correctas ejecutivas","3":"-0.0862098524","4":"-6.547034e-08","5":"0.04198216","6":"-0.0862097870","7":"0.04119588","8":"-2.05348632","9":"0.0494350565"},{"1":"Fluidez (vegetales)","2":"Correctas atención","3":"0.0780799516","4":"-3.099137e-04","5":"0.04168673","6":"0.0783898653","7":"0.06059394","8":"1.88045112","9":"0.0714142871"},{"1":"Fluidez (animales)","2":"Correctas ejecutivas","3":"-0.0765855987","4":"4.694138e-04","5":"0.04258115","6":"-0.0770550125","7":"0.07059294","8":"-1.80960369","9":"0.0817391945"},{"1":"Fluidez (animales)","2":"Correctas atención","3":"0.0665717176","4":"1.396162e-04","5":"0.04141737","6":"0.0664321014","7":"0.11028897","8":"1.60396720","9":"0.1250519016"},{"1":"Fluidez (vegetales)","2":"Correctas ejecutivas","3":"-0.0680136956","4":"-3.900799e-04","5":"0.04274536","6":"-0.0676236157","7":"0.11178882","8":"-1.58201085","9":"0.1250519016"},{"1":"Correctas atención","2":"Errores ejecutivos","3":"0.0560505270","4":"-4.242971e-04","5":"0.04313338","6":"0.0564748241","7":"0.19448055","8":"1.30930664","9":"0.2139286071"},{"1":"Nivel educativo","2":"Errores ejecutivos","3":"0.0511055422","4":"1.851692e-04","5":"0.04239386","6":"0.0509203730","7":"0.22927707","8":"1.20112613","9":"0.2480702749"},{"1":"Correctas atención","2":"Correctas ejecutivas","3":"0.0463933990","4":"-5.268960e-04","5":"0.04359720","6":"0.0469202950","7":"0.28637136","8":"1.07622270","9":"0.3048469347"},{"1":"Errores de atención","2":"Correctas ejecutivas","3":"0.0231595463","4":"-5.460661e-04","5":"0.04362404","6":"0.0237056124","7":"0.59854015","8":"0.54340711","9":"0.6203379662"},{"1":"Atención","2":"Correctas ejecutivas","3":"0.0229870507","4":"5.968692e-04","5":"0.04464467","6":"0.0223901815","7":"0.60153985","8":"0.50151968","9":"0.6203379662"},{"1":"Nivel educativo","2":"Fluidez (vegetales)","3":"-0.0168250495","4":"-4.537884e-04","5":"0.04173746","6":"-0.0163712612","7":"0.68803120","8":"-0.39224379","9":"0.6986162922"},{"1":"Errores de atención","2":"Errores ejecutivos","3":"-0.0001739692","4":"3.751467e-04","5":"0.04326468","6":"-0.0005491159","7":"0.99640036","8":"-0.01269201","9":"0.9964003600"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


``` r
colinearity_tbl <- map_dfr(
  vars,
  ~ colinearity_score(
      data  = data2,
      x     = .x,
      vars  = vars,
      n_perm = 10000   # o 5000 / 10000 si querés
    )
)

colinearity_tbl %>%
  arrange(desc(colinearity_strength))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["var"],"name":[1],"type":["chr"],"align":["left"]},{"label":["colinearity_strength"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["mean_delta"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["max_delta"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["n_links"],"name":[5],"type":["int"],"align":["right"]}],"data":[{"1":"Funciones ejecutivas","2":"4.706877","3":"0.4278979","4":"0.7130564","5":"11"},{"1":"Atención","2":"4.381311","3":"0.3983010","4":"0.7077632","5":"11"},{"1":"Memoria de trabajo","2":"3.762653","3":"0.3420593","4":"0.5357602","5":"11"},{"1":"MMSE","2":"3.750392","3":"0.3409447","4":"0.5423040","5":"11"},{"1":"Fluidez (animales)","2":"3.175547","3":"0.2886861","4":"0.6333071","5":"11"},{"1":"Memoria inmediata","2":"2.980138","3":"0.2709217","4":"0.5355043","5":"11"},{"1":"Fluidez (vegetales)","2":"2.616517","3":"0.2378652","4":"0.6333290","5":"11"},{"1":"Errores ejecutivos","2":"2.185074","3":"0.1986431","4":"0.6568471","5":"11"},{"1":"Correctas ejecutivas","2":"2.174937","3":"0.1977216","4":"0.7126281","5":"11"},{"1":"Correctas atención","2":"2.105695","3":"0.1914269","4":"0.6137428","5":"11"},{"1":"Errores de atención","2":"2.090200","3":"0.1900182","4":"0.4261368","5":"11"},{"1":"Nivel educativo","2":"2.013350","3":"0.1830318","4":"0.3453700","5":"11"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
## Modelo logístico basal con todas las variables


``` r
#Modelo logistico con todas las variables para ver cuales dan "significativas"
modelo_todas <- glm(clinical_diagnosis~., data2, family=binomial)
summary(modelo_todas)
```

```
## 
## Call:
## glm(formula = clinical_diagnosis ~ ., family = binomial, data = data2)
## 
## Coefficients:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -2.0489005  1.4691475  -1.395 0.163131    
## `Nivel educativo`       0.0598843  0.0295263   2.028 0.042543 *  
## `Memoria inmediata`     0.0013200  0.0708769   0.019 0.985141    
## `Memoria de trabajo`   -0.0734425  0.0868618  -0.846 0.397826    
## MMSE                    0.1427961  0.0369396   3.866 0.000111 ***
## `Fluidez (animales)`   -0.0904753  0.0280834  -3.222 0.001274 ** 
## `Fluidez (vegetales)`  -0.0354267  0.0314615  -1.126 0.260151    
## Atención                0.0009477  0.0048826   0.194 0.846103    
## `Errores de atención`   0.3527964  0.1639014   2.152 0.031359 *  
## `Correctas atención`   -0.0354815  0.0386131  -0.919 0.358150    
## `Funciones ejecutivas` -0.0057870  0.0020984  -2.758 0.005818 ** 
## `Errores ejecutivos`    0.0609458  0.0419743   1.452 0.146507    
## `Correctas ejecutivas`  0.0322205  0.0227271   1.418 0.156274    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 502.21  on 398  degrees of freedom
## Residual deviance: 442.07  on 386  degrees of freedom
##   (213 observations deleted due to missingness)
## AIC: 468.07
## 
## Number of Fisher Scoring iterations: 4
```

``` r
vif(modelo_todas)
```

```
##      `Nivel educativo`    `Memoria inmediata`   `Memoria de trabajo` 
##               1.206504               1.536875               1.769802 
##                   MMSE   `Fluidez (animales)`  `Fluidez (vegetales)` 
##               1.508477               1.913680               1.717200 
##               Atención  `Errores de atención`   `Correctas atención` 
##               2.669761               1.321394               1.447586 
## `Funciones ejecutivas`   `Errores ejecutivos` `Correctas ejecutivas` 
##               2.450613               1.400061               2.314099
```

``` r
#A pesar de que las variables son colineales, el VIF no es preocupantemente alto
```


``` r
#Hago modelo logistico con todas las variables y pero evaluando el AUC por cross validation

library(dplyr)
library(rsample)
library(purrr)
library(pROC)

#Funcion para calcular AUC de regresión logística por CV

cv_auc_logistic <- function(data, outcome, predictors, v = 10) {
  
  df <- data[, c(outcome, predictors)]
  
  formula <- as.formula(
    paste(outcome, "~", paste(sprintf("`%s`", predictors), collapse = " + "))
  )
  
  folds <- vfold_cv(
    df,
    v = v,
    strata = !!sym(outcome)
  )
  
  aucs <- purrr::map_dbl(folds$splits, function(split) {
    
    train <- rsample::analysis(split)
    test  <- rsample::assessment(split)
    
    fit <- glm(
      formula,
      data = train,
      family = binomial()
    )
    
    prob <- predict(fit, test, type = "response")
    
    pROC::roc(
      response  = test[[outcome]],
      predictor = prob,
      quiet = TRUE
    )$auc
  })
  
  tibble::tibble(
    auc_mean = mean(aucs),
    auc_sd   = sd(aucs),
    auc_min  = min(aucs),
    auc_max  = max(aucs)
  )
}
```


``` r
#Calculo con todas las variables, AUC medio de 0.67
set.seed(123)

auc_full <- cv_auc_logistic(
  data       = data2,
  outcome    = "clinical_diagnosis",
  predictors = vars,
  v          = 10
)

auc_full
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["auc_mean"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["auc_sd"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["auc_min"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["auc_max"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.6707351","2":"0.1014528","3":"0.4497041","4":"0.8053333"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

``` r
#Grafico ese AUC "out of fold"
library(dplyr)
library(rsample)
library(purrr)
library(pROC)

cv_oof_pred <- function(data, outcome, predictors, v = 10, seed = 123) {
  
  # 🛡 1) limpiar predictores
  predictors <- predictors[!is.na(predictors)]
  predictors <- predictors[predictors != ""]
  
  # 🛡 2) backticks para nombres con espacios / tildes
  predictors_q <- sprintf("`%s`", predictors)
  
  set.seed(seed)
  
  df <- data[, c(outcome, predictors)]
  
  formula <- as.formula(
    paste(outcome, "~", paste(predictors_q, collapse = " + "))
  )
  
  folds <- rsample::vfold_cv(df, v = v, strata = !!rlang::sym(outcome))
  
  purrr::map_dfr(folds$splits, function(split) {
    
    train <- rsample::analysis(split)
    test  <- rsample::assessment(split)
    
    fit <- glm(
      formula,
      data = train,
      family = binomial()
    )
    
    prob <- predict(fit, test, type = "response")
    
    tibble::tibble(
      y = test[[outcome]],
      prob = as.numeric(prob)
    )
  })
}

oof <- cv_oof_pred(
  data2,
  "clinical_diagnosis",
  vars,
  v = 10,
  seed = 123
)

roc_oof <- roc(
  response  = oof$y,
  predictor = oof$prob,
  quiet = TRUE
)

auc_val <- auc(roc_oof)

roc_df <- tibble(
  fpr = 1 - roc_oof$specificities,
  tpr = roc_oof$sensitivities
)

library(ggplot2)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(linewidth = 1.3, color = "#1f77b4") +
  geom_abline(
    intercept = 0, slope = 1,
    linetype = "dashed",
    color = "grey60"
  ) +
  coord_equal() +
  labs(
    title = "ROC sin descartar variables",
    subtitle = paste0("AUC = ", round(auc_val, 3)),
    x = "1 − Especificidad",
    y = "Sensibilidad"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
```

![](EEAA_files/figure-html/unnamed-chunk-27-1.png)<!-- -->
## Descarte de variables por "score" de colinealidad


``` r
#Tenia este "orden" de "Fortaleza de colinealidad". Voy a ir sacando de a una y a analizar el AUC
colinearity_tbl %>%
  arrange(desc(colinearity_strength))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["var"],"name":[1],"type":["chr"],"align":["left"]},{"label":["colinearity_strength"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["mean_delta"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["max_delta"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["n_links"],"name":[5],"type":["int"],"align":["right"]}],"data":[{"1":"Funciones ejecutivas","2":"4.706877","3":"0.4278979","4":"0.7130564","5":"11"},{"1":"Atención","2":"4.381311","3":"0.3983010","4":"0.7077632","5":"11"},{"1":"Memoria de trabajo","2":"3.762653","3":"0.3420593","4":"0.5357602","5":"11"},{"1":"MMSE","2":"3.750392","3":"0.3409447","4":"0.5423040","5":"11"},{"1":"Fluidez (animales)","2":"3.175547","3":"0.2886861","4":"0.6333071","5":"11"},{"1":"Memoria inmediata","2":"2.980138","3":"0.2709217","4":"0.5355043","5":"11"},{"1":"Fluidez (vegetales)","2":"2.616517","3":"0.2378652","4":"0.6333290","5":"11"},{"1":"Errores ejecutivos","2":"2.185074","3":"0.1986431","4":"0.6568471","5":"11"},{"1":"Correctas ejecutivas","2":"2.174937","3":"0.1977216","4":"0.7126281","5":"11"},{"1":"Correctas atención","2":"2.105695","3":"0.1914269","4":"0.6137428","5":"11"},{"1":"Errores de atención","2":"2.090200","3":"0.1900182","4":"0.4261368","5":"11"},{"1":"Nivel educativo","2":"2.013350","3":"0.1830318","4":"0.3453700","5":"11"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


``` r
#Voy sacando de a una y calculando, sumo todo a una tabla


vars_reduced <- c("Atención", "Memoria de trabajo", "MMSE", "Fluidez (animales)", "Memoria inmediata", "Fluidez (vegetales)","Errores ejecutivos", "Correctas ejecutivas", "Correctas atención", "Errores de atención", "Nivel educativo")

set.seed(123)

auc_reduced <- cv_auc_logistic(
  data       = data2,
  outcome    = "clinical_diagnosis",
  predictors = vars_reduced,
  v          = 10
)

auc_reduced
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["auc_mean"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["auc_sd"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["auc_min"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["auc_max"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.6553093","2":"0.08475118","3":"0.5429688","4":"0.8030075"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

``` r
vars_reduced2<- c( "Memoria de trabajo", "MMSE", "Fluidez (animales)", "Memoria inmediata", "Fluidez (vegetales)","Errores ejecutivos", "Correctas ejecutivas", "Correctas atención", "Errores de atención", "Nivel educativo")


set.seed(123)

auc_reduced2 <- cv_auc_logistic(
  data       = data2,
  outcome    = "clinical_diagnosis",
  predictors = vars_reduced2,
  v          = 10
)

auc_reduced2
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["auc_mean"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["auc_sd"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["auc_min"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["auc_max"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.6523647","2":"0.06451942","3":"0.5716374","4":"0.7592593"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

``` r
vars_reduced3 <- c( "MMSE", "Fluidez (animales)", "Memoria inmediata", "Fluidez (vegetales)","Errores ejecutivos", "Correctas ejecutivas", "Correctas atención", "Errores de atención", "Nivel educativo")

set.seed(123)

auc_reduced3 <- cv_auc_logistic(
  data       = data2,
  outcome    = "clinical_diagnosis",
  predictors = vars_reduced3,
  v          = 10
)

auc_reduced3
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["auc_mean"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["auc_sd"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["auc_min"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["auc_max"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.6487754","2":"0.06675664","3":"0.5589286","4":"0.7698413"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

``` r
vars_reduced4 <- c("Fluidez (animales)", "Memoria inmediata", "Fluidez (vegetales)","Errores ejecutivos", "Correctas ejecutivas", "Correctas atención", "Errores de atención", "Nivel educativo")

set.seed(123)

auc_reduced4 <- cv_auc_logistic(
  data       = data2,
  outcome    = "clinical_diagnosis",
  predictors = vars_reduced4,
  v          = 10
)

auc_reduced4
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["auc_mean"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["auc_sd"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["auc_min"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["auc_max"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.6072205","2":"0.06647887","3":"0.5108359","4":"0.715873"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


``` r
#A medida que sacamos variables por colinealidad, PERDEMOS AUC

tablaauc <- rbind(auc_reduced, auc_reduced2, auc_reduced3, auc_reduced4)
rownames(tablaauc) <- c("Sacando 1 variable", "Sacando 2 variables", "Sacando 3 variables", "Sacando 4 variables")
tablaauc
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["auc_mean"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["auc_sd"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["auc_min"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["auc_max"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.6553093","2":"0.08475118","3":"0.5429688","4":"0.8030075","_rn_":"1"},{"1":"0.6523647","2":"0.06451942","3":"0.5716374","4":"0.7592593","_rn_":"2"},{"1":"0.6487754","2":"0.06675664","3":"0.5589286","4":"0.7698413","_rn_":"3"},{"1":"0.6072205","2":"0.06647887","3":"0.5108359","4":"0.7158730","_rn_":"4"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## Regularización: Elastic Net, Ridge, Lasso


``` r
#Entonces probamos hacer Elastic Net (Lasso + Ridge, o cada una de ellas) para manejar colinealidad y seleccionar variables

library(glmnet)
library(pROC)
library(rsample)
library(purrr)
library(dplyr)

cv_auc_elastic <- function(data, outcome, predictors, alpha = 0.5, v = 10, seed = 123) {
  
  predictors <- predictors[!is.na(predictors)]  # 🛡 evita NA
  predictors_q <- sprintf("`%s`", predictors)   # 🛡 nombres con espacios
  
  set.seed(seed)
  folds <- rsample::vfold_cv(data, v = v, strata = !!rlang::sym(outcome))
  
  aucs <- purrr::map_dbl(seq_along(folds$splits), function(i) {
    set.seed(seed + i)
    
    split <- folds$splits[[i]]
    train <- rsample::analysis(split)
    test  <- rsample::assessment(split)
    
    X_train <- model.matrix(
      as.formula(paste(outcome, "~", paste(predictors_q, collapse = " + "))),
      train
    )[,-1]
    
    X_test <- model.matrix(
      as.formula(paste(outcome, "~", paste(predictors_q, collapse = " + "))),
      test
    )[,-1]
    
    y_train <- train[[outcome]]
    y_test  <- test[[outcome]]
    
    cv_fit <- glmnet::cv.glmnet(
      X_train, y_train,
      family = "binomial",
      alpha = alpha,
      nfolds = 5
    )
    
    prob <- predict(cv_fit, X_test, s = "lambda.min", type = "response")
    pROC::roc(y_test, as.vector(prob), quiet = TRUE)$auc
  })
  
  tibble::tibble(
    model = paste0("Elastic Net (alpha=", alpha, ")"),
    auc_mean = mean(aucs),
    auc_sd   = sd(aucs)
  )
}
```


``` r
data2_clean <- data2 %>% drop_na()

#Ridge es el mejor, porque regulariza y maneja la colinealidad pero no elimina variables

auc_enet_ridge <- cv_auc_elastic(
  data       = data2_clean,
  outcome    = "clinical_diagnosis",
  predictors = vars,
  alpha      = 0,
  v          = 10
)

auc_enet_ridge
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["model"],"name":[1],"type":["chr"],"align":["left"]},{"label":["auc_mean"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["auc_sd"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"Elastic Net (alpha=0)","2":"0.6918091","3":"0.07665151"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

``` r
auc_enet <- cv_auc_elastic(
  data       = data2_clean,
  outcome    = "clinical_diagnosis",
  predictors = vars,
  alpha      = 0.5,
  v          = 10
)

auc_enet
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["model"],"name":[1],"type":["chr"],"align":["left"]},{"label":["auc_mean"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["auc_sd"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"Elastic Net (alpha=0.5)","2":"0.6850665","3":"0.07551046"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

``` r
auc_enet_lasso <- cv_auc_elastic(
  data       = data2_clean,
  outcome    = "clinical_diagnosis",
  predictors = vars,
  alpha      = 1,
  v          = 10
)

auc_enet_lasso
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["model"],"name":[1],"type":["chr"],"align":["left"]},{"label":["auc_mean"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["auc_sd"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"Elastic Net (alpha=1)","2":"0.6796771","3":"0.07558205"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>



``` r
#Esta calcula tambien los coeficientes para ver qué selecciona Lasso

cv_auc_elastic_with_coefs <- function(
  data, outcome, predictors,
  alpha = 1, v = 10, seed = 123
) {
  # 🛡 1) limpiar NAs y vacíos
  predictors <- predictors[!is.na(predictors)]
  predictors <- predictors[predictors != ""]
  
  # 🛡 2) backticks para nombres con espacios/tildes
  predictors_q <- sprintf("`%s`", predictors)
  
  set.seed(seed)
  folds <- rsample::vfold_cv(data, v = v, strata = !!rlang::sym(outcome))
  
  aucs <- numeric(length(folds$splits))
  selected_vars <- vector("list", length(folds$splits))
  
  for (i in seq_along(folds$splits)) {
    set.seed(seed + i)
    
    split <- folds$splits[[i]]
    train <- rsample::analysis(split)
    test  <- rsample::assessment(split)
    
    fml <- as.formula(paste(outcome, "~", paste(predictors_q, collapse = " + ")))
    
    X_train <- model.matrix(fml, train)[, -1, drop = FALSE]
    X_test  <- model.matrix(fml, test)[, -1, drop = FALSE]
    
    y_train <- train[[outcome]]
    y_test  <- test[[outcome]]
    
    cv_fit <- glmnet::cv.glmnet(
      X_train, y_train,
      family = "binomial",
      alpha = alpha,
      nfolds = 5
    )
    
    # --- AUC ---
    prob <- predict(cv_fit, X_test, s = "lambda.min", type = "response")
    aucs[i] <- pROC::roc(y_test, as.vector(prob), quiet = TRUE)$auc
    
    # --- VARIABLES SELECCIONADAS ---
    coefs <- coef(cv_fit, s = "lambda.min")   # ✅ método S3 correcto
    sel <- rownames(coefs)[as.numeric(coefs) != 0]
    sel <- setdiff(sel, "(Intercept)")
    
    selected_vars[[i]] <- sel
  }
  
  tibble::tibble(
    auc_mean = mean(aucs),
    auc_sd   = sd(aucs),
    selected_vars = list(unlist(selected_vars))
  )
}
```


``` r
res_lasso <- cv_auc_elastic_with_coefs(
  data2_clean,
  "clinical_diagnosis",
  vars,
  alpha = 1
)
```


``` r
#Aca se ve la importancia Lasso de cada variable
library(dplyr)

lasso_importance <- tibble(var = res_lasso$selected_vars[[1]]) %>%
  count(var) %>%
  mutate(freq = n / 10) %>%   # 10 folds
  arrange(desc(freq))

lasso_importance
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["var"],"name":[1],"type":["chr"],"align":["left"]},{"label":["n"],"name":[2],"type":["int"],"align":["right"]},{"label":["freq"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"MMSE","2":"10","3":"1.0"},{"1":"`Errores de atención`","2":"10","3":"1.0"},{"1":"`Fluidez (animales)`","2":"10","3":"1.0"},{"1":"`Funciones ejecutivas`","2":"10","3":"1.0"},{"1":"`Nivel educativo`","2":"10","3":"1.0"},{"1":"`Correctas ejecutivas`","2":"9","3":"0.9"},{"1":"`Fluidez (vegetales)`","2":"9","3":"0.9"},{"1":"`Memoria de trabajo`","2":"6","3":"0.6"},{"1":"`Correctas atención`","2":"3","3":"0.3"},{"1":"`Errores ejecutivos`","2":"2","3":"0.2"},{"1":"Atención","2":"1","3":"0.1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
## Importancia de variables por permutación


``` r
#Ahora calculo la importancia por permutación de cada variable: de a una variable, permuto y veo cuánto cambia el AUC

perm_importance_cv <- function(
  data, outcome, predictors,
  alpha = 0, v = 10, n_perm = 20, seed = 123
) {
  # 🛡 1) limpiar predictores
  predictors <- predictors[!is.na(predictors)]
  predictors <- predictors[predictors != ""]
  
  # 🛡 2) backticks para nombres con espacios / tildes
  predictors_q <- sprintf("`%s`", predictors)
  
  set.seed(seed)
  folds <- rsample::vfold_cv(data, v = v, strata = !!rlang::sym(outcome))
  
  all_imp <- list()
  
  for (i in seq_along(folds$splits)) {
    set.seed(seed + i)
    
    split <- folds$splits[[i]]
    train <- rsample::analysis(split)
    test  <- rsample::assessment(split)
    
    fml <- as.formula(
      paste(outcome, "~", paste(predictors_q, collapse = " + "))
    )
    
    X_train <- model.matrix(fml, train)[, -1, drop = FALSE]
    X_test  <- model.matrix(fml, test)[, -1, drop = FALSE]
    
    y_train <- train[[outcome]]
    y_test  <- test[[outcome]]
    
    # --- modelo base ---
    cv_fit <- glmnet::cv.glmnet(
      X_train, y_train,
      family = "binomial",
      alpha = alpha,
      nfolds = 5
    )
    
    prob_base <- predict(
      cv_fit,
      X_test,
      s = "lambda.min",
      type = "response"
    )
    
    auc_base <- pROC::roc(
      y_test,
      as.vector(prob_base),
      quiet = TRUE
    )$auc
    
    # --- permutaciones ---
    for (var in colnames(X_test)) {
      
      auc_perm <- replicate(n_perm, {
        Xp <- X_test
        Xp[, var] <- sample(Xp[, var])
        
        prob <- predict(
          cv_fit,
          Xp,
          s = "lambda.min",
          type = "response"
        )
        
        pROC::roc(
          y_test,
          as.vector(prob),
          quiet = TRUE
        )$auc
      })
      
      all_imp[[length(all_imp) + 1]] <- tibble::tibble(
        var = var,
        delta_auc = as.numeric(auc_base - mean(auc_perm))
      )
    }
  }
  
  dplyr::bind_rows(all_imp) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(
      mean_delta_auc = mean(delta_auc),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(mean_delta_auc))
}
```


``` r
#Se ve que la importancia por permutación (o sea importancia de cada variable en la capacidad predictiva del modelo) es diferente que la importancia de las variables segun Lasso
set.seed(123)

perm_imp_ridge <- perm_importance_cv(
  data       = data2_clean,
  outcome    = "clinical_diagnosis",
  predictors = vars,
  alpha      = 0,      # 🔑 Ridge
  v          = 10,     # CV externo
  n_perm     = 20,     # permutaciones por variable por fold
  seed       = 123
)

perm_imp_ridge
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["var"],"name":[1],"type":["chr"],"align":["left"]},{"label":["mean_delta_auc"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"MMSE","2":"8.249881e-02"},{"1":"`Funciones ejecutivas`","2":"6.083333e-02"},{"1":"`Fluidez (animales)`","2":"4.521961e-02"},{"1":"`Nivel educativo`","2":"3.401472e-02"},{"1":"`Errores de atención`","2":"2.297483e-02"},{"1":"`Correctas ejecutivas`","2":"2.231600e-02"},{"1":"`Fluidez (vegetales)`","2":"8.160019e-03"},{"1":"`Errores ejecutivos`","2":"4.645062e-03"},{"1":"`Memoria de trabajo`","2":"1.583571e-03"},{"1":"`Correctas atención`","2":"7.478632e-05"},{"1":"`Memoria inmediata`","2":"-2.532051e-03"},{"1":"Atención","2":"-2.590218e-03"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
## Importancia de variables: importancia por permutación vs estabilidad Lasso


``` r
library(dplyr)

# Permutation importance (Ridge)
perm_tbl <- perm_imp_ridge %>%
  rename(
    var = var,
    delta_auc = mean_delta_auc
  )

# LASSO estabilidad
lasso_tbl <- lasso_importance %>%
  rename(
    var = var,
    lasso_freq = freq
  )

# Unir todo
importance_tbl <- perm_tbl %>%
  left_join(lasso_tbl, by = "var") %>%
  mutate(
    lasso_freq = ifelse(is.na(lasso_freq), 0, lasso_freq)
  )
importance_tbl
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["var"],"name":[1],"type":["chr"],"align":["left"]},{"label":["delta_auc"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["n"],"name":[3],"type":["int"],"align":["right"]},{"label":["lasso_freq"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"MMSE","2":"8.249881e-02","3":"10","4":"1.0"},{"1":"`Funciones ejecutivas`","2":"6.083333e-02","3":"10","4":"1.0"},{"1":"`Fluidez (animales)`","2":"4.521961e-02","3":"10","4":"1.0"},{"1":"`Nivel educativo`","2":"3.401472e-02","3":"10","4":"1.0"},{"1":"`Errores de atención`","2":"2.297483e-02","3":"10","4":"1.0"},{"1":"`Correctas ejecutivas`","2":"2.231600e-02","3":"9","4":"0.9"},{"1":"`Fluidez (vegetales)`","2":"8.160019e-03","3":"9","4":"0.9"},{"1":"`Errores ejecutivos`","2":"4.645062e-03","3":"2","4":"0.2"},{"1":"`Memoria de trabajo`","2":"1.583571e-03","3":"6","4":"0.6"},{"1":"`Correctas atención`","2":"7.478632e-05","3":"3","4":"0.3"},{"1":"`Memoria inmediata`","2":"-2.532051e-03","3":"NA","4":"0.0"},{"1":"Atención","2":"-2.590218e-03","3":"1","4":"0.1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

``` r
#Si bien lasso y la importancia por permutaciones coinciden en varias variables, en otras no tanto. Veamos el grafico

library(ggplot2)
library(ggrepel)

ggplot(
  importance_tbl,
  aes(x = lasso_freq, y = delta_auc, label = var)
) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(
    size = 4,
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.3,
    min.segment.length = 0
  ) +
  geom_vline(xintercept = 0.5, linetype = 3, color = "grey50") +
  geom_hline(yintercept = median(importance_tbl$delta_auc, na.rm = TRUE),
             linetype = 3, color = "grey50") +
  scale_x_continuous(
    limits = c(0, 1.05),
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  labs(
    x = "Estabilidad LASSO (frecuencia de selección)",
    y = "Importancia predictiva (ΔAUC permutado)",
    title = "Importancia de variables: selección vs contribución predictiva",
    subtitle = "Comparación entre estabilidad bajo LASSO y pérdida de AUC al permutar"
  ) +
  theme_minimal(base_size = 14)
```

![](EEAA_files/figure-html/unnamed-chunk-44-1.png)<!-- -->

## Modelo logístico con regularización Ridge y reducido a 4 variables


``` r
#Entonces tengo: Variables imprescindibles, arriba a la derecha; variables prescindibles en todos los sentidos, abajo a la izquierda
#Pruebo sacar las variables prescindibles por AUC y vuelvo a calcular AUC: MEJORA un poco! a 0.696

vars2 <- c("MMSE", "Fluidez (animales)", "Funciones ejecutivas", "Nivel educativo")
auc_enet_ridge <- cv_auc_elastic(
  data       = data2_clean,
  outcome    = "clinical_diagnosis",
  predictors = vars2,
  alpha      = 0,
  v          = 10
)

auc_enet_ridge
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["model"],"name":[1],"type":["chr"],"align":["left"]},{"label":["auc_mean"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["auc_sd"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"Elastic Net (alpha=0)","2":"0.6945394","3":"0.09879081"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

``` r
#Grafico curva ROC

oof <- cv_oof_pred(
  data2_clean,
  "clinical_diagnosis",
  vars2,
  v = 10,
  seed = 123
)

roc_oof <- roc(
  response  = oof$y,
  predictor = oof$prob,
  quiet = TRUE
)

auc_val <- auc(roc_oof)

roc_df <- tibble(
  fpr = 1 - roc_oof$specificities,
  tpr = roc_oof$sensitivities
)

library(ggplot2)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(linewidth = 1.3, color = "#1f77b4") +
  geom_abline(
    intercept = 0, slope = 1,
    linetype = "dashed",
    color = "grey60"
  ) +
  coord_equal() +
  labs(
    title = "ROC descartando variables",
    subtitle = paste0("AUC = ", round(auc_val, 3)),
    x = "1 − Especificidad",
    y = "Sensibilidad"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
```

![](EEAA_files/figure-html/unnamed-chunk-46-1.png)<!-- -->


# Water Level Analysis – Deal Island & Farm Creek Marsh
This repository contains datasets and analysis code for studying water level patterns, soil saturation, and hydrological dynamics in the Deal Island and Farm Creek Marsh regions.
Preprint of this work can be found below: https://www.biorxiv.org/content/10.1101/2023.09.19.558260v2#:~:text=We%20found%20that%20both%20tidal,marsh%20interior%20500%20m%20away.
Author: Man Qi; Date: April 8, 2025

## 📁 Repository Structure
├── Data/         # Raw and processed datasets
├── Code/         # R scripts and analysis notebooks
├── README.md     # This file
-
## DATA OVERVIEW
-------------

This repository contains three main datasets located in the Data/ folder.

1. WATER LEVEL TIME SERIES

Filename                  | Site             | Period(s) Covered                                | Interval  | Notes
--------------------------|------------------|--------------------------------------------------|-----------|-----------------------
20574344_DI_G_E.xlsx      | DI_Dieback       | 2019/5/2–2020/3/4, 2020/10/7–2021/5/21           | 15 min    | -
20574343_DI_G_H.xlsx      | DI_Healthy       | 2019/5/2–2020/3/4, 2020/10/7–2021/3/5            | 15 min    | -
20292500_DI_G_P.xlsx      | DI_Pond          | 2019/5/2–2020/3/4, 2020/10/7–2020/10/19          | 15 min    | -
20138781_DI_G_TC.xlsx     | DI_Tidal_Creek   | 2019/5/2–2020/3/4, 2020/3/4–2020/9/29            | 15 min    | -
20574347_FCM_G_DC.xlsx    | FCM_Dieback      | 2019/5/2–2020/3/5, 2020/10/14–2021/5/28          | 15 min    | -
20574349_FCM_G_HC.xlsx    | FCM_Healthy      | 2019/5/2–2020/10/8, 2020/10/14–2021/5/28         | 15 min    | -
Site4S-FCM_G_PC.csv       | FCM_Pond         | 2017/11/6–2020/4/14                              | 15 min    | USGS-collected
bishop.csv                | Bishop Station   | 2019/5/2–2021/7/30                               | 1 hour    | -

Note: FCM pond data are collected by USGS. Water levels are recorded in feet, referenced to NAVD88.

2. WELL INSTALLATION INFO

File: Well info.txt

This file includes the following fields for each well:
- Well_ID
- S/N
- Well_top_to_logger (cm)
- Well_top_elevation_2019 (m)
- Well_length_above_soil (cm)
- Logger_depth_2019 (cm)
- Soil_surface_elevation_2019 (m)
- Note
- Well_top_elevation_2020 (m)
- Well_height_2020
- Logger_depth_2020 (cm)
- Elevation_2020 (m)

This information is used to calculate logger depth and surface elevation relative to a fixed benchmark.

3. FLOOD TOLERANCE DATA

File: flood tolerance extracted from papers.xls

Structure:
No. | Inundation | Value | Species           | Index                      | Site | Source
----|------------|-------|--------------------|-----------------------------|------|-----------------------------
1   | 7.9815     | -323  | J. roemerianus     | Aboveground biomass change | LOLA | Voss, Christian & Morris (2013)
2   | 24.2556    | -304  | J. roemerianus     | Aboveground biomass change | LOLA | Voss, Christian & Morris (2013)
3   | 48.9922    | -216  | J. roemerianus     | Aboveground biomass change | LOLA | Voss, Christian & Morris (2013)
4   | 52.7793    | -618  | J. roemerianus     | Aboveground biomass change | LOLA | Voss, Christian & Morris (2013)

This file compiles experimental results from literature and is used to assess species-specific responses to inundation stress.

## 💻 Code

The `Code/` folder contains all R scripts in the analysis.

Key files include:

- `Water level analysis.R` – Script to analyze hydrological regime and output figures
- `flood tolerance.R` – Script to plot the flood tolerance curves of marsh plants
- `03_generate_figures.R` – Generates plots for publication/report

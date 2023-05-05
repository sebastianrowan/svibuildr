---
title: 'svibuildr: an R package to download or construct Social Vulnerability Index datasets for the United States as well as single- or multi-state study regions'
tags: 
    - R
    - spatial
    - social vulnerability
    - geography
    - sociology
    - demographics
authors:
    - Sebastian Rowan
    - orcid: 0000-0002-0085-3906
    - affiliation: "1, 2"
affiliations:
    - Department of Civil and Environmental Engineering, University of New Hampshire, USA
      index: 1
    - ORISE Graduate Research Fellow, U.S. Army Corps of Engineers, Coastal and Hydraulics Laboratory, USA
      index: 2
date: 01 August 2023
bibliography: paper.bib
---

# Summary

`svibuildr` is an R package that allows users to download or construct Social Vulnerability Index datasets as tidyverse dataframes or as simple features dataframes for spatial analyses. `svibuildr` allows users to generate a multi-state SVI in which census tracts or counties are ranked against only other tracts or counties in the study region, rather than relying on national rankings for inter-state analyses. For single-state or nationwide analyses, the package can download pre-compiled datasets directly from the CDC's SVI website. 

# Statement of need

The Social Vulnerability Index (SVI), developed by the U.S. Center for Disease Control and Prevention Agency for Toxic Substance and Disease Registry (ATSDR), is an important tool in many disciplines, allowing researchers to use U.S. Census Bureau data to identify regions in the U.S. with populations that may be more vulnerable to natural hazards @flanagan_social_2011, emerging public health threats @fletcher_social_2021, or other health risks @gay_can_2016.
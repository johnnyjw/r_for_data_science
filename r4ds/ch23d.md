---
title: "Untitled"
author: "jjw"
date: "9 November 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# html widgets

```{r }
library(leaflet)
leaflet() %>% 
  setView(174.764, -36.877, zoom = 16) %>% 
  addTiles() %>% 
  addMarkers(174.764, -36.877, popup = "Maungawhau")
```

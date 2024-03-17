---
date: 2024-03-15
output: pdf_document
title: Assignment 1
---

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

First let's load the dataset and packages:

``` {r}
connections <- read.csv("/cloud/project/connections.csv")
attach(connections)

install.packages("tidyverse")
library(tidyverse)

install.packages("tidygraph")
library(tidygraph)

install.packages("igraph")
library(igraph)
```

We have to count the number of contacts that work at or are associated
to a current employer as well as the total number overall:

``` {r}
number_of_connections = connections %>%
  group_by(Company) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

total_num = nrow(connections)

number_of_connections
total_num
# I have 583 connections in total
```

We'll install ggraph to plot later on:

``` {r}
install.packages("ggraph")
library(ggraph) 
```

We remove our contacts' full last names and replace them with only the
first letter:

``` {r}
# Create a unique identifier for each contact
connections <- connections %>%
  mutate(contact_id = row_number(),
         label = paste(connections$First, substr(connections$Last, 1, 1), sep = " "))
```

We create our nodes for each contact and specify which companies I'm
affiliated to:

``` {r}
# Create nodes dataframe
nodes <- connections %>%
  select(contact_id, label, Company) %>%
  distinct()

# Create a dataframe to map companies to specific groups for adding edges to specified companies
specified_companies <- c("McGill University - Desautels Faculty of Management",
                         "McGill University", "MetaMusique", 
                         "Elite Transport Solutions", "Elite Transport Solutions inc")
```

We create the edges based on the contacts who are part of a company I am
or was associated to:

``` {r}
# Create edges based on company affiliation
edges <- connections %>%
  select(contact_id, Company) %>%
  filter(Company %in% specified_companies) %>%
  distinct() %>%
  # Dummy node for me with id 0 & connect to contacts from companies
  mutate(from = 0, to = contact_id) %>%
  select(from, to)

# Add a dummy node for yourself to the nodes dataframe
my_node <- data.frame(contact_id = 0, label = "Me", Company = NA)
nodes <- bind_rows(my_node, nodes)
```

We create internal edges and combine everything to set up the graph:

``` {r}
install.packages("purrr")
library(purrr)

# Create internal edges 
internal_edges <- connections %>%
  select(contact_id, Company) %>%
  distinct() %>%
  group_by(Company) %>%
  filter(n() > 1 & !Company %in% specified_companies) %>%
  summarise(contact_ids = list(contact_id), .groups = 'drop') %>%
  mutate(pairs = map(contact_ids, ~combn(.x, 2, simplify = FALSE))) %>%
  unnest(pairs) %>%
  mutate(from = map_chr(pairs, ~as.character(.x[1])),
         to = map_chr(pairs, ~as.character(.x[2]))) %>%
  select(from, to)

# Convert 'from' and 'to' columns to numeric as they were generated as characters
internal_edges <- internal_edges %>%
  mutate(from = as.numeric(from), to = as.numeric(to))

# Combine the specified company edges with internal company edges
edges <- bind_rows(edges, internal_edges)

# Now, all contacts referenced in edges should exist in nodes
# Recreate the graph with corrected data
network <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
```

We plot the graph:

``` {r}
# Visualize the network with ggraph (the optional step)
ggraph(network, layout = 'fr') +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = label), repel = TRUE) +
  theme_graph()
```

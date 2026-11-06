En este tutorial uso los datos de
[TidyTuesday](https://github.com/rfordatascience/tidytuesday) de la
semana del 3 de noviembre de 2026, que provienen de **Grambank**, una
base de datos de rasgos gramaticales de miles de lenguas del mundo. La
pregunta es sencilla: *¿puede un artículo concordar con el sustantivo en
género o clase nominal?* Y la respuesta la vamos a mirar por macroárea.

El resultado final combina tres piezas con `cowplot`: un mapa mundial
coloreado por macroárea, un gráfico de barras con el número de familias
lingüísticas y un gráfico de torta por macroárea.

## Librerías

``` r
pacman::p_load(tidyverse, sf, rnaturalearth, cowplot)
```

## Datos

Los tres archivos se leen directamente desde el repositorio de
TidyTuesday.

``` r
base_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-03/"

values_raw     <- read_csv(str_c(base_url, "grambank_values.csv"),     show_col_types = FALSE)
languages_raw  <- read_csv(str_c(base_url, "grambank_languages.csv"),  show_col_types = FALSE)
parameters_raw <- read_csv(str_c(base_url, "grambank_parameters.csv"), show_col_types = FALSE)
```

- `values_raw`: el valor de cada rasgo gramatical para cada lengua.
- `languages_raw`: la información de cada lengua (familia, macroárea,
  coordenadas).
- `parameters_raw`: la descripción de cada rasgo.

``` r
glimpse(values_raw)
```

    ## Rows: 362,025
    ## Columns: 4
    ## $ language_id  <chr> "abad1241", "abad1241", "abad1241", "abad1241", "abad1241…
    ## $ parameter_id <chr> "GB024", "GB025", "GB026", "GB027", "GB028", "GB030", "GB…
    ## $ value        <dbl> 2, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, …
    ## $ code_id      <chr> "GB024-2", "GB025-1", "GB026-0", "GB027-0", "GB028-1", "G…

``` r
glimpse(languages_raw)
```

    ## Rows: 2,467
    ## Columns: 8
    ## $ language_id   <chr> "abad1241", "abar1238", "abau1245", "abee1242", "aben124…
    ## $ language_name <chr> "Abadi", "Mungbam", "Abau", "Abé", "Abenlen Ayta", "Abip…
    ## $ glottocode    <chr> "abad1241", "abar1238", "abau1245", "abee1242", "aben124…
    ## $ macroarea     <chr> "Papunesia", "Africa", "Papunesia", "Africa", "Papunesia…
    ## $ latitude      <dbl> -9.033890, 6.580500, -3.972220, 5.596820, 15.413100, -29…
    ## $ longitude     <dbl> 146.99200, 10.22670, 141.32400, -4.38497, 120.20000, -61…
    ## $ family_name   <chr> "Austronesian", "Atlantic-Congo", "Sepik", "Atlantic-Con…
    ## $ lineage       <chr> "aust1307/mala1545/cent2237/east2712/ocea1241/west2818/p…

``` r
glimpse(parameters_raw)
```

    ## Rows: 195
    ## Columns: 5
    ## $ parameter_id        <chr> "GB020", "GB021", "GB022", "GB023", "GB024", "GB02…
    ## $ feature_name        <chr> "Are there definite or specific articles?", "Do in…
    ## $ feature_description <chr> "## Are there definite or specific articles?\r\n\r…
    ## $ word_order          <dbl> NA, NA, 0, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ locus_of_marking    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

## Elegir el rasgo

Busco los rasgos relacionados con el género:

``` r
parameters_raw |>
  filter(str_detect(feature_name, "gender")) |>
  select(feature_name)
```

    ## # A tibble: 14 × 1
    ##    feature_name                                                                 
    ##    <chr>                                                                        
    ##  1 Is there a gender distinction in independent 3rd person pronouns?            
    ##  2 Is there a gender/noun class system where sex is a factor in class assignmen…
    ##  3 Is there a gender/noun class system where shape is a factor in class assignm…
    ##  4 Is there a gender/noun class system where animacy is a factor in class assig…
    ##  5 Is there a gender/noun class system where plant status is a factor in class …
    ##  6 Can an adnominal property word agree with the noun in gender/noun class?     
    ##  7 Can an adnominal demonstrative agree with the noun in gender/noun class?     
    ##  8 Can an article agree with the noun in gender/noun class?                     
    ##  9 Can the verb carry a marker of animacy of argument, unrelated to any gender/…
    ## 10 Is there a gender system where a noun's phonological properties are a factor…
    ## 11 Can an adnominal numeral agree with the noun in gender/noun class?           
    ## 12 Can augmentative meaning be expressed productively by a shift of gender/noun…
    ## 13 Can diminutive meaning be expressed productively by a shift of gender/noun c…
    ## 14 Is there a large class of nouns whose gender/noun class is not phonologicall…

El que me interesa es **GB172**: *Can an article agree with the noun in
gender/noun class?*

``` r
values_raw |>
  filter(parameter_id == "GB172")
```

    ## # A tibble: 2,118 × 4
    ##    language_id parameter_id value code_id
    ##    <chr>       <chr>        <dbl> <chr>  
    ##  1 abad1241    GB172            0 GB172-0
    ##  2 abar1238    GB172            1 GB172-1
    ##  3 abee1242    GB172            0 GB172-0
    ##  4 aben1249    GB172            0 GB172-0
    ##  5 abkh1244    GB172            0 GB172-0
    ##  6 abua1245    GB172            1 GB172-1
    ##  7 abun1252    GB172            0 GB172-0
    ##  8 acha1249    GB172            0 GB172-0
    ##  9 ache1245    GB172            0 GB172-0
    ## 10 achi1256    GB172            0 GB172-0
    ## # ℹ 2,108 more rows

Lo uno a la tabla de lenguas para tener, en una sola tabla, la macroárea
y el valor del rasgo:

``` r
languages_gb172 <- languages_raw |>
  left_join((values_raw |>
               filter(parameter_id == "GB172") |>
               select(language_id, value)), by = "language_id")

languages_gb172 |>
  count(macroarea, value)
```

    ## # A tibble: 20 × 3
    ##    macroarea     value     n
    ##    <chr>         <dbl> <int>
    ##  1 Africa            0   405
    ##  2 Africa            1    87
    ##  3 Africa           NA    70
    ##  4 Australia         0   103
    ##  5 Australia         1     2
    ##  6 Australia        NA    38
    ##  7 Eurasia           0   502
    ##  8 Eurasia           1    31
    ##  9 Eurasia          NA    30
    ## 10 North America     0   218
    ## 11 North America     1    16
    ## 12 North America    NA     9
    ## 13 Papunesia         0   547
    ## 14 Papunesia         1    37
    ## 15 Papunesia        NA   144
    ## 16 South America     0   159
    ## 17 South America     1    10
    ## 18 South America    NA    55
    ## 19 <NA>              0     1
    ## 20 <NA>             NA     3

Y cuántas familias lingüísticas hay en cada macroárea:

``` r
languages_raw |>
  distinct(macroarea, family_name) |>
  count(macroarea)
```

    ## # A tibble: 7 × 2
    ##   macroarea         n
    ##   <chr>         <int>
    ## 1 Africa           36
    ## 2 Australia        21
    ## 3 Eurasia          27
    ## 4 North America    43
    ## 5 Papunesia        62
    ## 6 South America    42
    ## 7 <NA>              1

## Mapa base

### Países

Descargo los países con `rnaturalearth`, quito la Antártida y valido las
geometrías. Desactivo `s2` para trabajar con geometría plana y evitar
problemas con polígonos que cruzan el meridiano 180°.

``` r
sf_use_s2(FALSE)

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(admin != "Antarctica") |>
  st_make_valid() |>
  select(adm0_a3, admin)
```

### Asignar una macroárea a cada país

Grambank da coordenadas para cada lengua, no para cada país. Así que
convierto las lenguas en puntos y asigno a cada una el país más cercano:

``` r
pts <- languages_raw |>
  filter(!is.na(latitude), !is.na(longitude), !is.na(macroarea)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

pts <- pts |>
  st_join(world |> select(adm0_a3), join = st_nearest_feature)
```

Cada país recibe la macroárea más frecuente entre sus lenguas:

``` r
country_area <- pts |>
  st_drop_geometry() |>
  count(adm0_a3, macroarea) |>
  slice_max(n, by = adm0_a3, with_ties = FALSE)

map_df <- world |>
  left_join(country_area, by = "adm0_a3") |>
  mutate(imputed = is.na(macroarea))    # para saber cuáles se rellenaron
```

### Rellenar los países sin datos

Algunos países no tienen ninguna lengua en Grambank. A esos les asigno
la macroárea del país con datos más cercano:

``` r
with_data <- map_df |>
  filter(!imputed)

map_df <- map_df |>
  mutate(
    nearest_idx     = st_nearest_feature(geometry, with_data),
    nearest_country = if_else(imputed, with_data$admin[nearest_idx], NA_character_),
    macroarea       = coalesce(macroarea, with_data$macroarea[nearest_idx])
  ) |>
  select(-nearest_idx)
```

Siempre conviene revisar qué se imputó:

``` r
map_df |>
  st_drop_geometry() |>
  filter(imputed) |>
  select(admin, nearest_country, macroarea)
```

    ##                                  admin                  nearest_country
    ## 1                              Vatican                            Italy
    ## 2                              Uruguay                        Argentina
    ## 3         United States Virgin Islands                      Puerto Rico
    ## 4                       American Samoa                            Samoa
    ## 5        South Georgia and the Islands                        Argentina
    ## 6       British Indian Ocean Territory                        Sri Lanka
    ## 7                         Saint Helena                          Liberia
    ## 8                     Pitcairn Islands                 French Polynesia
    ## 9                             Anguilla                      Netherlands
    ## 10                    Falkland Islands                        Argentina
    ## 11                      Cayman Islands                          Jamaica
    ## 12                             Bermuda         United States of America
    ## 13              British Virgin Islands                      Puerto Rico
    ## 14            Turks and Caicos Islands                      The Bahamas
    ## 15                          Montserrat                           France
    ## 16                              Jersey                           France
    ## 17                            Guernsey                           France
    ## 18                         Isle of Man                   United Kingdom
    ## 19                United Arab Emirates                             Oman
    ## 20                        Turkmenistan                       Uzbekistan
    ## 21                 Trinidad and Tobago                        Venezuela
    ## 22                            eSwatini                     South Africa
    ## 23                          Somaliland                         Ethiopia
    ## 24                            Slovakia                          Ukraine
    ## 25                           Singapore                         Malaysia
    ## 26                          Seychelles                       Madagascar
    ## 27                  Republic of Serbia           Bosnia and Herzegovina
    ## 28               São Tomé and Principe                Equatorial Guinea
    ## 29                          San Marino                            Italy
    ## 30    Saint Vincent and the Grenadines                           France
    ## 31                         Saint Lucia                           France
    ## 32               Saint Kitts and Nevis                      Netherlands
    ## 33                              Rwanda Democratic Republic of the Congo
    ## 34                               Qatar                     Saudi Arabia
    ## 35                              Norway                          Finland
    ## 36                         North Korea                            China
    ## 37                               Aruba                        Venezuela
    ## 38                             Curaçao                      Netherlands
    ## 39                      Western Sahara                          Algeria
    ## 40                          Montenegro                          Albania
    ## 41                             Moldova                          Ukraine
    ## 42                              Monaco                           France
    ## 43                           Mauritius                           France
    ## 44                            Maldives                            India
    ## 45                          Luxembourg                           France
    ## 46                       Liechtenstein                      Switzerland
    ## 47                             Lesotho                     South Africa
    ## 48                          Kyrgyzstan                       Uzbekistan
    ## 49                              Kuwait                     Saudi Arabia
    ## 50                          Kazakhstan                            China
    ## 51                              Jordan                           Israel
    ## 52                           Palestine                           Israel
    ## 53                             Grenada                        Venezuela
    ## 54                             Germany                           France
    ## 55                              Gambia                          Senegal
    ## 56           Saint Pierre and Miquelon                           Canada
    ## 57                        Saint Martin                      Netherlands
    ## 58                    Saint Barthelemy                      Netherlands
    ## 59 French Southern and Antarctic Lands                     South Africa
    ## 60                               Aland                           Sweden
    ## 61                  Dominican Republic                            Haiti
    ## 62                            Djibouti                         Ethiopia
    ## 63                              Cyprus                  Northern Cyprus
    ## 64                                Cuba                      The Bahamas
    ## 65                             Croatia           Bosnia and Herzegovina
    ## 66                             Comoros                           France
    ## 67                         Macao S.A.R                            China
    ## 68                    Hong Kong S.A.R.                            China
    ## 69                          Cabo Verde                          Senegal
    ## 70                             Burundi Democratic Republic of the Congo
    ## 71                            Bulgaria                           Greece
    ## 72                             Belgium                           France
    ## 73                            Barbados                           France
    ## 74                             Austria                            Italy
    ## 75   Heard Island and McDonald Islands                     South Africa
    ## 76                      Norfolk Island                    New Caledonia
    ## 77         Ashmore and Cartier Islands                        Indonesia
    ## 78                 Antigua and Barbuda                           France
    ## 79                             Andorra                           France
    ## 80                     Siachen Glacier                            India
    ## 81                        Sint Maarten                      Netherlands
    ##        macroarea
    ## 1        Eurasia
    ## 2  South America
    ## 3  North America
    ## 4      Papunesia
    ## 5  South America
    ## 6        Eurasia
    ## 7         Africa
    ## 8      Papunesia
    ## 9        Eurasia
    ## 10 South America
    ## 11 North America
    ## 12 North America
    ## 13 North America
    ## 14 North America
    ## 15       Eurasia
    ## 16       Eurasia
    ## 17       Eurasia
    ## 18       Eurasia
    ## 19       Eurasia
    ## 20       Eurasia
    ## 21 South America
    ## 22        Africa
    ## 23        Africa
    ## 24       Eurasia
    ## 25     Papunesia
    ## 26        Africa
    ## 27       Eurasia
    ## 28        Africa
    ## 29       Eurasia
    ## 30       Eurasia
    ## 31       Eurasia
    ## 32       Eurasia
    ## 33        Africa
    ## 34       Eurasia
    ## 35       Eurasia
    ## 36       Eurasia
    ## 37 South America
    ## 38       Eurasia
    ## 39        Africa
    ## 40       Eurasia
    ## 41       Eurasia
    ## 42       Eurasia
    ## 43       Eurasia
    ## 44       Eurasia
    ## 45       Eurasia
    ## 46       Eurasia
    ## 47        Africa
    ## 48       Eurasia
    ## 49       Eurasia
    ## 50       Eurasia
    ## 51       Eurasia
    ## 52       Eurasia
    ## 53 South America
    ## 54       Eurasia
    ## 55        Africa
    ## 56 North America
    ## 57       Eurasia
    ## 58       Eurasia
    ## 59        Africa
    ## 60       Eurasia
    ## 61 North America
    ## 62        Africa
    ## 63       Eurasia
    ## 64 North America
    ## 65       Eurasia
    ## 66       Eurasia
    ## 67       Eurasia
    ## 68       Eurasia
    ## 69        Africa
    ## 70        Africa
    ## 71       Eurasia
    ## 72       Eurasia
    ## 73       Eurasia
    ## 74       Eurasia
    ## 75        Africa
    ## 76     Papunesia
    ## 77     Papunesia
    ## 78       Eurasia
    ## 79       Eurasia
    ## 80       Eurasia
    ## 81       Eurasia

### El mapa

Uso la proyección Robinson y un color por macroárea:

``` r
colors <- c("#388a06", "#41e2ba", "#f7e733", "#ff5964", "#38618c", "#58b6ff")

map_plot <- ggplot(map_df) +
  geom_sf(aes(fill = macroarea), alpha = 0.7, color = "#474A51", linewidth = 0.2,
          key_glyph = draw_key_point) +
  scale_fill_manual(values = colors, name = "Macroarea") +
  coord_sf(crs = "+proj=robin") +
  theme_void() +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 3, color = NA))) +
  theme(panel.background = element_rect(fill = "#fcfbfa"),
        legend.position = c(.1, .5),
        legend.background = element_rect(fill = "#fcfbfa", color = NA),
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 5))

map_plot
```

![](blog/figs/tidytuesday-grambank-map-1.png)<!-- -->

## Familias lingüísticas por macroárea

Un gráfico de barras pequeño, que irá como recuadro dentro del mapa:

``` r
inset <- languages_raw |>
  distinct(macroarea, family_name) |>
  count(macroarea) |>
  drop_na() |>
  ggplot(aes(x = reorder(macroarea, n), y = n, fill = macroarea)) +
  geom_col(alpha = 0.7, color = "#474A51", linewidth = 0.2) +
  scale_fill_manual(values = colors) +
  labs(x = "", y = "N° of languages") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 65, 15), limits = c(0, 62)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#fcfbfa"),
        panel.border = element_rect(colour = "#474A51", linewidth = .25),
        axis.text.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed", linewidth = .2),
        axis.text.y = element_text(size = 4),
        axis.title.y = element_text(size = 5),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 5),
        rect = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "#fcfbfa", color = NA))

inset
```

![](blog/figs/tidytuesday-grambank-inset-1.png)<!-- -->

## Gráficos de torta por macroárea

Primero calculo, para cada macroárea, la proporción de lenguas con
“Yes”, “No” o sin dato:

``` r
pie_data <- languages_gb172 |>
  filter(!is.na(macroarea)) |>
  mutate(value = factor(value, levels = c(1, 0), labels = c("Yes", "No")) |>
           fct_na_value_to_level("NA")) |>
  count(macroarea, value) |>
  mutate(pct = n / sum(n), .by = macroarea)

pie_data
```

    ## # A tibble: 18 × 4
    ##    macroarea     value     n    pct
    ##    <chr>         <fct> <int>  <dbl>
    ##  1 Africa        Yes      87 0.155 
    ##  2 Africa        No      405 0.721 
    ##  3 Africa        NA       70 0.125 
    ##  4 Australia     Yes       2 0.0140
    ##  5 Australia     No      103 0.720 
    ##  6 Australia     NA       38 0.266 
    ##  7 Eurasia       Yes      31 0.0551
    ##  8 Eurasia       No      502 0.892 
    ##  9 Eurasia       NA       30 0.0533
    ## 10 North America Yes      16 0.0658
    ## 11 North America No      218 0.897 
    ## 12 North America NA        9 0.0370
    ## 13 Papunesia     Yes      37 0.0508
    ## 14 Papunesia     No      547 0.751 
    ## 15 Papunesia     NA      144 0.198 
    ## 16 South America Yes      10 0.0446
    ## 17 South America No      159 0.710 
    ## 18 South America NA       55 0.246

Después hago una torta por macroárea, cada una con su color. La posición
de las etiquetas se ajusta a mano en algunas para que no se monten
cuando una porción es muy pequeña.

``` r
alpha <- .8

# Africa
africa_pie <- pie_data |>
  filter(macroarea == "Africa") |>
  ggplot(aes(x = 1, y = pct, fill = value)) +
  geom_col(alpha = alpha, width = 1, color = "#474A51", linewidth = 0.2) +
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.5), size = 1.2, color = "#474A51") +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(0.5, 1.7), expand = c(0, 0)) +
  scale_fill_manual(values = c("#388a06", "#aefa7f", "gray"), name = NULL) +
  theme_void() +
  theme(legend.position = "none")

# Australia
australia_pie <- pie_data |>
  filter(macroarea == "Australia") |>
  ggplot(aes(x = 1, y = pct, fill = value)) +
  geom_col(alpha = alpha, width = 1, color = "#474A51", linewidth = 0.2) +
  geom_text(aes(x = c(1.6, 1, 1), label = value),
            position = position_stack(vjust = 0.5), size = 1.2, color = "#474A51") +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(0.5, 1.7), expand = c(0, 0)) +
  scale_fill_manual(values = c("#41e2ba", "#96f2db", "gray"), name = NULL) +
  theme_void() +
  theme(legend.position = "none")

# Eurasia
eurasia_pie <- pie_data |>
  filter(macroarea == "Eurasia") |>
  ggplot(aes(x = 1, y = pct, fill = value)) +
  geom_col(alpha = alpha, width = 1, color = "#474A51", linewidth = 0.2) +
  geom_text(aes(x = c(1.6, 1, 1.6), y = c(0.972, 0.499, 0.0266), label = value),
            size = 1.2, color = "#474A51") +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(0.5, 1.7), expand = c(0, 0)) +
  scale_fill_manual(values = c("#f7e733", "#f2e488", "gray"), name = NULL) +
  theme_void() +
  theme(legend.position = "none")

# North America
NA_pie <- pie_data |>
  filter(macroarea == "North America") |>
  ggplot(aes(x = 1, y = pct, fill = value)) +
  geom_col(alpha = alpha, width = 1, color = "#474A51", linewidth = 0.2) +
  geom_text(aes(x = c(1.6, 1, 1.6), y = c(0.972, 0.499, 0.0266), label = value),
            size = 1.2, color = "#474A51") +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(0.5, 1.7), expand = c(0, 0)) +
  scale_fill_manual(values = c("#FF5964", "#ffa6ac", "gray"), name = NULL) +
  theme_void() +
  theme(legend.position = "none")

# Papunesia
papunesia_pie <- pie_data |>
  filter(macroarea == "Papunesia") |>
  ggplot(aes(x = 1, y = pct, fill = value)) +
  geom_col(alpha = alpha, width = 1, color = "#474A51", linewidth = 0.2) +
  geom_text(aes(x = c(1.6, 1, 1), y = c(0.972, 0.499, 0.1), label = value),
            size = 1.2, color = "#474A51") +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(0.5, 1.7), expand = c(0, 0)) +
  scale_fill_manual(values = c("#38618c", "#91afcf", "gray"), name = NULL) +
  theme_void() +
  theme(legend.position = "none")

# South America
SA_pie <- pie_data |>
  filter(macroarea == "South America") |>
  ggplot(aes(x = 1, y = pct, fill = value)) +
  geom_col(alpha = alpha, width = 1, color = "#474A51", linewidth = 0.2) +
  geom_text(aes(x = c(1.6, 1, 1), y = c(0.972, 0.499, 0.1), label = value),
            size = 1.2, color = "#474A51") +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(0.5, 1.7), expand = c(0, 0)) +
  scale_fill_manual(values = c("#58b6ff", "#98ccf5", "gray"), name = NULL) +
  theme_void() +
  theme(legend.position = "none")
```

## Todo junto

Con `cowplot::ggdraw()` coloco el mapa de fondo y encima el recuadro de
barras y las seis tortas, cada una cerca de su macroárea. Las posiciones
`x` e `y` van de 0 a 1 respecto al lienzo completo.

``` r
plot.with.inset <- ggdraw() +
  draw_plot(map_plot) +
  draw_plot(inset, x = 0.04, y = .08, width = .15, height = .25) +
  draw_plot(africa_pie, x = 0.4, y = .05, width = .2, height = .2) +
  draw_plot(australia_pie, x = 0.65, y = .07, width = .2, height = .2) +
  draw_plot(eurasia_pie, x = 0.8, y = .7, width = .2, height = .2) +
  draw_plot(NA_pie, x = 0.28, y = .52, width = .2, height = .2) +
  draw_plot(papunesia_pie, x = 0.76, y = .45, width = .2, height = .2) +
  draw_plot(SA_pie, x = 0.18, y = .15, width = .2, height = .2) +
  draw_label(label = "Can an article agree with the noun in gender/noun class?",
             x = 0.35, y = .98, color = "#474A51", size = 12, fontface = "bold")

plot.with.inset
```

![](blog/figs/tidytuesday-grambank-final-1.png)<!-- -->

Para guardarlo como imagen (18 × 8 cm):

``` r
ggsave(plot.with.inset, file = "plot.jpeg", width = 18, height = 8, units = "cm")
```

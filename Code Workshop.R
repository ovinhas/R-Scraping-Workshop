# Preparando o R ----------------------------------------------------------

# Estabelecer Diretorio de Trabalho
setwd("G:\\My Drive\\Workshop")


# Instalar Pacotes do R para auxilar o Scraping / Raspagem
'Pacote "rvest" - interage e extrai dados de websites' 
'Pacote "dplyr" - auxilia a trabalhar com dados no R'
'Pacote "httr" - requisita dados de APIs e webistes'

install.packages("rvest")
install.packages("dplyr")
install.packages("httr")


# Carregar Pacotes
'Carregar os pacotes do R que ja estao instalados no computador'

library(rvest)
library(dplyr)
library(httr)



# Checar Permissoes do Website --------------------------------------------
'Acessar o arquivo robots.txt do website para checar as permissoes de raspagem do website'

# Definir o link base do website
website <- "https://www.gov.br/secom/"

# Fazer Download do robots.txt file diretamente do website
robots_txt <- httr::content(httr::GET(paste0(website, "/robots.txt")), 
                            as = "text", encoding = "UTF-8")

# Salvar Arquivo no Diretorio de Trabalho
filename <- "robots_txt_11_11_24"
write(robots_txt, filename)



# Inspecionar Paginacao -----------------------------------------------------

### Usar o rvest para ler a URL base area paginada do website
'Para fazer isso, necessario identificar a area do website em que estao
paginados e armazenados os artigos / material a ser extraido'

url_base <- read_html("https://www.gov.br/secom/pt-br/fatos/brasil-contra-fake/noticias?b_start:int=0")

### Ler Elemento do Website que guarda o link para cada artigo
'Para identificar o elemento, utilizar o add-on SelectorGadget no Chrome'

url_base %>%
  html_nodes(".titulo a") %>%
  html_attr("href")
  

# Função para extrair links de uma página específica
'Configurar funcao para iterar com TODAS as paginas do website e
extrair os links de cada artigo armazenado nelas'

extrair_links <- function(url) {
  pagina <- read_html(url)
  links <- pagina %>%
    html_nodes(".titulo a") %>%
    html_attr("href")
  return(links)
}

# Definir a base da URL e o número máximo de páginas (modificar conforme necessário)
'Para identificar a URL Base aqui, eh necessario identificar o padrao de paginacao
exibido no link das paginas do website'

url_base <- "https://www.gov.br/secom/pt-br/fatos/brasil-contra-fake/noticias?b_start:int="
n_paginas <- 13 # número de páginas para percorrer (modificar conforme necessário)
incremento <- 30 # número total de páginas presente na paginacao

# Definir lista para armazenar todos os links
todos_links <- list()

# Loop para percorrer as páginas
'Comando para determinar o R para iteragir com todas as paginas determinadas acima'
for (i in seq(0, (n_paginas - 1) * incremento, by = incremento)) {
  url_pagina <- paste0(url_base, i)
  links_pagina <- extrair_links(url_pagina)
  todos_links <- c(todos_links, links_pagina)
  
  # Pedir ao R para pausar 3 segundos entre cada requisição
  'Essa e uma boa pratica para evitar bloqueios e sobrecarregar o website'
  Sys.sleep(3)
}

# Printar no Console todos os links extraídos
print(todos_links)






# Raspar Informacoes dos Links --------------------------------------------

### Inspecionar Paginas e Definir Padroes



### Funcao Scraping


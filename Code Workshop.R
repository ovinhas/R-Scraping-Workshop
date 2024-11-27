# Preparando o R ----------------------------------------------------------

# Estabelecer Diretorio de Trabalho
setwd("G:\\My Drive\\Workshop")


# Instalar Pacotes do R para auxilar o Scraping / Raspagem
'Pacote "rvest" - interage e extrai dados de websites' 
'Pacote "dplyr" - auxilia a trabalhar com dados no R'
'Pacote "httr" - requisita dados de APIs e websites'

install.packages("rvest")
install.packages("dplyr")
install.packages("httr")



# Carregar Pacotes
'Carregar os pacotes do R que ja estao instalados no computador'

library(rvest)
library(dplyr)
library(httr)
library(tibble)



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



# Extrair Links dos Artigos -----------------------------------------------


# Definir URL base e o número máximo de páginas 
'Para identificar a URL base, eh necessario identificar o padrao de paginacao
exibido no link das paginas do website'

url_base <- "https://www.gov.br/secom/pt-br/fatos/brasil-contra-fake/noticias?b_start:int="
n_paginas <- 13 # número de páginas para percorrer 
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


"Apos extrair os links, é importante checar manualmente se o número total de
URLs obtidas é igual à quantidade total de URLs exibidas pela paginação do website.

É possível que nem todos os links tenham sido extraídos por problemas no website."




# Raspar Informacoes dos Links --------------------------------------------


# Inspecionar Artigos e Definir Padroes de Coleta
"Passo de interagir com a estrutura do código html dos artigos usando o SelectorGadget.
O objetivo é identificar os nomes das diferentes partes que vamos extrair."


# Ler links dos artigos atraves do R
"Escolher artigos para inspecionar. É recomendável inspecionar páginas recentes
e antigas, uma vez que a estrutura pode variar conforme o tempo."


# Ler links de Teste
link_teste1 <- read_html("https://www.gov.br/secom/pt-br/fatos/brasil-contra-fake/noticias/2023/03/e-falso-que-texto-viral-anuncie-tarifas-de-pix-para-pessoas-fisicas")

link_teste2 <- read_html("https://www.gov.br/secom/pt-br/fatos/brasil-contra-fake/noticias/2024/11/exercito-brasileiro-nao-foi-retirado-da-amazonia")


### Inspeção do Link 1

# Titulo
link_teste1 %>% 
  html_nodes(".documentFirstHeading") %>% 
  html_text()

# Subtitulo
link_teste1 %>% 
  html_nodes(".documentDescription") %>% 
  html_text() %>% 
  gsub("\\\\", "", .) %>% # Remove barras invertidas
  gsub("\"", "", .) %>%   # Remove aspas duplas escapadas
  gsub("^\\[1\\] ", "", .) # Remove a numeração inicial do vetor (se aplicável)

"Adicionar o 'gsub' no código de coleta auxilia a limpar os dados (remover
caracteres desnecessários) por padrão. Porém, é recomendável limpar os dados
após a extração."

# Data
link_teste1 %>% 
  html_nodes(".documentPublished .value") %>% 
  html_text() %>% 
  gsub(" .*", "", .) %>% # Remove a parte do horário
  as.Date(format = "%d/%m/%Y") %>% # Converte para o formato de data
  format("%d-%m-%Y") # Transforma para o formato desejado

# Texto
link_teste1 %>% 
  html_nodes("#parent-fieldname-text p") %>% 
  html_text() %>% 
  gsub("\\\\", "", .) %>% # Remove barras invertidas
  gsub("\"", "", .) %>%   # Remove aspas duplas escapadas
  gsub("^\\[1\\] ", "", .) # Remove a numeração inicial do vetor (se aplicável)

# Imagem
link_teste1 %>% 
  html_nodes(".captioned") %>% 
  html_attr("src")

# Imagem 2
link_teste2 %>% 
  html_nodes("#media img") %>% 
  html_attr("src")

"Aqui, identificamos que os artigos usam diferentes padrões no código
para exibir imagens, então inspecionamos o Link de Teste 2."

# Hyperlinks
link_teste1 %>% 
  html_nodes("#parent-fieldname-text a") %>% 
  html_attr("href")

# Categoria
link_teste1 %>% 
  html_nodes("#form-widgets-categoria") %>% 
  html_text()

# Tags
link_teste1 %>% 
  html_nodes(".link-category") %>% 
  html_text()

# Sessao
link_teste1 %>% 
  html_nodes(".nitfSubtitle") %>% 
  html_text()



# Funcao Scraping
"Baseado nos parametros que definimos no passo acima, pedimos para o ChatGPT
escrever uma funcao de Scraping que

1) extraia 'in loop' dados de todas as URLs que coletamos~
2) contenha uma funcao 'sleep' para evitar bloqueios pelo website
3) que armazene os resultados em uma tabela"


# Função para scraping com suporte a múltiplas URLs
extrair_dados_multi <- function(urls) {
  # Cria uma lista vazia para armazenar os resultados
  resultados <- list()
  
  # Loop sobre cada URL no vetor
  for (i in seq_along(urls)) {
    url <- urls[i]
    cat("Extraindo dados da URL:", url, "\n") # Mensagem de progresso
    
    # Pausa de 3 segundos antes de processar a próxima URL
    Sys.sleep(3)
    
    # Ler o HTML da página
    pagina <- read_html(url)
    
    # Extração dos dados
    imagem <- pagina %>% 
      html_nodes(".captioned") %>% 
      html_attr("src")
    
    # Verifica se a imagem principal foi extraída; caso contrário, tenta "imagem2"
    if (length(imagem) == 0) {
      imagem <- pagina %>% 
        html_nodes("#media img") %>% 
        html_attr("src")
    }
    
    # Consolidar os dados em uma tibble
    dados <- tibble(
      url = url,
      titulo = pagina %>% 
        html_nodes(".documentFirstHeading") %>% 
        html_text(trim = TRUE) %>% 
        paste(collapse = " "),
      
      subtitulo = pagina %>% 
        html_nodes(".documentDescription") %>% 
        html_text(trim = TRUE) %>% 
        gsub("\\\\", "", .) %>% 
        gsub("\"", "", .) %>%   
        gsub("^\\[1\\] ", "", .) %>% 
        paste(collapse = " "),
      
      data = pagina %>% 
        html_nodes(".documentPublished .value") %>% 
        html_text(trim = TRUE) %>% 
        gsub(" .*", "", .) %>% 
        as.Date(format = "%d/%m/%Y") %>% 
        format("%d-%m-%Y"),
      
      texto = pagina %>% 
        html_nodes("#parent-fieldname-text p") %>% 
        html_text(trim = TRUE) %>% 
        gsub("\\\\", "", .) %>% 
        gsub("\"", "", .) %>%   
        gsub("^\\[1\\] ", "", .) %>% 
        paste(collapse = " "),
      
      imagem = paste(imagem, collapse = " "),
      
      hyperlinks = pagina %>% 
        html_nodes("#parent-fieldname-text a") %>% 
        html_attr("href") %>% 
        paste(collapse = " "),
      
      categoria = pagina %>% 
        html_nodes("#form-widgets-categoria") %>% 
        html_text(trim = TRUE) %>% 
        paste(collapse = " "),
      
      tags = pagina %>% 
        html_nodes(".link-category") %>% 
        html_text(trim = TRUE) %>% 
        paste(collapse = ", "),
      
      sessao = pagina %>% 
        html_nodes(".nitfSubtitle") %>% 
        html_text(trim = TRUE) %>% 
        paste(collapse = " ")
    )
    
    # Armazena os dados na lista de resultados
    resultados[[i]] <- dados
  }
  
  # Combina todas as tibbles em uma única tabela
  resultados_tabela <- bind_rows(resultados)
  
  return(resultados_tabela)
}




#Executar Scraping
"Antes de executar a coleta, criamos um sample dos links coletados para teste."

# Criar lista de 5 links aleatorios do BrasilContraFake
urls_teste <- sample(todos_links, 10)


# Deslistar objeto com os 5 links
"É necessário remover o formato 'lista' dos links para a função de coleta
conseguir funcionar e iteragir entre os links."

urls_teste_deslistados <- unlist(urls_teste)


# Testar extracao dos dados com as URLs teste deslistadas 
dados_extraidos_teste <- extrair_dados_multi(urls_teste_deslistados)


# Exibir os dados
"É sempre importante verificar se o teste funcionou e corrigir possíveis erros
na coleta. Geralmente os erros são resolvidos ajustando os identificadores
para realizar o scraping, alterando a função Scraping"
print(dados_extraidos)



### Extrair dados de TODOS os links
"Uma vez tendo funcionado, vamos agora deslistar a lista de TODOS os
links de artigos que coletamos e extrair os dados com a função de 
Scraping que criamos"

todos_links_deslistados <- unlist(todos_links)

# Exibir resultados
"Os resultados vão estar em uma tabela que pode ser acessada
clicando no nome dela no Global Environment do R."
resultados <- extrair_dados_multi(todos_links_deslistados)


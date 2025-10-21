# ============================================================
# Etiquetado automático de tópicos (k) para un LDA (topicmodels)
# usando un modelo local de Ollama (p. ej., "qwen2.5:latest" o "llama3.1:latest").
# Salida principal: "label_sentence" (una sola frase natural por tópico),
# más "confidence", "rationale" y los "top_terms" usados.
# ============================================================

# ---- Paquetes ----
library(topicmodels)
library(dplyr)
library(purrr)
library(stringr)
library(httr2)
library(jsonlite)
library(glue)
library(tidyr)

# ---- Auxiliares ----
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

sanitize_sentence <- function(s) {
  if (is.null(s) || length(s) == 0 || is.na(s) || !nzchar(s)) return(NA_character_)
  s <- str_squish(as.character(s))
  # Capitaliza primera letra y quita espacios antes de signos.
  s <- paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
  s <- str_replace_all(s, "\\s+([,.;:!?])", "\\1")
  # Limita a ~160 chars por seguridad
  s <- str_sub(s, 1, 160)
  s
}

# ------------------------------------------------------------
# get_topic_context()
# ------------------------------------------------------------
get_topic_context <- function(lda_fit, docs_df, top_n_terms = 10, top_n_docs = 10) {
  stopifnot(all(c("doc_id","text") %in% names(docs_df)))
  
  term_mat <- terms(lda_fit, top_n_terms)
  term_df <- map_dfr(seq_len(nrow(term_mat)), function(k) {
    tibble(topic = k, top_terms = paste(term_mat[k, ], collapse = ", "))
  })
  
  post  <- posterior(lda_fit)
  gamma <- post$topics
  stopifnot(nrow(gamma) == nrow(docs_df))
  
  top_docs_df <- map_dfr(seq_len(ncol(gamma)), function(k) {
    idx <- order(gamma[, k], decreasing = TRUE)[seq_len(min(top_n_docs, nrow(gamma)))]
    tibble(
      topic   = k,
      doc_id  = docs_df$doc_id[idx],
      gamma   = gamma[idx, k],
      title   = if ("title" %in% names(docs_df)) docs_df$title[idx] else NA_character_,
      snippet = docs_df$text[idx] %>% str_squish() %>% str_sub(1, 240)
    ) %>%
      mutate(snippet = paste0(snippet, ifelse(nchar(snippet) == 240, "…", "")))
  })
  
  list(top_terms = term_df, top_docs = top_docs_df)
}

# ------------------------------------------------------------
# build_prompt(): ahora pide UNA frase ("label_sentence")
# ------------------------------------------------------------
build_prompt <- function(k, terms_chr, docs_tbl) {
  ejemplos <- docs_tbl %>%
    mutate(item = if (!all(is.na(title))) {
      paste0("• Título: ", title %||% "(s/título)", " | Extracto: ", snippet)
    } else {
      paste0("• Extracto: ", snippet)
    }) %>%
    pull(item) %>%
    paste(collapse = "\n")
  
  glue(
    "Actúa como un experto en ciencia de datos y periodismo de datos.
Tarea: generar UNA etiqueta en forma de FRASE natural (en español) que resuma el tema del siguiente TÓPICO de un modelo LDA.

REQUISITOS PARA LA ETIQUETA:
- Debe ser una sola oración, clara y natural (≈ 6–14 palabras).
- Debe sonar como título descriptivo periodístico (no telegráfico de 2-3 palabras).
- No uses comillas, corchetes, ni código; evita listar palabras sueltas.

FORMATO DE SALIDA (JSON, una sola línea, sin texto extra):
{{
  \"topic\": {k},
  \"label_sentence\": \"(una oración clara de 6–14 palabras)\",
  \"confidence\": 0.0-1.0,
  \"rationale\": \"(1-2 frases muy breves que expliquen por qué esa etiqueta)\"
}}

PALABRAS PRINCIPALES (del tópico):
{terms_chr}

ARTÍCULOS REPRESENTATIVOS:
{ejemplos}

Responde ÚNICAMENTE con el JSON indicado (una línea)."
  )
}

# ------------------------------------------------------------
# Cliente Ollama
# ------------------------------------------------------------
ollama_generate <- function(prompt,
                            model = "qwen2.5:latest",
                            temperature = 0.2,
                            num_ctx = 4096,
                            timeout_sec = 120,
                            max_retries = 2) {
  req <- request("http://localhost:11434/api/generate") %>%
    req_body_json(list(
      model  = model,      # <-- Asegúrate de pasar un STRING, p.ej. "llama3.1:latest"
      prompt = prompt,
      stream = FALSE,
      options = list(
        temperature = temperature,
        num_ctx     = num_ctx
      )
    )) %>%
    req_timeout(timeout_sec)
  
  for (i in seq_len(max_retries + 1)) {
    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(resp, "response")) {
      out <- resp_body_json(resp, simplifyVector = TRUE)
      return(out$response %||% out)
    } else if (i <= max_retries) {
      Sys.sleep(1.5 * i)
    } else {
      stop("Fallo al consultar Ollama: ", conditionMessage(resp))
    }
  }
}

# ------------------------------------------------------------
# Parser robusto del JSON del LLM
# ------------------------------------------------------------
parse_llm_json <- function(txt) {
  clean <- txt %>%
    str_replace_all("^[`]+json\\s*|^[`]+\\s*|[`]+$", "") %>%
    str_squish()
  
  res <- suppressWarnings(tryCatch(jsonlite::fromJSON(clean), error = function(e) NULL))
  if (is.null(res)) {
    m <- str_match(clean, "\\{.*\\}")
    if (!is.na(m[1])) {
      res <- suppressWarnings(tryCatch(jsonlite::fromJSON(m[1]), error = function(e) NULL))
    }
  }
  res
}

# ------------------------------------------------------------
# Fallback heurístico: construir UNA frase
# ------------------------------------------------------------
heuristic_sentence <- function(terms_chr, docs_tbl) {
  base_terms <- str_split(terms_chr, "\\s*,\\s*")[[1]]
  # arma un pequeño resumen con 2-3 términos top
  text_blob <- paste(
    (if ("title" %in% names(docs_tbl)) paste(docs_tbl$title, collapse = " ") else ""),
    paste(docs_tbl$snippet, collapse = " "),
    collapse = " "
  ) %>% tolower()
  
  counts <- tibble(term = base_terms) %>%
    mutate(n = str_count(text_blob, fixed(tolower(term))))
  
  top_terms <- counts %>% arrange(desc(n), term) %>% slice_head(n = 3) %>% pull(term)
  if (length(top_terms) == 0) top_terms <- head(base_terms, 3)
  
  # construye frase simple
  raw <- paste(
    "Temática sobre",
    paste(top_terms, collapse = ", "),
    "con enfoque en los artículos representativos"
  )
  tibble(
    label_sentence = sanitize_sentence(raw),
    confidence = 0.35,
    rationale  = "Frase heurística a partir de términos frecuentes y snippets."
  )
}

# ------------------------------------------------------------
# label_topics_with_ollama(): salida principal = label_sentence
# ------------------------------------------------------------
label_topics_with_ollama <- function(lda_fit,
                                     docs_df,
                                     model = "qwen2.5:latest",
                                     language = "es",
                                     top_n_terms = 10,
                                     top_n_docs  = 10,
                                     temperature = 0.2,
                                     parallel = FALSE) {
  k <- lda_fit@k
  ctx <- get_topic_context(lda_fit, docs_df, top_n_terms, top_n_docs)
  
  per_topic <- map_dfr(seq_len(k), function(tk) {
    terms_chr <- ctx$top_terms %>%
      dplyr::filter(topic == tk) %>%
      dplyr::pull(top_terms) %>%
      .[[1]]
    
    docs_tbl <- ctx$top_docs %>%
      dplyr::filter(topic == tk)
    
    prompt <- build_prompt(k = tk, terms_chr = terms_chr, docs_tbl = docs_tbl)
    
    llm_txt <- tryCatch(
      ollama_generate(prompt, model = model, temperature = temperature),
      error = function(e) NA_character_
    )
    
    parsed <- if (!is.na(llm_txt)) parse_llm_json(llm_txt) else NULL
    
    if (!is.null(parsed)) {
      lab <- parsed$label_sentence %||% parsed$label_long %||% NA_character_
      tibble(
        topic          = tk,
        label_sentence = sanitize_sentence(lab),
        confidence     = as.numeric(parsed$confidence %||% NA_real_),
        rationale      = parsed$rationale %||% NA_character_,
        top_terms      = terms_chr
      )
    } else {
      h <- heuristic_sentence(terms_chr, docs_tbl)
      tibble(
        topic          = tk,
        label_sentence = h$label_sentence,
        confidence     = h$confidence,
        rationale      = h$rationale,
        top_terms      = terms_chr
      )
    }
  })
  
  per_topic %>% arrange(topic)
}

# ===================== EJEMPLO DE USO =======================
# Asegúrate SIEMPRE de pasar el nombre del modelo como STRING.
# modelo_ollama <- "llama3.1:latest"  # o "qwen2.5:latest", etc.
# etiquetas <- label_topics_with_ollama(
#   lda_fit,
#   docs_df,
#   model       = modelo_ollama,
#   top_n_terms = 10,
#   top_n_docs  = 10,
#   temperature = 0.2
# )
# View(etiquetas)
# readr::write_csv(etiquetas, "etiquetas_topicos.csv")

# install.packages(c("httr2", "jsonlite"))
library(httr2)

# ---- Report metadata ---------------------------------------------------------

.rapport_specs <- function() {
  list(
    individ = list(
      path = "api/v1/rapportgenerator/individ",
      params = c(
        "Art", "Fylke", "Jaktfelt", "Vald", "Kommune",
        "Villreinvald", "Villreinområde", "FraDato", "TilDato",
        "Sesong", "BrukerEpost", "Jegernummer", "Merkelapp",
        "Godkjent", "Rapporteringskilde", "Terreng",
        "pageSize", "page", "Kolonner"
      ),
      required = character(0),
      enums = list(
        Art = c("Elg", "Hjort", "Rådyr", "Villrein"),
        Godkjent = c("Ja", "Nei"),
        Terreng = c("Innmark", "Utmark", "Ukjent")
      )
    ),

    fallvilt = list(
      path = "api/v1/rapportgenerator/fallvilt",
      params = c(
        "Fylke", "Kommune", "FraDato", "TilDato",
        "FraTidspunkt", "TilTidspunkt", "SistEndret",
        "Art", "Kjønn", "Alder", "Årsaker", "Utfall",
        "pageSize", "page", "Kolonner"
      ),
      required = character(0),
      enums = list(
        Art = c("Elg", "Hjort", "Rådyr", "Villrein", "Havørn", "Kongeørn",
                "Villsvin", "Oter", "Grevling", "Rev", "Hjortedyr"),
        Kjønn = c("Hunn", "Hann", "Ukjent"),
        Alder = c("Kalv", "Ungdyr", "Voksen", "Ukjent"),
        Årsaker = c("PåkjørtAvMotorkjøretøy", "PåkjørtAvTog", "SykdomOgSkade",
                    "FeltAvSikkerhetshensyn", "FeltSomSkadedyr",
                    "FeltUlovlig", "AndreÅrsaker"),
        Utfall = c("DødtPåStedet", "AvlivetPåStedet", "AvlivetEtterEttersøk",
                   "FunnetDødEtterEttersøk", "ObservertAntattUskadd",
                   "EttersøkAvsluttetIkkefunnet", "UkjentUtfall")
      )
    ),

    jaktdag = list(
      path = "api/v1/rapportgenerator/jaktdag",
      params = c(
        "Art", "Terreng", "Godkjent", "Fylke", "Kommune",
        "Bpo", "Vald", "Jaktfelt", "FraDato", "TilDato", "Sesong",
        "BrukerEpost", "Jegernummer", "Rapporteringskilde",
        "pageSize", "page", "Kolonner"
      ),
      required = c("Art"),
      enums = list(
        Art = c("Elg", "Hjort"),
        Terreng = c("Innmark", "Utmark")
      )
    ),

    telledata = list(
      path = "api/v1/rapportgenerator/telledata",
      params = c(
        "villreinomrade", "tellemetode", "fraAr", "tilAr",
        "pageSize", "page", "Kolonner"
      ),
      required = character(0),
      enums = list()
    ),

    omrade = list(
      subtypes = list(
        bestandsplanomrade = list(
          path = "api/v1/rapportgenerator/omrade/bestandsplanomrade",
          params = c("Art", "Bpo", "Vald", "Fylke", "Kommune",
                     "pageSize", "page", "Kolonner"),
          required = character(0),
          enums = list(Art = c("Elg", "Hjort"))
        ),

        jaktfelt = list(
          path = "api/v1/rapportgenerator/omrade/jaktfelt",
          params = c("Art", "Fylke", "Kommune", "Bpo", "Vald", "Jaktfelt",
                     "pageSize", "page", "Kolonner"),
          required = character(0),
          enums = list(Art = c("Elg", "Hjort"))
        ),

        vald = list(
          path = "api/v1/rapportgenerator/omrade/vald",
          params = c("Art", "Fylke", "Kommune", "Bpo", "Vald",
                     "pageSize", "page", "Kolonner"),
          required = character(0),
          enums = list(Art = c("Elg", "Hjort", "Rådyr"))
        ),

        villreinomrade = list(
          path = "api/v1/rapportgenerator/omrade/villreinomrade",
          params = c("Villreinnemnd", "Villreinområde", "Villreinvald",
                     "pageSize", "page", "Kolonner"),
          required = character(0),
          enums = list()
        ),

        villreinvald = list(
          path = "api/v1/rapportgenerator/omrade/villreinvald",
          params = c("Villreinnemnd", "Villreinområde", "Villreinvald",
                     "pageSize", "page", "Kolonner"),
          required = character(0),
          enums = list()
        )
      )
    )
  )
}

# ---- Helpers ----------------------------------------------------------------

.is_empty_value <- function(x) {
  is.null(x) ||
    length(x) == 0 ||
    (length(x) == 1 && is.na(x)) ||
    (is.character(x) && length(x) == 1 && trimws(x) == "")
}

.normalize_param_value <- function(x) {
  if (is.factor(x)) x <- as.character(x)

  if (length(x) > 1) {
    return(paste(as.character(x), collapse = ","))
  }

  if (is.logical(x) && length(x) == 1 && !is.na(x)) {
    return(tolower(as.character(x)))
  }

  x
}

.split_csv_values <- function(x) {
  trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])
}

.validate_date <- function(x, nm) {
  ok <- grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(x))
  if (!ok) stop(sprintf("'%s' must be in format YYYY-MM-DD.", nm), call. = FALSE)
}

.validate_time <- function(x, nm) {
  ok <- grepl("^\\d{2}:\\d{2}:\\d{2}$", as.character(x))
  if (!ok) stop(sprintf("'%s' must be in format HH:mm:ss.", nm), call. = FALSE)
}

.validate_datetime <- function(x, nm) {
  ok <- grepl("^\\d{4}-\\d{2}-\\d{2}[ T]\\d{2}:\\d{2}:\\d{2}$", as.character(x))
  if (!ok) {
    stop(sprintf("'%s' must be in format YYYY-MM-DD HH:mm:ss.", nm), call. = FALSE)
  }
}

.validate_integer_scalar <- function(x, nm, min_value = NULL, max_value = NULL) {
  if (length(x) != 1 || is.na(suppressWarnings(as.integer(x)))) {
    stop(sprintf("'%s' must be a single integer.", nm), call. = FALSE)
  }
  xi <- as.integer(x)
  if (!is.null(min_value) && xi < min_value) {
    stop(sprintf("'%s' must be >= %s.", nm, min_value), call. = FALSE)
  }
  if (!is.null(max_value) && xi > max_value) {
    stop(sprintf("'%s' must be <= %s.", nm, max_value), call. = FALSE)
  }
}

.validate_enum <- function(x, nm, allowed) {
  values <- .split_csv_values(x)
  bad <- setdiff(values, allowed)
  if (length(bad) > 0) {
    stop(
      sprintf(
        "Invalid value(s) for '%s': %s. Allowed values: %s",
        nm,
        paste(bad, collapse = ", "),
        paste(allowed, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

.get_report_spec <- function(rapport, omrade = NULL) {
  specs <- .rapport_specs()
  rapport <- tolower(rapport)

  if (!rapport %in% names(specs)) {
    stop(
      sprintf(
        "Unknown rapport '%s'. Allowed: %s",
        rapport, paste(names(specs), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (rapport != "omrade") {
    return(specs[[rapport]])
  }

  if (is.null(omrade) || !nzchar(omrade)) {
    stop(
      "For rapport = 'omrade', you must provide omrade = one of: bestandsplanomrade, jaktfelt, vald, villreinomrade, villreinvald.",
      call. = FALSE
    )
  }

  omrade <- tolower(omrade)

  if (!omrade %in% names(specs$omrade$subtypes)) {
    stop(
      sprintf(
        "Unknown omrade '%s'. Allowed: %s",
        omrade, paste(names(specs$omrade$subtypes), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  specs$omrade$subtypes[[omrade]]
}

.validate_params <- function(params, spec, rapport, omrade = NULL) {
  if (!is.list(params)) {
    stop("'params' must be a named list.", call. = FALSE)
  }
  if (is.null(names(params)) || any(names(params) == "")) {
    stop("'params' must be a named list.", call. = FALSE)
  }

  invalid_names <- setdiff(names(params), spec$params)
  if (length(invalid_names) > 0) {
    where <- if (tolower(rapport) == "omrade") {
      sprintf("rapport='%s', omrade='%s'", rapport, omrade)
    } else {
      sprintf("rapport='%s'", rapport)
    }

    stop(
      sprintf(
        "Invalid parameter(s) for %s: %s\nAllowed parameters: %s",
        where,
        paste(invalid_names, collapse = ", "),
        paste(spec$params, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  missing_required <- setdiff(spec$required, names(params))
  if (length(missing_required) > 0) {
    stop(
      sprintf(
        "Missing required parameter(s): %s",
        paste(missing_required, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Generic checks
  if ("pageSize" %in% names(params)) .validate_integer_scalar(params$pageSize, "pageSize", 1, 10000)
  if ("page" %in% names(params)) .validate_integer_scalar(params$page, "page", 1, Inf)
  if ("Jegernummer" %in% names(params)) .validate_integer_scalar(params$Jegernummer, "Jegernummer", 1, Inf)
  if ("fraAr" %in% names(params)) .validate_integer_scalar(params$fraAr, "fraAr")
  if ("tilAr" %in% names(params)) .validate_integer_scalar(params$tilAr, "tilAr")

  if ("FraDato" %in% names(params)) .validate_date(params$FraDato, "FraDato")
  if ("TilDato" %in% names(params)) .validate_date(params$TilDato, "TilDato")
  if ("FraTidspunkt" %in% names(params)) .validate_time(params$FraTidspunkt, "FraTidspunkt")
  if ("TilTidspunkt" %in% names(params)) .validate_time(params$TilTidspunkt, "TilTidspunkt")
  if ("SistEndret" %in% names(params)) .validate_datetime(params$SistEndret, "SistEndret")

  # Enum checks
  if (length(spec$enums) > 0) {
    for (nm in names(spec$enums)) {
      if (nm %in% names(params)) {
        .validate_enum(params[[nm]], nm, spec$enums[[nm]])
      }
    }
  }

  # Special rule: jaktdag
  if (tolower(rapport) == "jaktdag") {
    has_sesong <- "Sesong" %in% names(params)
    has_date <- any(c("FraDato", "TilDato") %in% names(params))

    if (has_sesong && has_date) {
      stop(
        "For rapport='jaktdag', 'Sesong' cannot be combined with 'FraDato'/'TilDato'.",
        call. = FALSE
      )
    }

    if ("Godkjent" %in% names(params)) {
      ok <- is.logical(params$Godkjent) ||
        (is.character(params$Godkjent) && tolower(params$Godkjent) %in% c("true", "false"))
      if (!ok) {
        stop(
          "For rapport='jaktdag', 'Godkjent' must be TRUE/FALSE or 'true'/'false'.",
          call. = FALSE
        )
      }
    }
  }

  invisible(TRUE)
}

.rapport_error_body <- function(resp) {
  txt <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  if (!nzchar(txt)) {
    return(NULL)
  }

  body <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(body)) {
    return(txt)
  }

  msg <- character(0)

  if (!is.null(body$title) && nzchar(body$title)) {
    msg <- c(msg, body$title)
  }

  if (!is.null(body$errors) && is.list(body$errors)) {
    for (nm in names(body$errors)) {
      vals <- unlist(body$errors[[nm]], use.names = FALSE)
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals) > 0) {
        msg <- c(msg, sprintf("%s: %s", nm, paste(vals, collapse = " ")))
      }
    }
  }

  if (!is.null(body$detail) && nzchar(body$detail)) {
    msg <- c(msg, body$detail)
  }

  if (!is.null(body$message) && nzchar(body$message)) {
    msg <- c(msg, body$message)
  }

  if (length(msg) == 0) {
    return(txt)
  }

  unique(msg)
}

# ---- Helpers ----------------------------------------------------------------

.is_empty_value <- function(x) {
  is.null(x) ||
    length(x) == 0 ||
    (length(x) == 1 && is.na(x)) ||
    (is.character(x) && length(x) == 1 && trimws(x) == "")
}

.normalize_param_value <- function(x) {
  if (is.factor(x)) x <- as.character(x)

  if (length(x) > 1) {
    return(paste(as.character(x), collapse = ","))
  }

  if (is.logical(x) && length(x) == 1 && !is.na(x)) {
    return(tolower(as.character(x)))
  }

  x
}

.split_csv_values <- function(x) {
  trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])
}

.validate_date <- function(x, nm) {
  ok <- grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(x))
  if (!ok) stop(sprintf("'%s' must be in format YYYY-MM-DD.", nm), call. = FALSE)
}

.validate_time <- function(x, nm) {
  ok <- grepl("^\\d{2}:\\d{2}:\\d{2}$", as.character(x))
  if (!ok) stop(sprintf("'%s' must be in format HH:mm:ss.", nm), call. = FALSE)
}

.validate_datetime <- function(x, nm) {
  ok <- grepl("^\\d{4}-\\d{2}-\\d{2}[ T]\\d{2}:\\d{2}:\\d{2}$", as.character(x))
  if (!ok) {
    stop(sprintf("'%s' must be in format YYYY-MM-DD HH:mm:ss.", nm), call. = FALSE)
  }
}

.validate_integer_scalar <- function(x, nm, min_value = NULL, max_value = NULL) {
  if (length(x) != 1 || is.na(suppressWarnings(as.integer(x)))) {
    stop(sprintf("'%s' must be a single integer.", nm), call. = FALSE)
  }
  xi <- as.integer(x)
  if (!is.null(min_value) && xi < min_value) {
    stop(sprintf("'%s' must be >= %s.", nm, min_value), call. = FALSE)
  }
  if (!is.null(max_value) && xi > max_value) {
    stop(sprintf("'%s' must be <= %s.", nm, max_value), call. = FALSE)
  }
}

.validate_enum <- function(x, nm, allowed) {
  values <- .split_csv_values(x)
  bad <- setdiff(values, allowed)
  if (length(bad) > 0) {
    stop(
      sprintf(
        "Invalid value(s) for '%s': %s. Allowed values: %s",
        nm,
        paste(bad, collapse = ", "),
        paste(allowed, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

.get_report_spec <- function(rapport, omrade = NULL) {
  specs <- .rapport_specs()
  rapport <- tolower(rapport)

  if (!rapport %in% names(specs)) {
    stop(
      sprintf(
        "Unknown rapport '%s'. Allowed: %s",
        rapport, paste(names(specs), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (rapport != "omrade") {
    return(specs[[rapport]])
  }

  if (is.null(omrade) || !nzchar(omrade)) {
    stop(
      "For rapport = 'omrade', you must provide omrade = one of: bestandsplanomrade, jaktfelt, vald, villreinomrade, villreinvald.",
      call. = FALSE
    )
  }

  omrade <- tolower(omrade)

  if (!omrade %in% names(specs$omrade$subtypes)) {
    stop(
      sprintf(
        "Unknown omrade '%s'. Allowed: %s",
        omrade, paste(names(specs$omrade$subtypes), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  specs$omrade$subtypes[[omrade]]
}

.validate_params <- function(params, spec, rapport, omrade = NULL) {
  if (!is.list(params)) {
    stop("'params' must be a named list.", call. = FALSE)
  }
  if (is.null(names(params)) || any(names(params) == "")) {
    stop("'params' must be a named list.", call. = FALSE)
  }

  invalid_names <- setdiff(names(params), spec$params)
  if (length(invalid_names) > 0) {
    where <- if (tolower(rapport) == "omrade") {
      sprintf("rapport='%s', omrade='%s'", rapport, omrade)
    } else {
      sprintf("rapport='%s'", rapport)
    }

    stop(
      sprintf(
        "Invalid parameter(s) for %s: %s\nAllowed parameters: %s",
        where,
        paste(invalid_names, collapse = ", "),
        paste(spec$params, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  missing_required <- setdiff(spec$required, names(params))
  if (length(missing_required) > 0) {
    stop(
      sprintf(
        "Missing required parameter(s): %s",
        paste(missing_required, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if ("pageSize" %in% names(params)) .validate_integer_scalar(params$pageSize, "pageSize", 1, 10000)
  if ("page" %in% names(params)) .validate_integer_scalar(params$page, "page", 1, Inf)
  if ("Jegernummer" %in% names(params)) .validate_integer_scalar(params$Jegernummer, "Jegernummer", 1, Inf)
  if ("fraAr" %in% names(params)) .validate_integer_scalar(params$fraAr, "fraAr")
  if ("tilAr" %in% names(params)) .validate_integer_scalar(params$tilAr, "tilAr")

  if ("FraDato" %in% names(params)) .validate_date(params$FraDato, "FraDato")
  if ("TilDato" %in% names(params)) .validate_date(params$TilDato, "TilDato")
  if ("FraTidspunkt" %in% names(params)) .validate_time(params$FraTidspunkt, "FraTidspunkt")
  if ("TilTidspunkt" %in% names(params)) .validate_time(params$TilTidspunkt, "TilTidspunkt")
  if ("SistEndret" %in% names(params)) .validate_datetime(params$SistEndret, "SistEndret")

  if (length(spec$enums) > 0) {
    for (nm in names(spec$enums)) {
      if (nm %in% names(params)) {
        .validate_enum(params[[nm]], nm, spec$enums[[nm]])
      }
    }
  }

  if (tolower(rapport) == "jaktdag") {
    has_sesong <- "Sesong" %in% names(params)
    has_date <- any(c("FraDato", "TilDato") %in% names(params))

    if (has_sesong && has_date) {
      stop(
        "For rapport='jaktdag', 'Sesong' cannot be combined with 'FraDato'/'TilDato'.",
        call. = FALSE
      )
    }

    if ("Godkjent" %in% names(params)) {
      ok <- is.logical(params$Godkjent) ||
        (is.character(params$Godkjent) && tolower(params$Godkjent) %in% c("true", "false"))
      if (!ok) {
        stop(
          "For rapport='jaktdag', 'Godkjent' must be TRUE/FALSE or 'true'/'false'.",
          call. = FALSE
        )
      }
    }
  }

  invisible(TRUE)
}

.perform_rapport_request <- function(base_req, spec, params, parse_json = TRUE) {
  req <- base_req |>
    httr2::req_url_path_append(spec$path) |>
    httr2::req_error(body = .rapport_error_body)

  if (length(params) > 0) {
    req <- req |>
      httr2::req_url_query(!!!params)
  }

  resp <- req |> httr2::req_perform()

  if (!parse_json) {
    return(resp)
  }

  ctype <- tryCatch(httr2::resp_content_type(resp), error = function(e) "")
  if (grepl("json", ctype, ignore.case = TRUE)) {
    return(jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE))
  }

  httr2::resp_body_string(resp)
}

.find_first_named_value <- function(x, target_names) {
  if (is.null(x)) return(NULL)

  if (is.list(x)) {
    nms <- names(x)

    if (!is.null(nms)) {
      for (nm in target_names) {
        idx <- match(nm, nms, nomatch = 0)
        if (idx > 0) {
          val <- x[[idx]]
          if (!is.list(val) && length(val) == 1) {
            return(val)
          }
        }
      }
    }

    for (i in seq_along(x)) {
      val <- .find_first_named_value(x[[i]], target_names)
      if (!is.null(val)) return(val)
    }
  }

  NULL
}

.extract_data_frame <- function(x) {
  if (is.null(x)) return(NULL)

  if (is.data.frame(x)) {
    return(x)
  }

  if (is.list(x)) {
    preferred_keys <- c(
      "items", "data", "results", "result", "content",
      "records", "entries", "rows", "value", "values"
    )

    for (nm in preferred_keys) {
      if (!is.null(x[[nm]])) {
        out <- .extract_data_frame(x[[nm]])
        if (!is.null(out)) return(out)
      }
    }

    out <- tryCatch(jsonlite::rbind_pages(x), error = function(e) NULL)
    if (is.data.frame(out)) {
      return(out)
    }
  }

  NULL
}

.has_more_pages <- function(parsed, current_page, page_size, n_rows) {
  total_pages <- .find_first_named_value(
    parsed,
    c("totalPages", "TotalPages", "total_pages", "pageCount", "page_count")
  )
  if (!is.null(total_pages) && !is.na(suppressWarnings(as.integer(total_pages)))) {
    return(current_page < as.integer(total_pages))
  }

  total_count <- .find_first_named_value(
    parsed,
    c("totalCount", "TotalCount", "total_count", "totalElements", "total_elements")
  )
  if (!is.null(total_count) && !is.na(suppressWarnings(as.integer(total_count)))) {
    return((current_page * page_size) < as.integer(total_count))
  }

  next_page <- .find_first_named_value(
    parsed,
    c("nextPage", "NextPage", "next_page")
  )
  if (!is.null(next_page)) {
    if (is.logical(next_page) && !is.na(next_page)) {
      return(isTRUE(next_page))
    }
    if (!is.na(suppressWarnings(as.integer(next_page)))) {
      return(as.integer(next_page) > current_page)
    }
  }

  n_rows > 0 && n_rows == page_size
}
# ---- Main function -----------------------------------------------------------

get_individrapport <- function(base_req,
                               params = list(),
                               rapport = "individ",
                               omrade = NULL,
                               parse_json = TRUE,
                               all_pages = FALSE,
                               max_pages = Inf) {
  spec <- .get_report_spec(rapport = rapport, omrade = omrade)

  params <- params[!vapply(params, .is_empty_value, logical(1))]
  params <- lapply(params, .normalize_param_value)

  if (!all_pages && !"page" %in% names(params)) {
    params$page <- 1L
    message(
      paste(
        "'page' not supplied in 'params'; defaulting to 1.",
        "Set 'all_pages = TRUE' if you want to download all data."
      )
    )
  }
  if ("page" %in% names(params) && !all_pages) {
    "Set 'all_pages = TRUE' if you want to download all data."
  }
  if (!"pageSize" %in% names(params)) {
    params$pageSize <- 1000L
  }

  .validate_params(params, spec, rapport = tolower(rapport), omrade = tolower(omrade))

  if (!all_pages) {
    return(.perform_rapport_request(
      base_req = base_req,
      spec = spec,
      params = params,
      parse_json = parse_json
    ))
  }

  if (!parse_json) {
    stop("'all_pages = TRUE' requires 'parse_json = TRUE'.", call. = FALSE)
  }

  if (!(length(max_pages) == 1 && (is.infinite(max_pages) || (!is.na(suppressWarnings(as.integer(max_pages))) && as.integer(max_pages) >= 1)))) {
    stop("'max_pages' must be a single positive integer or Inf.", call. = FALSE)
  }

  start_page <- if ("page" %in% names(params)) as.integer(params$page) else 1L
  page_size  <- if ("pageSize" %in% names(params)) as.integer(params$pageSize) else 1000L

  params$page <- start_page
  params$pageSize <- page_size

  pages <- list()
  page_num <- start_page
  pages_downloaded <- 0L

  repeat {
    if (!is.infinite(max_pages) && pages_downloaded >= as.integer(max_pages)) {
      stop(
        sprintf(
          "Stopped after %s pages. Increase 'max_pages' if you want to continue.",
          as.integer(max_pages)
        ),
        call. = FALSE
      )
    }

    params$page <- page_num

    parsed <- .perform_rapport_request(
      base_req = base_req,
      spec = spec,
      params = params,
      parse_json = TRUE
    )

    df <- .extract_data_frame(parsed)

    if (is.null(df)) {
      stop(
        paste(
          "Could not extract tabular data from the paginated response.",
          "Try one page first with all_pages = FALSE and inspect the JSON structure."
        ),
        call. = FALSE
      )
    }

    pages_downloaded <- pages_downloaded + 1L
    pages[[length(pages) + 1L]] <- df

    n_rows <- nrow(df)

    if (n_rows == 0) {
      break
    }

    if (!.has_more_pages(parsed, current_page = page_num, page_size = page_size, n_rows = n_rows)) {
      break
    }

    page_num <- page_num + 1L
  }

  out <- if (length(pages) == 0) {
    data.frame()
  } else {
    jsonlite::rbind_pages(pages)
  }

  attr(out, "pages_downloaded") <- pages_downloaded
  attr(out, "start_page") <- start_page
  attr(out, "page_size") <- page_size

  out
}


# ---- Optional helpers --------------------------------------------------------

valid_rapport_params <- function(rapport = "individ", omrade = NULL) {
  spec <- .get_report_spec(rapport = rapport, omrade = omrade)
  spec$params
}

rapport_template <- function(rapport = "individ", omrade = NULL) {
  spec <- .get_report_spec(rapport = rapport, omrade = omrade)
  out <- as.list(rep(NA, length(spec$params)))
  names(out) <- spec$params

  if ("pageSize" %in% names(out)) out$pageSize <- 1000
  if ("page" %in% names(out)) out$page <- 1

  out
}

# ---- Save report -------------------------------------------------------------
save_rapport <- function(x, file, overwrite = FALSE, na_string = "") {
  if (!is.data.frame(x)) {
    stop("'x' must be a data.frame.", call. = FALSE)
  }

  if (!is.character(file) || length(file) != 1 || !nzchar(file)) {
    stop("'file' must be a single file path.", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(file))

  if (!ext %in% c("xlsx", "csv", "parquet")) {
    stop(
      "Unsupported file extension. Please use '.xlsx', '.csv', or '.parquet'.",
      call. = FALSE
    )
  }

  if (file.exists(file) && !isTRUE(overwrite)) {
    stop(
      sprintf(
        "File already exists: %s. Use overwrite = TRUE to replace it.",
        file
      ),
      call. = FALSE
    )
  }

  if (ext == "xlsx") {

    .ensure_openxlsx()

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "data")

    openxlsx::writeData(
      wb, "data", x,
      withFilter = TRUE,
      keepNA = FALSE,
      na.string = na_string
    )

    header_style <- openxlsx::createStyle(textDecoration = "bold")

    openxlsx::addStyle(
      wb, "data",
      style = header_style,
      rows = 1,
      cols = seq_len(ncol(x)),
      gridExpand = TRUE,
      stack = TRUE
    )

    openxlsx::setColWidths(
      wb, "data",
      cols = seq_len(ncol(x)),
      widths = pmax(nchar(names(x)), 10)
    )

    openxlsx::freezePane(
      wb, "data",
      firstRow = TRUE
    )

    openxlsx::saveWorkbook(
      wb,
      file,
      overwrite = overwrite
    )

  } else if (ext == "csv") {

    .write_csv_utf8_bom(
      x = x,
      file = file,
      na_string = na_string,
      sep = ";"
    )

  } else if (ext == "parquet") {

    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop(
        "Package 'arrow' is required to write .parquet files. ",
        "Install it with install.packages('arrow').",
        call. = FALSE
      )
    }

    arrow::write_parquet(
      x,
      sink = file
    )
  }

  message(
    sprintf(
      "Saved file: %s",
      normalizePath(file, winslash = "/", mustWork = FALSE)
    )
  )

  invisible(file)
}

.write_csv_utf8_bom <- function(x, file, na_string = "", sep = ";") {
  # Write BOM in binary mode
  con <- file(file, open = "wb")
  writeBin(charToRaw("\ufeff"), con)
  close(con)

  # Append CSV text in UTF-8
  con <- file(file, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  utils::write.table(
    x = x,
    file = con,
    sep = sep,
    row.names = FALSE,
    col.names = TRUE,
    na = na_string,
    quote = TRUE,
    qmethod = "double"
  )

  invisible(file)
}

.ensure_openxlsx <- function() {
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    return(invisible(TRUE))
  }

  if (!interactive()) {
    stop(
      paste(
        "Package 'openxlsx' is required to save .xlsx files.",
        "This session is not interactive, so automatic install cannot be confirmed.",
        "Please run install.packages('openxlsx') first."
      ),
      call. = FALSE
    )
  }

  answer <- readline(
    prompt = "Package 'openxlsx' is not installed. Download and install it now? [y/n]: "
  )

  answer <- trimws(tolower(answer))

  if (!answer %in% c("y", "yes")) {
    stop(
      "Cannot save .xlsx without package 'openxlsx'. Installation declined.",
      call. = FALSE
    )
  }

  install.packages("openxlsx")

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package 'openxlsx' could not be loaded after installation.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# Examples:

# # base_req must already contain Authorization header
# # base_req <- request(base_url) |>
# #   req_headers(
# #     Authorization = paste("Bearer", access_token),
# #     Accept = "application/json"
# #   )
#
# # 1) Individ
# params_individ <- list(
#   Art = "Villrein",
#   Villreinområde = "2222",
#   Sesong = "2024",
#   Godkjent = "Ja",
#   pageSize = 1000,
#   page = 1
# )
#
# res_individ <- get_individrapport(
#   base_req = base_req,
#   params = params_individ,
#   rapport = "individ"
# )
#
# # 2) Fallvilt
# params_fallvilt <- list(
#   Fylke = "18,50",
#   FraDato = "2024-01-01",
#   TilDato = "2024-12-31",
#   Art = "Hjort",
#   pageSize = 1000,
#   page = 1
# )
#
# res_fallvilt <- get_individrapport(
#   base_req = base_req,
#   params = params_fallvilt,
#   rapport = "fallvilt"
# )
#
# # 3) Jaktdag
# params_jaktdag <- list(
#   Art = "Elg",
#   Sesong = "2024",
#   Terreng = "Utmark",
#   Godkjent = TRUE,
#   pageSize = 1000,
#   page = 1
# )
#
# res_jaktdag <- get_individrapport(
#   base_req = base_req,
#   params = params_jaktdag,
#   rapport = "jaktdag"
# )
#
# # 4) Område / vald
# params_omrade_vald <- list(
#   Art = "Hjort",
#   Fylke = "Troms",
#   pageSize = 1000,
#   page = 1
# )
#
# res_omrade_vald <- get_individrapport(
#   base_req = base_req,
#   params = params_omrade_vald,
#   rapport = "omrade",
#   omrade = "vald"
# )
#
# # 5) Telledata
# params_telledata <- list(
#   villreinomrade = "Hardangervidda",
#   fraAr = 2020,
#   tilAr = 2025,
#   pageSize = 1000,
#   page = 1
# )
#
# res_telledata <- get_individrapport(
#   base_req = base_req,
#   params = params_telledata,
#   rapport = "telledata"
# )
#
# # helpers:
# valid_rapport_params("fallvilt")
# valid_rapport_params("omrade", "villreinvald")
#
# rapport_template("individ")
# rapport_template("omrade", "jaktfelt")

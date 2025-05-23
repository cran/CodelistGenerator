# Copyright 2025 DARWIN EU®
#
# This file is part of CodelistGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Stratify a codelist by the concepts included within it.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams keepOriginalDoc
#'
#' @return The codelist or a codelist with details with the required 
#' stratifications, as different elements of the list.
#' @export
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' cdm <- mockVocabRef()
#' codes <- list("concepts" = c(20,21))
#' new_codes <- stratifyByConcept(x = codes,
#'                                cdm = cdm,
#'                                keepOriginal = TRUE)
#' new_codes
#' }
stratifyByConcept <- function(x,
                              cdm,
                              keepOriginal = FALSE){


  if(inherits(x, "list") & !inherits(x, "codelist_with_details")){
    x <- omopgenerics::newCodelist(x)
  }

  x_start <- x

  if(inherits(x_start, "codelist")){
  x <- addDetails(x, cdm = cdm)
  }

  for(i in seq_along(x)){
    x[[i]] <- x[[i]] |>
      dplyr::mutate(c_name = names(x[i])) |>
      dplyr::mutate(new_c_name = paste0(.data$c_name, "_",
                                      omopgenerics::toSnakeCase(.data$concept_name)))
  }

  x <- purrr::list_rbind(x)

  if(any(is.na(x$concept_name))){
    nMissingConceptName <- sum(is.na(x$concept_name))
    cli::cli_warn("Dropping {nMissingConceptName} concepts that do not have a concept name")
    x <- x |>
      dplyr::filter(!is.na(.data$concept_name))
  }

  x <- split(x,
             x[, c("new_c_name")]
        )

  if(inherits(x_start, "codelist")){
    for(i in seq_along(x)){
      x[[i]] <- x[[i]] |>
        dplyr::pull("concept_id")
    }
  }

  if(inherits(x_start, "codelist_with_details")){
    for(i in seq_along(x)){
      x[[i]] <- x[[i]] |>
        dplyr::select(!"c_name") |>
        dplyr::select(!"new_c_name")
    }
  }

  if(isTRUE(keepOriginal)){
    x <- purrr::list_flatten(list(x_start, x))
  }

  x <- x[order(names(x))]

  if(inherits(x_start, "codelist")){
    x  <- omopgenerics::newCodelist(x)
  } else{
    x  <- omopgenerics::newCodelistWithDetails(x)
  }

  x

}

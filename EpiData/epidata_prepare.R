library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)


### --------- CONSTANTS

# Error messages
error_msg <- c(
    "OK"                    = "OK",
    "DUPLICATE-DB"          = "Duplicates in `db_field`:",
    "MISSING-TYPE"          = "Some fields have missing `type`:",
    "INVALID-TYPE"          = "Some fields have invalid `type`:",
    "MISSING-NOM-LIST"      = "Some nominal fields have missing in `nom_list`:",
    "MISSING-NOM-SHEET"     = "Missing data in `nom` sheet.",
    "MISSING-NOM-VALUES"    = "Some names in `nom_list` do not exist in `nom` sheet:",
    "HIDE-BEGIN"            = "Field name beginning the list of hide fields not found:",
    "HIDE-END"              = "Field name ending the list of hide fields not found:",
    "HIDE-END-BEFORE-BEGIN" = "List of hide fields - the ending field is before the beginning field:",
    "HIDE-PARTS-LT3"        = "Hide string has fewer than 3 parts:",
    "INVALID-HIDE-ACTION"   = "Invalid action in hide string:"
)

# Field types
types <- c("id", "text", "date", "bin", "nom", "quan")

# Default format for each field type
format_default <- c(
    "id"    = "t;5",
    "text"  = "n;20",
    "date"  = "n;1",
    "bin"   = "1",
    "nom"   = "5",
    "quan"  = "5"
)

# Pattern of the first part of the format string (for validation)
format1_pattern <- c(
    "id"    = "[ant]",
    "text"  = "^[aen]$",
    "date"  = "^[nt]$",
    "bin"   = "^[1y]$",
    "nom"   = "^[0-9]+$",
    "quan"  = "^[0-9]+$"
)

# Default values for the first part of the format string
format1_default <- c(
    "id"    = "t",
    "text"  = "n",
    "date"  = "n",
    "bin"   = "1",
    "nom"   = "5",
    "quan"  = "5"
)

# Default values for the second part of the format string
format2_default <- c(
    "id"    = 5,
    "text"  = 20,
    "date"  = 1,
    "bin"   = 1,
    "nom"   = 0,
    "quan"  = 0
)

# Date formats
date_format <- c("dd/mm/yyyy", "mm/dd/yyyy", "yyyy/mm/dd")


### --------- FUNCTIONS

# Validate data dictionary
validate_datadict <- function(d_field, d_nom) {
    # Check for duplicates in `db_field`
    error_fields <- d_field %>%
        count(db_field) %>%
        filter(n > 1) %>%
        pull(db_field)

    if (length(error_fields) > 0) {
        error_str <- sprintf("%s\n%s",
            error_msg["DUPLICATE-DB"],
            error_fields %>% paste(sep = ", ")
        )

        return(error_str)
    }

    # Check missing `type`
    error_fields <- d_field %>%
        filter(is.na(type)) %>%
        pull(db_field)

    if (length(error_fields) > 0) {
        error_str <- sprintf("%s\n%s",
            error_msg["MISSING-TYPE"],
            error_fields %>% paste(sep = ", ")
        )

        return(error_str)
    }

    # Check invalid `type`
    error_fields <- d_field %>%
        filter(!(type %in% types)) %>%
        pull(db_field)

    if (length(error_fields) > 0) {
        error_str <- sprintf("%s\n%s",
            error_msg["INVALID-TYPE"],
            error_fields %>% paste(sep = ", ")
        )

        return(error_str)
    }

    # Check missing `nom_list`
    error_fields <- d_field %>%
        filter(type == "nom", is.na(nom_list)) %>%
        pull(db_field)

    if (length(error_fields) > 0) {
        error_str <- sprintf("%s\n%s",
            error_msg["MISSING-NOM-LIST"],
            error_fields %>% paste(sep = ", ")
        )

        return(error_str)
    }

    # Check missing `nom` sheet
    if (any(is.na(d_nom))) {
        return(error_msg["MISSING-NOM-SHEET"])
    }

    # Check if names in `nom_list` exist in `nom` sheet
    nomlist_values <- d_field %>%
        drop_na(nom_list) %>%
        pull(nom_list) %>%
        unique()
    listname_values <- d_nom %>%
        pull(list_name) %>%
        unique()
    values_diff <- setdiff(nomlist_values, listname_values)

    if (length(values_diff) > 0) {
        error_str <- sprintf("%s\n%s",
            error_msg["MISSING-NOM-VALUES"],
            values_diff %>% paste(sep = ", ")
        )

        return(error_str)
    }

    return(error_msg["OK"])
}

collapse_str <- function(d, column, collapse = "") {
    output_str <- d %>%
        filter(!is.na(!!sym(column)) & str_length(!!sym(column)) > 0) %>%
        pull(!!sym(column)) %>%
        paste(collapse = collapse)

    return(output_str)
}


## ------- QES FILE GENERATION

# Replace an NA format string with default string
fill_na_format <- function(d_field, format_var, fmt_def_list) {
    d_field %>%
        mutate(
            !!format_var := if_else(
                is.na(!!sym(format_var)),
                fmt_def_list[type],
                !!sym(format_var)
            )
        )
}

# Validate the format strings and replace with default strings.
# This function separates the format string into `format1` and `format2`,
# which is needed for the next step. `format2` is converted to numeric.
process_format <- function(d_field) {
    d_field %>%
        fill_na_format("format", format_default) %>%
        separate(format, into = c("format1", "format2"), sep = ";", convert = TRUE, fill = "right") %>%
        mutate(
            # Replace invalid `format1`
            format1 = if_else(
                str_detect(format1, format1_pattern[type]),
                format1,
                format1_default[type]
            ),
            # Replace invalid `format2` for date fields
            format2 = if_else(
                (type == "date") & (format2 < 1 | format2 > 3),
                format2_default["date"],
                format2
            )
        ) %>%
        fill_na_format("format2", format2_default)
}


## ---- Functions to generate EpiData format string

gen_fmt_id_a <- function(format2) {
    if (format2 > 5) {
        return(sprintf("<IDNUM%s>", strrep(" ", format2 - 5)))
    }

    return("<IDNUM>")
}

gen_fmt_text <- function(format1, format2) {
    switch(format1,
        n = strrep("_", format2),
        a = sprintf("<A%s>", strrep(" ", format2 - 1)),
        e = sprintf("<E%s>", strrep(" ", format2 - 1))
    )
}

gen_fmt_date <- function(format1, format2) {
    switch(format1,
        n = sprintf("<%s>", date_format[format2]),
        t = sprintf("<Today-%s>", date_format[format2])
    )
}

gen_fmt_int <- function(format) {
    strrep("#", format)
}

gen_fmt_float <- function(format1, format2) {
    sprintf("%s.%s", strrep("#", format1), strrep("#", format2))
}

gen_fmt <- function(type, format1, format2) {
    switch(type,
        "id" = switch(format1,
            a = gen_fmt_id_a(format2),
            n = gen_fmt_int(format2),
            t = gen_fmt_text("n", format2)
        ),
        "text" = gen_fmt_text(format1, format2),
        "date" = gen_fmt_date(format2, format2),
        "bin" = switch(format1,
            "1" = "#",
            y = "<Y>"
        ),
        "nom" = gen_fmt_int(format1),
        "quan" = if_else(
            is.na(format2),
            gen_fmt_int(format1),
            gen_fmt_float(format1, format2)
        )
    )
}

gen_format_str <- function(d_field) {
    d_field %>%
        rowwise() %>%
        mutate(
            format_str = gen_fmt(type, format1, format2)
        )
}


## ---- Functions to generate guide string for nomimal fields

gen_nom_dict <- function(d_nom) {
    d_nom %>%
        nest(data = c(code, label)) %>%
        rowwise() %>%
        mutate(
            data = data %>%
                mutate(guide_str = sprintf("%s. %s", code, label)) %>%
                pull(guide_str) %>%
                paste(collapse = "\n    ")
        ) %>%
        deframe()
}

gen_guide_str <- function(d_field, d_nom) {
    nom_dict <- gen_nom_dict(d_nom)
    d_field %>%
        rowwise() %>%
        mutate(
            guide_str = if_else(
                type == "nom",
                paste0("    ", nom_dict[nom_list], "\n"),
                ""
            )
        )
}


# Write QES file
write_qes <- function(d_field, filename, title) {
    if (is.null(title) || title == "") {
        title <- filename
    }

    qes_str <- paste0(
        title, "\n\n",
        collapse_str(d_field, "qes_line"),
        "THE END"
    ) %>%
        str_replace_all("\\\\n", "\n")

    cat("Writing QES file...\n")

    # Write to console if filename is "[CONSOLE]"
    if (filename == "[CONSOLE]") {
        cat(qes_str)
        return()
    }

    qes_file <- file(sprintf("%s.qes", filename), "w")
    write(qes_str, qes_file)
    close(qes_file)
}


## ------- CHK FILE GENERATION

# Generate LABEL BLOCK code for nominal fields
gen_nom_chk <- function(d_nom) {
    d_nom %>%
        nest(data = c(code, label)) %>%
        rowwise() %>%
        # Example: `1 "Nam"  2 "Nu"  3 "Khac"`
        mutate(
            data = data %>%
                mutate(guide_str = sprintf("%s \"%s\"", code, label)) %>%
                pull(guide_str) %>%
                paste(collapse = "  ")
        ) %>%
        ungroup() %>%
        # Example: `  LABEL gioi  1 "Nam"  2 "Nu"  3 "Khac"  END`
        mutate(
            guide_str = sprintf("  LABEL %s  %s  END", list_name, data)
        ) %>%
        pull(guide_str) %>%
        paste(collapse = "\n") %>%
        sprintf("LABELBLOCK\n%s\nEND\n\n", .)
}


## ---- Functions to process hide codes

get_field_begin_end <- function(db_field_all, field_begin, field_end) {
    # Validation: make sure that field names are correct
    pos_begin <- which(db_field_all == field_begin)
    if (length(pos_begin) == 0) {
        stop(paste(error_msg["HIDE-BEGIN"], field_begin))
    }

    pos_end <- which(db_field_all == field_end)
    if (length(pos_end) == 0) {
        stop(paste(error_msg["HIDE-END"], field_end))
    }

    if (pos_begin > pos_end) {
        stop(paste(error_msg["HIDE-END-BEFORE-BEGIN"], field_begin, "-->", field_end))
    }

    # Beginning and ending fields are okay. Return the list of fields
    return(db_field_all[pos_begin:pos_end])
}

get_field_list <- function(db_field_all, hide_code) {
    # Split the hide code into parts and process each part
    map(
        str_split_1(hide_code, ";"),
        ~ {
            # Check if the hide code represents a range
            if (str_detect(.x, "-")) {
                field_beginend <- str_split_1(.x, "-")

                # Return the list of fields in the range
                return(get_field_begin_end(
                    db_field_all,
                    str_trim(field_beginend[1]),
                    str_trim(field_beginend[2])
                ))
            }

            # Return the single field
            return(str_trim(.x))
        }
    ) %>%
        unlist() %>%    # Flatten the list
        unique()        # Remove duplicates
}

process_single_hide <- function(db_field_all, hide_code) {
    hide_parts <- str_split_fixed(hide_code, ";", 3)

    # Validate the hide code
    if (length(hide_parts) < 3) {
        stop(paste(error_msg["HIDE-PARTS-LT3"], hide_code))
    }

    # Get the HIDE / UNHIDE action from the first part
    action_if <- str_to_lower(hide_parts[1])

    if (action_if != "hide" && action_if != "unhide") {
        stop(paste(error_msg["INVALID-HIDE-ACTION"], hide_code))
    }

    action_else <- ifelse(action_if == "hide", "unhide", "hide")

    # Get the conditions from the second part
    condition <- hide_parts[2]

    # Get the list of fields from the third part
    field_list <- get_field_list(db_field_all, hide_parts[3])

    # Construct the hide command
    paste(
        sprintf("  IF %s THEN", condition),
        sprintf("    %s %s", action_if, field_list) %>% paste(collapse = "\n"),
        "  ELSE",
        sprintf("    %s %s", action_else, field_list) %>% paste(collapse = "\n"),
        "  ENDIF",
        sep = "\n"
    )
}

process_field_hide <- function(db_field_all, hide_code) {
    if (is.na(hide_code)) {
        return("")
    }

    hide_str <- map(
        str_split_1(hide_code, "\\|"),
        ~ process_single_hide(db_field_all, .x)
    ) %>%
        paste(collapse = "\n")

    if (str_length(hide_str) > 0) {
        return(sprintf("%s\n", hide_str))
    }

    return("")
}

gen_hide_chk <- function(d_field) {
    d_field %>%
        rowwise() %>%
        mutate(
            hide_str = process_field_hide(d_field$db_field, hide)
        ) %>%
        ungroup() %>%
        mutate(
            before_record = paste0(before_record, hide_str),
            after_entry = paste0(after_entry, hide_str),
            .keep = "unused"
        )
}


## ---- Functions to process skip codes

process_skip_code <- function(skip_code) {
    if (is.na(skip_code) || skip_code == "") {
        return("")
    }

    str_split_1(skip_code, ";") %>%
        sprintf("    %s\n", .) %>%
        paste(collapse = "")
}

gen_skip_codes <- function(d_field) {
    d_field %>%
        rowwise() %>%
        mutate(
            skip_code = process_skip_code(skip_code)
        )
}


# Generate audit codes
gen_audit_chk <- function(d_field) {
    d_field %>%
        mutate(
            audit = if_else(
                (type == "id" & format1 == "a") |
                    (type == "date" & format1 == "t"),
                0, audit
            ),
            format_str = if_else(
                type == "text" & "format1" != "n",
                gen_fmt_text("n", format2),
                format_str
            ),

            audit_before_record_str = if_else(
                audit == 1,
                sprintf("  DEFINE %so %s\n  %so = %s\n", db_field, format_str, db_field, db_field),
                ""
            ),
            before_record = paste0(before_record, audit_before_record_str),

            audit_before_entry_str = if_else(
                audit == 1,
                sprintf("    %so = %s\n", db_field, db_field),
                ""
            ),
            before_entry = paste0(before_entry, audit_before_entry_str),

            audit_after_entry_str = if_else(
                audit == 1,
                paste(
                    sprintf("    IF (%so <> %s) AND (rec <> -1) THEN", db_field, db_field),
                    sprintf("      WRITENOTE \"Value changed from @%so to @%s\"", db_field, db_field),
                    "    ENDIF\n",
                    sep = "\n"
                ),
                ""
            ),
            after_entry = paste0(after_entry, audit_after_entry_str)
        ) %>%
        select(-c(audit_before_record_str, audit_before_entry_str, audit_after_entry_str))
}


# Generate AFTER RECORD code for checking missing upon data entry completion
gen_required2_chk <- function(d_field) {
    d_field %>%
        mutate(
            after_record = if_else(
                required == 2,
                paste0(
                    after_record,
                    paste0(
                        sprintf("  IF %s = . THEN\n", db_field),
                        sprintf("    HELP \"Field %s cannot be empty.\" TYPE=ERROR\n", db_field),
                        sprintf("    GOTO %s\n  ENDIF\n", db_field)
                    )
                ),
                after_record
            )
        )
}


# Generate check code for each field
gen_field_chk <- function(d_field) {
    first_id_field <- d_field %>%
        filter(type == "id") %>%
        slice(1) %>%
        pull(db_field)

    d_field %>%
        mutate(
            chk_str = paste0(
                case_when(
                    key == 1 ~ "  KEY\n",
                    key == 2 ~ "  KEY UNIQUE\n",
                    TRUE     ~ ""
                ),
                if_else(db_field == first_id_field, "  TYPE STATUSBAR \"ID = \"\n", ""),
                if_else(type == "nom", sprintf("  COMMENT LEGAL USE %s\n", nom_list), ""),
                if_else(required == 1 | required == 2, "  MUSTENTER\n", ""),
                if_else(!is.na(range), sprintf("  RANGE %s\n", range), ""),
                if_else(skip_code != "", sprintf("  JUMPS\n%s  END\n", skip_code), ""),
                if_else(no_enter == 1, "  NOENTER\n", ""),
                if_else(
                    str_length(before_entry) > 0,
                    paste0("  BEFORE ENTRY\n", before_entry, "  END\n"),
                    ""
                ),
                if_else(
                    str_length(after_entry) > 0,
                    paste0("  AFTER ENTRY\n", after_entry, "  END\n"),
                    ""
                )
            )
        ) %>%
        filter(str_length(chk_str) > 0) %>%
        mutate(
            chk_str = paste0(
                db_field, "\n",
                chk_str,
                "END\n"
            )
        ) %>%
        pull(chk_str) %>%
        paste(collapse = "\n")
}


# Generate BEFORE FILE code
gen_before_file_chk <- function(d_field, confirm, comment) {
    before_file_str <- collapse_str(d_field, "before_file")

    if (confirm) {
        before_file_str <- paste0("  CONFIRM\n", before_file_str)
    }
    if (comment) {
        before_file_str <- paste0("  TYPE COMMENT ALLFIELDS Black\n", before_file_str)
    }

    if (str_length(before_file_str) > 0) {
        return(paste0("BEFORE FILE\n", before_file_str, "END\n\n"))
    }

    return("")
}


# Generate AFTER FILE code
gen_after_file_chk <- function(d_field) {
    after_file_str <- collapse_str(d_field, "after_file")

    if (str_length(after_file_str) > 0) {
        return(paste0("AFTER FILE\n", after_file_str, "END\n\n"))
    }

    return("")
}


# Generate BEFORE RECORD code
gen_before_record_chk <- function(d_field) {
    before_record_str <- collapse_str(d_field, "before_record")

    if (str_length(before_record_str) > 0) {
        return(paste0("BEFORE RECORD\n", before_record_str, "END\n\n"))
    }

    return("")
}


# Generate AFTER RECORD code
gen_after_record_chk <- function(d_field) {
    after_record_str <- collapse_str(d_field, "after_record")

    if (str_length(after_record_str) > 0) {
        return(paste0("AFTER RECORD\n", after_record_str, "END\n\n"))
    }

    return("")
}


# Write CHK file
write_chk <- function(d_field, d_nom, filename, confirm, comment) {
    chk_str <- paste0(
        gen_nom_chk(d_nom),
        gen_before_file_chk(d_field, confirm, comment),
        gen_before_record_chk(d_field),
        gen_field_chk(d_field),
        gen_after_record_chk(d_field),
        gen_after_file_chk(d_field)
    ) %>%
        str_replace_all("\\\\n", "\n")

    cat("\n\nWriting CHK file...\n")

    # Write to console if filename is "[CONSOLE]"
    if (filename == "[CONSOLE]") {
        cat(chk_str)
        return()
    }

    chk_file <- file(sprintf("%s.chk", filename), "w")
    write(chk_str, chk_file)
    close(chk_file)
}


# Pre-process and validate data
preprocess_validate <- function(d_field, d_nom, incl_guide_str, confirm, comment, incl_audit) {
    # Pre-processing
    max_len <- max(nchar(d_field$db_field) + nchar(d_field$question))
    d_field <- d_field %>%
        mutate(
            type = tolower(as.character(type)),
            format = tolower(as.character(format)),
            unit = replace_na(unit, ""),
            text_before = replace_na(text_before, ""),
            space_before_textbox = strrep(" ", max_len - str_length(db_field) - str_length(question) + 2),

            across(
                c(before_file, before_record, before_entry, after_file, after_record, after_entry),
                ~ as.character(.x) %>% replace_na("")
            )
        )

    # Validate data dictionary
    validate_msg <- validate_datadict(d_field, d_nom)
    if (validate_msg != error_msg["OK"]) {
        stop(validate_msg)
    }

    # Generate format strings
    d_field <- d_field %>%
        process_format() %>%
        gen_format_str()

    # Generate validation and instruction for 0/1 binary fields
    d_field <- d_field %>%
        mutate(
            range = if_else(type == "bin" & format1 == "1", "0 1", range),
            unit = if_else(type == "bin" & format1 == "1", "(0 = False, 1 = True)", unit)
        )

    # Generate guide strings
    if (incl_guide_str) {
        d_field <- gen_guide_str(d_field, d_nom)
    }

    # Generate the entire line for each field
    d_field <- d_field %>%
        mutate(
            qes_line = paste0(
                text_before,
                db_field, "  ", question, space_before_textbox, format_str, "   ", unit, "\n",
                if_else(incl_guide_str, guide_str, "")
            )
        )

    # Generate check codes
    d_field <- d_field %>%
        gen_skip_codes() %>%
        gen_hide_chk() %>%
        gen_required2_chk()

    # Generate audit checks
    if (incl_audit) {
        d_field <- gen_audit_chk(d_field)
    }

    return(d_field)
}


# MAIN FUNCTION
epidata_prepare <- function(
    d_field, d_nom, filename, title,
    incl_guide_str = FALSE, confirm = FALSE, comment = FALSE, incl_audit = FALSE
) {
    d_field <- preprocess_validate(d_field, d_nom, incl_guide_str, confirm, comment, incl_audit)

    write_qes(d_field, filename, title)
    write_chk(d_field, d_nom, filename, confirm, comment)

    cat("\n\nEpiDate preparation COMPLETED!\n")
}

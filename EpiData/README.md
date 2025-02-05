# EpiDataPrepare
Long Hoang, MD MPH
2025-02-05

- [<span class="toc-section-number">1</span>
  Introduction](#introduction)
- [<span class="toc-section-number">2</span> Description](#description)
  - [<span class="toc-section-number">2.1</span> EpiData
    dictionary](#epidata-dictionary)
    - [<span class="toc-section-number">2.1.1</span> `field`
      table](#field-table)
    - [<span class="toc-section-number">2.1.2</span> `nom`
      table](#nom-table)
  - [<span class="toc-section-number">2.2</span>
    `epidata_prepare()`](#epidata_prepare)
- [<span class="toc-section-number">3</span> Example](#example)
- [<span class="toc-section-number">4</span> Technical
  notes](#technical-notes)

# Introduction

The `epidata_prepare.R` file contains a lot of functions, but the main
function for end-users is `epidata_prepare()`. The function will process
the information defined in the EpiData dictionary to create a QES
(`.qes`) and a CHK (`.chk`) file for EpiData. One can use the QES file
to create the REC file, which is used in combination with the CHK file
for data entry.

Since the `epidata_prepare()` function relies on other functions in the
`epidata_prepare.R` file, it is recommended to source the file before
using the function:

``` r
source("epidata_prepare.R")
```

# Description

## EpiData dictionary

We need to store the data of the following two tables: `field` and
`nom`. The best way to prepare a data dictionary for EpiData is to copy
the Excel template file included in the folder since the template tables
have been formatted with data validation. One can create their own files
but they need to follow the same structure as the template file.

### `field` table

It must contain the following columns:

| Column | Description |
|----|----|
| `db_field` | Field name in the database. |
| `conv_field` | Field name in the final dataset. Not used for `epidata_prepare()`. |
| `type` | Field type. Must be one of these: `id` (ID), `text` (text), `date` (date), `bin` (binary), `nom` (nominal), `quan` (quantitative). |
| `identifiable` | `1` if the field contains identifiable information. Not used for `epidata_prepare()`. |
| `nom_list` | Name of the list of code/label pairs used for nominal data. Must exist in `nom` sheet. |
| `question` | Often the question in the paper CRF, and also appears in the EpiData QES file (before textboxes). |
| `format` | Format of the textbox, depending on the field type. See [Format](#format) for more details. |
| `unit` | Instruction for entry people about unit of the data field. |
| `required` | `1` if the field is required but not checked upon data entry completion, `2` if the field is required but checked upon data entry completion. |
| `audit` | `1` if audit trail is used for the field. Avoid creating fields with the names `rec` or `field_name + "o"`. |
| `range` | Acceptable range of data; same format as in EpiData. |
| `skip_code` | Skip code; same format as in EpiData, separate multiple skip codes with a semicolon (`;`). |
| `text_before` | Add lines of text before this line. Use `\n` to create a new line. |
| `key` | `1` if the field is key but not unique, `2` if the field is key and unique. |
| `hide` | Specify the condition to hide/unhide which fields. Syntax: `hide/unhide;condition;field_begin-field_end;single_field`. Separate multiple hide codes by a vertical bar (`\|`). Example: `unhide;b2=1;b3-b6;b8;c1-c3\|hide;b2=2;b7;b9`. |
| `no_enter` | `1` if the field cannot be entered. |
| `before_file` | EpiData code before doing anything with a REC file. Use `\n` to create a new line. |
| `before_record` | EpiData code before doing anything with a record. Use `\n` to create a new line. |
| `before_entry` | EpiData code before data entry (triggered when a field is focused). Use `\n` to create a new line. |
| `after_file` | EpiData code after finishing everything with a REC file. Use `\n` to create a new line. |
| `after_record` | EpiData code before saving a record. Use `\n` to create a new line. |
| `after_entry` | EpiData code after data entry (leaving the field). Use `\n` to create a new line. |

The `text_before` field and EpiData codes in the `before_file`,
`before_record`, `before_entry`, `after_file`, `after_record`, and
`after_entry` columns should **always end with a new line character**
(`\n`).

#### Format

| Field type | Format | Description |
|----|----|----|
| `id` | `a;<X>` | An autonumber field (`<IDNUM>`), `<X>` is the maximum number of character. |
| `id` | `n;<X>` | A numeric ID field. |
| `id` | `t;<X>` | A text ID field. |
| `id` | missing | `t;5` |
| `text` | `n;<X>` | A normal text field. |
| `text` | `a;<X>` | An all-caps text field. |
| `text` | `e;<X>` | An encrypted text field (password is required to open the EpiData file). |
| `text` | missing | `n;20` |
| `date` | `n;<X>` | Normal date. `<X>` can be `1` for `dmy`, `2` for `mdy`, or `3` for `ymd`. |
| `date` | `t;<X>` | Today. |
| `date` | missing | `n;1` |
| `bin` | `1` | Enter `0` or `1`, data saved as `0` or `1`. |
| `bin` | `y` | Enter `0`/`N` or `1`/`Y`, data saved as `N` or `Y`. |
| `bin` | missing | `1` |
| Other | `<X>;<Y>` | `<X>`: number of digits before the decimal; `<Y>`: number of digits after the decimal. |
| Other | missing | `5` |

The format is case-insensitive. If `<X>` is not defined (for `id` and
`text`), the default value is set to `5`. If `<Y>` is invalid, it is set
to missing.

### `nom` table

| Column      | Description                           |
|-------------|---------------------------------------|
| `list_name` | Name of the list of code/label pairs. |
| `code`      | Numeric code.                         |
| `label`     | Text label.                           |

## `epidata_prepare()`

**Syntax**:
`epidata_prepare(d_field, d_nom, filename, title, incl_guide_str, confirm, comment, incl_audit)`

**Arguments**:

| Argument | Description |
|----|----|
| `d_field` | Dataframe / Tibble of the `field` table. If you read this from an Excel file using `readxl::read_excel()`, you should set the `trim_ws` to `FALSE` to keep the leading and trailing spaces in the check codes. |
| `d_nom` | Dataframe / Tibble of the `nom` table. |
| `filename` | Name of the output files. If set to `[CONSOLE]`, the function will print the output to the console. |
| `title` | Title of the EpiData project. |
| `incl_guide_str` | `TRUE` to include guide strings in the QES file. Guide strings are the options for the nominal fields (see the example below). |
| `confirm` | `TRUE` if the user needs to press Enter before moving to the next field even when the current field has been filled. |
| `comment` | `TRUE` if the user wants the value of the selected option to be displayed to the right of the field when the cursor is moved to another field. |
| `incl_audit` | `TRUE` if the user wants to enable the audit trail. This means all changes made during data entry will be recorded in a NOT file. |

# Example

``` r
library(readxl)
source("epidata_prepare.R")

datadict_path <- "datadict.xlsx"
d_field <- read_excel(datadict_path, "field", trim_ws = FALSE)
d_nom <- read_excel(datadict_path, "nom")

epidata_prepare(
    d_field, d_nom,
    filename = "[CONSOLE]",
    title = "EPIDATA PREPARE DEMO",
    incl_guide_str = TRUE,
    confirm = TRUE,
    comment = TRUE,
    incl_audit = TRUE
)
```

    Writing QES file...
    EPIDATA PREPARE DEMO

    id  Study ID                     _____   
    name  Patient's full name        <A                             >   
    dosc  Date of sample collection  <dd/mm/yyyy>   

    A. TEST RESULT
    Sample 1
    a1  Sample quality               #   
        0. Poor
        1. Good
        9. Not collected
    a1a  Rapid test                  #   
        0. Negative
        1. Positive
    a1b  Concentration available     #   (0 = False, 1 = True)
    a1c  Concentration               ###.#   mmol/L

    Sample 2
    a2  Sample quality               #   
        0. Poor
        1. Good
        9. Not collected
    a2a  Rapid test                  #   
        0. Negative
        1. Positive
    a2b  Concentration available     #   (0 = False, 1 = True)
    a2c  Concentration               ###.#   mmol/L

    B. CONCLUSION
    b1  Final diagnosis available    #   (0 = False, 1 = True)
    b2  Diagnosis                    ________________________________________________________________________________   

    ***
    entry  Entry                     <A                   >   

    dateentry  Entry date            <dd/mm/yyyy>   
    datemodi  Date modified          <dd/mm/yyyy>   
    THE END

    Writing CHK file...
    LABELBLOCK
      LABEL quality  0 "Poor"  1 "Good"  9 "Not collected"  END
      LABEL negpos  0 "Negative"  1 "Positive"  END
    END

    BEFORE FILE
      TYPE COMMENT ALLFIELDS Black
      CONFIRM
    END

    BEFORE RECORD
      DEFINE ido _____
      ido = id
      DEFINE nameo ______________________________
      nameo = name
      DEFINE dosco <dd/mm/yyyy>
      dosco = dosc
      IF a1 = 1 THEN
        unhide a1a
      ELSE
        hide a1a
      ENDIF
      IF a1 = . THEN
        hide a2
      ELSE
        unhide a2
      ENDIF
      DEFINE a1o #
      a1o = a1
      IF a1a = 1 THEN
        unhide a1b
        unhide a1c
      ELSE
        hide a1b
        hide a1c
      ENDIF
      DEFINE a1ao #
      a1ao = a1a
      IF a1b = 1 THEN
        unhide a1c
      ELSE
        hide a1c
      ENDIF
      DEFINE a1bo #
      a1bo = a1b
      DEFINE a1co ###.#
      a1co = a1c
      IF a2 = 1 THEN
        unhide a2a
        unhide a2b
        unhide a2c
      ELSE
        hide a2a
        hide a2b
        hide a2c
      ENDIF
      DEFINE a2o #
      a2o = a2
      IF a2a = 1 THEN
        unhide a2b
        unhide a2c
      ELSE
        hide a2b
        hide a2c
      ENDIF
      DEFINE a2ao #
      a2ao = a2a
      IF a2b = 1 THEN
        unhide a2c
      ELSE
        hide a2c
      ENDIF
      DEFINE a2bo #
      a2bo = a2b
      DEFINE a2co ###.#
      a2co = a2c
      IF b1 = 1 THEN
        unhide b2
      ELSE
        hide b2
      ENDIF
      DEFINE b1o #
      b1o = b1
      DEFINE b2o ________________________________________________________________________________
      b2o = b2
      DEFINE entryo ____________________
      entryo = entry
      IF dateentry = . THEN
        dateentry = TODAY
      ENDIF
    END

    id
      KEY UNIQUE
      TYPE STATUSBAR "ID = "
      MUSTENTER
      BEFORE ENTRY
        ido = id
      END
      AFTER ENTRY
        IF (ido <> id) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @ido to @id"
        ENDIF
      END
    END

    name
      MUSTENTER
      BEFORE ENTRY
        nameo = name
      END
      AFTER ENTRY
        IF (nameo <> name) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @nameo to @name"
        ENDIF
      END
    END

    dosc
      MUSTENTER
      BEFORE ENTRY
        dosco = dosc
      END
      AFTER ENTRY
        IF (dosco <> dosc) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @dosco to @dosc"
        ENDIF
      END
    END

    a1
      COMMENT LEGAL USE quality
      MUSTENTER
      JUMPS
        1 a1a
      END
      BEFORE ENTRY
        a1o = a1
      END
      AFTER ENTRY
      IF a1 = 1 THEN
        unhide a1a
      ELSE
        hide a1a
      ENDIF
      IF a1 = . THEN
        hide a2
      ELSE
        unhide a2
      ENDIF
        IF (a1o <> a1) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @a1o to @a1"
        ENDIF
      END
    END

    a1a
      COMMENT LEGAL USE negpos
      MUSTENTER
      BEFORE ENTRY
        a1ao = a1a
      END
      AFTER ENTRY
      IF a1a = 1 THEN
        unhide a1b
        unhide a1c
      ELSE
        hide a1b
        hide a1c
      ENDIF
        IF (a1ao <> a1a) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @a1ao to @a1a"
        ENDIF
      END
    END

    a1b
      MUSTENTER
      RANGE 0 1
      BEFORE ENTRY
        a1bo = a1b
      END
      AFTER ENTRY
      IF a1b = 1 THEN
        unhide a1c
      ELSE
        hide a1c
      ENDIF
        IF (a1bo <> a1b) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @a1bo to @a1b"
        ENDIF
      END
    END

    a1c
      MUSTENTER
      RANGE 10 500
      BEFORE ENTRY
        a1co = a1c
      END
      AFTER ENTRY
        IF (a1co <> a1c) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @a1co to @a1c"
        ENDIF
      END
    END

    a2
      COMMENT LEGAL USE quality
      BEFORE ENTRY
        a2o = a2
      END
      AFTER ENTRY
      IF a2 = 1 THEN
        unhide a2a
        unhide a2b
        unhide a2c
      ELSE
        hide a2a
        hide a2b
        hide a2c
      ENDIF
        IF (a2o <> a2) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @a2o to @a2"
        ENDIF
      END
    END

    a2a
      COMMENT LEGAL USE negpos
      BEFORE ENTRY
        a2ao = a2a
      END
      AFTER ENTRY
      IF a2a = 1 THEN
        unhide a2b
        unhide a2c
      ELSE
        hide a2b
        hide a2c
      ENDIF
        IF (a2ao <> a2a) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @a2ao to @a2a"
        ENDIF
      END
    END

    a2b
      RANGE 0 1
      BEFORE ENTRY
        a2bo = a2b
      END
      AFTER ENTRY
      IF a2b = 1 THEN
        unhide a2c
      ELSE
        hide a2c
      ENDIF
        IF (a2bo <> a2b) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @a2bo to @a2b"
        ENDIF
      END
    END

    a2c
      RANGE 10 500
      BEFORE ENTRY
        a2co = a2c
      END
      AFTER ENTRY
        IF (a2co <> a2c) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @a2co to @a2c"
        ENDIF
      END
    END

    b1
      RANGE 0 1
      BEFORE ENTRY
        b1o = b1
      END
      AFTER ENTRY
      IF b1 = 1 THEN
        unhide b2
      ELSE
        hide b2
      ENDIF
        IF (b1o <> b1) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @b1o to @b1"
        ENDIF
      END
    END

    b2
      BEFORE ENTRY
        b2o = b2
      END
      AFTER ENTRY
        IF (b2o <> b2) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @b2o to @b2"
        ENDIF
      END
    END

    entry
      MUSTENTER
      BEFORE ENTRY
        entryo = entry
      END
      AFTER ENTRY
        IF (entryo <> entry) AND (rec <> -1) THEN
          WRITENOTE "Value changed from @entryo to @entry"
        ENDIF
      END
    END

    dateentry
      NOENTER
    END

    datemodi
      NOENTER
    END
    AFTER RECORD
      IF a2 = . THEN
        HELP "Field a2 cannot be empty." TYPE=ERROR
        GOTO a2
      ENDIF
    END



    EpiDate preparation COMPLETED!

# Technical notes

In case you want to know more about the functions in the
`epidata_prepare.R` file, you can read the documentation of each
function in the source code. Below is a graph summarizing how the
functions are called.

![Summary of function calls in
epidata_prepare.R](./epidataprepare_function_calls.png)

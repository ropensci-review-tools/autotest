
get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint

rm_internal_namespace <- function (x) {

    regmatches (x,
                gregexpr ("(?<=\\:\\:\\:).*",
                          x,
                          perl = TRUE)) [[1]]
}

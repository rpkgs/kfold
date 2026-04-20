listk <- function(...) {
    cols <- as.list(substitute(list(...)))[-1]
    vars <- names(cols)
    Id_noname <- if (is.null(vars)) {
          seq_along(cols)
      } else {
        which(vars == "")
    }
    if (length(Id_noname) > 0) {
          vars[Id_noname] <- sapply(cols[Id_noname], deparse)
      }
    x <- setNames(list(...), vars)
    return(x)
}

dt_round <- function(d, digits = 4) {
    mutate(d, across(where(is.double), ~ round(.x, digits)))
}

#' @importFrom dplyr select
#' @export
select.matrix <- function(.data, ...) {
    # browser()
    .data %>% as.data.frame() %>%
        dplyr::select(...) %>%
        as.matrix()
}

trans <- function(x) as.matrix(x) %>% t()

# chunk <- function(x, nchunk = 6) {
#   split(x, cut(seq_along(x), nchunk, labels = FALSE)) %>% set_names(NULL)
# }

#' @export
chunk_stratified <- function(y, kfold = 5, seed = 1) {
    set.seed(1) # 固定种子，保证交叉验证结果可精确复现

    # 1. 获取按目标变量 Y 值大小排序的对应索引
    idx_sorted <- order(y)

    # 2. 计算能被切分成多少个大小为 kfold 的区块
    n_blocks <- ceiling(length(y) / kfold)

    # 3. 在每个区块内部进行 1:kfold 的随机乱序排列
    #    保证局部随机性，同时维持宏观的分布均匀
    groups <- unlist(lapply(1:n_blocks, function(x) sample(1:kfold)))

    # 4. 截去尾端多余的组号（对应 length(y) 不能整除 kfold 的情况）
    groups <- groups[1:length(y)]

    # 5. 将排序后的索引按照打乱后的组号分发，并去除 list 的 names
    ind_lst <- unname(split(idx_sorted, groups))
    return(ind_lst)
}

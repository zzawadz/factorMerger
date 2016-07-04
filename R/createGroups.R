createGroups = function(x, cutoff = 0.1)
{
  vals = x %>% unique() %>% sort
  ux = expand.grid(vals, vals, stringsAsFactors = FALSE)

  dlDistance = stringdist(tolower(ux$Var1), tolower(ux$Var2))

  maxLength = apply(as.matrix(ux), 1, function(x) max(nchar(as.character(x))))

  scaledDistance = 1 - dlDistance / maxLength

  dstMat = matrix(scaledDistance, ncol = sqrt(length(dlDistance)))

  rownames(dstMat) = vals
  colnames(dstMat) = vals

  dstMat = as.dist(1 - dstMat)

  fit = hclust(dstMat)

  tree = cutree(fit, h = cutoff)
  groups = lapply(sort(tree) %>% unique, function(i) sort(names(tree[tree == i])))
  names(groups) = sapply(groups, "[[", 1)
  lapply(groups, as.list)
}

remapData = function(data, remapConfig)
{
  if(length(remapConfig) == 0) return(data)
  namesConfig = names(remapConfig)

  for(i in seq_along(remapConfig))
  {
    config = remapConfig[[i]]

    dt = data[[namesConfig[[i]]]]
    dt = factor(dt)

    levels = levels(dt)

    for(cnf in config)
    {
      levels[levels %in% cnf] = cnf[1]
    }

    levels(dt) = levels
    data[[namesConfig[[i]]]] = dt
  }

  data
}

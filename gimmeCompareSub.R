# In order to run this code, do gimme analysis beforehand.

# Start off your batch script if needed.

rm(list = ls()) 

# Import necessary R libraries

{
library(rstatix)
library(dplyr)
library(tibble)
}

###########################################################################
############     SUBGROUP COMPARISON with gimmeCompareSub     #############
###########################################################################


# 1-1.Import the output csv file named "indivPathEstimates" and assign it to the object "data".
# 1-2. getwd() retrieves the location of the current working directory.
# 1-3. Specify the path to the imported file including the name of the output file.

data_path = paste0(getwd(), "/indivPathEstimates.csv") 
data = read.csv(data_path, head = TRUE) # The object "data" is the input of gimmeCompareSub.


########## 2. gimmeCompareSub FUNCTION ############

# 2.1 gimmeCompareSub compares coefficient estimates of significant paths across subgroups. 
# 2.2 If the number of subgroups is 2, Bonferroni t-test is conducted.
# 2.3 If the number of subgroups is greater than 2, ANOVA followed by the post-hoc analysis with Tukey's HSD method is conducted.

gimmeCompareSub = function(data){
  
  # Check how data looks like
  head(data)
  
  # Create a column "path" displaying the directionality of each path (lhs --> rhs)
  
  data = dplyr::mutate(data, path = c(paste0(data$lhs,"-->", data$rhs)))
  
  # Get subgroup type
  subgroups = unique(data$sub_membership)
  
  # Allocate objects
  sub = list() # list by subgroup
  UniquePath = list() # path type in each subgroup
  Intersect = c() # intersection of path types across subgroups
  
  # Get unique paths from each subgroup
  for(i in subgroups){
    sub[[i]] = data %>% filter(sub_membership == i)
    sub[[i]] = mutate(sub[[i]], path = c(paste0(sub[[i]]$lhs,"-->", sub[[i]]$rhs)))
    UniquePath[[i]] = unique(sub[[i]]$path)
  }
  
  # Final intersection of path types across subgroups
  Intersect = Reduce(intersect, UniquePath) 
  
  #########################
  ### Bonferroni t-test ###
  #########################
  if(length(subgroups) == 2){
    
    # Allocate objects
    PathList = list()
    tTest = list()
    No_tTest = c()
    Sig_tTest  = c()
    Sig_i = c()
    
    # Bonferroni t-test is conducted for each significant path.
    # t-test is not run for paths that were significant for only one person in one of the subgroups, resulting in warning messages.
    for(i in Intersect){
      PathList[[i]] = data %>% filter(path == i) %>% select(file, beta, pval, path, sub_membership)
      x = table(PathList[[i]]$sub_membership)
      names(x)= NULL
      if(any(x == 1)){
        No_tTest = c(No_tTest, i)
      } else {
        tTest[[i]] = PathList[[i]] %>% t_test(beta ~ sub_membership) %>% adjust_pvalue(method = "bonferroni") %>% add_significance("p.adj")
        if(tTest[[i]]$p.adj.signif != "ns"){
          Sig_i = c(Sig_i, i)
          Sig_tTest = rbind(Sig_tTest, tTest[[i]])
        }
        SigPath = cbind(Sig_i, Sig_tTest)[,-c(2,9)]
      }
    }
    
    colnames(SigPath)[1] = "significant path"
    SigPath = data.frame(SigPath)
    print(paste0("Warning: t-test was not run for the path ", No_tTest, " due to a single input in any of the subgroups"), sep = "\n")
    print(as_tibble(SigPath))
    write.csv(SigPath, file = paste0(getwd(), "/SigDiffPaths_tTest.csv"))
    
  } else {
    
    
    ##########################################################
    ### ANOVA with post-hoc analysis by Tukey's HSD method ###
    ##########################################################
    
    
    # Allocate objects
    PathList = list()
    No_ANOVA = c()
    FSig_i = c()
    Sig_i = c()
    Sig_ANOVA = c()
    tukey_sig = c()
    group_sig = c()
    SigPath = c()
    
    # ANOVA test is not run for paths that were significant for only one person in one of the subgroups, resulting in warning messages.
    for(i in Intersect){
      PathList[[i]] = data %>% filter(path == i) %>% select(file, beta, pval, path, sub_membership)
      PathList[[i]]$sub_membership = as.factor(PathList[[i]]$sub_membership)
      x = table(PathList[[i]]$sub_membership)
      names(x)= NULL
      if(any(x == 1)){
        No_ANOVA= c(No_ANOVA, i)
      } else{
        anova = aov(beta ~ sub_membership, PathList[[i]])
        F_test_sig = ifelse(summary(anova)[[1]][["Pr(>F)"]][1] <.05, "s", "ns") # F-test significance ("ns" or "s")
        if(F_test_sig == "s"){
          FSig_i = c(FSig_i, i)
        }
        tukey_Test = TukeyHSD(anova, conf.level = 0.95) # multiple two-group comparisons by Tukey's method
        tt = tukey_Test$sub_membership
        tt = as.data.frame(tt) %>% rownames_to_column()
        if(any(tukey_Test$sub_membership[,'p adj'] < .05)){
          Sig_i = c(Sig_i, i) # significant path in  a pair of subgroups
          tukey_sig =  tt[which(tt$`p adj` < .05),]
          combined = cbind(rep(i, nrow(tukey_sig)), tukey_sig, rep(F_test_sig, nrow(tukey_sig)))
          SigPath = rbind(SigPath, combined) 
        }
      }
    }
    colnames(SigPath)[1] = "significant.path"
    colnames(SigPath)[2] = "subgroups"
    colnames(SigPath)[3] = "mean.difference"
    colnames(SigPath)[4] = "upper.CI"
    colnames(SigPath)[5] = "lower.CI"
    colnames(SigPath)[ncol(SigPath)-1] = "p.adj"
    colnames(SigPath)[ncol(SigPath)] = "F-test.significance"
    
    
    SigPath = data.frame(SigPath)
    print(paste0("A significant path according to F-test: ", FSig_i), sep = "\n")
    print(paste0("Warning:ANOVA test was not run for the path ", No_ANOVA, " due to a single input in any of the subgroups"), sep = "\n")
    print(as_tibble(SigPath))
    write.csv(SigPath, file = paste0(getwd(), "/SigDiffPaths_ANOVA.csv"))
  }
}

# Run gimmeCompareSub function
gimmeCompareSub(data = data)


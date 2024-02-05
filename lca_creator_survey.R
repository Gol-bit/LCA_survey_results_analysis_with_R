### Purpose: Read in data and run LCA on it with different number of classes, and with a covariate

library(poLCA)  # Load poLCA package for Latent Class Analysis

# Read the data
poll_data <- read.table(file="lca_proper.csv", header=TRUE, sep=",", as.is=TRUE)

# Correcting the variable names
vars <- c("YouTube", "Instagram", "TikTok", "Twitch", "buy_vidmaking", "buy_cloud", "buy_mus", "buy_host",
          "ad_ggl", "ad_ut", "ad_fb", "ad_ig", "ad_tt", "ad_post", "ad_partner", "m_spons", "m_ads", "m_don",
          "m_subs", "m_paidcont", "m_owngoods", "m_good", "earn")

# Incrementing all selected variables by 1
for (i in vars) {
  poll_data[[i]] <- poll_data[[i]] + 1
}

# Corrected formula without concatenated variable names
f1 <- as.formula(cbind(Twitch, buy_vidmaking, buy_cloud, buy_mus, buy_host, ad_ggl, ad_ut, ad_fb, ad_ig,
                       ad_tt, ad_post, ad_partner, m_spons, m_ads, m_don, m_subs, m_paidcont, m_owngoods,
                       m_good) ~ 1)

# Running LCA for 2, 3, 4, and 5 classes with various settings
LCA2 <- poLCA(f1, data=poll_data, nclass=2, nrep=2)
plot(LCA2)

# Running LCA for 2, 3, 4, and 5 classes with various settings
LCA3 <- poLCA(f1, data=poll_data, nclass=3, maxiter=3000, nrep=2)
plot(LCA3)

LCA4 <- poLCA(f1, data=poll_data, nclass=4, maxiter=3000, nrep=2)
plot(LCA4)

LCA5 <- poLCA(f1, data=poll_data, nclass=5, maxiter=5000, nrep=2)

# Plotting the results
plot(LCA3)




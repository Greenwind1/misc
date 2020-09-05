# w/o docker env
# https://www.r-bloggers.com/quick-way-of-installing-all-your-old-r-libraries-on-a-new-device/

# save installed
installed <- as.data.frame(installed.packages())
write.csv(installed, 'installed_packages.csv')

# install
installed <- read.csv('installed_packages.csv')
baseR <- as.data.frame(installed.packages())
toInstall <- setdiff(installedPreviously, baseR)
install.packages(toInstall)

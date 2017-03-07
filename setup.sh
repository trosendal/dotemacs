##
## System Packages
##
sudo apt-get update
sudo apt-get upgrade

##
## LaTeX
##
sudo apt-get -y install texlive-full

##
## git
##
sudo apt-add-repository ppa:git-core/ppa
sudo apt-get update
sudo apt-get install git
git config --global user.name "Thomas Rosendal"
git config --global user.email "thomas.rosendal@sva.se"

##
## R
##
sudo apt-get install -y software-properties-common && \
sudo apt-get install -y apt-transport-https && \
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 && \
sudo add-apt-repository "deb https://cran.rstudio.com/bin/linux/ubuntu xenial/" && \
sudo apt-get update && sudo apt-get install -y r-base && sudo apt-get install -y r-base-dev


##
## emacs
## Enable source code in ubuntu
sudo apt-get update
sudo apt-get -y install build-essential
sudo apt-get -y build-dep emacs24
wget http://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.gz
tar -zxvf emacs-24.5.tar.gz
cd emacs-24.5
./autogen.sh
./configure
make
make install
cd ..
wget https://raw.githubusercontent.com/trosendal/dotemacs/master/.emacs


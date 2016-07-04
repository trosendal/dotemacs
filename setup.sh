##
## System Packages
##
sudo apt-get update
sudo apt-get upgrade

##
## LaTeX
##
sudo apt-get -y install texlive

##
## git
##
sudo apt-get install -y libcurl4-gnutls-dev libexpat1-dev gettext libz-dev libssl-dev
sudo apt-get install -y asciidoc xmlto docbook2x
wget https://www.kernel.org/pub/software/scm/git/git-2.9.0.tar.gz
tar -zxf git-2.9.0.tar.gz
cd git-2.9.0
make configure
./configure --prefix=/usr
make all doc info
sudo make install install-doc install-html install-info
git config --global user.name "Thomas Rosendal"
git config --global user.email "thomas.rosendal@sva.se"

##
## R
##
sudo apt-get install -y software-properties-common && \
sudo apt-get install -y apt-transport-https && \
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 && \
sudo add-apt-repository "deb https://cran.rstudio.com/bin/linux/ubuntu trusty/" && \
sudo apt-get update && sudo apt-get install -y r-base && sudo apt-get install -y r-base-dev


##
## emacs
##
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


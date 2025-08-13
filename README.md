# Prerequisites
## WSL specific
* Launch wsl in admin cmd.exe
``` shell
wsl --exec dbus-launch true
```
## Debian like
``` shell
apt-get update
apt-get install git ssh apt-show-versions emacs elpa-magit-forge screen
```
## ssh key
* Create a pub key if never done
``` shell
ssh-keygen -t rsa
```
* Push pub key to Github : https://github.com/settings/keys
## Logging into host with Putty
### Download
* Donwload & install from https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html
### Configure Putty
* Terminal > Keyboard > The function key and escape : SCO
* Window > Translation > Remote character set : UTF-8
* Connection > SSH > X11 > Enable X11 forwarding
### Install Xming
* Download & install from https://sourceforge.net/projects/xming/
# Get last rc files
## Get sources
``` shell
cd
mkdir git
cd git
wget -q https://github.com/cretinon/rc/archive/refs/heads/main.tar.gz -O - | tar -zxvf - -C /tmp/
```
## Clone repo
``` shell
cd
mkdir git
cd git
git clone https://github.com/cretinon/rc.git
```
# Link rc files
``` shell
cd
rm -rf .bashrc
ln -s git/rc/.bashrc .
ln -s git/rc/.screenrc .
ln -s git/rc/.emacs .
ln -s git/rc/.bash_logout .
ln -s git/rc/.authinfo.gpg .
mkdir .emacs.d
cd .emacs.d
ln -s ../git/rc/.emacs.d/snippets .
cd
touch .emacs.custom
source .bashrc
```
# Post install
## Emacs
* Run emacs then ```M-x all-the-icons-install-fonts```
## Forge
* Create Github token https://github.com/settings/tokens
* Add in ~/.authinfo
```
machine api.github.com login cretinon^forge password changeme
```
* In emacs, add a repo
```M-x forge-add-repository``` or ```M-x forge-add-user-repository ```
* Then check with
```M-x forge-dispatch l r```
* We may need to setup Github access for repo
``` shell
git remote set-url origin git@github.com:cretinon/rc.git
```
## GPTel
* add in ~/.authinfo
```
machine api.openai.com login apikey password changeme
```
# Git Emacs Github and user/pass management (.authinfo)
1. In order to not have .authinfo with all access on a host, that file will be removed when logout (.bash_logout)

2. In order to decrypt it :

``` shell
_decrypt_authinfo
```

## Cloning a Github repo
If cloning a repo using https like :

``` shell
git clone https://github.com/cretinon/REPO.git
```
don't forget to edit .git/config, find line

```
url = https://github.com/cretinon/REPO.git
```
then change it with
```
url = https://user_name%40domain_name.com@github.com/cretinon/REPO.git
```

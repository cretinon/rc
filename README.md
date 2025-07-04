# Prerequisites
## WSL specific
Launch wsl in admin cmd.exe
``` shell
wsl --exec dbus-launch true
```
## Debian like
``` shell
apt-get update
apt-get install git ssh
mkdir git
cd git
```
# Get last rc files
## Get sources
``` shell
wget -q https://github.com/cretinon/rc/archive/refs/heads/main.tar.gz -O - | tar -zxvf - -C /tmp/
```

## Clone repo
``` shell
git clone https://github.com/cretinon/rc.git
```

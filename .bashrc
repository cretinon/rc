# ~/.bashrc: executed by bash(1) for non-login shells.

# Note: PS1 and umask are already set in /etc/profile. You should not
# need this unless you want different defaults for root.
# PS1='${debian_chroot:+($debian_chroot)}\h:\w\$ '
# umask 022

# You may uncomment the following lines if you want `ls' to be colorized:
export LS_OPTIONS='--color=auto'
eval "$(dircolors)"
alias ls='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -l'
alias l='ls $LS_OPTIONS -lAi'
#
# Some more alias to avoid making mistakes:
# alias rm='rm -i'
# alias cp='cp -i'
# alias mv='mv -i'
alias df='df -h'

alias gitd='git diff'
alias gitc='git commit'
alias gitp='git push'

alias rc='source ~/.bashrc'
alias ra='source ~/.aliases'
alias main='/root/git/shell/main.sh'
alias screen='TERM=xterm screen'
alias my_ip='curl ipinfo.io/ip && echo ""'
alias drop_cache='sync; echo 3 > /proc/sys/vm/drop_caches'


function ssh-screen() {
    screen -t $1 ssh $@
}

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

count_untracked() {
    _modified__=$(git status --porcelain 2> /dev/null | grep "^ M" | wc -l)
    _added__=$(git status --porcelain 2> /dev/null | grep "^ A" | wc -l)
    _deleted__=$(git status --porcelain 2> /dev/null | grep "^ D" | wc -l)
    _untrack__=$(git status --porcelain 2> /dev/null | grep "^ \?\?" | wc -l)

    _result__=""
    
    if [ $_modified__ -ne 0 ]; then
	_result__=$_result__"M:$_modified__"
    fi
    if [ $_added__ -ne 0 ]; then
	if [ "a$_result__" = "a" ]; then
	    _result__=$_result__"A:$_added__"
	else
	    _result__=$_result__" A:$_added__"  
	fi
    fi
    if [ $_deleted__ -ne 0 ]; then
	if [ "a$_result__" = "a" ]; then
	    _result__=$_result__"D:$_deleted__"
	else
	    _result__=$_result__" D:$_deleted__"
	fi
    fi
    if [ $_untrack__ -ne 0 ]; then
	if [ "a$_result__" = "a" ]; then
	    _result__=$_result__"?:$_untrack__"
	else
	    _result__=$_result__" ?:$_untrack__"
	fi
    fi
															       
    echo $_result__
}

PS1="\[\033[33m\]\u\[\033[00m\]@\[\033[35m\]\h\[\033[00m\] \w\[\033[32m\]\$(parse_git_branch)\[\033[00m\]\$(count_untracked) $ "

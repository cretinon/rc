# ~/.bashrc: executed by bash(1) for non-login shells.

# This is JCRE' .bashrc
# * to get all rc files : "get_last_rc" or "wget -q https://github.com/cretinon/rc/archive/refs/heads/main.tar.gz -O - | tar -zxvf - -C /tmp/"
# * to clone rc repo : "clone_rc" or "git clone https://github.com/cretinon/rc.git"

export LS_OPTIONS='--color=auto'
eval "$(dircolors)"
alias ls='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -l'
alias l='ls $LS_OPTIONS -lAi'

alias df='df -h'

alias grep='grep --color $@ 2>/dev/null'

alias rc='source ~/.bashrc'
alias ra='source ~/.aliases'

alias my_ip='curl ipinfo.io/ip && echo ""'
alias drop_cache='sync; echo 3 > /proc/sys/vm/drop_caches'
alias get_last_rc='wget -q https://github.com/cretinon/rc/archive/refs/heads/main.tar.gz -O - | tar -zxvf - -C /tmp/'
alias clone_rc='git clone https://github.com/cretinon/rc.git'

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
    _untrack__=$(git status --porcelain 2> /dev/null | grep "^??" | wc -l)

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

_my_wrap() {
    # global var for debugging purpose
    VERBOSE=false
    DEBUG=false
    FUNC_LIST=()

    CUR_DIR="/root/git/shell"
    CUR_NAME="main"

    unalias grep

    # load conf files and libs
    source $CUR_DIR/lib/lib_dev.sh

    _load_conf "$CUR_DIR/conf/$CUR_NAME.conf"

    # process options
    if _process_opts "$@" ; then
	_load_libs 
	# process previous options in LIB user choose
	if _exist $LIB; then _process_lib_$LIB "$OPTS"; fi
    fi

    alias grep='grep --color $@ 2>/dev/null'
}

PS1="\[\033[33m\]\u\[\033[00m\]@\[\033[35m\]\h\[\033[00m\] \w\[\033[32m\]\$(parse_git_branch)\[\033[00m\]\$(count_untracked) $ "

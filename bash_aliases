alias frequency='sort | uniq -c | sort -g'
alias cmd_frequency='history | cut -c8- | sort | uniq -c | sort -rn | head'
alias glog='git log --pretty=format:"%h - %an, [%ar] : %s" --graph'
alias clip='xsel --clipboard'
alias note='vim /home/lukasz/Dropbox/documents/journal/08_2012.markdown'
alias gulp6='node --harmony `which gulp`'
alias randhash='date +%s | sha256sum | base64 | head -c 32 ; echo'

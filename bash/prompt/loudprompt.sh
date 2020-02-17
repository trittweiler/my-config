#!/bin/bash
##########################################################
#Please edit "User Configuration" section before using   #
##########################################################

#=========================================================
#Terminal Color Codes
#=========================================================
source "$(dirname "${BASH_SOURCE}")"/default_colors

#=========================================================
# User Configuration
#=========================================================
# Colors
cLINES=$GRAY #Lines and Arrow
cBRACKETS=$GRAY # Brackets around each data item
cERROR=$LIGHTRED # Error block when previous command did not return 0
cTIME=$LIGHTGRAY # The current time
cMPX1=$YELLOW # Color for terminal multiplexer threshold 1
cMPX2=$RED # Color for terminal multiplexer threshold 2
cBGJ1=$YELLOW # Color for background job threshold 1
cBGJ2=$RED # Color for background job threshold 2
cSTJ1=$YELLOW # Color for background job threshold 1
cSTJ2=$RED # Color for  background job threshold 2
cSSH=$PINK # Color for brackets if session is an SSH session
cUSR=$LIGHTBLUE # Color of user
cUHS=$GRAY # Color of the user and hostname separator, probably '@'
cHST=$GREEN # Color of hostname
cRWN=$RED # Color of root warning
cPWD=$BLUE # Color of current directory
cCMD=$DEFAULT # Color of the command you type
cGIT=$CYAN # Color of git branch
cVENV=$PINK

# Enable block
eNL=1  # Have a newline between previous command output and new prompt
eERR=1 # Previous command return status tracker
eTIM=1 # Time block
eMPX=1 # Terminal multiplexer tracker enabled
eSSH=1 # Track if session is SSH
eBGJ=1 # Track background jobs
eSTJ=1 # Track stopped jobs
eUSH=1 # Show user and host
ePWD=1 # Show current directory
eGIT=1 # Show git branch
eVENV=1 # show python virtualenv

# Block settins
MPXT1=0 # Terminal multiplexer threshold 1 value
MPXT2=2 # Terminal multiplexer threshold 2 value
BGJT1=0 # Background job threshold 1 value
BGJT2=2 # Background job threshold 2 value
STJT1=0 # Stopped job threshold 1 value
STJT2=2 # Stopped job threshold 2 value
UHS="@"

if [ -n "${OVERRIDE_HOSTNAME}" ]; then
    HOSTNAME="${OVERRIDE_HOSTNAME}"
fi

function promptcmd()
{
        PREVRET=$?
        #=========================================================
        #check if user is in ssh session
        #=========================================================
        if [[ $SSH_CLIENT ]] || [[ $SSH2_CLIENT ]]; then
                lSSH_FLAG=1
        else
                lSSH_FLAG=0
        fi

        #=========================================================
        # Insert a new line to clear space from previous command
        #=========================================================
        if [ $eNL -eq 1 ] ; then
                PS1="\n"
        fi

        #=========================================================
        # Beginning of first line (arrow wrap around and color setup)
        #=========================================================
        PS1="${PS1}${cLINES}\342\224\214\342\224\200"

        #=========================================================
        # First Dynamic Block - Previous Command Error
        #=========================================================
        if [ $eERR -eq 1 ] && [ $PREVRET -ne 0 ] ; then
                PS1="${PS1}${cBRACKETS}[${cERROR}:(${cBRACKETS}]${cLINES}\342\224\200"
        fi

        #=========================================================
        # First static block - Current time
        #=========================================================
        if [ $eTIM -eq 1 ] ; then
                PS1="${PS1}${cBRACKETS}[${cTIME}\t${cBRACKETS}]${cLINES}\342\224\200"
        fi

        #=========================================================
        # Detached Screen Sessions
        #=========================================================
        if [ $eMPX -eq 1 ] ; then
                hTMUX=0
                hSCREEN=0
                MPXC=0
                hash tmux --help 2>/dev/null || hTMUX=1
                hash screen --version 2>/dev/null || hSCREEN=1
                if [ $hTMUX -eq 0 ] && [ $hSCREEN -eq 0 ] ; then
                        MPXC=$(echo "$(screen -ls | grep -c -i detach) + $(tmux ls 2>/dev/null | grep -c -i -v attach)" | bc)
                elif [ $hTMUX -eq 0 ] && [ $hSCREEN -eq 1 ] ; then
                        MPXC=$(tmux ls 2>/dev/null | grep -c -i -v attach)
                elif [ $hTMUX -eq 1 ] && [ $hSCREEN -eq 0 ] ; then
                        MPXC=$(screen -ls | grep -c -i detach)
                fi
                if [[ $MPXC -gt $MPXT2 ]] ; then
                        PS1="${PS1}${cBRACKETS}[${cMPX2}\342\230\220:${MPXC}${cBRACKETS}]${cLINES}\342\224\200"
                elif [[ $MPXC -gt $MPXT1 ]] ; then
                        PS1="${PS1}${cBRACKETS}[${cMPX1}\342\230\220:${MPXC}${cBRACKETS}]${cLINES}\342\224\200"
                fi
        fi

        #=========================================================
        # Backgrounded running jobs
        #=========================================================
        if [ $eBGJ -eq 1 ] ; then
                BGJC=$(jobs -r | wc -l )
                if [ $BGJC -gt $BGJT2 ] ; then
                        PS1="${PS1}${cBRACKETS}[${cBGJ2}&:${BGJC}${cBRACKETS}]${cLINES}\342\224\200"
                elif [ $BGJC -gt $BGJT1 ] ; then
                        PS1="${PS1}${cBRACKETS}[${cBGJ1}&:${BGJC}${cBRACKETS}]${cLINES}\342\224\200"
                fi
        fi

        #=========================================================
        # Stopped Jobs
        #=========================================================
        if [ $eSTJ -eq 1 ] ; then
                STJC=$(jobs -s | wc -l )
                if [ $STJC -gt $STJT2 ] ; then
                        PS1="${PS1}${cBRACKETS}[${cSTJ2}\342\234\227:${STJC}${cBRACKETS}]${cLINES}\342\224\200"
                elif [ $STJC -gt $STJT1 ] ; then
                        PS1="${PS1}${cBRACKETS}[${cSTJ1}\342\234\227:${STJC}${cBRACKETS}]${cLINES}\342\224\200"
                fi
        fi

        #=========================================================
        # Second Static block - User@host
        #=========================================================
        # set color for brackets if user is in ssh session
        if [ $eSSH -eq 1 ] && [ $lSSH_FLAG -eq 1 ] ; then
                sesClr="$cSSH"
        else
                sesClr="$cBRACKETS"
        fi
        # don't display user if root
        if [ $EUID -eq 0 ] ; then
                PS1="${PS1}${sesClr}[${cRWN}!"
        else
                PS1="${PS1}${sesClr}[${cUSR}\u${cUHS}${UHS}"
        fi
        PS1="${PS1}${cHST}${HOSTNAME}${sesClr}]${cLINES}\342\224\200"

        #=========================================================
        # Third Static Block - Current Directory
        #=========================================================
        if [ $ePWD -eq 1 ] ; then
                PS1="${PS1}[${cPWD}\w${cBRACKETS}]"
        fi

        if [ $eGIT -eq 1 ]; then
            local branch="$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')"
            if [ -n "${branch}" ]; then
                PS1="${PS1}${cLINES}\342\224\200${cBRACKETS}(${cGIT}${branch}${cBRACKETS})"
            fi
        fi

        if [ $eVENV -eq 1 ]; then
            if test -n "$VIRTUAL_ENV" ; then
                local venv="$(basename "$VIRTUAL_ENV")"
                PS1="${PS1}${cLINES}\342\224\200${cBRACKETS}(${cVENV}${venv}${cBRACKETS})"
            fi
        fi

        #=========================================================
        # Second Line
        #=========================================================
        PS1="${PS1}\n${cLINES}\342\224\224\342\224\200\342\224\200> ${cCMD}"
}

function load_prompt () {
    # Get PIDs
    # local parent_process=$(cat /proc/$PPID/cmdline | cut -d \. -f 1)
    # local my_process=$(cat /proc/$$/cmdline | cut -d \. -f 1)

    # if  [[ $parent_process == script* ]]; then
    #     PROMPT_COMMAND=""
    #     PS1="\t - \# - \u@\H { \w }\$ "
    # elif [[ $parent_process == emacs* || $parent_process == xemacs* ]]; then
    #     PROMPT_COMMAND=""
    #     PS1="\u@\h { \w }\$ "
    # else
    #     PROMPT_COMMAND=promptcmd
    #  fi

    PROMPT_COMMAND=promptcmd
    export PS1 PROMPT_COMMAND
}

load_prompt

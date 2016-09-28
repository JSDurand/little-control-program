#!/bin/bash
function fim() { /usr/bin/find ~/Music/iTunes/iTunes\ Media/Music/ -iname "$1"; }

function fpm() { /usr/bin/find ~/Music/iTunes/iTunes\ Media/Music/ -iname "$1" -execdir mpv --loop=inf {} \;; }

ydlc="/Users/durand/Desktop/Centre/Musique/commands/youtube-dl-shell.command";
ltmc="/Users/durand/Desktop/Centre/Musique/commands/listen_to_musique.command";
udmc="/Users/durand/Desktop/Centre/Musique/commands/update-musique.command";
rdrc="/Users/durand/Desktop/Centre/Reminder/Reminder.org";
emc="/usr/local/Cellar/emacs/24.5/bin/emacs";
continue="true";
while [ "$continue" == "true" ]; do
    tput clear;
    tput cup 0 10;
    echo "Choose a function.";
    echo "          The correspondence follows:";
    list=( "Listen: l" "Download: d" "Update: u" "Reminder: r" "Exit: e" "Romans: ro" "htop: h" "wifi on/off: wo/wf" "subilme text: su" "safari: sf" "mpsyt: m" "open an application: o" "fim: fi" "fpm: fp" );
    for mes in "${list[@]}"; do
	echo "          $mes";
    done;
    read ans;
    case "$ans" in
	"l") source "$ltmc";;
	"d") source "$ydlc";;
	"u") source "$udmc";;
	"r") "$emc" "$rdrc";;
	"ro") python Desktop/Centre/Romans/Parsing.py;;
	"h") htop;;
	"wo") networksetup -setairportpower en0 on;;
	"wf") networksetup -setairportpower en0 off;;
	"su") subl;;
	"o") echo "Enter the name of application:";
		 read name;
		 open -a "$name";;
	"sf") open -a safari;;
	"fi") echo "Enter search keyword:";
		  read name;
		  fim "*$name*";;	
  	"fp") echo "Enter search keyword:";
		  read name;
		  fpm "*$name*";;
	"m") mpsyt;;
	"e") continue="false";;
    esac;
done;
return;

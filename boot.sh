#!/bin/bash                                                                     

# start the subdomain app in the background from within screen                  

cd `dirname $0`

screen -d -m -S subdomain -c screenrc

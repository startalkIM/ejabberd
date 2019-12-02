#!/bin/sh                                                                                                                                                                                       

pd="qchat/$(date +%Y-%-m-%-d)"
mkdir -p "$pd"
sudo cp -rf /home/q/ejabberd1609/lib "$pd"
sudo cp -rf /home/q/ejabberd1609/etc/ejabberd/ejabberd.yml "$pd"
sudo cp -rf /home/q/ejabberd1609/etc/ejabberd/ejabberdctl.cfg "$pd"


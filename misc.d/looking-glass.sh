#!/bin/sh
sudo setfacl -m u:pluser:rw /dev/shm/looking-glass
~/.local/bin/looking-glass-client -k egl:scale=2 egl:vsync=yes input:rawMouse=yes /dev/shm/looking-glass

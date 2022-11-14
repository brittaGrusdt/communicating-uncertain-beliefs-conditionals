#!/bin/bash

# code taken from this Stack Overflow post:
# https://askubuntu.com/questions/922098/how-to-non-interactively-take-screenshot-of-a-predefined-area-and-save-it-automa

# Change these values to match your preferences
imageQuality=100    # scrot default is 75
screenshotDir="$HOME/Pictures/blocks"
# imageName="$(date +%Y-%m-%d.%H:%M:%S.%N).jpg"   # save image names as timestamp
# imageName="if2_$(date +%M-%S).jpg"
imageName="color_vision_red.png"
left=0     # begin crop this number of pixels from the left of the image
top=5      # begin crop this number of pixels from the top of the image
width=820   # crop this many pixels wide
height=450  # crop this many pixels tall

#Do not make any more changes from here down unless you know what you're doing
imagePath="$screenshotDir/$imageName"

scrot -q $imageQuality "$imagePath"
convert "$imagePath" -crop ${width}x${height}+${left}+${top} "$imagePath"

convert -resize -80% "$imagePath" "$imagePath"

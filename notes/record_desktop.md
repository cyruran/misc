```
ffmpeg -framerate 25 -f x11grab -pix_fmt yuv420p -i :1 /tmp/output.mp4
```

mplayer can play it without `-pix_fmt yuv420p`, win***s plays can't work without it.

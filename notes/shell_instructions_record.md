Requirements:
- scrot
- xbindkeys
- ImageMagick

Steps:
- Disable scrollbar in terminal emulator
- Create workdir, e.g.:
`mkdir /tmp/wrk`
- Bind keys to call scrot:
```
:~$ cat .xbindkeysrc
"scrot -u /tmp/wrk/%T.png"
      Shift+Mod2+Mod4 + s

:~$ xbindkeys -n
```
- Make screenshots. Check them up and cleanup some if needed.
- (optional) Rename screenshots:
```
cd /tmp/wrk
i=0
for j in *; do
  ((++i))
  mv $j $(printf "%08d" $i).png
done
```
- Trim:
```
mogrify -trim *.png
```

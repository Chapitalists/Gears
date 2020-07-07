Gears / multitouch (as pointer-events)

---

1 finger gestures

touch + release = click

touch + hold = click hold => solo ("s" keymode)

touch + move = drag (of touch element/zone) => move/pan (with "d" keymode or "editor" toolmode)


2 fingers gestures

touch + move + touch = drag in (from 1st to 2nd element) => play-link, harmonize-link, alternate (with "a" keymode)

double touch + release = "ctrl" click (on 1st touch) => multiselect (with "editor" toolmode)

double touch + move = (special) drag (of 1st touch element/zone) => move/pan (as "d" keymode)


3 fingers gestures

double touch + move + touch = drag and drop (of 1st element from 1st to 2nd zone) => copy to/from packzone, import to/from waveform, …

tripletouch + release = "shift" click (on 1st touch) => useless for now !

tripletouch + move = (very special) drag (of 1st touch element/zone) => volume/zoom (as "z" keymode)


---

Gears / iOS ugly ones

sound preview = no html.audio simple 'autoplay'

needed safari "advanced preferences" = experimental features / 'pointer-events' + 'resizeObserver'

---

Gears / install


elm make --output=elmApp.js src/Main.elm

npm i

node index.js

http://127.0.0.1:3000
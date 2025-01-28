/*
Copyright ou © ou Copr. Clément Bossut, (2024)
<bossut.clement@gmail.com>
*/


// TODOS
// slider for scheduler lookahead



const file = getId('file')
    , fr = new FileReader

let audio = getId('audio')
  , model = {}
  , buffers = {}
  , soundPlayer = null
  , pupil = {}
  , controls = getId("controls")


file.onchange = e => {
  ctx.resume()

  fr.readAsDataURL(file.files[0])
}

fr.onloadend = e => {
  audio.setAttribute('src', fr.result)
  let array = dataURLtoBuffer(fr.result)
  ctx.decodeAudioData(array)
   .then(buf => {
     model.soundPath = "LESON"
     model.mute = false
     model.volume = true
     model.loopPercents = [0,1]
     model.length = buf.duration
     model.startPercent = 0
     model.id = 'CONTENT'
     model.view = true

     buffers[model.soundPath] = buf

     sound = ctx.createBufferSource()
     sound.buffer = buf
     sound.connect(ctx.destination)

     pupil.interval = buf.duration
     pupil.startPercent = 0
     pupil.id = 'PUPIL'
     pupil.mute = false
     pupil.volume = 1
     pupil.view = true
     pupil.pupil = model

     pupil.el = getId("PUPIL")
     pupil.outer = pupil.el.getElementsByClassName("outer")[0]
     // pupil.inner = pupil.el.getElementsByClassName("inner")[0]
     pupil.head = pupil.el.getElementsByClassName("head")[0]
     model.el = getId("CONTENT")
     model.outer = model.el.getElementsByClassName("outer")[0]
     // model.inner = model.el.getElementsByClassName("inner")[0]
     model.head = model.el.getElementsByClassName("head")[0]

     pupil.chgD = model.chgD = function(n) {
       if (this.interval) this.interval = n
       if (this.length) this.length = n
       this.outer.setAttribute("r", n.toString())
       // pupil.inner.setAttribute("r", n.toString())
       this.outer.setAttribute("stroke-width", (n/30).toString())
       this.head.setAttribute("width", (n/30).toString())
       this.head.setAttribute("height", (n/15).toString())
       this.head.setAttribute("x", (n/60).toString())
       this.head.setAttribute("y", (n/30).toString())
       this.head.setAttribute("transform", "translate(0 "+(-(n+3*n/30)).toString()+")")
     }

     pupil.chgD(pupil.interval)
     model.chgD(pupil.interval)

     addSlider('Interval', .01, 10, .01, buf.duration, n => pupil.chgD(n), controls)
     addSlider('Length', .01, 10, .01, buf.duration, n => model.chgD(n), controls)
     // addSlider('Start Pupil', 0, 1, .1, 0
     //   , n => pupil.startPercent = n)
     addButton('Start', () => scheduler.startThenPlay([pupil]), controls)
     addButton('Pause', () => scheduler.playPause([pupil]), controls)
     addButton('Stop', () => scheduler.stop(), controls)
   })
}

function debug(o) {
  getId('debug').innerHTML = o instanceof Object ? Object.keys(o) : Array.from(arguments).join(' ')
}

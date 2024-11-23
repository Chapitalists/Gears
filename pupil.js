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


file.onchange = e => {
  c.resume()

  fr.readAsDataURL(file.files[0])
}

fr.onloadend = e => {
  audio.setAttribute('src', fr.result)
  ctx.decodeAudioData(dataURLtoBuffer(fr.result))
   .then(buf => {
     model.soundPath = "LESON"
     buffers[model.soundPath] = buf
     sound = ctx.createBufferSource()
     sound.buffer = buf
     sound.connect(ctx.destination)
   })
}

function debug(o) {
  getId('debug').innerHTML = o instanceof Object ? Object.keys(o) : Array.from(arguments).join(' ')
}

/*
Copyright ou © ou Copr. Clément Bossut, (2024)
<bossut.clement@gmail.com>
*/

const file = getId('file')
    , c = new AudioContext
    , fr = new FileReader

let audio = getId('audio')
  , sound = c.createMediaElementSource(audio)

sound.connect(c.destination)

// c.audioWorklet.addModule('scratch-player.js').then(() => {
//   sound = new AudioWorkletNode(c, 'scratch-player')
// })

file.onchange = e => {
  c.resume()

  fr.readAsDataURL(file.files[0])
 // fr.readAsArrayBuffer(file.files[0])
}

fr.onloadend = e => {
  audio.setAttribute('src', fr.result)
  // c.decodeAudioData(fr.result)
  // .then(buf => 
  // {
  //   sound.port.postMessage(buf.getChannelData())
    
  //   sound = c.createBufferSource()
  //   sound.buffer = buf
  //   sound.loop = true
  //   sound.playbackRate.value = 0
  //   sound.connect(c.destination)
  //   slider.disabled = false
  // })
}

function debug(o) {
  getId('debug').innerHTML = o instanceof Object ? Object.keys(o) : Array.from(arguments).join(' ')
}


let urlTMP = window.location.href.split('/')
urlTMP.pop()
let baseUrl = urlTMP.join('/')

let sons = ['boom1.wav','boom2.wav','tchi1.wav','tchi2.wav']

addButton('PLAY', ()=>sound.start())
let slider = document.createElement('input')
slider.disabled = true
slider.setAttribute('type', 'range')
slider.setAttribute('min', '-10')
slider.setAttribute('max', '10')
slider.setAttribute('step', '.1')
slider.setAttribute('value', '0')
slider.oninput = e => audio.playbackRate = slider.valueAsNumber
body.appendChild(slider)

class ScratchPlayerNode extends AudioWorkletNode {
  constructor(context) {
    super(context, 'scratch-player')
  }
}



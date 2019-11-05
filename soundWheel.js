/*
Copyright ou © ou Copr. Clément Bossut, (2018)
<bossut.clement@gmail.com>
*/

const soundWheelProto = {
  soundPath = '/sons/sound.wav'
, soundPlayer = null
, playRate = 1
, startPos = 0 // ms
, playing = false

, playPause = () => {
  if ( playing ) soundPlayer.stop()
  else soundPlayer.play()
}

}

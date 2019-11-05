/*
Copyright ou © ou Copr. Clément Bossut, (2018)
<bossut.clement@gmail.com>
*/

const body = document.getElementsByTagName('body')[0]

function getId(id) {return document.getElementById(id)}
function getClasses(cl) {return document.getElementsByClassName(cl)}
function addClass(el, cl) {el.classList.contains(cl)?null:el.classList.add(cl)}
function rmClass(el, cl) {el.classList.remove(cl)}

function addButton(name, fn, parent = body) {
  let bt = document.createElement('button')
  bt.innerHTML = name
  bt.onclick = () => fn()
  parent.appendChild(bt)
  return bt
}

function dist(x,y) {return Math.sqrt(Math.pow(x,2)+Math.pow(y,2))}

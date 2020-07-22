// ==UserScript==
// @name     youtube useful stuff
// @version  1
// @grant    none
// @include  https://www.youtube.com/*
// ==/UserScript==

function x() {
  function addSlides() {
    var v = document.getElementsByTagName("video")[0];
    var c = v.parentElement.parentElement.parentElement.parentElement.parentElement.parentElement.parentElement.parentElement;

    var input_info = [
      {
        min: 1.0,
        max: 4.0,
        step: 0.1,
        get: x => x.playbackRate,
        set: (x, y) => x.playbackRate = y
      },
      {
        min: 0.0,
        max: 1.0,
        step: 0.01,
        get: x => x.volume,
        set: (x, y) => x.volume = y,
      }
    ];

    input_info.forEach(x => {
      var s = document.createElement("input");
      s.type = "range";
      s.min = x.min;
      s.max = x.max;
      s.step = x.step;
      
      s.value = x.get(v);

      var n = document.createElement("span");
      n.textContent = s.value;

      s.onchange = s.onpointerchange = s.onpointermove = () => {
        x.set(v, s.value);
        console.log(s.value);
        n.textContent = s.value;
      };

      c.appendChild(s);
      c.appendChild(n);
    });
    
    var t = document.createElement("input");
    t.type = "checkbox";
    t.checked = true;
   	t.onchange = () => v.hidden = !t.checked;
    c.appendChild(t);
  }

  var b = document.createElement("button");
  b.textContent = "+";
  b.onclick = addSlides;
  b.style.position = "fixed";
  b.style.left = "200px";
  b.style.top = "10px";
  b.style.zIndex = 3000;
  document.body.appendChild(b);
}

x();

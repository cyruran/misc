(() => {
    const mainHTML = `
<style>
  #cm_advanced_video_control * {
      all: revert;
  }

  #cm_advanced_video_control {
      position: fixed;
      color: black;
      font-size: 12px;
      font-family: sans-serif;
      z-index: 999999;
      user-select: none;
      background-color: white;
      border: black solid 1px;
      top: 0;
      left: 0;
  }

  #cm_advanced_video_control input {
      display: inline-block;
  }

  #cm_advanced_video_control button {
      border-width: 1px;
      border-style: solid;
      border-radius: 10%;
      background-color: #eeeeee;
      min-width: 20px;
      min-height: 15px;
      margin: 1px;
  }

  #cm_advanced_video_control button:hover {
      background-color: #cccccc;
  }

  #cm_advanced_video_control input[type=range] {
      appearance: auto;
      width: 85px;
  }

  #cm_advanced_video_control input[type=text] {
      width: 4rem;
  }

  #mover {
      background: #999;
      cursor: move;
      padding-bottom: 2px;
  }

  .container {
      margin: 2px !important;
      padding: 2px !important;
      border: gray dotted 1px !important;
  }

  #cm_advanced_video_control.vertical .container {
      display: inline-block;
      vertical-align: top;
      border: none !important;
  }

  #cm_select {
      display: none;
  }

  .controls button, #mover button {
      font-size: 9px !important;
      min-width: 1px !important;
      margin: 0 !important;
  }
</style>

<div id="cm_advanced_video_control">
  <div id="mover">
    <button id="close">c</button>
    <button id="select">v</button>
    <button id="reset_position">r</button>
    <button id="orient">o</button>
  </div>
  <div id="playback_container" class="container">
    <div class="slider">
      <input type="range" min="1" max="5" step="0.1" id="playback_range" value="1">
      <output id="playback_val">1</output>
    </div>
    <div class="temp">
      <button>r</button>
      <input type="text">
      <button>t</button>
    </div>
  </div>
  <div id="volume_container" class="container">
    <div class="slider">
      <input type="range" min="0" max="1" step="0.01" id="volume_range" value="1">
      <output id="volume_val">1</output>
    </div>
    <div class="temp">
      <button>r</button>
      <input type="text">
      <button>t</button>
    </div>
  </div>
  <div id="skip_container" class="container">
    <div class="slider">
      <input type="range" min="0.1" max="60" step="0.1" id="skip_range" value="5">
      <output id="skip_val">5</output>
    </div>
    <div>
      <button id="pos_back">&lt;</button>
      <button id="pos_fwd">&gt;</button>
      <button id="toggle_pause">p</button>
    </div>
  </div>
  <div class="container">
    <button id="pip">pip</button>
    <input id="show_hide" type="checkbox" style="height: 20px; width: 20px;">
    <button>ha</button>
  </div>
  <div class="controls">
    <span id="time_left">00:00:00</span>
    <input type="hidden" id="_interval_id" value="1779">
  </div>
  <div id="cm_select">
  </div>
</div>
`;

    const mainId = "cm_main";

    let c = document.getElementById(mainId);
    let intervalId;

    function stopUpdate() {
        clearInterval(intervalId);
    }

    if (c)
        c.remove();
    else {
        createMain();
        selectVideo();
    }

    function selectVideo() {
        stopUpdate();
        const sel = c.querySelector("#cm_select");
        sel.innerHTML = "";
        const videoEls = document.querySelectorAll("video");
        if (videoEls.length > 2) {
            videoEls.forEach((x, i) => {
                const el = document.createElement("button");
                el.textContent = i;
                el.onclick = () => {
                    bindVideo(x);
                    sel.style.display = "none";
                };
                sel.appendChild(el);
                sel.style.display = "block";
            });
        }
        else if (videoEls.length == 1) {
            bindVideo(videoEls[0]);
        }
    }

    function bindVideo(v) {
        stopUpdate();

        const playbackInput = c.querySelector("#playback_container .slider input");
        playbackInput.additionalAction = val => { v.playbackRate = val };
        playbackInput.value = v.playbackRate;
        playbackInput.oninput();

        const volumeInput = c.querySelector("#volume_container .slider input");
        volumeInput.additionalAction = val => { v.volume = val };
        volumeInput.value = v.volume;
        volumeInput.oninput();

        c.querySelector("#pos_back").onclick = () => { v.currentTime -= c.querySelector("#skip_container .slider input").value };
        c.querySelector("#pos_fwd").onclick = () => { v.currentTime -= -c.querySelector("#skip_container .slider input").value };
        c.querySelector("#toggle_pause").onclick = () => { v.paused ? v.play() : v.pause() };
        c.querySelector("#pip").onclick = () => { v.requestPictureInPicture() };
        c.querySelector("#show_hide").onchange = function() { v.hidden = this.checked; };
        intervalId = setInterval(
            () => {
                c.querySelector("#time_left").textContent = new Date((v.duration - v.currentTime) / v.playbackRate * 1000).toISOString().substr(11, 8);
            }, 1000);
        cleanup = () => {v.ontimeupdate = null};
    }

    function createMain() {
        c = document.createElement("div");
        c.id = mainId;
        c.innerHTML = mainHTML;
        document.body.appendChild(c);

        c.querySelector("#mover").onmousedown = e => {
            const sX = e.clientX;
            const sY = e.clientY;

            const vc = c.querySelector("#cm_advanced_video_control");

            const iT = vc.offsetTop;
            const iL = vc.offsetLeft;

            document.onmousemove = e => {
                const offX = e.clientX - sX;
                const offY = e.clientY - sY;
                vc.style.top = `${iT + offY}px`;
                vc.style.left = `${iL + offX}px`;
            };

            document.onmouseup = () => {
                document.onmousemove = null;
                document.onmouseup = null;
            };
        };

        c.querySelectorAll(".slider").forEach(x => {
            const r = x.querySelector("input");
            r.oninput = () => {
                x.querySelector("output").textContent = parseFloat(r.value).toFixed(2);
                if (r.additionalAction)
                    r.additionalAction(r.value);
            };
            x.querySelector("output").textContent = parseFloat(r.value).toFixed(2);
            r.onwheel = e => {
                const y = e.deltaY;
                if (y > 0)
                    r.value -= r.step;
                else if (y < 0) {
                    r.value = parseFloat(r.value) + parseFloat(r.step);
                }
                r.oninput();
                e.preventDefault();
            };

            const i = x.parentElement.querySelector(".temp input");
            if (i) {
                i.value = r.value;
                i.onwheel = e => {
                    let s = parseFloat(r.step);
                    if (e.deltaY < 0) {
                        if (r.value <= r.min)
                            return;
                        s = -s;
                    }
                    i.value = (i.value - s).toFixed(2);
                    e.preventDefault();
                };
            }
        });

        c.querySelector("#close").onclick = () => { c.remove(); };
        c.querySelector("#select").onclick = () => { selectVideo(); };
        c.querySelector("#reset_position").onclick = () => {
            const vc = c.querySelector("#cm_advanced_video_control");
            vc.style.top = "0";
            vc.style.left = "0";
        };
        c.querySelector("#orient").onclick = () => { c.querySelector("#cm_advanced_video_control").classList.toggle("vertical"); };
    }
})();

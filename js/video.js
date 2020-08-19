function sliderOnWheel(e) {
    e.target.value -= (e.deltaY < 0 ? -1 : 1) * e.target.step;
    e.target.oninput();
    e.preventDefault();
}

function createSlider(id, min, max, step, initVal = min, handler = null, resetButton = false, buttons = false, slideClass = "cm_slider") {
    var rangeId = id + "_range";
    var valId = id + "_val";

    var container = document.createElement("td");
    container.id = id + "_container";
    container.classList.add(slideClass);

    var slideRange = document.createElement("input");
    slideRange.type = "range";
    slideRange.min = min;
    slideRange.max = max;
    slideRange.step = step;
    slideRange.id = rangeId;
    slideRange.value = initVal;
    container.appendChild(slideRange);

    var slideVal = document.createElement("output");
    slideVal.id = valId;
    slideVal.value = initVal;
    slideVal.style.display = "inline-block";
    slideVal.style.width = "2rem";
    slideVal.style.margin = "2px";
    container.appendChild(slideVal);

    if (handler) {
        slideRange.oninput = () => {
            handler(slideRange.value);
            slideVal.value = slideRange.value;
        }
    } else {
        slideRange.oninput = () => (slideVal.value = slideRange.value);
    }

    slideRange.onwheel = sliderOnWheel;

    container.appendChild(document.createElement("br"));

    if (resetButton) {
        var bReset = document.createElement("button");
        bReset.textContent = "r";
        bReset.style.marginRight = "1rem";
        bReset.onclick = () => { slideRange.value = initVal; slideRange.oninput(); };
        container.appendChild(bReset);
    }

    if (buttons) {
        var bPrev = document.createElement("button");
        bPrev.textContent = "<";
        bPrev.onclick = () => {
            slideRange.value = Number(slideRange.value) - Number(slideRange.step);
            slideRange.oninput();
        }
        container.appendChild(bPrev);

        var bNext = document.createElement("button");
        bNext.textContent = ">";
        bNext.onclick = () => {
            slideRange.value = Number(slideRange.value) + Number(slideRange.step);
            slideRange.oninput();
        }
        container.appendChild(bNext);
    }

    return [container, rangeId];
}

function createControls() {
    console.log("advanced_video_control");
    var video = document.getElementsByTagName("video")[0];

    var mainId = "cm_advanced_video_control";
    var cMain = document.getElementById(mainId);

    if (cMain) {
        cMain.remove();
        return;
    }

    cMain = document.createElement("table");
    cMain.id = mainId;
    cMain.style.zIndex = 3000;

    var coords = video.getBoundingClientRect();
    cMain.style.position = "absolute";
    cMain.style.left = `${coords.left}px`;
    cMain.style.top = `${coords.bottom}px`;
    cMain.style.backgroundColor = "white";
    cMain.style.borderColor = "black";
    cMain.style.borderStyle = "solid";
    cMain.style.borderWidth = "thin";
    cMain.style.margin = "5px";

    var row = document.createElement("tr");
    cMain.appendChild(row);

    var input_info = [
        {
            min: 1.0,
            max: 4.0,
            step: 0.1,
            get: x => x.playbackRate,
            set: (x, y) => { x.playbackRate = y },
            id: "playback",
            buttons: true,
            reset: true
        },
        {
            min: 0.0,
            max: 1.0,
            step: 0.01,
            get: x => x.volume,
            set: (x, y) => { x.volume = y },
            id: "volume",
            buttons: true,
            reset: true
        }
    ];

    var cSlider;
    var id;

    input_info.forEach((x) => {
        [cSlider, id] = createSlider(x.id, x.min, x.max, x.step, x.get(video), (y) => x.set(video, y), x.reset, x.buttons);
        row.appendChild(cSlider);
    });

    var skip = {
        min: 1,
        max: 60,
        step: 1,
        id: "skip",
        buttons: true,
        reset: false
    };

    [cSlider, id] = createSlider(skip.id, skip.min, skip.max, skip.step, 5, null, skip.reset, skip.buttons);
    row.appendChild(cSlider);
    [
        {
            textContent: "<",
            onclick: () => { video.currentTime -= document.getElementById(id).value }
        },
        {
            textContent: ">",
            onclick: () => { video.currentTime -= -document.getElementById(id).value }
        }
    ].map((x) => {
        var b = document.createElement("button");
        Object.keys(x).forEach((y) => { b[y] = x[y] } );
        return b;
    }).forEach((x) => row.appendChild(x));

    var hide = document.createElement("input");
    hide.type = "checkbox";
    hide.style.width = hide.style.height = "20px";
    row.appendChild(hide);

    var playPause = document.createElement("button");
    playPause.onclick = () => video.paused ? video.play() : video.pause();
    playPause.textContent = "p";
    row.appendChild(playPause);

    document.body.appendChild(cMain);
}

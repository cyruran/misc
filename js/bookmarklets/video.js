function parseTime(sec_num) {
    let hours   = Math.floor(sec_num / 3600);
    let minutes = Math.floor((sec_num % 3600) / 60);
    let seconds = sec_num % 60;

    if (hours   < 10) hours   = "0"+hours;
    if (minutes < 10) minutes = "0"+minutes;
    if (seconds < 10) seconds = "0"+seconds;
    return hours+':'+minutes+':'+seconds;
}

function sliderOnWheel(e) {
    e.target.value -= (e.deltaY < 0 ? -1 : 1) * e.target.step;
    e.target.oninput();
    e.preventDefault();
}

function createSlider(params) {
    let id = params.id;
    let min = params.min;
    let max = params.max;
    let step = params.step;

    let initVal = params.initVal || min;
    let handler = params.handler || null;
    let resetButton = params.resetButton || false;
    let buttons = params.buttons || false;
    let slideClass = params.slideClass || "cm_slider";

    let rangeId = id + "_range";
    let valId = id + "_val";

    let container = document.createElement("td");
    container.id = id + "_container";
    container.classList.add(slideClass);

    let slideRange = document.createElement("input");
    slideRange.type = "range";
    slideRange.min = min;
    slideRange.max = max;
    slideRange.step = step;
    slideRange.id = rangeId;
    slideRange.value = initVal;
    container.appendChild(slideRange);

    let slideVal = document.createElement("output");
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
        let bReset = document.createElement("button");
        bReset.textContent = "r";
        bReset.style.marginRight = "1rem";
        bReset.onclick = () => { slideRange.value = initVal; slideRange.oninput(); };
        container.appendChild(bReset);
    }

    if (buttons) {
        let bPrev = document.createElement("button");
        bPrev.textContent = "<";
        bPrev.onclick = () => {
            slideRange.value = Number(slideRange.value) - Number(slideRange.step);
            slideRange.oninput();
        };
        container.appendChild(bPrev);

        let bNext = document.createElement("button");
        bNext.textContent = ">";
        bNext.onclick = () => {
            slideRange.value = Number(slideRange.value) + Number(slideRange.step);
            slideRange.oninput();
        };
        container.appendChild(bNext);

        let tempVal = document.createElement("input");
        tempVal.value = initVal;
        tempVal.onwheel = e => {
            let up = e.deltaY < 0;
            tempVal.value = (parseFloat(tempVal.value) + (up ? step : -step)).toFixed(1);
            e.preventDefault();
        };
        tempVal.style.width = "2rem";
        tempVal.style.margin = "0 5px";
        container.appendChild(tempVal);

        if (handler) {
            let bTemp = document.createElement("button");
            bTemp.textContent = "t";
            bTemp.onmousedown = e => {
                handler(tempVal.value);
            };
            bTemp.onmouseup = e => {
                handler(slideRange.value);
            };
            container.appendChild(bTemp);
        }
    }

    return [container, rangeId];
}

function getVideo() {
    let videoList = document.querySelectorAll("video");

    if (videoList.length == 1) {
        createControls(videoList[0]);
        return;
    } else if (videoList.length == 0) {
        return;
    }

    let d = document.createElement("div");
    d.style.position = "fixed";
    d.style.left = "30%";
    d.style.top = "30%";
    d.style.background = "white";
    d.style.border = "1px solid black";
    d.style.padding = "10px";
    d.style.zIndex = "99999";
    let sel = document.createElement("select");
    sel.size = videoList.length;
    d.appendChild(sel);

    videoList.forEach((x, i) => {
        let option = document.createElement("option");
        option.textContent = i;
        option.value = i;
        sel.appendChild(option);
    });
    let commit = document.createElement("button");
    commit.textContent = "ok";
    commit.onclick = () => {
        let video = videoList[sel.value];
        d.remove();
        createControls(video, true);
    };
    d.appendChild(commit);
    document.body.appendChild(d);
}

function setTimeLeft(video) {
    let timeLeft = Math.floor((video.duration - video.currentTime) / video.playbackRate);
    timeSpan = document.getElementById("_time_left");
    timeSpan.textContent = parseTime(timeLeft);
}

function removeControls(cMain) {
    let intervalIdEl = document.getElementById("_interval_id");

    if (intervalIdEl) {
        let intervalId = intervalIdEl.value;
        clearInterval(intervalId);
    }

    cMain.remove();
}

function createControls(video, reset) {
    console.log("advanced_video_control");

    let mainId = "cm_advanced_video_control";
    let cMain = document.getElementById(mainId);

    if (cMain) {
        removeControls(cMain);
        if (!reset)
            return;
    }

    cMain = document.createElement("table");
    cMain.id = mainId;
    cMain.style.zIndex = 999999;
    cMain.style.userSelect = "none";

    let videoCoords = video.getBoundingClientRect();
    let bodyCoords = document.body.getBoundingClientRect();
    cMain.style.position = "absolute";
    cMain.style.left = `${videoCoords.left - bodyCoords.left}px`;
    cMain.style.top = `${videoCoords.bottom - bodyCoords.top + 5}px`;
    cMain.style.backgroundColor = "white";
    cMain.style.borderColor = "black";
    cMain.style.borderStyle = "solid";
    cMain.style.borderWidth = "thin";
    cMain.style.margin = "5px";

    let row = document.createElement("tr");
    cMain.appendChild(row);

    let input_info = [
        {
            min: 1.0,
            max: 5.0,
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

    let cSlider;
    let id;

    input_info.forEach((x) => {
        [cSlider, id] = createSlider({
            "id": x.id,
            "min": x.min,
            "max": x.max,
            "step": x.step,
            "initVal": x.get(video),
            "handler": (y) => x.set(video, y),
            "resetButton": x.reset,
            "buttons": x.buttons
        });
        row.appendChild(cSlider);
    });

    let skip = {
        min: 0.1,
        max: 60,
        step: 0.1,
        id: "skip",
        buttons: true,
        reset: false
    };

    [cSlider, id] = createSlider({
        id: skip.id,
        min: skip.min,
        max: skip.max,
        step: skip.step,
        initVal: 5,
        handler: null,
        resetButton: skip.reset,
        buttons: skip.buttons
    });
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
        let b = document.createElement("button");
        Object.keys(x).forEach((y) => { b[y] = x[y] } );
        return b;
    }).forEach((x) => row.appendChild(x));

    let playPause = document.createElement("button");
    playPause.onclick = () => video.paused ? video.play() : video.pause();
    playPause.textContent = "p";
    playPause.style.marginLeft = "5px";
    row.appendChild(playPause);

    row.appendChild(document.createElement("br"));

    let pip = document.createElement("button");
    pip.textContent = "pip";
    pip.onclick = () => video.requestPictureInPicture();
    row.appendChild(pip);

    let hide = document.createElement("input");
    hide.type = "checkbox";
    hide.style.width = hide.style.height = "20px";
    hide.onchange = e => { video.hidden = e.target.checked };
    row.appendChild(hide);

    let hAnnot = document.createElement("button");
    hAnnot.textContent = "ha";
    hAnnot.onclick = () => {document.querySelectorAll(".ytp-ce-element").forEach(x => x.hidden = !x.hidden)};
    row.appendChild(hAnnot);

    let timeSpan = document.createElement("span");
    timeSpan.id = "_time_left";

    let closeControls = document.createElement("button");
    closeControls.textContent = "c";
    closeControls.style.fontSize = "0.5rem";
    closeControls.onclick = () => removeControls(cMain);

    let intVal = document.createElement("input");
    intVal.type = "hidden";
    intVal.id = "_interval_id";

    row = document.createElement("tr");
    row.appendChild(timeSpan);
    row.appendChild(intVal);
    row.appendChild(closeControls);

    cMain.appendChild(row);

    document.body.appendChild(cMain);

    setTimeLeft(video);

    let intervalId = setInterval(() => setTimeLeft(video), 1000);
    intVal.value = intervalId;

    /*document.querySelectorAll(`#${mainId} button`).forEach(x => x.onmousedown = e => e.preventDefault());*/
    return;
}

function addStyles() {
    let styleId = "cm_style";
    if (document.getElementById(styleId)) {
        return;
    }

    let styles = `
    #cm_advanced_video_control {
        color: black;
        font-size: 12px;
        font-family: sans-serif;
    }

    #cm_advanced_video_control input {
        display: inline-block;
    }

    #cm_advanced_video_control button {
        border-width: 1px;
        border-style: solid;
        border-radius: 10%;
        background-color: #eeeeee;
    }

    #cm_advanced_video_control button:hover {
        background-color: #cccccc;
    }

    #cm_advanced_video_control input[type=range] {
        appearance: auto;
    }
`;

    let styleSheet = document.createElement("style");
    styleSheet.type = "text/css";
    styleSheet.id = styleId;
    styleSheet.innerText = styles;
    document.head.appendChild(styleSheet);
}

addStyles();
getVideo();

function parseTime(sec_num) {
    let hours   = Math.floor(sec_num / 3600);
    let minutes = Math.floor((sec_num % 3600) / 60);
    let seconds = sec_num % 60;

    if (hours   < 10) {hours   = "0"+hours;}
    if (minutes < 10) {minutes = "0"+minutes;}
    if (seconds < 10) {seconds = "0"+seconds;}
    return hours+':'+minutes+':'+seconds;
};

function sliderOnWheel(e) {
    e.target.value -= (e.deltaY < 0 ? -1 : 1) * e.target.step;
    e.target.oninput();
    e.preventDefault();
}

function createSlider(id, min, max, step, initVal = min, handler = null, resetButton = false, buttons = false, slideClass = "cm_slider") {
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
    }

    return [container, rangeId];
}

function setTimeLeft() {
    let video = document.getElementsByTagName("video")[0];
    let timeLeft = Math.floor((video.duration - video.currentTime) / video.playbackRate);
    timeSpan = document.getElementById("_time_left");
    timeSpan.textContent = parseTime(timeLeft);
}

function createControls() {
    console.log("advanced_video_control");
    let video = document.getElementsByTagName("video")[0];

    let mainId = "cm_advanced_video_control";
    let cMain = document.getElementById(mainId);

    if (cMain) {
        let intervalIdEl = document.getElementById("_interval_id");

        if (intervalIdEl) {
            let intervalId = intervalIdEl.value;
            clearInterval(intervalId);
        }

        cMain.remove();
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

    let cSlider;
    let id;

    input_info.forEach((x) => {
        [cSlider, id] = createSlider(x.id, x.min, x.max, x.step, x.get(video), (y) => x.set(video, y), x.reset, x.buttons);
        row.appendChild(cSlider);
    });

    let skip = {
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
        let b = document.createElement("button");
        Object.keys(x).forEach((y) => { b[y] = x[y] } );
        return b;
    }).forEach((x) => row.appendChild(x));

    let hide = document.createElement("input");
    hide.type = "checkbox";
    hide.style.width = hide.style.height = "20px";
    row.appendChild(hide);

    let playPause = document.createElement("button");
    playPause.onclick = () => video.paused ? video.play() : video.pause();
    playPause.textContent = "p";
    row.appendChild(playPause);

    let timeSpan = document.createElement("span");
    timeSpan.id = "_time_left";

    let intVal = document.createElement("input");
    intVal.type = "hidden";
    intVal.id = "_interval_id";
        
    row = document.createElement("tr");
    row.appendChild(timeSpan);
    row.appendChild(intVal);

    cMain.appendChild(row);

    document.body.appendChild(cMain);

    setTimeLeft();

    let intervalId = setInterval(setTimeLeft, 1000);
    intVal.value = intervalId;
}

createControls();

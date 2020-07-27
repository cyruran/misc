function createSlider(id, min, max, step, initVal = min, handler = null, buttons = false, slideClass = "cm_slider") {
    var rangeId = id + "_range";
    var valId = id + "_val";

    var container = document.createElement("span");
    container.id = id + "_container";
    container.classList.add(slideClass);

    var slideRange = document.createElement("input");
    slideRange.type = "range";
    slideRange.min = min;
    slideRange.max = max;
    slideRange.step = step;
    slideRange.id = rangeId;
    container.appendChild(slideRange);

    var slideVal = document.createElement("output");
    slideVal.id = valId;
    container.appendChild(slideVal);

    slideRange.oninput = () => {
        handler.call(slideRange.value);
        slideVal.value = slideRange.value;
    }

    if (buttons) {
        var bPrev = document.createElement("button");
        bPrev.textContent = "<";
        bPrev.onclick = () => {
            slideRange.value -= parseInt(slideRange.step);
        }
        container.appendChild(bPrev);

        var bNext = document.createElement("button");
        bNext.textContent = ">";
        bNext.onclick = () => {
            slideRange.value += parseInt(slideRange.step);
        }
        container.appendChild(bNext);
    }

    return [container, rangeId];
}

var video = document.getElementsByTagName("video");

mainId = "cm_advanced_video_control";
var cMain = document.getElementById(mainId);

if (cMain) {
    cMain.remove();
}

cMain = document.createElement("span");
cMain.id = mainId;

var input_info = [
    {
        min: 1.0,
        max: 4.0,
        step: 0.1,
        get: x => x.playbackRate,
        set: (x, y) => (x.playbackRate = y),
        id: "playback"
    },
    {
        min: 0.0,
        max: 1.0,
        step: 0.01,
        get: x => x.volume,
        set: (x, y) => (x.volume = y),
        id: "volume"
    }
];

input_info.forEach((x) => {
    var cSlider;
    var id;
    [cSlider, id] = createSlider(x.id, x.min, x.max, x.step, initVal = x.get(video), handler = (y) => x.set(video, y), buttons = true);
    cMain.appendChild(cSlider);
});



// ==UserScript==
// @name     youtube useful stuff
// @version  1
// @grant    none
// @include  https://www.youtube.com/*
// ==/UserScript==

function x() {
    function addSlides() {
        var t = document.getElementById("my_stuff");
        if (t) {
            t.remove();
        }

        var v = document.getElementsByTagName("video")[0];
        var c = v.parentElement.parentElement.parentElement.parentElement.parentElement.parentElement.parentElement.parentElement;
        var stuff = document.createElement("span");
        stuff.id = "my_stuff";
        c.appendChild(stuff);

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

        function set_value(x, value, v, n) {
            x.set(v, value);
            console.log(value);
            n.textContent = value;
        };

        var reset = document.createElement("button");
        reset.textContent = "r";
        stuff.appendChild(reset);

        input_info.forEach(x => {
            var container = document.createElement("span");
            container.id = x.id;

            var s = document.createElement("input");
            s.type = "range";
            s.min = x.min;
            s.max = x.max;
            s.step = x.step;

            s.value = x.get(v);

            var n = document.createElement("span");
            n.textContent = s.value;

            s.onchange = s.onpointerchange = s.onpointermove = () => set_value(x, s.value, v, n);

            container.appendChild(s);
            container.appendChild(n);

            stuff.appendChild(container);
        });

        var t = document.createElement("input");
        t.type = "checkbox";
        t.checked = true;
   	t.onchange = () => (v.hidden = !t.checked);
        stuff.appendChild(t);

        (function () {
            var x = input_info[0];
            var [s, n] = document.getElementById(x.id).children;
            reset.onclick = function() {
                set_value(x, 1, v, n);
                s.value = 1;
            }
        })();

        var skip = document.createElement("span");
        skip.id = "my_skip";
        stuff.appendChild(skip);

        var skip_val = document.createElement("input");
        skip_val.id = "skip_val_range";
        skip_val.type = "range";
        skip_val.min = 1;
        skip_val.max = 60;
        skip_val.step = 1;
        skip_val.value = 5;
        skip_val.oninput = () => (skip_val_txt.value = skip_val.value);
        skip.appendChild(skip_val);

        var skip_val_txt = document.createElement("output");
        skip_val_txt.id = "skip_val_txt";
        skip_val_txt.value = skip_val.value;
        skip.appendChild(skip_val_txt);

        var b_back = document.createElement("button");
        var b_forw = document.createElement("button");

        b_back.textContent = "<";
        b_forw.textContent = ">";

        b_back.style.margin = "5px";
        b_back.style.height = "30px";
        b_forw.style.margin = "5px";
        b_forw.style.height = "30px";

        b_back.onclick = () => (v.currentTime -= parseInt(skip_val.value));
        b_forw.onclick = () => (v.currentTime += parseInt(skip_val.value));

        skip.appendChild(b_back);
        skip.appendChild(b_forw);

        var refresh = document.createElement("button");
        refresh.textContent = "refresh";
        refresh.onclick = () => (window.location.href = window.location.href.replace(/&t=.*$/, "") + "&t=" + parseInt(v.currentTime));
        stuff.appendChild(refresh);
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

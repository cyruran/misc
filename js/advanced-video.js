// ==UserScript==
// @name     advanced video control
// @version  1
// @grant    none
// @include  https://*
// ==/UserScript==

function x() {
    var appendButtonId = "cm_add_slides";
    var b = document.createElement("button");
    b.onclick = createControls;
    b.style.position = "fixed";
    b.style.left = "0px";
    b.style.top = "0px";
    b.style.zIndex = 3000;
    function setDefaultSize() {
        b.style.width = b.style.height = "5px";
    }
    setDefaultSize();
    var tId;
    b.onmouseleave = () => { tId = setTimeout(setDefaultSize, 700); };
    b.onmouseover = () => {
        b.style.width = b.style.height = "30px";
        clearTimeout(tId);
    };
    b.id = appendButtonId;
    document.body.appendChild(b);
}

x();

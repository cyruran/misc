// ==UserScript==
// @name     advanced video control
// @version  1
// @grant    none
// @include  https://*
// ==/UserScript==

function x() {
    var appendButtonId = "cm_add_slides";
    var appendButtonDivId = "cm_addSlidesDiv";

    let old;
    [appendButtonId, appendButtonDivId].map(x => document.getElementById(x)).forEach(x => x && x.remove())

    var div = document.createElement("div");
    var b = document.createElement("button");
    div.appendChild(b);
    b.onclick = createControls;
    b.style.marginRight = "5px";
    b.style.marginBottom = "5px";
    div.style.position = "fixed";
    div.style.left = "0px";
    div.style.top = "0px";
    div.style.zIndex = 3000;
    function setDefaultSize() {
        b.style.width = b.style.height = "5px";
    }
    setDefaultSize();
    var tId;
    div.onmouseleave = setDefaultSize, 700;
    div.onmouseover = () => b.style.width = b.style.height = "30px";
    b.id = appendButtonId;
    div.id = appendButtonDivId;
    document.body.appendChild(div);
}

x();

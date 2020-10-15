function magicMenu() {
    var magicDiv = "cm_magicMenu";

    var css = `
#${magicDiv} {
  position: fixed;
  left: 0px;
  top: 0px;
  height: 10px;
  width: 10px;
  background-color: grey;
  z-index: 3000;
}

#${magicDiv}:hover {
  height: auto;
  width: auto;
}

#${magicDiv} * {
  display: none;
}

#${magicDiv}:hover * {
  display: block;
  margin: 10px;
}
`;
    var style = document.createElement("style");

    if (style.styleSheet) {
        style.styleSheet.cssText = css;
    } else {
        style.appendChild(document.createTextNode(css));
    }

    document.getElementsByTagName('head')[0].appendChild(style);
    
    let old;
    var x = document.getElementById(magicDiv);
    x && x.remove();

    var div = document.createElement("div");
    var tId;
    div.id = magicDiv;
    document.body.appendChild(div);
    return div;
}

function addButton(md, caption, hndlr) {
    var button = document.createElement("button");
    button.textContent = caption;
    md.appendChild(button);
    md.onclick = hndlr;
}

var md = magicMenu();
addButton(md, "test", () => {});
addButton(md, "test1", () => {});
addButton(md, "test2", () => {});
addButton(md, "test3", () => {});

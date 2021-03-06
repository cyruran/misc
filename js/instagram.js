document.querySelectorAll(".KL4Bh").forEach(x => {
    let a = document.createElement("a");
    a.href = x.querySelector("img").src;
    a.target = "_blank";
    a.style.position = "relative";
    a.style.zIndex = 9999;
    a.style.background = "white";
    a.textContent = "d";
    x.appendChild(a);
});

document.querySelectorAll("._5wCQW").forEach(x => {
    let a = document.createElement("a");
    a.href = x.querySelector("video").src;
    a.target = "_blank";
    a.style.position = "relative";
    a.style.zIndex = 9999;
    a.style.background = "white";
    a.textContent = "d";
    x.appendChild(a);
});

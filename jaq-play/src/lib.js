// Functions that are called from the Rust code.

export function output_newline() {
    const new_line = document.createElement("div");
    document.getElementById("output").appendChild(new_line);
}

export function output_tag(tag, type) {
    let span = document.createElement("span");
    if (type) {
        span.className = type;
    }
    span.textContent = tag;

    let last_line = document.getElementById("output").lastElementChild;
    last_line.appendChild(span);
}


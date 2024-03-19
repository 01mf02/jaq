// currently active jaq thread
let worker = initWorker();

function startWorker() {
    showRunButton(false);

    // remove previous output
    document.getElementById('output').replaceChildren();

    //console.log("Starting run in JS ...");
    const filter = document.getElementById('filter').value;
    const input = document.getElementById('input').value;
    worker.postMessage({filter, input});
}

function initWorker() {
    let worker = new Worker("./src/worker.js", { type: "module" });
    worker.onmessage = event => receiveFromWorker(event.data);
    return worker
}

function receiveFromWorker(data) {
    if (data == "") {
        showRunButton(true);
        return;
    }

    let div = document.createElement("div");
    div.innerHTML = syntaxHighlight(data);
    document.getElementById("output").appendChild(div);
}

function showRunButton(show) {
    const display = b => b ? "block" : "none";
    document.getElementById("run" ).style.display = display(show);
    document.getElementById("stop").style.display = display(!show);
}

// Taken from: <https://stackoverflow.com/a/7220510>
function syntaxHighlight(json) {
    json = json.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    return json.replace(/("(\\u[a-zA-Z0-9]{4}|\\[^u]|[^\\"])*"(\s*:)?|\b(true|false|null)\b|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?)/g, function (match) {
        var cls = 'number';
        if (/^"/.test(match)) {
            if (/:$/.test(match)) {
                cls = 'key';
            } else {
                cls = 'string';
            }
        } else if (/true|false/.test(match)) {
            cls = 'boolean';
        } else if (/null/.test(match)) {
            cls = 'null';
        }
        return '<span class="' + cls + '">' + match + '</span>';
    });
}

function stopWorker() {
    console.log("Stopping worker ...");
    showRunButton(true)
    worker.terminate();
    worker = initWorker();
}

document.getElementById("run").onclick = async () => startWorker();
document.getElementById("stop").onclick = async () => stopWorker();

document.addEventListener('keydown', event => {
    // CTRL + Enter
    if (event.ctrlKey && event.key == 'Enter') { startWorker() }
});

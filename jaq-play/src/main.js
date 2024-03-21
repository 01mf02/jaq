// currently active jaq thread
let worker = initWorker();

function startWorker() {
    showRunButton(false);

    // remove previous output
    document.getElementById('output').replaceChildren();

    //console.log("Starting run in JS ...");
    const filter = document.getElementById('filter').value;
    const input = document.getElementById('input').value;
    const settings = getSettings();
    worker.postMessage({filter, input, settings});
}

function getSettings() {
    const get = id => document.getElementById(id);
    const input = ["raw-input", "slurp", "null-input", "in-place"];
    const output = ["raw-output", "compact", "join-output", "tab"];
    var acc = {};
    input.concat(output).forEach(id => acc[id] = get(id).checked);
    acc["indent"] = get("indent").value;
    return acc
}

function initWorker() {
    let worker = new Worker("./src/worker.js", { type: "module" });
    worker.onmessage = event => receiveFromWorker(event.data);
    return worker
}

function receiveFromWorker(data) {
    if (data == null) {
        showRunButton(true);
        return;
    }

    let div = document.createElement("div");
    div.innerHTML = data;
    document.getElementById("output").appendChild(div);
}

function showRunButton(show) {
    const display = b => b ? "block" : "none";
    document.getElementById("run" ).style.display = display(show);
    document.getElementById("stop").style.display = display(!show);
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

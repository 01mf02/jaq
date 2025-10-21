// currently active jaq thread
let worker = initWorker();

const param_ids = Object.entries({'q': 'filter', 'j': 'input'});

function getParams() {
    const urlParams = new URLSearchParams(window.location.search);
    for (const [param, id] of param_ids) {
        const value = urlParams.get(param);
        if (value !== null) {
            document.getElementById(id).value = value;
        }
    }
}

function setParams() {
    const url = new URL(window.location)
    for (const [param, id] of param_ids) {
        url.searchParams.set(param, document.getElementById(id).value);
    }
    history.pushState(null, '', url);
}

function startWorker() {
    setParams();
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
    const cbxs = document.querySelectorAll(".settings input[type=checkbox]");
    const nums = document.querySelectorAll(".settings input[type=number]");
    var acc = {};
    cbxs.forEach(node => acc[node.id] = node.checked);
    nums.forEach(node => acc[node.id] = node.value);
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

document.getElementById("input" ).oninput = async () => startWorker();
document.getElementById("filter").oninput = async () => startWorker();

getParams();
startWorker();

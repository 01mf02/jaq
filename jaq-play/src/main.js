import init, {run} from "../pkg/jaq_play.js";

document.getElementById("run").onclick = async () => {
    // remove previous output
    document.getElementById('output').replaceChildren();

    console.log("Starting run in JS ..."); 
    const filter = document.getElementById('filter').value;
    const input = document.getElementById('input').value;
    await run(filter, input);
};

document.addEventListener('keydown', event => {
    // CTRL + Enter
    if (event.ctrlKey && event.key == 'Enter') {
        document.getElementById('run').click()
    }
});

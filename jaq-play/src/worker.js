import init, {run} from "../pkg/jaq_play.js";

init();

// Set callback to handle messages passed to the worker.
self.onmessage = async event => {
    const { filter, input, settings } = event.data;
    await init();
    await run(filter, input, settings, self);
};

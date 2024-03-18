import init, {run} from "../pkg/jaq_play.js";

// Set callback to handle messages passed to the worker.
self.onmessage = async event => {
    await init();
    const { filter, input } = event.data;
    await run(filter, input, self);
};

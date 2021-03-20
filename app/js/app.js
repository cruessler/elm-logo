import { Elm } from "../elm/Main.elm";

const params = new URLSearchParams(window.location.search);
const initialCommandLine = params.get("initialCommandLine") ?? null;

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: initialCommandLine,
});

const worker = new Worker(new URL("./worker.js", import.meta.url));

app.ports.sendCommand.subscribe((command) => worker.postMessage(command));

worker.addEventListener("message", (event) =>
  app.ports.receiveResult.send(event.data)
);

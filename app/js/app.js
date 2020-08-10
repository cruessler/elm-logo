import { Elm } from "../elm/Main.elm";

const app = Elm.Main.init({ node: document.getElementById("app") });

const worker = new Worker("./worker.js");

app.ports.sendCommand.subscribe((command) => worker.postMessage(command));

worker.addEventListener("message", (event) =>
  app.ports.receiveResult.send(event.data)
);

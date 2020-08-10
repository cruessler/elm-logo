import { Elm } from "../elm/Worker.elm";

const worker = Elm.Worker.init();

self.addEventListener("message", (event) =>
  worker.ports.receiveCommand.send(event.data)
);

worker.ports.sendResult.subscribe((result) => self.postMessage(result));

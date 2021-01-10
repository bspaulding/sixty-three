import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

function getSocketHost() {
  const { host, port, protocol } = window.location;
  if (port === "3001") {
    return `ws://${host.split(":")[0]}:3000`;
  } else {
    // host includes port, hostname does not, so no need to append port here
    return `${protocol === "https:" ? "wss" : "ws"}://${host}`;
  }
}

const socket = new WebSocket(getSocketHost());

const app = Elm.Main.init({
  node: document.getElementById("root")
});

app.ports.sendMessage.subscribe(function(message) {
  socket.send(message);
});

socket.addEventListener("message", function(event) {
  app.ports.receiveMessage.send(event.data);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

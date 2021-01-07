import ReactDOM from 'react-dom';
import React from 'react';
import App from './components/App.jsx';

const socket = new WebSocket('ws://localhost:3000');
socket.addEventListener('message', event => {
  console.log('got a message: ', event);
});
socket.addEventListener('close', event => {
  console.log('vanilla socket closed!', event);
});

ReactDOM.render(React.createElement(App), document.getElementById("app"));
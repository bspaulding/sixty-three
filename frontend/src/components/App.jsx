import React, { useMemo, useRef } from 'react';
import useWebSocket from '../useWebSocket.js';

const App = () => {
  const { socket, lastEvent } = useWebSocket('ws://localhost:3000');
  console.log({ lastEvent });

  const events = useRef([]);
  events.current = useMemo(() => lastEvent ? events.current.concat(lastEvent) : events.current, [lastEvent]);

  const messages = events.current
    .filter(({ type }) => type === 'message')
    .map((event) => event.event.data);

  return (
    <>
      <h1>Sixty Three in App.jsx!</h1>
      <ul>
        {messages.map((msg, i) => <li key={i}>{msg}</li>)}
      </ul>
    </>
  );
};

export default App;
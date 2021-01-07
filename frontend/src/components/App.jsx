import React, { useEffect, useMemo, useRef, useState } from 'react';

const websocketEvents = ['open', 'close', 'error', 'message'];
const useWebSocket = (url, protocols = []) => {
  console.log('useWebSocket');
  const socketRef = useRef(null);
  const [lastEvent, setLastEvent] = useState(null);

  useEffect(() => {
    console.log('useEffect');
    if (!socketRef.current) {
      console.log('constructing socket');
      socketRef.current = new WebSocket(url, protocols);
    }

    const handlers = websocketEvents.reduce((acc, type) => {
      return ({
        ...acc,
        [type]: (event) => {
          console.log(type, event);
          setLastEvent({ type, event });
        }
      });
    },
      {}
    );
    Object.entries(handlers).map(entry => {
      socketRef.current.addEventListener(entry[0], entry[1]);
    });

    return () => {
      Object.entries(handlers).map(entry => {
        socketRef.current.removeEventListener(entry[0], entry[1]);
      });
      socketRef.current.close();
    };
  }, [url]);

  return { socket: socketRef.current, lastEvent };
};

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
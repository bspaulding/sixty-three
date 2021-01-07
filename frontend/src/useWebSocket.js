import { useEffect, useRef, useState } from 'react';

const websocketEvents = ['open', 'close', 'error', 'message'];
const useWebSocket = (url, protocols = []) => {
  const socketRef = useRef(null);
  const [lastEvent, setLastEvent] = useState(null);

  useEffect(() => {
    if (!socketRef.current) {
      socketRef.current = new WebSocket(url, protocols);
    }

    const handlers = websocketEvents.reduce((acc, type) => {
      return ({
        ...acc,
        [type]: (event) => {
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

export default useWebSocket;
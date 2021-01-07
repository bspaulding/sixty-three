import React, { useMemo, useReducer, useRef } from 'react';
import useWebSocket from '../useWebSocket.js';
import reducer, { actions, initialState } from '../reducer.js';
import WebsocketDebugMonitor from './WebsocketDebugMonitor.js';

const App = () => {
  const { lastEvent } = useWebSocket('ws://localhost:3000');

  const events = useRef([]);
  events.current = useMemo(() => lastEvent ? events.current.concat(lastEvent) : events.current, [lastEvent]);

  const [state, dispatch] = useReducer(reducer, initialState);

  return (
    <>
      <h1>Sixty Three in App.jsx!</h1>
      <WebsocketDebugMonitor
        showEvents={state.showWsDebug}
        events={events.current}
        onToggle={dispatch.bind(null, actions.wsDebugToggle())}
      />
    </>
  );
};

export default App;
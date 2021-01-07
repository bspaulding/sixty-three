import React, { useMemo, useReducer, useRef } from 'react';
import useWebSocket from '../useWebSocket.js';
import reducer, { actions, initialState } from '../reducer.js';
import styles from './App.module.css';
import mapReverse from '../mapReverse';

const App = () => {
  const { lastEvent } = useWebSocket('ws://localhost:3000');

  const events = useRef([]);
  events.current = useMemo(() => lastEvent ? events.current.concat(lastEvent) : events.current, [lastEvent]);

  const [state, dispatch] = useReducer(reducer, initialState);

  return (
    <>
      <h1>Sixty Three in App.jsx!</h1>
      <button onClick={dispatch.bind(null, actions.wsDebugToggle())}>Toggle WebSocket Monitor</button>
      {!!state.showWsDebug && (
        <ul className={styles.wsDebugView}>
          {mapReverse((msg, i) => <li key={i}><pre>[{msg.type}] {JSON.stringify(msg.event.data)}</pre></li>)(events.current)}
        </ul>
      )}
    </>
  );
};

export default App;
import React from 'react';
import styles from './WebsocketDebugMonitor.module.css';
import mapReverse from '../mapReverse';

const WebsocketDebugMonitor = ({ showEvents, events, onToggle }) => (
  <>
    <button onClick={onToggle}>Toggle WebSocket Monitor</button>
    {!!showEvents && (
      <ul className={styles.wsDebugView}>
        {mapReverse((msg, i) => <li key={i}><pre>[{msg.type}] {JSON.stringify(msg.event.data)}</pre></li>)(events)}
      </ul>
    )}
  </>
);

export default WebsocketDebugMonitor;
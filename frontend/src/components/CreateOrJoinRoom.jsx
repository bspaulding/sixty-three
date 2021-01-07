import React, { useState } from 'react';

const CreateOrJoinRoom = ({ onCreate, onJoin }) => {
  const [roomId, setRoomId] = useState("");
  return (
    <form>
      <button onCreate={onCreate}>Create a Room</button>
    or
      <input type="text" placeholder="enter room id" value={roomId} onInput={e => setRoomId(e.target.value)} />
      <button onClick={() => onJoin({ roomId })}>Join Room</button>
    </form>
  )
};

export default CreateOrJoinRoom;
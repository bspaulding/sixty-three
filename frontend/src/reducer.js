export const initialState = {
  showWsDebug: false,
  roomId: undefined
};

const actionTypes = {
  WS_DEBUG_TOGGLE: 'WS_DEBUG_TOGGLE',
  ROOM_CREATE: 'ROOM_CREATE',
  ROOM_JOIN: 'ROOM_JOIN'
};

export const actions = {
  wsDebugToggle: () => ({ type: actionTypes.WS_DEBUG_TOGGLE }),
  createRoom: () => ({ type: actionTypes.ROOM_CREATE }),
  joinRoom: ({ roomId }) => ({ type: actionTypes.ROOM_JOIN, payload: { roomId } })
};

function reducer(state, action) {
  if ('undefined' === typeof state) {
    return initialState;
  }

  switch (action.type) {
    case actionTypes.WS_DEBUG_TOGGLE:
      return { ...state, showWsDebug: !state.showWsDebug };
    default:
      return state;
  }

  return state;
}

export default reducer;
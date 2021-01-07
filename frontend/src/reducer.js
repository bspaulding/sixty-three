export const initialState = {
  showWsDebug: false,
  roomId: undefined
};

const actionTypes = {
  WS_DEBUG_TOGGLE: 'WS_DEBUG_TOGGLE'
};

export const actions = {
  wsDebugToggle: () => ({ type: actionTypes.WS_DEBUG_TOGGLE })
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
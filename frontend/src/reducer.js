export const initialState = {
  showWsDebug: false,
  roomId: undefined
};

export const actions = {
  WS_DEBUG_TOGGLE: 'WS_DEBUG_TOGGLE'
};

function reducer(state, action) {
  if ('undefined' === typeof state) {
    return initialState;
  }

  switch (action.type) {
    case actions.WS_DEBUG_TOGGLE:
      return { ...state, showWsDebug: !state.showWsDebug };
    default:
      return state;
  }

  return state;
}

export default reducer;
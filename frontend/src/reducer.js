export const initialState = {
  showWsDebug: true,
  roomId: undefined
};

function reducer(state, action) {
  if ('undefined' === typeof state) {
    return initialState;
  }

  switch (action.type) {
    default:
      return state;
  }

  return state;
}

export default reducer;
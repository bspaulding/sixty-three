const mapReverse = f => xs => {
  const ys = [];
  for (let i = xs.length - 1; i >= 0; i -= 1) {
    console.log(i, xs.length - 1 - i);
    ys[xs.length - 1 - i] = f(xs[i], i);
  }
  return ys;
};

export default mapReverse;
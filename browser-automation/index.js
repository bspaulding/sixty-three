const playwright = require('playwright');

async function signIn(page, name, roomId) {
  await page.fill('input[name="name"]', name);
  await page.click('button');

  if (!roomId) {
    await page.click('button#create-room');
  } else {
    await page.fill('input[name="room-id"]', roomId);
    await page.click('button#join-room');
  }
  await page.waitForSelector('#room-id');
  const roomIdHandle = await page.$('#room-id');
  const readRoomId = await roomIdHandle.evaluate(node => node.innerText);

  return { roomId: readRoomId };
}

async function signInOnNewPage(context, name, roomId) {
  const url = 'http://localhost:3001';
  const page = await context.newPage();
  await page.goto(url);
  return { page, ...(await signIn(page, name, roomId)) };
}

(async () => {
  const browserType = 'chromium';
  const browser = await playwright[browserType].launch({
    headless: false
  });
  const context = await browser.newContext();

  const { page: pageOne, roomId } = await signInOnNewPage(context, 'four');
  const { page: pageTwo } = await signInOnNewPage(context, 'one', roomId)
  const { page: pageThree } = await signInOnNewPage(context, 'three', roomId)
  const { page: pageFour } = await signInOnNewPage(context, 'two', roomId)
  const pages = [pageOne, pageTwo, pageThree, pageFour];

  await pages[0].click('#start-game');

  // pass
})();
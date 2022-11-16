const playwright = require("playwright");

async function signIn(page, name, roomId) {
  await page.fill("input#player-name", name);
  await page.click("button#submit-player-name");

  if (!roomId) {
    await page.click("button#create-room");
  } else {
    await page.fill('input[name="room-id"]', roomId);
    await page.click("button#join-room");
  }
  await page.waitForSelector("#room-id");
  const roomIdHandle = await page.$("#room-id");
  const readRoomId = await roomIdHandle.evaluate((node) => node.innerText);

  return { roomId: readRoomId };
}

async function signInOnNewPage(context, name, roomId) {
  const url = "http://localhost:3001";
  const page = await context.newPage();
  await page.goto(url);
  return { page, ...(await signIn(page, name, roomId)) };
}

(async () => {
  const headless = true;
  const exitOnGameOver = true;

  const browserType = "chromium";
  const browser = await playwright[browserType].launch({ headless });
  const context = await browser.newContext();

  const { page: pageOne, roomId } = await signInOnNewPage(context, "one");
  const { page: pageTwo } = await signInOnNewPage(context, "two", roomId);
  const { page: pageThree } = await signInOnNewPage(context, "three", roomId);
  const { page: pageFour } = await signInOnNewPage(context, "four", roomId);
  const pages = [pageOne, pageTwo, pageThree, pageFour];
  const pageNames = ["one", "two", "three", "four"];

  await pages[0].click("#start-game");

  // todo: would like to let every player say they are done, but error case would stall forever, so early exiting
  let playersDone = 0;
  while (playersDone === 0) {
    for (let i = 0; i < pages.length; i += 1) {
      let isDone = await nextMove(pages[i], pageNames[i]);
      playersDone += isDone ? 1 : 0;
      console.log("");
    }
  }
  console.log("Game is over.");
  if (exitOnGameOver) {
    browser.close();
  }
})();

async function nextMove(page, pageId) {
  console.log(`${pageId} takes a turn...`);
  await page.bringToFront();

  if (await page.$(".error")) {
    const errorMsg = await page.$eval(".error", (e) => e.innerText);
    console.log(`${pageId} is seeing an error message: '${errorMsg}'`);
    return true;
  }

  const pageContent = await page.content();

  // are we bidding?
  if (pageContent.indexOf("Waiting for a bid from") >= 0) {
    console.log(`${pageId} is waiting to bid`);
    return false;
  }

  const enterBidHandle = await page.$('input[id="enter-bid"]');
  if (enterBidHandle) {
    if (pageContent.indexOf("The bid belongs to") >= 0) {
      console.log(`${pageId} is not the first to bid, passing...`);
      await page.click("button#pass-bid");
    } else {
      console.log(`${pageId} is the first to bid, bidding 25`);
      await page.fill('input[id="enter-bid"]', "25");
      await page.click("#submit-bid");
    }
    return false;
  }

  if (pageContent.indexOf("waiting on trump to be selected") >= 0) {
    console.log(`${pageId} is waiting on trump to be selected`);
    return false;
  } else if (pageContent.indexOf("Please select a trump suit") >= 0) {
    const suit = randomSuit();
    console.log(`${pageId} is selecting trump as ${suit}`);
    await page.click(`#select-trump-${suit}`);
    return false;
  }

  if (
    pageContent.indexOf("trump is") >= 0 &&
    pageContent.indexOf("you have discarded") < 0 &&
    pageContent.indexOf("to play a card") < 0 &&
    pageContent.indexOf("click a card to play it") < 0
  ) {
    const trumpSuit = await page.$eval("#trump-suit", (el) =>
      el.innerText.toLowerCase()
    );
    console.log(`${pageId} sees that trump is ${trumpSuit}, discarding...`);
    const cardIds = (
      await page.locator(".card").evaluateAll((cards) => cards.map((c) => c.id))
    ).filter((id) => id.length);
    console.log(`${pageId} has cards ${cardIds}`);
    const nonTrumpCards = cardIds.filter((c) => !isTrump(trumpSuit, c));
    // TODO: need to leave at least six cards, cards - nonTrumpCards >= 6
    const numTrump = cardIds.length - nonTrumpCards.length;

    // discard non trump
    const numCardsToDiscard = numTrump - 6 + nonTrumpCards.length;
    console.log(
      `${pageId} has ${numTrump} trump cards, ${nonTrumpCards.length} non-trump. Discarding ${numCardsToDiscard} non trump cards.`
    );
    const cardIdsToDiscard = nonTrumpCards.slice(0, numCardsToDiscard);
    console.log(`${pageId} discarding cards ${cardIdsToDiscard}`);
    if (cardIdsToDiscard.length > 0) {
      for (let i = 0; i < cardIdsToDiscard.length; i += 1) {
        await page.click(`#${cardIdsToDiscard[i]}`);
      }
      await page.click("#discard-cards");
    }

    if (numTrump > 6) {
      // we have too many trump, need to pass some cards
      const numCardsToPass = numTrump - 6;
      const trumpCards = cardIds.filter((c) => isTrump(trumpSuit, c));
      const cardIdsToPass = trumpCards.slice(0, numCardsToPass);
      console.log(
        `${pageId} has too many trump, passing cards: ${cardIdsToPass}`
      );
      for (let i = 0; i < cardIdsToPass.length; i += 1) {
        await page.click(`#${cardIdsToPass[i]}`);
      }
      await page.click("#pass-cards");
    }

    return false;
  }

  if (pageContent.indexOf("you have discarded") >= 0) {
    console.log(`${pageId} is waiting for others to finish discarding`);
    return false;
  }

  if (pageContent.indexOf("click a card to play it") >= 0) {
    console.log(`${pageId} sees it is their turn to play a card`);
    const trumpSuit = await page.$eval("#trump-suit", (el) =>
      el.innerText.toLowerCase()
    );
    const cardIds = await page
      .locator(".player-hand-view .card")
      .evaluateAll((es) => es.map((e) => e.id));
    console.log(`${pageId} cardIds = ${cardIds}`);
    if (!cardIds || cardIds.length === 0) {
      console.log(
        `${pageId} thinks it should play a card, but has no cards to play`
      );
      return true;
    }
    const trumpIds = cardIds.filter((c) => isTrump(trumpSuit, c));
    const cardId = trumpIds.length ? trumpIds[0] : cardIds[0];
    console.log(`${pageId} is about to play card ${cardId}`);
    await page.click(`#${cardId}`);
    return false;
  }

  if (pageContent.indexOf("to play a card") >= 0) {
    console.log(`${pageId} is waiting for a card to be played`);
    return false;
  }

  if (pageContent.indexOf("Game is Over!") >= 0) {
    console.log(`${pageId} sees the game is over. What fun!`);
    const [teamOddScore, teamEvenScore] = await reportScore(page, pageId);
    if (teamOddScore > teamEvenScore) {
      if (pageId === "one" || pageId === "three") {
        console.log(`${pageId} is excited to be a winner!`);
      } else {
        console.log(`${pageId} is sad to lose.`);
      }
    } else {
      if (pageId === "one" || pageId === "three") {
        console.log(`${pageId} is sad to lose.`);
      } else {
        console.log(`${pageId} is excited to be a winner!`);
      }
    }
    return true;
  }

  console.log(`${pageId} doesn't know what else to do`);
  return true;
}

async function reportScore(page, pageId) {
  const scores = await page
    .locator(".scores tbody>tr>td")
    .evaluateAll((els) => els.map((e) => e.innerText));
  console.log(`${pageId} sees a score of ${scores}`);
  return scores;
}

function isTrump(trumpSuit, cardId) {
  return (
    cardId === "joker" ||
    cardId.indexOf(trumpSuit) >= 0 ||
    isOppositeFive(trumpSuit, cardId)
  );
}

function isOppositeFive(trumpSuit, cardId) {
  return cardId === `facecard-${oppositeSuit(trumpSuit)}-five`;
}

function oppositeSuit(suit) {
  return {
    hearts: "diamonds",
    diamonds: "hearts",
    clubs: "spades",
    spades: "clubs",
  }[suit];
}

function randomSuit() {
  return randomOneOf(["hearts", "diamonds", "clubs", "spades"]);
}

function randomOneOf(these) {
  return these[Math.floor(Math.random() * these.length)];
}

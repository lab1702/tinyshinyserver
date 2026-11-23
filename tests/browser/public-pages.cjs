// Run from the repository root with Playwright available on NODE_PATH.
// Checks the landing page and the page that a browser gets while an app
// starts, served from a local server so that the browser's back-forward
// cache applies as it does to the real server.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const http = require('node:http');
const path = require('node:path');
const { chromium } = require('playwright');

const templates = path.join(__dirname, '../../inst/templates');
const card = name => `<a class="app-card-link is-disabled" aria-disabled="true"><div class="app-card app-disabled" data-app="${name}">` +
  `<h3>${name}</h3><span class="status-badge" id="status-${name}"></span>` +
  `<span class="connections-count" id="connections-${name}"></span><span class="app-open"></span></div></a>`;
const landing = base => fs.readFileSync(path.join(templates, 'landing_page.html'), 'utf8')
  .replaceAll('{{base_url}}', base).replaceAll('{{title}}', 'Apps').replace('{{app_cards}}', card('sales'));
const starting = fs.readFileSync(path.join(templates, 'starting_page.html'), 'utf8')
  .replaceAll('{{base_url}}', '').replaceAll('{{title}}', 'Apps').replaceAll('{{app_name}}', 'sales')
  .replace('{{status}}', 'Starting').replace('{{retry_seconds}}', '1').replace('{{answer_seconds}}', '2')
  .replace('{{delay_seconds}}', '0');

let apiCalls = 0;
let oldLoads = 0;
let appMode = 'starting';
let appDelayMs = 0;
const server = http.createServer((req, res) => {
  const pathname = new URL(req.url, 'http://localhost').pathname;
  const send = (status, type, body, headers = {}) => {
    res.writeHead(status, { 'Content-Type': type, 'Cache-Control': 'no-store', ...headers });
    res.end(body);
  };
  if (pathname.startsWith('/templates/')) {
    const file = path.join(templates, pathname.slice('/templates/'.length));
    return send(200, file.endsWith('.css') ? 'text/css' : 'application/javascript', fs.readFileSync(file),
      { 'Cache-Control': 'max-age=300' });
  }
  if (pathname === '/') return send(200, 'text/html', landing(''), { 'Cache-Control': 'private' });
  if (pathname === '/api/apps') {
    apiCalls++;
    return send(200, 'application/json', JSON.stringify({
      sales: { name: 'sales', status: 'running', resident: true, connections: apiCalls }
    }), { 'X-TinyShinyServer-Base-Path': '' });
  }
  // A page under a prefix that a configuration reload removed
  if (pathname === '/old/') {
    oldLoads++;
    return send(200, 'text/html', landing('/old'));
  }
  if (pathname === '/proxy/sales/') {
    return setTimeout(() => appMode === 'starting'
      ? send(503, 'text/html', starting, { 'Retry-After': '1' })
      : send(200, 'text/html', '<!doctype html><title>app</title><p>the app</p>'), appDelayMs);
  }
  return send(404, 'application/json', '{"error":"404 - Not Found"}');
});

// sessionStorage throws, as where the browser blocks site data
const blockStorage = () => Object.defineProperty(window, 'sessionStorage', {
  get() { throw new DOMException('blocked', 'SecurityError'); }
});

(async () => {
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const origin = `http://127.0.0.1:${server.address().port}`;
  const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
  // Playwright turns the back-forward cache off by default
  const browser = await chromium.launch({ headless: true, executablePath: process.env.TSS_TEST_BROWSER || undefined,
    ignoreDefaultArgs: ['--disable-back-forward-cache'] });
  try {
    // The landing page keeps its status current when Back shows it again
    const page = await browser.newPage();
    await page.goto(origin + '/');
    await page.waitForFunction(() => document.querySelector('.app-card-link').hasAttribute('href'));
    await page.evaluate(() => { window.__sameDocument = true; });
    appMode = 'app';
    await page.click('.app-card-link');
    await page.waitForURL(/proxy/);
    // A page shown again from the cache fires no load event
    await page.goBack({ waitUntil: 'commit' });
    await sleep(500);
    if (await page.evaluate(() => window.__sameDocument === true)) {
      const before = apiCalls;
      await sleep(6000);
      assert.ok(apiCalls > before, 'the restored landing page asks for the apps\' status again');
    } else {
      console.log('SKIP: this browser did not keep the landing page in its back-forward cache');
    }
    await page.close();

    // Where site data is blocked, a page whose prefix was removed reloads
    // itself once, and not again within a minute
    for (const blocked of [false, true]) {
      oldLoads = 0;
      const context = await browser.newContext();
      if (blocked) await context.addInitScript(blockStorage);
      const old = await context.newPage();
      await old.goto(origin + '/old/');
      for (let i = 0; i < 50 && oldLoads < 2; i++) await sleep(100);
      assert.equal(oldLoads, 2, `the page reloads for its new address (storage blocked: ${blocked})`);
      await old.evaluate(() => updateAppStatus());
      await sleep(1000);
      assert.equal(oldLoads, 2, `the page reloads only once a minute (storage blocked: ${blocked})`);
      await context.close();
    }

    // The starting page continues its wait across its own reloads, even
    // when the server holds them, and starts a new one for a later visit
    for (const blocked of [false, true]) {
      const context = await browser.newContext();
      if (blocked) await context.addInitScript(blockStorage);
      const app = await context.newPage();
      const wait = () => app.evaluate(() => {
        try {
          return JSON.parse(sessionStorage.getItem('tss-starting:/proxy/sales/'));
        } catch (e) {
          return window.name.startsWith('tss-starting ') ? JSON.parse(window.name.slice(13)).wait : null;
        }
      });
      appMode = 'starting';
      appDelayMs = 1500;
      await app.goto(origin + '/proxy/sales/');
      const first = (await wait()).first;
      await app.waitForEvent('load');
      assert.equal((await wait()).first, first, `a reload continues the wait (storage blocked: ${blocked})`);
      if (blocked) {
        const name = await app.evaluate(() => window.name);
        assert.ok(name.startsWith('tss-starting ') && !name.includes('/proxy/'), 'the tab\'s name holds no address: ' + name);
      }
      // The page's reload reaches the app, which then stops; the visitor reloads
      appMode = 'app';
      appDelayMs = 0;
      await app.waitForEvent('load');
      assert.equal(await app.title(), 'app');
      appMode = 'starting';
      await sleep(2500);
      await app.reload();
      assert.ok((await wait()).first > first + 2000, `a later visit starts a new wait (storage blocked: ${blocked})`);
      await context.close();
    }
    console.log('PASS: the landing page refreshes after Back and reloads for a new prefix without site data, and the starting page keeps its wait only across its own reloads, without an address in the tab\'s name.');
  } finally {
    await browser.close();
    server.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });

// Run from the repository root with Playwright available on NODE_PATH.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { spawn } = require('node:child_process');
const { chromium } = require('playwright');
(async () => {
  const folder = fs.mkdtempSync(path.join(os.tmpdir(), 'tss-csrf-'));
  const server = spawn('Rscript', [path.join(__dirname, 'management-csrf-server.R'), folder]);
  let output = '';
  server.stdout.on('data', x => output += x);
  server.stderr.on('data', x => output += x);
  const exited = new Promise(resolve => server.on('exit', resolve));
  let browser;
  try {
    const info = path.join(folder, 'urls.json');
    const deadline = Date.now() + 15000;
    while (!fs.existsSync(info) && Date.now() < deadline && server.exitCode === null) {
      await new Promise(resolve => setTimeout(resolve, 25));
    }
    assert.ok(fs.existsSync(info), output);
    const urls = JSON.parse(fs.readFileSync(info, 'utf8'));
    browser = await chromium.launch({headless: true, executablePath: process.env.TSS_TEST_BROWSER || undefined});
    const page = await browser.newPage();
    await page.goto(urls.foreign);
    const state = async () => (await page.request.get(urls.foreign + '/state')).json();
    for (const route of ['/api/apps/app/restart', '/api/reload', '/api/shutdown']) {
      const url = urls.management + route;
      // Original no-cors attack.
      const response = page.waitForResponse(r => r.url() === url && r.request().method() === 'POST');
      await page.evaluate(url => fetch(url, {method: 'POST', mode: 'no-cors'}), url);
      assert.equal((await response).status(), 403);
      // A script adding the header must fail its cross-origin preflight.
      assert.equal(await page.evaluate(async url => {
        try { await fetch(url, {method: 'POST', headers: {'X-TinyShinyServer-Request': 'management'}}); return true; }
        catch { return false; }
      }, url), false);
      // Sandboxed form submission supplies an opaque (null) origin. Its request runs in
      // another renderer process, which Playwright can attach to too late to report,
      // so wait for the server's own record of it.
      const posts = async () => (await state()).posts.filter(post => post.startsWith(route + ' '));
      const before = (await posts()).length;
      await page.evaluate(url => {
        const frame = document.createElement('iframe');
        frame.sandbox = 'allow-forms allow-scripts';
        frame.srcdoc = '<form method="POST" action="' + url + '"></form><script>document.forms[0].submit()</script>';
        document.body.appendChild(frame);
      }, url);
      const formDeadline = Date.now() + 10000;
      while ((await posts()).length === before && Date.now() < formDeadline) {
        await new Promise(resolve => setTimeout(resolve, 25));
      }
      assert.equal((await posts())[before], route + ' 403');
    }
    const {restarts, reload, shutdown} = await state();
    assert.deepEqual({restarts, reload, shutdown}, {restarts: 0, reload: false, shutdown: false});
    // Real management UI buttons, including their automatic request headers.
    page.on('dialog', dialog => dialog.accept());
    await page.goto(urls.management);
    await page.getByRole('button', {name: 'Restart', exact: true}).click();
    await page.waitForFunction(() => document.querySelector('.restart-btn')?.textContent === 'Restart');
    assert.equal((await state()).restarts, 1);
    await page.getByRole('button', {name: 'Reload Config & Restart All'}).click();
    await page.waitForFunction(() => document.getElementById('notice').textContent.includes('Reloading configuration'));
    assert.equal((await state()).reload, true);
    await page.locator('.shutdown-btn').click();
    await page.waitForFunction(() => document.body.textContent.includes('Server Shutdown'));
    assert.equal((await state()).shutdown, true);
    // Programmatic clients remain supported with the documented header.
    const cli = await page.request.post(urls.management + '/api/apps/app/restart', {
      headers: {'X-TinyShinyServer-Request': 'management'}
    });
    assert.equal(cli.status(), 200);
    assert.equal((await state()).restarts, 2);
    console.log('PASS: foreign no-cors POSTs, opaque-origin forms, and custom-header preflights are blocked; real UI and API clients work.');
  } finally {
    if (browser) await browser.close();
    fs.writeFileSync(path.join(folder, 'stop'), 'stop');
    await exited;
    fs.rmSync(folder, {recursive: true, force: true});
  }
})().catch(e => {console.error(e); process.exitCode = 1;});

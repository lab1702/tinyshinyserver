// Run with Node.js and Playwright available on NODE_PATH.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { chromium } = require('playwright');

(async () => {
  const template = process.argv[2] || path.join(__dirname, '../../inst/templates/management_page.html');
  const html = fs.readFileSync(template, 'utf8').replaceAll('{{base_url}}', '');
  const normalAgent = 'Mozilla/5.0 <literal> & café "quoted"';
  const injectedAgent = '<img src=x onerror="window.__agentInjected=1">';
  const injectedIp = '<svg onload="window.__ipInjected=1"></svg>';
  let connections = {
    normal: { app_name: 'app', client_ip: '127.0.0.1', user_agent: normalAgent,
      connected_at: '2026-09-16 00:00:00', duration_seconds: 65, last_activity: '2026-09-16 00:01:05' },
    hostile: { app_name: 'app', client_ip: injectedIp, user_agent: injectedAgent,
      connected_at: '2026-09-16 00:00:00', duration_seconds: 0, last_activity: '2026-09-16 00:00:00' }
  };
  const browser = await chromium.launch({ headless: true, executablePath: process.env.TSS_TEST_BROWSER || undefined });
  try {
    const page = await browser.newPage();
    await page.route('**/*', route => {
      const pathname = new URL(route.request().url()).pathname;
      if (pathname === '/') return route.fulfill({ contentType: 'text/html', body: html });
      const data = pathname === '/api/connections' ? connections
        : pathname === '/api/status' ? { total_apps: 1, running_apps: 1, total_connections: 2 }
        : pathname === '/api/apps' ? {} : null;
      return route.fulfill({ status: data === null ? 404 : 200, contentType: 'application/json', body: JSON.stringify(data) });
    });
    await page.goto('http://tss.test/');
    await page.waitForSelector('#connectionsContainer tbody tr');
    const rows = page.locator('#connectionsContainer tbody tr');
    assert.equal(await rows.count(), 2);
    assert.deepEqual(await rows.nth(0).locator('td').allTextContents(), [
      'app', '127.0.0.1', normalAgent, '2026-09-16 00:00:00', '1m 5s', '2026-09-16 00:01:05'
    ]);
    assert.equal(await rows.nth(1).locator('td').nth(1).textContent(), injectedIp);
    assert.equal(await rows.nth(1).locator('.user-agent').textContent(), injectedAgent);
    assert.equal(await page.locator('#connectionsContainer img, #connectionsContainer svg').count(), 0);
    assert.equal(await page.evaluate(() => window.__agentInjected || window.__ipInjected || false), false);
    connections = {};
    await page.evaluate(() => updateConnections());
    await page.waitForFunction(() => document.getElementById('connectionsContainer').textContent === 'No active connections');
    console.log('PASS: hostile headers render literally; ordinary metadata and empty-state rendering work.');
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });

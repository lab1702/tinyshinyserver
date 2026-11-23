// Run with Node.js and Playwright available on NODE_PATH.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { chromium } = require('playwright');

(async () => {
  const templates = path.join(__dirname, '../../inst/templates');
  const template = process.argv[2] || path.join(templates, 'management_page.html');
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
  // A stopped app's missing PID arrives as {} from jsonlite; app paths are free-form.
  const markupPath = '/srv/<b>R&amp;D</b>';
  const apps = {
    live: { name: 'live', status: 'running', resident: true, port: 3001, connections: 1, path: '/apps/live', pid: 4242,
      memory_bytes: 150 * 1024 * 1024, uptime_seconds: 90061 },
    dormant: { name: 'dormant', status: 'stopped', resident: false, port: 3002, connections: 0, path: markupPath, pid: {} }
  };
  let lastReload = {};
  let runningApps = 1;
  const browser = await chromium.launch({ headless: true, executablePath: process.env.TSS_TEST_BROWSER || undefined });
  try {
    const serve = route => {
      const pathname = new URL(route.request().url()).pathname;
      if (pathname === '/') return route.fulfill({ contentType: 'text/html', body: html });
      if (pathname.startsWith('/templates/')) {
        const file = path.join(templates, pathname.slice('/templates/'.length));
        return route.fulfill({ contentType: file.endsWith('.css') ? 'text/css' : 'application/javascript',
          body: fs.readFileSync(file, 'utf8') });
      }
      if (pathname === '/api/apps/live/restart') {
        // The restarted app is down until it has started again
        runningApps = 0;
        return route.fulfill({ contentType: 'application/json',
          body: JSON.stringify({ success: true, message: 'App live restarted' }) });
      }
      if (pathname === '/api/apps/dormant/restart') {
        // An unexpected error is answered with the "error" field alone
        return route.fulfill({ status: 500, contentType: 'application/json',
          body: JSON.stringify({ error: '500 - Unexpected error; see server.log for the reason' }) });
      }
      const data = pathname === '/api/connections' ? connections
        : pathname === '/api/status' ? { total_apps: 1, running_apps: runningApps, total_connections: 2, uptime_seconds: 3725,
            server_memory_bytes: 90 * 1024 * 1024, apps_memory_bytes: 1.5 * 1024 ** 3, ...lastReload }
        : pathname === '/api/reload' ? { success: true, message: 'Reloading configuration and restarting all apps' }
        : pathname === '/api/apps' ? apps : null;
      return route.fulfill({ status: data === null ? 404 : 200, contentType: 'application/json', body: JSON.stringify(data) });
    };
    const page = await browser.newPage();
    await page.route('**/*', serve);
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
    await page.waitForFunction(() => document.querySelectorAll('#appsContainer .app-card').length === 2);
    const pids = await page.locator('#appsContainer .app-detail').filter({ hasText: 'PID:' }).allTextContents();
    assert.deepEqual(pids, ['PID: 4242', 'PID: N/A']);
    const paths = await page.locator('#appsContainer .app-detail').filter({ hasText: 'Path:' }).allTextContents();
    assert.deepEqual(paths, ['Path: /apps/live', 'Path: ' + markupPath]);
    assert.equal(await page.locator('#appsContainer b').count(), 0);
    // Usage the server could not read (a stopped app) shows as N/A.
    const detailTexts = label => page.locator('#appsContainer .app-detail').filter({ hasText: label + ':' }).allTextContents();
    assert.deepEqual(await detailTexts('Memory'), ['Memory: 150 MB', 'Memory: N/A']);
    assert.deepEqual(await detailTexts('Uptime'), ['Uptime: 1d 1h 1m', 'Uptime: N/A']);
    const statusCards = await page.locator('#systemStatus .status-card').allTextContents();
    assert.ok(statusCards.includes('Uptime1h 2m 5s'), statusCards.join(' | '));
    assert.ok(statusCards.includes('Memory in use1.6 GBServer 90 MB · Apps 1.5 GB'), statusCards.join(' | '));
    // A reload that fails after it was accepted is reported once it has run.
    lastReload = { last_reload: { success: false, message: 'Could not stop apps: live' } };
    page.once('dialog', dialog => dialog.accept());
    await page.locator('.reload-btn').click();
    await page.waitForFunction(() => document.getElementById('notice').textContent === 'Reload failed: Could not stop apps: live');
    await page.waitForFunction(() => !document.querySelector('.reload-btn').disabled);
    connections = {};
    await page.evaluate(() => updateConnections());
    await page.waitForFunction(() => document.getElementById('connectionsContainer').textContent === 'No active connections');
    // The landing page reloads when a configuration reload changes the set of apps.
    const card = name => `<a class="app-card-link"><div class="app-card" data-app="${name}">` +
      `<span class="status-badge" id="status-${name}"></span><span id="connections-${name}"></span>` +
      `<span class="app-open"></span></div></a>`;
    const landing = fs.readFileSync(path.join(templates, 'landing_page.html'), 'utf8')
      .replaceAll('{{base_url}}', '').replaceAll('{{title}}', 'Apps');
    let landingCards = ['gone', 'live'];
    let landingLoads = 0;
    const landingPage = await browser.newPage();
    await landingPage.route('**/*', route => {
      if (new URL(route.request().url()).pathname !== '/landing') return serve(route);
      landingLoads++;
      const body = landing.replace('{{app_cards}}', landingCards.map(card).join(''));
      landingCards = Object.keys(apps);
      return route.fulfill({ contentType: 'text/html', body });
    });
    await landingPage.goto('http://tss.test/landing');
    await landingPage.waitForFunction(() => document.getElementById('status-dormant')?.textContent === 'stopped');
    assert.equal(landingLoads, 2);
    assert.equal(await landingPage.locator('[data-app="gone"]').count(), 0);
    await landingPage.evaluate(() => updateAppStatus());
    await landingPage.waitForFunction(() => document.getElementById('status-live').textContent === 'running');
    assert.equal(landingLoads, 2);
    // The theme follows the system until toggled; toggling back to the system theme clears the override.
    const context = await browser.newContext({ colorScheme: 'dark' });
    const themed = await context.newPage();
    await themed.route('**/*', serve);
    await themed.goto('http://tss.test/');
    const theme = () => themed.evaluate(() => [document.documentElement.dataset.theme, localStorage.getItem('tss-theme')]);
    assert.deepEqual(await theme(), ['dark', null]);
    await themed.getByRole('button', { name: 'Switch to light theme' }).click();
    assert.deepEqual(await theme(), ['light', 'light']);
    await themed.reload();
    assert.deepEqual(await theme(), ['light', 'light']);
    await themed.emulateMedia({ colorScheme: 'light' });
    await themed.getByRole('button', { name: 'Switch to dark theme' }).click();
    assert.deepEqual(await theme(), ['dark', 'dark']);
    await themed.getByRole('button', { name: 'Switch to light theme' }).click();
    assert.deepEqual(await theme(), ['light', null]);
    await themed.emulateMedia({ colorScheme: 'dark' });
    await themed.waitForFunction(() => document.documentElement.dataset.theme === 'dark');
    await context.close();
    // An open landing page whose address a reload of base_path removed reloads itself from the server
    const oldLanding = fs.readFileSync(path.join(templates, 'landing_page.html'), 'utf8')
      .replaceAll('{{base_url}}', '/old').replaceAll('{{title}}', 'Apps')
      .replace('{{app_cards}}', Object.keys(apps).map(card).join(''));
    let oldLoads = 0;
    const oldPage = await browser.newPage();
    await oldPage.route('**/*', route => {
      if (new URL(route.request().url()).pathname !== '/old/') return serve(route);
      oldLoads++;
      return oldLoads === 1 ? route.fulfill({ contentType: 'text/html', body: oldLanding })
        : route.fulfill({ status: 404, contentType: 'application/json', body: '{"error":"404 - Not Found"}' });
    });
    await oldPage.goto('http://tss.test/old/');
    for (let i = 0; i < 50 && oldLoads < 2; i++) await new Promise(resolve => setTimeout(resolve, 100));
    assert.equal(oldLoads, 2);
    await oldPage.close();
    // A refresh updates the app cards in place, so the keyboard focus stays on a Restart button
    await page.evaluate(() => clearInterval(refreshInterval));
    const liveRestart = page.locator('#appsContainer .app-card').filter({ hasText: 'live' })
      .getByRole('button', { name: 'Restart', exact: true });
    await liveRestart.focus();
    apps.live.uptime_seconds += 5;
    await page.evaluate(() => updateApps());
    await page.waitForFunction(() => document.querySelector('[data-detail="uptime"] .detail-value').textContent === '1d 1h 1m');
    await new Promise(resolve => setTimeout(resolve, 200));
    assert.equal(await page.evaluate(() => document.activeElement.textContent), 'Restart');
    assert.equal(await page.evaluate(() =>
      document.activeElement.closest('.app-card').querySelector('.app-name').textContent), 'live');
    // A dormant app that waits out restart_delay after a failure offers a restart, which ends the wait
    apps.waiting = { name: 'waiting', status: 'dormant', resident: false, port: 3003, connections: 0,
      path: '/apps/waiting', restart_in_seconds: 42 };
    await page.evaluate(() => updateApps());
    const waiting = page.locator('#appsContainer .app-card').filter({ hasText: 'waiting' });
    await waiting.getByRole('button', { name: 'Restart', exact: true }).waitFor();
    assert.equal(await waiting.locator('.app-info').textContent(), 'Failed; starts again when opened in 42s');
    delete apps.waiting.restart_in_seconds;
    await page.evaluate(() => updateApps());
    await page.waitForFunction(() => Array.from(document.querySelectorAll('#appsContainer .app-card'))
      .some(card => card.textContent.includes('Starts automatically when opened')));
    assert.equal(await waiting.getByRole('button').count(), 0);
    delete apps.waiting;
    await page.evaluate(() => updateApps());
    await page.waitForFunction(() => document.querySelectorAll('#appsContainer .app-card').length === 2);
    // A restart refreshes the status tiles with the app cards. The timer is stopped so that only the
    // restart can have refreshed them.
    await page.evaluate(() => clearInterval(refreshInterval));
    const runningTile = () => page.locator('#systemStatus .status-card').filter({ hasText: 'Running apps' })
      .locator('.value').textContent();
    assert.equal(await runningTile(), '1');
    await page.locator('#appsContainer .app-card').filter({ hasText: 'live' })
      .getByRole('button', { name: 'Restart', exact: true }).click();
    await page.waitForFunction(() => document.getElementById('notice').textContent === 'App live restarted');
    await page.waitForFunction(() => Array.from(document.querySelectorAll('#systemStatus .status-card'))
      .some(card => card.textContent === 'Running apps0'), null, { timeout: 5000 });
    // A failed restart shows the server's reason, not "undefined"
    await page.locator('#appsContainer .app-card').filter({ hasText: 'dormant' })
      .getByRole('button', { name: 'Restart', exact: true }).click();
    await page.waitForFunction(() => document.getElementById('notice').textContent ===
      'Failed to restart dormant: Unexpected error; see server.log for the reason');
    console.log('PASS: hostile headers and app paths render literally; ordinary metadata, empty-state, landing app-set reload, theme toggling, the refresh after a restart, and a failed restart\'s reason work.');
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });

# tinyshinyserver

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/tinyshinyserver)](https://CRAN.R-project.org/package=tinyshinyserver)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![Platform](https://img.shields.io/badge/Platform-Windows%20%7C%20Linux%20%7C%20macOS-lightgrey.svg)
<!-- badges: end -->

tinyshinyserver serves several Shiny apps, interactive R Markdown documents, and Quarto dashboards from one R session. A lightweight proxy forwards HTTP and WebSocket traffic to the apps, each of which runs in its own R process, and a web dashboard shows what is running and lets you restart apps.

It is intended for development and internal use, and has no authentication or TLS of its own. Before making it reachable from other machines, read [Network access and security](#network-access-and-security).

## Features

- One proxy for many apps, each in its own R process
- **Resident** apps, which start with the server and restart after a failure, and **on-demand** apps, which start when first opened and stop when no longer used
- Shiny apps, R Markdown documents with `runtime: shiny`, and Quarto documents with `server: shiny`
- A management dashboard and API to monitor apps and connections, restart apps, reload the configuration, and shut down
- Log files for the server and for each app's output and errors

## Installation

Install the released version from CRAN:

```r
install.packages("tinyshinyserver")
```

Or the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("lab1702/tinyshinyserver")
```

tinyshinyserver needs R 4.1.0 or later. R Markdown apps also need [Pandoc](https://pandoc.org/), and Quarto apps need the [Quarto CLI](https://quarto.org/). Neither is an R package, so install them separately.

## Quick start

The package includes four example apps and a configuration file that serves them. In R, from a directory where you want to keep a copy of the examples, run:

```r
install.packages(c("DT", "plotly", "dplyr", "flexdashboard"))  # used by the examples
library(tinyshinyserver)

file.copy(system.file("examples", package = "tinyshinyserver"), ".", recursive = TRUE)
start_tss(config = "examples/config.json")
```

The examples are:

| App | Type | Mode | Also needs |
|-----|------|------|------------|
| `sales` | Shiny app in one file (`app.R`) | Resident | |
| `inventory` | Shiny app in two files (`ui.R` and `server.R`) | On-demand | |
| `reports` | R Markdown flexdashboard | On-demand | Pandoc |
| `dashboard` | Quarto dashboard | Resident | Quarto CLI |

`dashboard` is resident, so it starts with the server, and the example configuration needs the Quarto CLI. Without it, use the [minimal configuration](#configuration) below, which serves only `sales`.

With the server running, open:

| URL | Page |
|-----|------|
| http://localhost:3838 | Landing page, with a tile for each app |
| http://localhost:3838/proxy/sales/ | The `sales` app; each app is at `/proxy/{name}/` |
| http://localhost:3839 | Management dashboard |

`start_tss()` keeps the R console busy until the server stops. To stop it, click **Shutdown Server** on the management dashboard or press **Ctrl-C** in R. Either way, the server closes all connections and stops the app processes. To shut down from a script, see [Management API](#management-api). If an unexpected error stops the server, `start_tss()` also stops the apps, and then signals the error, so that a server run with `Rscript` exits with a failure status that a service manager can act on.

## Configuration

The server reads a JSON configuration file. This minimal configuration serves only the `sales` example. Save it as `config.json` in the directory that holds the copied `examples` directory, and run `start_tss()`, which reads `config.json` by default:

```json
{
  "apps": [
    {
      "name": "sales",
      "path": "./examples/sales",
      "resident": true
    }
  ],
  "starting_port": 3001,
  "proxy_port": 3838,
  "proxy_host": "127.0.0.1",
  "management_port": 3839,
  "log_dir": "./logs"
}
```

- `apps`, `starting_port`, and `log_dir` are required. Every other setting has a default.
- Relative paths, in each app's `path` and in `log_dir`, are resolved from the R working directory (`getwd()`), **not** from the directory of the configuration file. Absolute paths also work.
- JSON does not allow comments. A field that the server does not recognize, such as a misspelled option, has no effect; it is reported as a warning in `server.log` at startup, and in the dashboard's notice after a reload.
- A field given twice, at the top level or in one app, makes the file invalid, because JSON readers disagree on which copy counts.

To change the configuration while the server runs, see [Reloading the configuration](#reloading-the-configuration). `help("config-format")` gives the validation rules for every setting.

### App settings

Each entry in `apps` has these settings:

| Setting | Default | Description |
|---------|---------|-------------|
| `name` | Required | Name used in the app's URL and log file names: 1–50 ASCII letters, digits, underscores, or hyphens. Names must be unique, ignoring letter case |
| `path` | Required | The app's directory; see [Writing apps](#writing-apps) |
| `resident` | `false` | `true` starts the app with the server and restarts it after a failure; `false` starts it on demand. See [App lifecycle](#app-lifecycle) |
| `appstart_timeout` | `2` | Seconds, counted from the app's start, that a request waits for the app to be ready before receiving HTTP 503; fractions are allowed, up to 86400 (one day). See [Startup](#startup) |

### Server settings

| Setting | Default | Description |
|---------|---------|-------------|
| `starting_port` | Required | First port to try for apps; see [Processes and ports](#processes-and-ports) |
| `log_dir` | Required | Directory for log files; see [Logging](#logging) |
| `proxy_port` | `3838` | Port for the landing page and apps |
| `proxy_host` | `"127.0.0.1"` | Network interface the proxy listens on; see [Network access and security](#network-access-and-security) |
| `management_port` | `3839` | Port for the management dashboard and API, which listen only on `127.0.0.1` |
| `base_path` | `""` | Public URL prefix, such as `"/shiny"`, when a reverse proxy serves the apps under a path; see [Serving under a URL prefix](#serving-under-a-url-prefix) |
| `title` | `"Tiny Shiny Server"` | Name shown in the browser tab and top bar of the landing and management pages; up to 100 characters |
| `restart_delay` | `5` | Seconds to wait before restarting a failed app, up to 86400 (one day): a resident app restarts then, and an on-demand app at the next request after that. A restart requested from the dashboard or API does not wait |
| `health_check_interval` | `10` | Seconds between health checks, which find app processes that have exited; up to 86400 (one day) |
| `max_request_size_mb` | `100` | Largest request body, in megabytes, that the proxy accepts; see [Limits and timeouts](#limits-and-timeouts) |
| `max_response_size_mb` | `1024` | Largest app response, in megabytes, that the proxy forwards; see [Limits and timeouts](#limits-and-timeouts) |
| `idle_timeout_minutes` | `30` | Minutes without a WebSocket message, in either direction, after which a session is closed; `0` keeps idle sessions open |
| `max_unanswered_mb` | `256` | Most data, in megabytes, that an app may send a WebSocket session without a message from the browser in between; `0` sets no limit. See [Limits and timeouts](#limits-and-timeouts) |
| `log_level` | `"INFO"` | Least severe messages written to `server.log`; see [Log levels](#log-levels) |

## Writing apps

### Entry points

Point each app's `path` at a directory that contains one of these entry points:

| App type | Entry point | Requirement |
|----------|-------------|-------------|
| Shiny, one file | `app.R` | |
| Shiny, two files | `ui.R` and `server.R` | |
| R Markdown | A `.Rmd` document | `runtime: shiny` and Pandoc |
| Quarto | A `.qmd` document | `server: shiny` and the Quarto CLI |

`app.R`, or `ui.R` and `server.R`, take precedence over documents, and R Markdown documents over Quarto documents. After documents, the server also accepts, as `shiny::runApp()` does, the Shiny file names in any letter case, and `server.R` alone with the page in `www/index.html`.

When a directory holds several documents:

- Extensions may be in any letter case, such as `report.rmd`.
- Files whose names start with `_` are skipped, because R Markdown and Quarto treat them as partials that other documents include.
- Of the rest, the server chooses an `index` or `ui` document first, then one whose front matter sets `runtime: shiny` or `server: shiny`, then one spelled `.Rmd` or `.qmd`, then the first in alphabetical order. The chosen document is the one served at the app's address.

To make the choice explicit, keep one entry document per directory. An R Markdown or Quarto app serves only its entry document: `rmarkdown`, which serves both, would otherwise render, and run the code of, any other file under the directory whose path a page or its WebSocket names, such as another `.Rmd` or `.qmd` document or an R script. A request for another document's page receives HTTP 404, and a WebSocket for any other file is closed, whether or not its path is percent-encoded. Its other files, such as images and data, are served as usual.

If an app's directory is missing, or holds none of these entry points, the app is not started, and `server.log` gives the reason:

- A resident app is shown as **stopped**, and the server tries again at each health check after `restart_delay`.
- An on-demand app is shown as **unavailable**, its landing page tile is disabled, and a request for it receives HTTP 502. The reason is logged when the configuration is loaded and again when someone opens the app, at most once a minute. The status changes as soon as files are added to or removed from the directory, or within 30 seconds after a change that leaves the directory's modification time alone, such as a fixed permission or a file copied to a FAT or exFAT drive.

### App environment

An app's R process starts in the app's directory and reads the `.Renviron` and `.Rprofile` there, as R does when you run the app on its own. The app's own environment variables, such as credentials, therefore apply to it, and a profile that activates renv gives it its own project library. That holds under a server that itself runs in a renv project: the app starts without the `RENV_PROJECT` variable of the server's session, which renv's activation script would read in place of the app's directory, unless the app's own `.Renviron` sets it.

- As at R's startup, `R_ENVIRON_USER` and `R_PROFILE_USER` take precedence: when one is set, the app reads the file it names instead, and when one is set but empty, the app reads no such file. An app directory without one of these files falls back to the user's `~/.Renviron` or `~/.Rprofile`. An app's own `.Renviron` that sets `R_PROFILE_USER` chooses its profile in the same way: the app runs the file that it names, or none when it is empty, and not the `.Rprofile` of its directory as well. On Windows, which cannot hold an empty environment variable, a variable set to an empty value counts as not set, as it does for R there.
- The `.Renviron` and `.Rprofile` of the directory that `start_tss()` is called from do not apply to apps. Apps do inherit the environment variables of the R session that runs `start_tss()`, including any that it read from a `.Renviron` when it started.
- Unless the app's own `.Renviron` sets `R_LIBS` or `R_LIBS_USER`, or its profile changes them, apps use the library paths of the R session that runs `start_tss()`. An app's `.Renviron` that sets one of these gives the app the library paths that R makes from them at its startup. The library variables of `~/.Renviron`, or of the file that `R_ENVIRON_USER` names, do not change an app's library paths, so an app without its own `.Renviron` keeps the server's, such as a renv library.
- The site profile, `Rprofile.site` or the file that `R_PROFILE` names, runs in apps as it does at R's startup.
- As at R's startup, an error in an app's profile is written to the app's error log, and the app still starts.

### R Markdown and Quarto documents

A Quarto document is rendered before it is served, which can take many seconds. To keep starts and restarts fast, the server renders it with the Quarto CLI, the one that `QUARTO_PATH` names or that `PATH` finds in the app's environment, only when the rendered page (`{name}.html`, next to the `.qmd`) may be out of date, and then serves it with `rmarkdown::run()`, as `quarto::quarto_serve()` does. The app's own R process does not need the quarto R package, so a renv project library without it works. The R session that Quarto starts to render the document has the app's library paths and environment variables; it reads no user environment file, since the app's already apply, and runs the app's profile, so one that activates renv sets the library paths there too.

The server records each render it makes in `{log_dir}/{app_name}_render.rds`. It renders the document again when:

- the page is missing;
- its last render did not finish, for example because the app was stopped while it rendered, which can leave an incomplete page;
- a file in the app directory has been added, removed, or changed since its last render, including a file copied in with an older modification time;
- the page's `{name}_files` directory, which holds its scripts and styles, is missing, as when the page was deployed without it;
- R, or a package that the page was rendered with, now has another version or is no longer installed. The page records these versions itself, for the packages whose scripts and styles it includes and those that its code loaded, so this applies to a page that the server did not render too.

A page that the server did not render, such as one deployed with the app or one that has changed since the server rendered it, is served while it is newer than every other file in the app directory. What rendering writes beside the page, `{name}_files`, `{name}_data`, and `{name}_cache`, does not count, and neither do the server's own log files and render records when `log_dir` lies in the app directory. Nor do the intermediate files that a render stopped part-way leaves, such as `{name}.rmarkdown`, or files that the document's own code writes into the app directory as it renders, such as a plot that it saves: the server records the files as the render left them, with the document as it was when the render began. Neither do hidden files and directories, such as `.git`, `.Rproj.user`, and `.Rhistory`, except a top-level `.Rprofile` or `.Renviron`, nor the packages that renv installs (`renv/library`, `renv/staging`, and renv's other package directories); `renv.lock` does count. The app's error log records whether the document was rendered, and the output of the render.

This check does not notice:

- **Other changes outside the app directory**, such as to data read while the document renders, to system libraries, or a newer Quarto CLI. Delete the `.html` file to force a render at the next start.
- **A changed file whose size and modification time are both unchanged.**
- **The passing of time.** Code that runs when the document is rendered, rather than in its server context, keeps its results from the last render. Compute dates and other values that change in the server context, or keep them fixed, as the example dashboard does.

Quarto documents, and R Markdown documents that `rmarkdown` serves prerendered (`runtime: shinyrmd`, `runtime: shiny_prerendered`, or `server: shiny`), are rendered into their own directory, as `{name}.html` and a `{name}_files` directory. Two renders of one document at once overwrite each other's files and can leave a broken page, which a server that did not make the last render serves at every start because it is newer than the document. Therefore:

- A configuration in which two apps use the same such directory is rejected.
- Do not serve one such directory from two server instances either; give each instance its own copy.
- If a page is broken, delete its `.html` file so that it is rendered again.

Other R Markdown documents are rendered into a temporary directory for each visit, so their directories may be shared.

### Adding an app

Create the app's directory, add an entry with its `name` and `path` to the configuration file, and click **Reload Config & Restart All** on the management dashboard.

### Running an app on its own

While developing an app, you can run it directly from R, with tinyshinyserver stopped or on a port that the server does not use. For the copied examples:

```r
shiny::runApp("examples/sales", port = 3001)
rmarkdown::run("examples/reports/report.Rmd",
               shiny_args = list(port = 3003, host = "127.0.0.1"))
quarto::quarto_serve("examples/dashboard/dashboard.qmd",
                     port = 3004, host = "127.0.0.1")
```

Each of these commands runs until it is interrupted, so run one at a time.

## App lifecycle

### Processes and ports

The R session that runs `start_tss()` serves the proxy and the management dashboard. Each running app has its own background R process, which listens on `127.0.0.1` at the app's port; the proxy forwards HTTP requests and WebSocket messages to it. If the R session that runs the server is killed, its app processes stop too.

Apps receive ports in configuration order, starting from `starting_port` and skipping the proxy port, the management port, and ports already in use, including a port that another program listens on at every interface. With the example configuration and no conflicts, the four apps receive ports 3001–3004. The assignments are written to `server.log` and are made again at each configuration reload.

The proxy forwards traffic to an app only when the app's own process is listening on the app's port. If another program holds that port, such as a second server instance with the same `starting_port`, the proxy returns HTTP 503 and logs a warning instead of sending your users' requests to that program. This check needs the `ps` package to list the process's sockets, which it cannot do on every platform.

### Resident and on-demand apps

| Mode | Starts | Stops | Use when |
|------|--------|-------|----------|
| Resident (`"resident": true`) | With the server; again after a process failure | At shutdown, restart, or configuration reload | Immediate access matters |
| On-demand (`"resident": false`, the default) | At the first HTTP request or WebSocket connection | When no longer used, as described below | Saving resources matters more than startup delay |

An on-demand app stops once it has had no WebSocket connection and no HTTP request in progress for 30 seconds. The grace period lets a reloaded or newly opened page reconnect without restarting the app. A page's WebSocket opens only once the browser has the page, so after a large response the grace period also allows for a slow browser to receive it, at 128 KB per second, for up to 10 more minutes.

An app that is still starting at that point is left to finish, for up to 10 minutes from its start, and stops within 30 seconds after it is ready if it is still unused. A slow start, such as a Quarto render, is therefore not thrown away when the visitor's page stops waiting. On platforms where the `ps` package cannot list a process's sockets, the server cannot tell that an app is still starting, and stops it.

A failed on-demand app, such as one that stops with an error at startup or crashes while in use, starts again at the first request that comes at least `restart_delay` seconds after the failure. An earlier request receives HTTP 503 with a `Retry-After` header for the time that is left, and a browser receives the page that reloads itself, so a failing app is not started again by every reload. A restart from the management dashboard ends the wait. An app that ends itself without an error, as one that calls `shiny::stopApp()` does, has not failed, and an on-demand one starts again at the next request.

### Startup

A request for an app that is starting waits for the app to be ready, for up to `appstart_timeout` seconds (default: 2) counted from the app's start. After that, the proxy answers HTTP 503 with a `Retry-After` header. A browser opening the app's page receives a page that reloads itself until the app is ready, for up to two minutes; scripts and other clients receive JSON. Increase `appstart_timeout` for apps that start slowly.

If an app still does not answer after 30 seconds, or after `appstart_timeout` if that is longer, the next request receives HTTP 502 ("App startup timed out") once, and later requests receive HTTP 503 again. The server keeps checking an app whose process is still running for up to 10 minutes from its start, and logs it as ready once it answers.

### Failures and restarts

Every `health_check_interval` seconds, the server looks for app processes that have exited, and restarts a resident app after `restart_delay` seconds. An app process that exits with an error, or is killed, has failed, and `server.log` says that the app died; one that ends without an error, as an app that calls `shiny::stopApp()` does, is logged as having exited. These checks do not test whether a running app responds or produces correct results.

## Monitoring and management

| Page | Default URL | Shows |
|------|-------------|-------|
| Landing page | http://localhost:3838 | A tile for each app, with its status and connection count |
| Management dashboard | http://localhost:3839 | Server uptime and memory use, R version, and operating system; each app's mode, status, process ID, port, path, memory use, and uptime; and each WebSocket connection's client IP address, user agent, connection time, and last activity |

Both pages refresh their status every 5 seconds. They follow the system's light or dark theme by default; the theme button in the top bar switches between the two, and the browser remembers that choice until you switch back to match the system.

### App status

| Status | Meaning |
|--------|---------|
| **running** | The app's process is running |
| **dormant** | An on-demand app that is not running, including one whose process has exited; opening it starts it, after `restart_delay` if its process failed |
| **unavailable** | An on-demand app whose directory cannot be served; `server.log` gives the reason |
| **stopped** | A resident app that is not running, such as one waiting to be restarted |
| **crashed** | A resident app whose process has exited and has not yet been cleaned up by a health check |

The landing page links to running and dormant apps. Tiles for unavailable, stopped, and crashed apps are disabled, and all tiles are disabled while the server is unreachable.

Client IP addresses are taken from `X-Forwarded-For` or `X-Real-IP` only when the request comes from a reverse proxy on the same machine, and only when the value is an IP address. The last `X-Forwarded-For` entry is used, so the reverse proxy must add the client's address to that header rather than pass on the one the client sent; Caddy does so by default, and nginx needs `proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;`. Behind a reverse proxy on another machine, the dashboard shows that proxy's address.

Memory figures are the resident memory (RSS) of the server's own R process and of each app's R process. Processes that an app starts itself, such as Pandoc or a database driver's helper, are not included. An app's uptime starts from zero whenever its process starts again. Memory shows as N/A on platforms where the `ps` package cannot read it.

### Restarting an app

Running, stopped, and crashed apps can be restarted from the management dashboard. A restart disconnects the app's users once their app's process has stopped; if it cannot be stopped, the restart fails and they stay connected. A resident app starts again at once, without waiting for `restart_delay`; an on-demand app stops and starts again when it is next opened. Dormant apps start when opened, and an unavailable app becomes dormant once its directory can be served. A dormant app whose process failed shows how long it waits before it can start again, and restarting it ends that wait.

### Reloading the configuration

**Reload Config & Restart All** on the management dashboard reads the configuration file again, stops every app, and disconnects their users. It then assigns app ports under the new configuration and starts the resident apps; on-demand apps start when next opened. Use it to add, remove, or change apps, or to apply a changed `log_level`, without restarting the server.

The new file is checked first. Nothing is stopped if the file is invalid, if the server log cannot be written in its `log_dir`, if other programs hold the ports that the apps would need, or if it changes `proxy_host`, `proxy_port`, or `management_port`; those three settings take effect only when the server restarts. If an app cannot be stopped, the previous configuration stays in effect, the users of that app stay connected, as when a restart cannot stop an app, and the stopped resident apps start again. The dashboard reports the outcome once the reload finishes.

### Proxy endpoints

These read-only endpoints are served on the proxy port. Like the landing page, they answer the other methods that the proxy forwards with HTTP 405, and methods that it does not forward at all with HTTP 501.

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Returns `{"status": "healthy"}` while the proxy is responding; does not check the apps |
| `/api/apps` | GET | Name, status, mode, and connection count of each app, used by the landing page |

### Management API

These endpoints are served on the management port. `POST` requests must carry the header `X-TinyShinyServer-Request: management`, which browsers send from another website's page only where CORS allows it; the dashboard sends it automatically. The dashboard and the `GET` endpoints also answer `HEAD`, and an endpoint asked with a method it does not serve answers HTTP 405. The header is checked first, so a `POST` without it receives HTTP 403 from every endpoint, and a method that the server does not recognize at all, such as `PROPFIND`, receives HTTP 501.

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/status` | GET | Server overview: app and connection counts, `uptime_seconds`, `server_memory_bytes`, `apps_memory_bytes` (the total of running apps), and the outcome of the latest configuration reload |
| `/api/apps` | GET | Detailed status of each app, including `pid`, `memory_bytes`, and `uptime_seconds` for running apps, and `restart_in_seconds` for a dormant app whose process failed |
| `/api/connections` | GET | Details of each active WebSocket connection |
| `/api/apps/{name}/restart` | POST | Restart the named app |
| `/api/reload` | POST | Reload the configuration file and restart all apps |
| `/api/shutdown` | POST | Shut down the server |

The `POST` endpoints answer JSON with `success` and `message` fields. A request refused before it reaches the action, such as one without the required header or with an invalid app name, receives JSON with an `error` field instead. A `POST` without the required header receives HTTP 403, a restart of an app that is not configured receives HTTP 404, and a reload of an invalid configuration receives HTTP 400 with the reason. An accepted reload runs after its response is sent; its outcome appears in `/api/status` as `last_reload` once it finishes.

For example:

```bash
# Get the server status
curl http://localhost:3839/api/status

# Restart the sales app
curl -X POST -H "X-TinyShinyServer-Request: management" http://localhost:3839/api/apps/sales/restart

# Reload the configuration and restart all apps
curl -X POST -H "X-TinyShinyServer-Request: management" http://localhost:3839/api/reload

# Shut down the server
curl -X POST -H "X-TinyShinyServer-Request: management" http://localhost:3839/api/shutdown
```

## Network access and security

`proxy_host` sets the network interface that the proxy listens on. The management server and the apps always listen on `127.0.0.1` only.

| `proxy_host` | Reachable from |
|--------------|----------------|
| `"127.0.0.1"` (default) or `"localhost"` | This machine, over IPv4 |
| `"::1"` | This machine, over IPv6 |
| `"0.0.0.0"` | All IPv4 interfaces |
| `"::"` | All IPv6 interfaces, and on most systems all IPv4 interfaces too |

### Security model

The server has no authentication or TLS of its own. For access from other machines, put an authenticated HTTPS reverse proxy in front of it, and use firewall rules so that only the reverse proxy can connect to the proxy port. Also keep in mind:

- **Identity headers.** The proxy passes the client's request headers, apart from `Host` and connection-level headers, on to the app, both on HTTP requests and on the WebSocket connection whose headers Shiny exposes as `session$request`. The WebSocket connection does not carry the browser's `Origin`, which the proxy checks itself and which would not match the app's internal address, nor the browser's own `Sec-WebSocket-` fields. An app can therefore read an identity header that the reverse proxy sets, such as `X-Forwarded-User`. Rely on such a header only if the reverse proxy overwrites it and clients cannot reach the proxy port directly; otherwise a client can send its own. The proxy also leaves out every header that the request's `Connection` header names, as HTTP asks of a proxy, so the reverse proxy must set `Connection` itself and not pass the client's on: a client that could name `X-Forwarded-User` there would reach the app without the identity that the reverse proxy set, and an app must not take a missing identity header for a trusted visitor. Headers whose names contain an underscore are not passed on, because R's web server reads `X_Forwarded_User` as `X-Forwarded-User`, in place of the value that the reverse proxy set.
- **DNS rebinding.** The proxy checks the `Host` header, which blocks DNS rebinding, only when `proxy_host` is a loopback address. With `"0.0.0.0"` or `"::"`, any website visited by someone who can reach the proxy port can point its own domain at this server, and then read and control the apps through that person's browser. A firewall that admits a whole network therefore does not keep the apps private from the web; allow direct connections to the proxy port only from the reverse proxy.
- **App ports.** Each app process listens on `127.0.0.1` at its own port and does not check the `Host` header, so a website opened in a browser on the server machine can still use DNS rebinding to reach a running app directly. Do not browse untrusted websites on the server machine while apps that handle sensitive data are running.
- **The management server** has no authentication. Anyone who can connect to its port on `127.0.0.1`, including every other user account on the server machine, can see client IP addresses and user agents, restart apps, reload the configuration, and shut down the server. Run the server only on a machine whose local accounts may be given that control.
- **Shared origin.** All apps are served from the same origin, so they are not isolated from each other: a script in one app's page can send requests to another app with that app's cookies, read its pages, and open its WebSocket sessions. Serve together only apps that trust each other, and run the others in separate servers under different host names.
- **Cookies.** The proxy gives each cookie that an app sets the path of the app's URL, such as `/proxy/sales/`, so that apps do not receive each other's cookies. An app cannot therefore use a cookie whose name starts with `__Host-`, which browsers keep only with `Path=/`; `server.log` warns once when an app sets one. Use a cookie without the prefix, with the `Secure` attribute behind HTTPS.

### Reverse proxy requirements

The proxy uses the `Host` header to block DNS rebinding, and accepts an app's WebSocket connection only when the browser's `Origin` matches the public host. A reverse proxy must therefore send these headers:

- **On the same machine** (`proxy_host` is a loopback address): forward the upstream address as the `Host` header, because the proxy rejects requests with a `Host` header other than `localhost`, `127.0.0.1`, or `[::1]`. Also set `X-Forwarded-Host` to the public host, including a non-default port, so that the WebSocket `Origin` check passes: from a reverse proxy on the same machine that sends this header, the `Origin` is matched against it alone, and no longer against `Host`. The [Caddy example](#caddy-example) does both: `header_up Host {upstream_hostport}` sets the host, and Caddy sets `X-Forwarded-Host` by default. With nginx, use `proxy_set_header X-Forwarded-Host $http_host;` (not `$host`, which drops the port), set `proxy_http_version 1.1;` and the `Upgrade` and `Connection` headers that WebSockets need, with `Connection` taken from a `map` of `$http_upgrade` as in nginx's WebSocket documentation and not from the client's own header (`$http_connection`), which may name headers for the proxy to drop, and add the client's address with `proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;`; otherwise nginx passes on a client's own `X-Forwarded-For`, and the dashboard shows whatever address the client chose. Because the proxy then sees only the upstream address as the host, the reverse proxy takes over the DNS-rebinding check: it must serve the apps only under their own host names, and refuse requests for any other name, as Caddy's site blocks do. nginx sends a request for a name that no `server_name` matches to the port's default server, which is the first server block for the port unless another is marked `default_server`, so a configuration with a single server block passes every host name to the proxy, and a rebinding website could reach the apps through a visitor's browser. Add a default server that refuses other names, such as `server { listen 80 default_server; listen 443 ssl default_server; ssl_reject_handshake on; return 444; }`.
- **HTTPS**: a reverse proxy that serves the apps over HTTPS should send `X-Forwarded-Proto: https`, as Caddy does by default. The proxy then refuses a WebSocket that a page on the plain-HTTP site of the same host tries to open, so that a page injected into unencrypted traffic cannot use the HTTPS site's sessions.
- **On another machine** (`proxy_host` is `"0.0.0.0"` or `"::"`): preserve the public `Host` header, because `X-Forwarded-Host` is trusted only from the same machine. With nginx, use `proxy_set_header Host $http_host;`, not `$host`, which drops a non-default port and makes the WebSocket `Origin` check fail. The proxy then checks no host name, so the reverse proxy must refuse requests for names other than the apps' own, with a default server as above, or a rebinding website could reach the apps through it.
- **Management site**: put the management server behind the reverse proxy only if you need remote administration. It always rejects requests with a `Host` header other than `localhost`, `127.0.0.1`, or `[::1]`, so forward the upstream address as the host. Keep CORS disabled, and serve it on a different host name from the apps, such as `manage.example.com`, never under a path on the apps' site. Otherwise scripts in other pages could send the management API's required request header, and restart apps or shut down the server.

### Caddy example

This configuration serves the apps and the management dashboard on two host names, each with a password. Keep `proxy_host` set to `"127.0.0.1"`, and replace the domains, usernames, and password hashes. Generate each hash with `caddy hash-password`; Caddy's [`basic_auth` directive](https://caddyserver.com/docs/caddyfile/directives/basic_auth) requires hashed passwords. The browser sends the password with every request, in the `Authorization` header, and `header_up -Authorization` keeps it from being passed on to every app.

```caddyfile
myapp.example.com {
    reverse_proxy 127.0.0.1:3838 {
        header_up Host {upstream_hostport}
        header_up -Authorization
    }
    basic_auth {
        username REPLACE_WITH_PASSWORD_HASH
    }
}

manage.myapp.example.com {
    reverse_proxy 127.0.0.1:3839 {
        header_up Host {upstream_hostport}
    }
    basic_auth {
        admin REPLACE_WITH_ADMIN_PASSWORD_HASH
    }
}
```

### Serving under a URL prefix

To serve the apps under a path on an existing site, such as `https://example.com/shiny/`, set `base_path` to that path:

```json
"base_path": "/shiny"
```

The landing page is then at `/shiny/`, and each app at `/shiny/proxy/{name}/`. Links, redirects, and cookie paths include the prefix. The proxy accepts requests with or without the prefix, so the reverse proxy may either pass the path through unchanged or strip the prefix. Every app therefore has two addresses at the proxy, `/shiny/proxy/{name}/` and `/proxy/{name}/`: where the reverse proxy limits access by path, its rules must cover both, and it should forward only paths under the prefix, since behind one that strips the prefix, `/shiny/shiny/proxy/{name}/` reaches the same app. In Caddy:

```caddyfile
example.com {
    redir /shiny /shiny/ 308

    # Pass the path through; use handle_path instead to strip the prefix
    handle /shiny/* {
        reverse_proxy 127.0.0.1:3838 {
            header_up Host {upstream_hostport}
        }
    }
}
```

In nginx, `location /shiny/ { proxy_pass http://127.0.0.1:3838; }` passes the path through, and `proxy_pass http://127.0.0.1:3838/;` strips it. The [reverse proxy requirements](#reverse-proxy-requirements) still apply.

The first segment of `base_path` may not be `proxy`, `api`, `templates`, or `health`, which the proxy already uses. Once `base_path` is set, open the apps through URLs that include it: app cookies are scoped to the prefixed path, so an app opened at `http://localhost:3838/proxy/{name}/` does not receive them.

The management dashboard uses relative URLs and needs no setting. A reverse proxy may serve it under a prefix that it strips, opened with the trailing slash, but on its own host name, as described in [Reverse proxy requirements](#reverse-proxy-requirements).

## Limits and timeouts

Requests:

- A request for an app that is not configured receives HTTP 404. `/proxy/{name}` without the trailing slash redirects to `/proxy/{name}/` (HTTP 307).
- The proxy forwards `GET`, `HEAD`, `POST`, `PUT`, `PATCH`, `DELETE`, and `OPTIONS` requests. Other methods receive HTTP 501 (Not Implemented).
- Request bodies larger than `max_request_size_mb` (default: 100 MB), and bodies sent with chunked transfer encoding, are rejected with HTTP 413 before they are read. Apps that raise `shiny.maxRequestSize` above this limit need a larger value.
- At most 64 requests with a body, such as uploads, are received or awaited at a time, on the proxy and the management server together; a further one receives HTTP 503 with a `Retry-After` header before its body is read. R's web server holds one of the R session's 128 connections for each such request until the app has answered it, and the server needs the others to write its log, serve its pages, and start apps. A request with a body that no app would receive, such as one for an unknown path or app, is refused before its body is read.
- Query strings longer than 8,192 characters are rejected with HTTP 400, and WebSocket handshakes with such a query string are refused.
- Paths longer than 1,000 characters, counting the `/proxy/{name}` prefix, paths with a `..` segment, and paths or query strings with a `#`, which browsers do not send, are rejected with HTTP 400. Otherwise, the path after `/proxy/{name}` reaches the app exactly as the client sent it.
- Request headers whose names contain an underscore, such as `X_Custom`, are not passed on to apps; see [Security model](#security-model).

Responses:

- If a running app does not accept connections, the proxy returns HTTP 503. Once the app accepts a connection, a request fails with HTTP 502 if the connection breaks or the app sends no data for 10 minutes; there is no limit on the total transfer time.
- A response larger than `max_response_size_mb` (default: 1024 MB) is not forwarded: the proxy stops receiving it and answers HTTP 502, and `server.log` says why. The proxy cannot tell when a visitor stops waiting, so without this limit a response that an app streams without end would fill the disk and keep an on-demand app from stopping. A response that never ends but stays below the limit, such as one that sends a few bytes every few minutes, still keeps its request, and an on-demand app, in use until the app ends it.
- A response reaches the browser once the app has finished sending it. A response that an app streams, such as server-sent events, is therefore not delivered piece by piece, and a download shows no progress until the proxy has received all of it.
- A response larger than 8 MB is written to a file in R's temporary directory as it arrives, sent from there, and then deleted. It needs that much free disk space for the duration of the request, instead of the server's memory. If the file cannot be written, for example because the disk is full, the request receives HTTP 502, and `server.log` says why.
- If a client disconnects while a response is being sent over a keep-alive connection, which browsers and most reverse proxies use, R's web server, httpuv, does not release the response ([rstudio/httpuv#448](https://github.com/rstudio/httpuv/issues/448)). It stays in memory, or in that file, until the R session that runs the server ends. Repeated aborted downloads of large responses therefore use up memory or disk; restart the server if that happens.

WebSocket sessions:

- A message from the browser larger than 1 MB closes the session. The check runs only after the whole message has been received, so it does not limit the memory that a single very large message uses; expose the proxy port only to trusted or authenticated clients.
- A message from an app to the browser may be up to 2 GB, as long as the app has not sent more than `max_unanswered_mb` (default: 256 MB, below) since the browser's last message.
- A session that has carried no message in either direction for `idle_timeout_minutes` (default: 30) is closed, and the browser shows the app as disconnected. Sessions are checked every 5 minutes, and each one closed is recorded in `server.log`. For dashboards that people leave open without interacting, raise the value, or set it to `0` to keep idle sessions open.
- The proxy cannot tell how much of what it sends a browser has been read. A browser that stops reading while the app keeps sending would therefore make the proxy hold the app's messages in memory, and the app's traffic would keep the session from counting as idle. A session to which the app has sent more than `max_unanswered_mb` (default: 256 MB) without a message from the browser in between is closed, and `server.log` records it. A dashboard that people leave open without interacting, and that receives more than that, is disconnected too; raise the value, or set it to `0`. Each message from the browser starts the count again, so a client that keeps sending messages while it does not read is not limited. The limit applies to each session, and the number of sessions is not limited, so expose the proxy port only to trusted or authenticated clients.
- Up to 100 messages, and up to 16 MB, per session are held while the app's WebSocket opens. One more closes the session, and the browser shows the app as disconnected.
- An app's R process completes a WebSocket handshake only between its computations. If it is busy, for example with another visitor's long calculation, the proxy tries again for up to 2 minutes while the browser waits, and then closes the browser's session.

## Logging

### Log files

The server writes these files to `log_dir`:

| File | Contents |
|------|----------|
| `server.log` | The server's own log |
| `{name}_output.log` | The app's standard output |
| `{name}_error.log` | The app's standard error, including R errors and warnings |
| `{name}_output.prev.log`, `{name}_error.prev.log` | The same logs from the app's previous run |
| `{name}_render.rds` | For a Quarto app, the record of the server's renders of its document; see [R Markdown and Quarto documents](#r-markdown-and-quarto-documents) |

App output goes straight to these files and is not held in memory. When an app starts, its logs from the previous run are kept as the `.prev.log` files, so the output of a crash survives the automatic restart.

The server does not rotate or trim its logs. `server.log` grows for as long as the server runs, and the next start appends to it; the server opens it for each line it writes, so a tool such as logrotate can rotate it while the server runs. An app's logs grow until the app restarts.

To follow the server log from a terminal:

```bash
tail -f logs/server.log
```

```powershell
Get-Content logs/server.log -Wait
```

### Log levels

`log_level` chooses the least severe level written to `server.log` (default: `"INFO"`). A configuration reload applies a changed level.

| Level | Records |
|-------|---------|
| `"DEBUG"` | Each request's routing, port probing at configuration reloads, connection bookkeeping, the full address of each app WebSocket connection, and the first 100 bytes of every WebSocket message |
| `"INFO"` | Normal operations, such as apps starting and stopping and connections opening and closing |
| `"WARN"` | Warning conditions, such as a session closed for a limit, or a refused WebSocket handshake |
| `"ERROR"` | Errors that need attention |

A refused WebSocket handshake, such as one from another website's page, or one for an app that cannot start or whose process is not listening on its port, is logged at WARN at most once every 10 seconds for each reason, and for each app when the reason lies with the app, with the number of others since then, which are logged at DEBUG. A page that opens handshakes in a loop therefore does not fill `server.log`.

WebSocket messages can include what users type, and WebSocket addresses can carry tokens in their query strings, so use `"DEBUG"` only while diagnosing a problem.

## Troubleshooting

### Apps don't start

- Check the R console for errors at startup, such as a proxy or management port that is already in use or cannot be opened. A port counts as in use when another program listens on it at 127.0.0.1 or `[::1]`. A port that cannot be opened may be below 1024, which on Linux needs administrator rights, or `proxy_host` may be `"::1"` or `"::"` on a machine without IPv6.
- Check `server.log`. It names an app directory that is missing or holds no supported [entry point](#entry-points), and records the ports assigned to apps.
- Check the app's error log, `{name}_error.log`. After a crash and automatic restart, the crash output is in `{name}_error.prev.log`.
- R Markdown apps need Pandoc, and Quarto apps need the Quarto CLI.

### WebSocket connections fail

- Check the app's status on the management dashboard, and the WebSocket messages in `server.log`.
- Check that no firewall blocks WebSocket connections.
- Behind a reverse proxy, check that it forwards WebSocket upgrades and meets the [reverse proxy requirements](#reverse-proxy-requirements). The proxy rejects requests for other hosts and cross-origin WebSocket connections.

### The management dashboard is not reachable

- Check the R console to confirm that the server is running, and `server.log` for the management server's startup messages.
- Open http://localhost:3839 on the server machine itself, not through its external IP address or host name. Requests with a `Host` header other than `localhost`, `127.0.0.1`, or `[::1]` receive HTTP 403.
- Check that no firewall blocks localhost connections.

## Development

Clone the repository:

```bash
git clone https://github.com/lab1702/tinyshinyserver.git
cd tinyshinyserver
```

Then, in R from the repository root:

```r
devtools::install_deps(dependencies = TRUE)
devtools::load_all()  # load the package for interactive use
devtools::test()      # run the tests
devtools::check()     # run R CMD check
```

The code is in `R/`, the example apps are in `inst/examples/`, and the web page templates and styles are in `inst/templates/`. Help pages are generated from the roxygen comments in `R/`: edit those, then run `devtools::document()` to update `man/`.

The browser tests in `tests/browser/` check the management dashboard, the landing page, and the page shown while an app starts in a headless Chromium. They need Node.js, with Playwright on `NODE_PATH`, and are not part of `R CMD check`. Run them from the repository root; `management-csrf.cjs` also needs `Rscript` on the `PATH`. Set `TSS_TEST_BROWSER` to use a specific browser executable.

```bash
node tests/browser/management-page.cjs
node tests/browser/management-csrf.cjs
node tests/browser/public-pages.cjs
```

To contribute, open an issue or a pull request on GitHub. Add tests for your change, and run `devtools::check()` before submitting.

## Getting help

- **In R**: `?start_tss`, `help("config-format")`, `help("example-config")`, and `?tinyshinyserver`
- **Release notes**: [NEWS.md](NEWS.md), including the changes in 0.3.0 that reverse proxies need
- **Bug reports and questions**: [GitHub Issues](https://github.com/lab1702/tinyshinyserver/issues)

// Theme handling shared by all Tiny Shiny Server pages.
// Pages follow the system color scheme until the visitor picks the other one
// with a [data-theme-toggle] button; picking the system scheme again clears
// the override. Load this synchronously in <head> so the theme applies before
// the first paint.
(function () {
  var KEY = "tss-theme";
  var root = document.documentElement;
  var media = window.matchMedia ? window.matchMedia("(prefers-color-scheme: dark)") : null;

  function systemTheme() {
    return media && media.matches ? "dark" : "light";
  }

  function storedTheme() {
    try {
      var value = window.localStorage.getItem(KEY);
      return value === "light" || value === "dark" ? value : null;
    } catch (e) {
      return null;
    }
  }

  function store(value) {
    try {
      if (value) window.localStorage.setItem(KEY, value);
      else window.localStorage.removeItem(KEY);
    } catch (e) {
      // Storage may be unavailable (private mode, blocked site data)
    }
  }

  function apply() {
    var theme = storedTheme() || systemTheme();
    root.setAttribute("data-theme", theme);
    var next = theme === "dark" ? "light" : "dark";
    var buttons = document.querySelectorAll("[data-theme-toggle]");
    for (var i = 0; i < buttons.length; i++) {
      buttons[i].setAttribute("aria-label", "Switch to " + next + " theme");
      buttons[i].setAttribute("title", "Switch to " + next + " theme");
    }
  }

  function toggle() {
    var next = root.getAttribute("data-theme") === "dark" ? "light" : "dark";
    store(next === systemTheme() ? null : next);
    apply();
  }

  apply();

  if (media) {
    var onChange = function () { apply(); };
    if (media.addEventListener) media.addEventListener("change", onChange);
    else if (media.addListener) media.addListener(onChange);
  }

  // Another tab of the same page changed the theme
  window.addEventListener("storage", function (event) {
    if (event.key === KEY) apply();
  });

  document.addEventListener("DOMContentLoaded", function () {
    apply();
    var buttons = document.querySelectorAll("[data-theme-toggle]");
    for (var i = 0; i < buttons.length; i++) {
      buttons[i].addEventListener("click", toggle);
    }
  });
})();

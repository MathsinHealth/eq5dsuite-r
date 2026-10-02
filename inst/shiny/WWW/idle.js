// Idle watch, used only in online mode.
//
// The server cannot tell a user reading a table from a user who has walked
// away, so the timer runs here and tells the server when it fires. Any real
// interaction resets it.
(function () {
  var warnAt = null, endAt = null, warned = false, timer = null;

  function reset() {
    if (warnAt === null) return;
    warned = false;
    clearTimeout(timer);
    schedule();
  }

  function schedule() {
    timer = setTimeout(function () {
      if (!warned) {
        warned = true;
        Shiny.setInputValue("eq5d_idle_warning", Date.now(), {priority: "event"});
        timer = setTimeout(function () {
          // Close the connection; Shiny shows its disconnected screen and the
          // server tears the session down.
          if (Shiny.shinyapp && Shiny.shinyapp.$socket) Shiny.shinyapp.$socket.close();
        }, (endAt - warnAt) * 1000);
      }
    }, warnAt * 1000);
  }

  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("eq5d_idle_start", function (m) {
      warnAt = m.warnAfter; endAt = m.endAfter;
      ["mousemove", "mousedown", "keydown", "touchstart", "scroll"]
        .forEach(function (ev) {
          document.addEventListener(ev, function () { if (!warned) reset(); },
                                    {passive: true});
        });
      schedule();
    });
    Shiny.addCustomMessageHandler("eq5d_idle_reset", function () { reset(); });
  });
})();

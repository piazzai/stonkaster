Shiny.addCustomMessageHandler("alert",
  function(message) {
    alert(JSON.stringify(message));
  }
);

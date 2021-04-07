if ("serviceWorker" in navigator) {
  const sw = "service-worker.js";

  navigator.serviceWorker
    .register(sw)
    .then((_registration) => {
      console.log("Service worker registered.");
    })
    .catch((err) => {
      console.log("Service worker not registered. This happened:", err);
    });
}

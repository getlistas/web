// TODO: run  this as FFI on Main

if ("serviceWorker" in navigator) {
  const sw = "service-worker.js";

  navigator.serviceWorker
    .register(sw)
    .then((_registration) => {
      console.log("Service worker registered.");
    })
    .catch((err) => {
      console.error("Service worker not registered. This happened:", err);
    });
}

exports.register_ = () => {
  if ("serviceWorker" in navigator) {
    const sw = "service-worker.js";

    try {
      return navigator.serviceWorker
        .register(sw)
        .then((_registration) => {
          console.log("Service worker registered.");

          return true;
        })
        .catch((err) => {
          console.error("Service worker not registered. This happened:", err);

          return false;
        });
    } catch (err) {
      console.error("Service worker not registered. This happened:", err);
      return Promise.resolve(false);
    }
  }

  console.error("No serviceWorker in navigator");
  return Promise.resolve(false);
};

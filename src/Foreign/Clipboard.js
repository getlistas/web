exports.writeText_ = (str) =>
  navigator
    .clipboard
    .writeText(str)
    .then(() => true)
    .catch(() => false)

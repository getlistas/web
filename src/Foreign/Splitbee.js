const splitbee = require('@splitbee/web').default;

exports.init = () => { splitbee.init() }

exports.track = eventName => () => {
  splitbee
    .track(eventName)
    .catch(() => {})
}

exports.trackWithData = (eventName) => (data) => () => {
  splitbee
    .track(eventName, data)
    .catch(() => {})
}

exports.userSet = (data) => () => {
  splitbee
    .user.set(data)
    .catch(() => {})
}

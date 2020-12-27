module.exports = {
  purge: false,
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {},
  },
  variants: {
    extend: {
      backgroundColor: ['checked'],
      borderColor: ['checked'],
      borderWidth: ['hover'],
    }
  },
  plugins: [
    require('@tailwindcss/forms'),
  ],
}

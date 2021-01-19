module.exports = {
  purge: false,
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {
      colors: {
        gray: {
          400: "#565B6C",
          300: "#7B829A",
          200: "#A0AEC0",
          100: "#ECEDF1",
          10: "#FAF9F9",
        },
        kiwi: {
          DEFAULT: "#89B0AE",
        },
        durazno: {
          light: "#F8D7BE",
          DEFAULT: "#F9B7A9",
        },
        manzana: {
          DEFAULT: "#E9755B",
        },
      },
    },
  },
  variants: {
    extend: {
      backgroundColor: ["checked"],
      borderColor: ["checked"],
      borderWidth: ["hover"],
      cursor: ["disabled"],
      opacity: ["disabled"],
    },
  },
  plugins: [
    require("@tailwindcss/forms"),
    require("@tailwindcss/line-clamp"),
    require("tailwind-scrollbar"),
  ],
};

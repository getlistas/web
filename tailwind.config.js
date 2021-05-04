module.exports = {
  purge: false,
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {
      gridTemplateRows: {
        '7': 'repeat(7, minmax(0, 1fr))'
      },
      gridTemplateColumns: {
        '52': 'repeat(52, minmax(0, 1fr))'
      },
      animation: {
        "spin-slow": "spin 2s linear infinite",
      },
      colors: {
        gray: {
          400: "#565B6C",
          300: "#7B829A",
          200: "#A0AEC0",
          100: "#ECEDF1",
          10: "#FAF9F9",
        },
        kiwi: {
          light: "#C1D4D3",
          DEFAULT: "#89B0AE",
          dark: "#749593",
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
      backgroundColor: ["checked", "disabled"],
      borderColor: ["checked"],
      borderWidth: ["hover", "group-hover"],
      cursor: ["disabled"],
      display: ["group-hover"],
      opacity: ["disabled"],
      ringColor: ["hover"],
      ringOffsetColor: ["hover"],
      ringOffsetWidth: ["hover"],
      ringOpacity: ["hover"],
      ringWidth: ["hover"],
      textColor: ["disabled"],
    },
  },
  plugins: [
    require("@tailwindcss/forms"),
    require("@tailwindcss/line-clamp"),
    require("tailwind-scrollbar"),
    require("@tailwindcss/aspect-ratio"),
  ],
};

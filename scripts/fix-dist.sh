styles="$(ls dist | grep styles)"
mainjs="$(ls dist | grep main)"
sed "s|styles-.*.css|$styles|" -i dist/index.html
sed "s|main-.*.js|$mainjs|" -i dist/index.html

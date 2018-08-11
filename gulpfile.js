const organiser = require("gulp-organiser");

organiser.registerAll("./tasks", {
    "build-elm": {
        watch: "src/**/*",
        src: "src/Main.elm",
        dest: "build",
        moduleName: "index",
        ext: "js"
    }
});

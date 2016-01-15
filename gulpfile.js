var gulp        = require('gulp');
var elm  		 = require('gulp-elm');
var rename 	    = require("gulp-rename");
var browserSync = require('browser-sync').create();

gulp.task('elm', function () {
    return gulp.src('Main.elm')
        .pipe(elm.make())
        .on("error", function(err) {
            console.log(err.message);
            this.emit('end');
            })
        .pipe(rename({basename: 'elm'}))
        .pipe(gulp.dest('./'))
});

gulp.task('elm-watch', ['elm'], function(finish) {
	browserSync.reload();
	finish();
});

gulp.task('css-watch', function() {
	return gulp.src('*.css')
		.pipe(browserSync.stream());
});

gulp.task('serve', ['elm'], function () {
    browserSync.init({
        server: {
            baseDir: "./"
        }
    });

    gulp.watch("*.elm", ['elm-watch']);
    gulp.watch('*.css', ['css-watch']);
});

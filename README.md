Contact App in Elm
==================

How to run the app 
------------------

As we have included the latest compiled build in the repo (`elm.js`), one should be able to simply open up `index.html` in a browser. To avoid problems with XHR request security restrictions (requesting data from a non-file-system source), it is recommended to use a simple web server, instead. 

How to build the app
--------------------

For development, the following requirements should be met and be available in the path: 

* Gulp.js installed globally 
* Node/npm installed globally 
* Elm environment installed 

Clone the repo and **delete the elm.js file**. It will be rebuilt later on. Run `elm package install` followed by `npm install`. You can now type `gulp serve` and start making changes. The gulp task will automatically recompile and reload the browser page if changes are detected.

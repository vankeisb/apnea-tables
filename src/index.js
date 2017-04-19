'use strict';

// require the App
var App = require('./App.elm').App;

// init Elm app
var a = App.embed(document.getElementById('main'));

window.app = a;

function gapi() {
    return window.gapi;
}

function withFile(callback, errback) {
    gapi().client.drive.files.list({
        'q': "name='apnee.json'",
        'pageSize': 1,
        'fields': "nextPageToken, files(id, name, webContentLink)"
    }).then(function(response) {
        var files = response.result.files;
        if (files && files.length > 0) {
            var found = false;
            for (var i = 0; i < files.length; i++) {
                var file = files[i];
                if (file.name === 'apnee.json') {
                    found = true;
                    callback(file);
                    break;
                }
            }
            if (!found) {
                errback("Data file not found on your drive !");
            }
        } else {
            errback("file not found !");
        }
    });
}


a.ports.driveReadFile.subscribe(function(msg) {

    function error(s) {
        a.ports.driveOnFileReadError.send(s);
    }

    function success(file) {
        // get the contents of the file
        var accessToken = gapi().client.getToken().access_token;
        // var accessToken = gapi.auth2.getAuthInstance().access_token;
        var xhr = new XMLHttpRequest();
        xhr.open('GET', "https://www.googleapis.com/drive/v3/files/" + file.id + "?alt=media");
        xhr.setRequestHeader('Authorization', 'Bearer ' + accessToken);
        xhr.onload = function () {
            if (xhr.status !== 200) {
                error(xhr.responseText);
            } else {
                // Got the file, notify the app
                a.ports.driveOnFileRead.send({fileId: file.id, content: xhr.responseText});
            }
        };
        xhr.onerror = function () {
            error("unable to read file !");
        };
        xhr.send();
    }


    withFile(success, error);
});


a.ports.driveAuthenticate.subscribe(function(msg) {
    gapi().auth2.getAuthInstance().signIn();
});


a.ports.driveSaveFile.subscribe(function(msg) {
    gapi().client.request({
        'path': '/upload/drive/v2/files/' + msg.fileId + "?uploadType=media",
        'method': 'PUT',
        'headers': {
            'Content-Type': 'application/json',
            'Content-Length': msg.content.length
        },
        'body': msg.content
    }).then(
        function(resp) {
            a.ports.driveOnFileSave.send(null);
        },
        function(err) {
            a.ports.driveOnFileSaveError.send(err);
        }
    );
});





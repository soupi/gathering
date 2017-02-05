"use strict"

var convertDates = function() {
    var datetimes = document.getElementsByClassName('datetime');
    for (var i = 0; i < datetimes.length; i++) {
        var dt = datetimes[i];
        var original = dt.innerHTML.slice(0, -6);
        dt.innerHTML = moment.utc(original, 'YYYY-MM-DD HH:mm').local().format("dddd, MMMM Do YYYY, h:mm a (UTCZZ)");
    }
};

convertDates();

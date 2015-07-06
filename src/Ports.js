// initialize the Shanghai component which keeps track of
// shipping data in and out of the Port of Shanghai.
var shanghai = Elm.fullscreen(Elm.MyGo, {
    incomingMove : null
  });

var firebaseData = new Firebase('boiling-heat-190.firebaseIO.com');
var gameData = firebaseData.child("games/game" + window.location.search.toString());

gameData.on('child_added', function(snapshot) {
    console.log("ON: " + snapshot.val());
    shanghai.ports.incomingMove.send(snapshot.val());
});

shanghai.ports.moveSink.subscribe(
    function (x) {
        console.log("Write: " + x);
        gameData.push(x);
    }
);

shanghai.ports.debugSink.subscribe(
    function (x) {
        console.log("DEBUG: " + x);
    });

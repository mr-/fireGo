// initialize the Shanghai component which keeps track of
// shipping data in and out of the Port of Shanghai.
var shanghai = Elm.fullscreen(Elm.MyGo, {
    incomingMove : null
  });


var firebaseData = new Firebase('boiling-heat-190.firebaseIO.com');
firebaseData.on('child_added', function(snapshot) {
    console.log("ON: " + snapshot.val());
    shanghai.ports.incomingMove.send(snapshot.val());
});

function logger(x) {
    console.log("Write: " + x);
    firebaseData.push(x);
}

shanghai.ports.moveSink.subscribe(logger);

shanghai.ports.debugSink.subscribe(
    function (x) {
        console.log("DEBUG: " + x);
    });

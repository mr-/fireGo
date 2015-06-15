var firebaseData = new Firebase(``);
var elm = Elm.fullscreen(Elm.FireGo.Main);
firebaseData.on('child_added', function(snapshot) {
    elm.send('played', snapshot.val());
});

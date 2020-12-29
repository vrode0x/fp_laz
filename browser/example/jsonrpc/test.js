function btn1Click() {
  changeBtn1Caption('OK');
}

function changeBtn1Caption(s) {
  document.getElementById("btn1").textContent = s;
}

function showMessageInApp(s) {
  //__onReceiveJsonString("ddddd");
  __jrpc.call('showMessage', [btn2.textContent, btn1.textContent]).then(function(result) {
    addLineToTextArea('wb request showMessageInApp()', result);
  });
}

function notifyApp(s) {
  __jrpc.notification('notification_1', [s]);
}

__jrpc.on('test_func', function (s) {
    changeBtn1Caption(s);
    addLineToTextArea('app request test_func()', s);
    return 1;
});

__jrpc.on('notification_1', function (s) {
    addLineToTextArea('app notification_1', s);
});

function addLineToTextArea(prefix, s) {
  var el = document.getElementById("tarea1");
  el.value = el.value + '\n' + prefix + ': ' + s;
}




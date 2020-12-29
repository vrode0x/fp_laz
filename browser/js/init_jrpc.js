__jrpc = new simple_jsonrpc();
__jrpc.toStream = function(msg) {
  __doPostJsonString(msg);
};
function __onReceiveJsonString(msg) {
  __jrpc.messageHandler(msg);
}



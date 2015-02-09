function hs_ajaxBinary(url, cb) {
  var oReq = new XMLHttpRequest();
  oReq.open("GET", url, true);
  oReq.responseType = "arraybuffer";
  oReq.onload = function (oEvent) {
    cb(oReq.response);
  };
  oReq.send(null);
}

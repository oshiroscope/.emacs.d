// https://github.com/chuntaro/NTEmacs64/issues/3
var shell = new ActiveXObject("WScript.Shell");
WScript.Sleep(100);
shell.SendKeys("{ESC}");
shell = null;

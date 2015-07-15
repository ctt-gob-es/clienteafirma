
if (document.all && !window.setTimeout.isPolyfill) {
	var __nativeST__ = window.setTimeout;
	window.setTimeout = function (vCallback, nDelay /*, argumentToPass1, argumentToPass2, etc. */) {
		var aArgs = Array.prototype.slice.call(arguments, 2);
		return __nativeST__(vCallback instanceof Function ? function () {
			vCallback.apply(null, aArgs);
		} : vCallback, nDelay);
	};
	window.setTimeout.isPolyfill = true;
}

var AutoFirma = ( function ( window, undefined ) {

		var VERSION = "1.3";

		var origin = null;
		
		/* Cadena que determina el fin de una respuesta */
		var EOF = "%%EOF%%";

		/**
		 * Determina con un boolean si se accede a la web con Chrome
		 */
		function isChrome () {
			return navigator.userAgent.toUpperCase().indexOf("CHROME") != -1 ||
				navigator.userAgent.toUpperCase().indexOf("CHROMIUM") != -1;
		}
		
		/**
		 * Establece el origen desde el que se realizara la operacion, pero solo la primera vez
		 * que se invoca a este metodo. En las siguientes invocaciones no se hara nada para evitar
		 * que se cambie el origen.
		 * @param originDomain Origen de la pagina.
		 * @returns
		 */
		function setOrigin (originDomain) {
			if (origin == null) {
				origin = originDomain;
			}
		}

		/**
		 * Realiza una operacion de firma/multifirma.
		 * @param params Array con todos los parametros para la ejecucion de la operacion de firma.
		 */
		function signOperation (params) {

			var idSession = generateNewIdSession();
			var cipherKey = ""; //generateCipherKey();
		
			var url = buildUrl(params);
			
			execAppIntent(url, idSession, cipherKey);
		}

		/**
		 * Funciones auxiliares del objeto JS del cliente de firma.
		 **/
		var MAX_NUMBER = 2147483648;

		/* Caracteres validos para los ID de sesion */
		var VALID_CHARS_TO_ID = "1234567890abcdefghijklmnopqrstuwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

		/* Genera un identificador de sesion. */
		function generateNewIdSession () {
			var ID_LENGTH = 20;
			var random = "";
			var randomInts;
			if (typeof window.crypto != "undefined" && typeof window.crypto.getRandomValues != "undefined") {
				randomInts = new Uint32Array(ID_LENGTH);
				window.crypto.getRandomValues(randomInts);
			}
			else {
				randomInts = new Array(ID_LENGTH);
				for (var i = 0; i < ID_LENGTH; i++) {
					randomInts[i] = rnd() * MAX_NUMBER;
				}
			}

			for (var i = 0; i < ID_LENGTH; i++) {
				random += VALID_CHARS_TO_ID.charAt(Math.floor(randomInts[i] % VALID_CHARS_TO_ID.length));
			}

			return random;
		}

		/* Genera un numero aleatorio para utilizar como clave de cifrado. */
		function generateCipherKey() {
			var random;
			if (typeof window.crypto != "undefined" && typeof window.crypto.getRandomValues != "undefined") {
				var randomInts = new Uint32Array(1);
				window.crypto.getRandomValues(randomInts);
				random = zeroFill(randomInts[0] % 100000000, 8);
			}
			else {
				random = zeroFill(Math.floor(((rnd() * MAX_NUMBER) + 1) % 100000000), 8);
			}

			return random;
		}

		/* Completa un numero con ceros a la izquierda. */
		function zeroFill(number, width) {
			width -= number.toString().length;
			if (width > 0) {
				return new Array(width + (/\./.test(number) ? 2 : 1)).join('0')
				+ number;
			}
			return number + "";
		}

		/**
		 * Genera numeros aleatorios con una distribucion homogenea
		 */
		var seed;
		function rnd () {
			if (seed == undefined) {
				seed = new Date().getMilliseconds() * 1000 * Math.random();
			}
		    seed = (seed * 9301 + 49297) % 233280;
		    return seed / 233280;
		}

		/**
		 * Construye una URL para la invocaci&oacute;n del Cliente @firma nativo.
		 * params: Par\u00E1metros para la configuraci\u00F3n de la operaci\u00F3n.
		 */
		function buildUrl (params) {

			// Operacion seleccionada
			var intentURL;
			if (params != null && params != undefined && params.length > 0) {
				intentURL = 'afirma://' + params[0].value + '?';	// Agregamos como dominio el identificador del tipo de operacion
				for (var i = 0; i < params.length; i++) {
					intentURL += (i != 0 ? '&' : '') + params[i].key + '=' + encodeURIComponent(params[i].value); 
				}
			}
			return intentURL;
		}

		/**
		 * Invoca un Intent con la operacion seleccionada, la configuraci\u00F3n establecida y las campos del
		 * formulario pasado como parametro. Si se define un callback para tratar el caso de exito o error de
		 * la operacion, se intentara descargar el resultado devuelto por la app del servidor intermedio de
		 * comunicacion. 
		 *
		 * url: URL tradicional de llamada a la aplicaci\u00F3n nativa.
		 * idSession: Identificador de la sesi\u00F3n para la recuperaci\u00F3n del resultado.
		 * cipherKey: Clave de cifrado para la respuesta del servidor.
		 * successCallback: Actuaci\u00F3n a realizar cuando se recupera el resultado de la operaci&oacute;n.
		 * errorCallback: Actuaci\u00F3n a realizar cuando ocurre un error al recuperar el resultado.
		 */
		function execAppIntent (url, idSession, cipherKey) {

			// Calculamos los puertos
			var ports = getRandomPorts();
			
			// Invocamos a la aplicacion nativa
			openNativeApp(ports, idSession);

			// Enviamos la peticion a la app despues de esperar un tiempo prudencia
			setTimeout(executeOperationByService, getWaitingTime(), ports, url, cipherKey);
		}
		
		/**
		 * Obtiene un puerto aleatorio para la comunicaci\u00F3n con la aplicaci\u00F3n nativa.
		 */
		function getRandomPorts () {
			var MIN_PORT = 49152;
			var MAX_PORT = 65535;
			 
			var ports = new Array();
			ports[0] = Math.floor((Math.random() * (MAX_PORT - MIN_PORT))) + MIN_PORT;
			ports[1] = Math.floor((Math.random() * (MAX_PORT - MIN_PORT))) + MIN_PORT;
			ports[2] = Math.floor((Math.random() * (MAX_PORT - MIN_PORT))) + MIN_PORT;
			
			return ports;
		}
		
		/**
		 * Obtiene un puerto aleatorio para la comunicaci\u00F3n con la aplicaci\u00F3n nativa.
		 */
		function openNativeApp (ports, idSession) {
			
			var portsLine = "";
			for (var i = 0; i < ports.length; i++) {
				portsLine += ports[i];
				if (i < (ports.length - 1)) {
					portsLine += ",";
				}
			}
			openUrl("afirma://service?ports=" + portsLine + "&idsession=" + idSession)
		}
		
		/**
		 * Llama a la aplicacion de firma a traves de la URL de invocacion sin que afecte
		 * a la pagina que se esta mostrando.
		 * @param url URL de invocacion.
		 */
		function openUrl (url) {
			
			// Usamos document.location porque tiene mejor soporte por los navegadores que
			// window.location que es el mecanismo estandar
			if (isChrome()) {
				document.location = url;
			}
			else {
				if (document.getElementById("iframeAfirma") != null) {
					document.getElementById("iframeAfirma").src = url;
				}
				else {
					var iframeElem = document.createElement("iframe");

					var idAttr = document.createAttribute("id");
					idAttr.value = "iframeAfirma";
					iframeElem.setAttributeNode(idAttr);

					var srcAttr = document.createAttribute("src");
					srcAttr.value = url;
					iframeElem.setAttributeNode(srcAttr);

					var heightAttr = document.createAttribute("height");
					heightAttr.value = 1;
					iframeElem.setAttributeNode(heightAttr);

					var widthAttr = document.createAttribute("width");
					widthAttr.value = 1;
					iframeElem.setAttributeNode(widthAttr);

					var styleAttr = document.createAttribute("style");
					styleAttr.value = "display: none;";
					iframeElem.setAttributeNode(styleAttr);

					document.body.appendChild(iframeElem);
				}
			}
		}

		/**
		 * Determina el tiempo de espera prudencial (en milisegundos) que, dependiendo del
		 * sistema, es necesario esperar desde la llamada a la aplicaci\u00F3n nativa hasta
		 * que se le pueda solicitar que realice una operaci\u00F3n.
		 */
		function getWaitingTime () {
			return 20000;	//TODO: Cambiar a 30000
		}

		function executeOperationByService (ports, url, cipherKey) {

			var i = 0;
			var portError = false;
			var httpRequest = getHttpRequest();
			do {
				httpRequest.open("POST", "http://127.0.0.1:" + ports[i] + "/afirma", false);
				httpRequest.setRequestHeader("Content-type","application/x-www-form-urlencoded");

				try {
					httpRequest.send("cmd=" + Base64.encode(url, true));
					portError = false;
				}
				catch(e) {
					if (httpRequest.status == 404) {
						// Interpretamos que este error viene por un problema con el puerto
						portError = true;
						i++;
					} else {
						// Interpretamos que este error viene de la aplicacion
						errorServiceResponseFunction("java.lang.IOException", "Ocurrio un error de red en la llamada al servicio de firma");
						return;
					}
				}
			} while (portError && i < ports.length);

			// Operamos segun el resultado
			if (httpRequest.readyState == 4 && httpRequest.status == 200) {
				successServiceResponseFunction(Base64.decode(httpRequest.responseText, true), cipherKey);
				return;
			}
			
			if (errorCallback != null) {
				errorServiceResponseFunction(null, httpRequest.responseText);
			}
		}
		
		function getHttpRequest() {
			var activexmodes=["Msxml2.XMLHTTP", "Microsoft.XMLHTTP"]; //activeX versions to check for in IE
			if (window.ActiveXObject){ //Test for support for ActiveXObject in IE first (as XMLHttpRequest in IE7 is broken)
				for (var i=0; i<activexmodes.length; i++) {
					try {
						return new ActiveXObject(activexmodes[i]);
					}
					catch(e) {
						//suppress error
					}
				}
			}
			else if (window.XMLHttpRequest) { // if Mozilla, Safari etc
				return new XMLHttpRequest();
			}
			else {
				return false;
			}
		}

		/**
		 * Lee el resultado devuelto por el servicio, 'CANCEL' o empieza por 'SAF-', ejecutara el metodo
		 * de error, si es 'OK' o cualquier otra cosa (que se intepretara como el resultado en base 64)
		 * se ejecutara el metodo de exito. En este ultimo caso, se descifrara el resultado. 
		 * @param data Resultado obtenido.
		 * @param cipherKey Clave para el descifrado del resultado si no es un error.
		 */
		function successServiceResponseFunction (data, cipherKey) {

			// No se ha obtenido respuesta
			if (data == undefined || data == null) {
				errorServiceResponseFunction("es.gob.afirma.core.AOCancelledOperationException", "Operacion cancelada por el usuario");
				return;
			}

			// Termina bien y no devuelve ningun resultado
			if (data == "OK") {
				sendOkToIframe();
				return;
			}
			
			// Se ha cancelado la operacion
			if (data == "CANCEL") {
				errorServiceResponseFunction("es.gob.afirma.core.AOCancelledOperationException", "Operacion cancelada por el usuario");
				return;
			}

			// Se ha producido un error
			if (data.lenght > 4 && data.substr(0, 4) == "SAF_") {
				errorServiceResponseFunction("java.lang.Exception", data);
				return;
			}
			
			// Se ha producido un error y no se ha identificado el tipo
			if (data == "NULL") {
				errorServiceResponseFunction("java.lang.Exception", "Error desconocido");
				return;
			}
			
			// Interpretamos el resultado como un base 64 y el certificado y los datos cifrados
			var signature;
			var certificate = null;
			var sepPos = data.indexOf("|");

			if (sepPos == -1) {
				if (cipherKey != undefined && cipherKey != null && cipherKey.length > 0) {
					signature = decipher(Base64.decode(data, true), cipherKey);
				} else {
					signature = Base64.decode(data, true).replace(/\-/g, "+").replace(/\_/g, "/");
				}
			}
			else {
				if (cipherKey != undefined && cipherKey != null && cipherKey.length > 0) {
					certificate = decipher(data.substring(0, sepPos), cipherKey);
					signature = decipher(data.substring(sepPos + 1), cipherKey);
				} else {
					certificate = data.substring(0, sepPos).replace(/\-/g, "+").replace(/\_/g, "/");
					signature = data.substring(sepPos + 1).replace(/\-/g, "+").replace(/\_/g, "/");
				}
			}

			sendSignatureToIframe(signature, certificate);
		}

		
		function errorServiceResponseFunction (type, message) {
			sendErrorToIframe(
				type ? type : "java.lang.Exception",
				message ? message : "No se ha podido extablecer la comunicaci\u00F3n entre la aplicaci\u00F3n de firma y la p\u00E1gina web"
			);
		}
		
		/**
		 * Notificamos al iframe que hace de pasarela con la aplicacion principal que ha ocurrido un error.
		 * type: 	Tipo de error.
		 * message:	Mensaje de error.
		 */
		function sendErrorToIframe (type, message) {
			var wrapper = new Object();
			wrapper.result = "error";
			wrapper.type = type;
			wrapper.message = message;
			window.frames[0].postMessage(wrapper, origin);
		}
		
		/**
		 * Notificamos al iframe que hace de pasarela con la aplicacion principal que ha ocurrido un error.
		 * type: 	Tipo de error.
		 * message:	Mensaje de error.
		 */
		function sendSignatureToIframe (signature, certificate) {
			var wrapper = new Object();
			wrapper.result = "signature";
			wrapper.signature = signature;
			wrapper.cert = certificate;
			window.frames[0].postMessage(wrapper, origin);
		}
		
		/**
		 * Notificamos al iframe que hace de pasarela con la aplicacion principal que ha ocurrido un error.
		 * type: 	Tipo de error.
		 * message:	Mensaje de error.
		 */
		function sendOkToIframe () {
			var wrapper = new Object();
			wrapper.result = "Ok";
			window.frames[0].postMessage(wrapper, origin);
		}
		
		/**
		 * Realiza un descifrado DES compatible con Java (Algoritmo DES, modo CBC, sin Padding).
		 * Recibe en base 64 la cadena de texto cifrado antecedido por el padding anadido manualmente
		 * a los datos para permitir el cifrado DES (separado por un punto ('.')), ademas de la clave
		 * para descifrar.
		 * Como resultado devuelve la cadena de texto descifrada en base 64.
		 */
		function decipher(cipheredData, key) {
			
			var dotPos = cipheredData.indexOf('.');
			var padding = cipheredData.substr(0, dotPos);
			
			var deciphered = des(key, base64ToString(cipheredData.substr(dotPos + 1).replace(/\-/g, "+").replace(/\_/g, "/")), 0, 0, null);
			
			return stringToBase64(deciphered.substr(0, deciphered.length - padding - 8));
		}
		
		/* Metodos que publicamos del objeto AppAfirmaJS */
		return {
			setOrigin : setOrigin,
			signOperation : signOperation
		}
})(window, undefined);


/**
 *  Base64 encode / decode
 *  http://www.webtoolkit.info/
 */
var Base64 = {

		// private property
		_keyStr : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",
		_keyStr_URL_SAFE : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_=",

		// public method for encoding
		encode : function (input, URL_SAFE) {
			
			var keyStr = (URL_SAFE == true) ? this._keyStr_URL_SAFE : this._keyStr;
			
			var output = "";
			var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
			var i = 0;

			input = Base64._utf8_encode(input);

			while (i < input.length) {

				chr1 = input.charCodeAt(i++);
				chr2 = input.charCodeAt(i++);
				chr3 = input.charCodeAt(i++);

				enc1 = chr1 >> 2;
				enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
				enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
				enc4 = chr3 & 63;

				if (isNaN(chr2)) {
					enc3 = enc4 = 64;
				} else if (isNaN(chr3)) {
					enc4 = 64;
				}

				output = output +
				keyStr.charAt(enc1) + keyStr.charAt(enc2) +
				keyStr.charAt(enc3) + keyStr.charAt(enc4);

			}

			return output;
		},

		// public method for decoding
		decode : function (input, URL_SAFE) {
			
			var keyStr = (URL_SAFE == true) ? this._keyStr_URL_SAFE : this._keyStr;
			
			var output = "";
			var chr1, chr2, chr3;
			var enc1, enc2, enc3, enc4;
			var i = 0;

			input = (URL_SAFE == true) ? input.replace(/[^A-Za-z0-9\-\_\=]/g, "") : input.replace(/[^A-Za-z0-9\+\/\=]/g, "");

			while (i < input.length) {

				enc1 = keyStr.indexOf(input.charAt(i++));
				enc2 = keyStr.indexOf(input.charAt(i++));
				enc3 = keyStr.indexOf(input.charAt(i++));
				enc4 = keyStr.indexOf(input.charAt(i++));

				chr1 = (enc1 << 2) | (enc2 >> 4);
				chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
				chr3 = ((enc3 & 3) << 6) | enc4;

				output = output + String.fromCharCode(chr1);

				if (enc3 != 64) {
					output = output + String.fromCharCode(chr2);
				}
				if (enc4 != 64) {
					output = output + String.fromCharCode(chr3);
				}

			}

			output = Base64._utf8_decode(output);

			return output;

		},

		// private method for UTF-8 encoding
		_utf8_encode : function (string) {
			string = string.replace(/\r\n/g,"\n");
			var utftext = "";

			for (var n = 0; n < string.length; n++) {

				var c = string.charCodeAt(n);

				if (c < 128) {
					utftext += String.fromCharCode(c);
				}
				else if((c > 127) && (c < 2048)) {
					utftext += String.fromCharCode((c >> 6) | 192);
					utftext += String.fromCharCode((c & 63) | 128);
				}
				else {
					utftext += String.fromCharCode((c >> 12) | 224);
					utftext += String.fromCharCode(((c >> 6) & 63) | 128);
					utftext += String.fromCharCode((c & 63) | 128);
				}

			}

			return utftext;
		},

		// private method for UTF-8 decoding
		_utf8_decode : function (utftext) {
			var string = "";
			var i = 0;
			var c = c1 = c2 = 0;

			while ( i < utftext.length ) {

				c = utftext.charCodeAt(i);

				if (c < 128) {
					string += String.fromCharCode(c);
					i++;
				}
				else if((c > 191) && (c < 224)) {
					c2 = utftext.charCodeAt(i+1);
					string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
					i += 2;
				}
				else {
					c2 = utftext.charCodeAt(i+1);
					c3 = utftext.charCodeAt(i+2);
					string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
					i += 3;
				}
			}

			return string;
		}

};

//Paul Tero, July 2001
//http://www.tero.co.uk/des/

//Optimised for performance with large blocks by Michael Hayworth, November 2001
//http://www.netdealing.com

//THIS SOFTWARE IS PROVIDED "AS IS" AND
//ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
//ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
//FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
//OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
//HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
//LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
//OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
//SUCH DAMAGE.

//des
//this takes the key, the message, and whether to encrypt or decrypt
function des (key, message, encrypt, mode, iv, padding) {
	  //declaring this locally speeds things up a bit
	  var spfunction1 = new Array (0x1010400,0,0x10000,0x1010404,0x1010004,0x10404,0x4,0x10000,0x400,0x1010400,0x1010404,0x400,0x1000404,0x1010004,0x1000000,0x4,0x404,0x1000400,0x1000400,0x10400,0x10400,0x1010000,0x1010000,0x1000404,0x10004,0x1000004,0x1000004,0x10004,0,0x404,0x10404,0x1000000,0x10000,0x1010404,0x4,0x1010000,0x1010400,0x1000000,0x1000000,0x400,0x1010004,0x10000,0x10400,0x1000004,0x400,0x4,0x1000404,0x10404,0x1010404,0x10004,0x1010000,0x1000404,0x1000004,0x404,0x10404,0x1010400,0x404,0x1000400,0x1000400,0,0x10004,0x10400,0,0x1010004);
	  var spfunction2 = new Array (-0x7fef7fe0,-0x7fff8000,0x8000,0x108020,0x100000,0x20,-0x7fefffe0,-0x7fff7fe0,-0x7fffffe0,-0x7fef7fe0,-0x7fef8000,-0x80000000,-0x7fff8000,0x100000,0x20,-0x7fefffe0,0x108000,0x100020,-0x7fff7fe0,0,-0x80000000,0x8000,0x108020,-0x7ff00000,0x100020,-0x7fffffe0,0,0x108000,0x8020,-0x7fef8000,-0x7ff00000,0x8020,0,0x108020,-0x7fefffe0,0x100000,-0x7fff7fe0,-0x7ff00000,-0x7fef8000,0x8000,-0x7ff00000,-0x7fff8000,0x20,-0x7fef7fe0,0x108020,0x20,0x8000,-0x80000000,0x8020,-0x7fef8000,0x100000,-0x7fffffe0,0x100020,-0x7fff7fe0,-0x7fffffe0,0x100020,0x108000,0,-0x7fff8000,0x8020,-0x80000000,-0x7fefffe0,-0x7fef7fe0,0x108000);
	  var spfunction3 = new Array (0x208,0x8020200,0,0x8020008,0x8000200,0,0x20208,0x8000200,0x20008,0x8000008,0x8000008,0x20000,0x8020208,0x20008,0x8020000,0x208,0x8000000,0x8,0x8020200,0x200,0x20200,0x8020000,0x8020008,0x20208,0x8000208,0x20200,0x20000,0x8000208,0x8,0x8020208,0x200,0x8000000,0x8020200,0x8000000,0x20008,0x208,0x20000,0x8020200,0x8000200,0,0x200,0x20008,0x8020208,0x8000200,0x8000008,0x200,0,0x8020008,0x8000208,0x20000,0x8000000,0x8020208,0x8,0x20208,0x20200,0x8000008,0x8020000,0x8000208,0x208,0x8020000,0x20208,0x8,0x8020008,0x20200);
	  var spfunction4 = new Array (0x802001,0x2081,0x2081,0x80,0x802080,0x800081,0x800001,0x2001,0,0x802000,0x802000,0x802081,0x81,0,0x800080,0x800001,0x1,0x2000,0x800000,0x802001,0x80,0x800000,0x2001,0x2080,0x800081,0x1,0x2080,0x800080,0x2000,0x802080,0x802081,0x81,0x800080,0x800001,0x802000,0x802081,0x81,0,0,0x802000,0x2080,0x800080,0x800081,0x1,0x802001,0x2081,0x2081,0x80,0x802081,0x81,0x1,0x2000,0x800001,0x2001,0x802080,0x800081,0x2001,0x2080,0x800000,0x802001,0x80,0x800000,0x2000,0x802080);
	  var spfunction5 = new Array (0x100,0x2080100,0x2080000,0x42000100,0x80000,0x100,0x40000000,0x2080000,0x40080100,0x80000,0x2000100,0x40080100,0x42000100,0x42080000,0x80100,0x40000000,0x2000000,0x40080000,0x40080000,0,0x40000100,0x42080100,0x42080100,0x2000100,0x42080000,0x40000100,0,0x42000000,0x2080100,0x2000000,0x42000000,0x80100,0x80000,0x42000100,0x100,0x2000000,0x40000000,0x2080000,0x42000100,0x40080100,0x2000100,0x40000000,0x42080000,0x2080100,0x40080100,0x100,0x2000000,0x42080000,0x42080100,0x80100,0x42000000,0x42080100,0x2080000,0,0x40080000,0x42000000,0x80100,0x2000100,0x40000100,0x80000,0,0x40080000,0x2080100,0x40000100);
	  var spfunction6 = new Array (0x20000010,0x20400000,0x4000,0x20404010,0x20400000,0x10,0x20404010,0x400000,0x20004000,0x404010,0x400000,0x20000010,0x400010,0x20004000,0x20000000,0x4010,0,0x400010,0x20004010,0x4000,0x404000,0x20004010,0x10,0x20400010,0x20400010,0,0x404010,0x20404000,0x4010,0x404000,0x20404000,0x20000000,0x20004000,0x10,0x20400010,0x404000,0x20404010,0x400000,0x4010,0x20000010,0x400000,0x20004000,0x20000000,0x4010,0x20000010,0x20404010,0x404000,0x20400000,0x404010,0x20404000,0,0x20400010,0x10,0x4000,0x20400000,0x404010,0x4000,0x400010,0x20004010,0,0x20404000,0x20000000,0x400010,0x20004010);
	  var spfunction7 = new Array (0x200000,0x4200002,0x4000802,0,0x800,0x4000802,0x200802,0x4200800,0x4200802,0x200000,0,0x4000002,0x2,0x4000000,0x4200002,0x802,0x4000800,0x200802,0x200002,0x4000800,0x4000002,0x4200000,0x4200800,0x200002,0x4200000,0x800,0x802,0x4200802,0x200800,0x2,0x4000000,0x200800,0x4000000,0x200800,0x200000,0x4000802,0x4000802,0x4200002,0x4200002,0x2,0x200002,0x4000000,0x4000800,0x200000,0x4200800,0x802,0x200802,0x4200800,0x802,0x4000002,0x4200802,0x4200000,0x200800,0,0x2,0x4200802,0,0x200802,0x4200000,0x800,0x4000002,0x4000800,0x800,0x200002);
	  var spfunction8 = new Array (0x10001040,0x1000,0x40000,0x10041040,0x10000000,0x10001040,0x40,0x10000000,0x40040,0x10040000,0x10041040,0x41000,0x10041000,0x41040,0x1000,0x40,0x10040000,0x10000040,0x10001000,0x1040,0x41000,0x40040,0x10040040,0x10041000,0x1040,0,0,0x10040040,0x10000040,0x10001000,0x41040,0x40000,0x41040,0x40000,0x10041000,0x1000,0x40,0x10040040,0x1000,0x41040,0x10001000,0x40,0x10000040,0x10040000,0x10040040,0x10000000,0x40000,0x10001040,0,0x10041040,0x40040,0x10000040,0x10040000,0x10001000,0x10001040,0,0x10041040,0x41000,0x41000,0x1040,0x1040,0x40040,0x10000000,0x10041000);

	  //create the 16 or 48 subkeys we will need
	  var keys = des_createKeys (key);
	  var m=0, i, j, temp, right1, right2, left, right, looping;
	  var cbcleft, cbcleft2, cbcright, cbcright2;
	  var endloop, loopinc;
	  var len = message.length;
	  var chunk = 0;
	  //set up the loops for single and triple des
	  var iterations = keys.length == 32 ? 3 : 9; //single or triple des
	  if (iterations == 3) {looping = encrypt ? new Array (0, 32, 2) : new Array (30, -2, -2);}
	  else {looping = encrypt ? new Array (0, 32, 2, 62, 30, -2, 64, 96, 2) : new Array (94, 62, -2, 32, 64, 2, 30, -2, -2);}

	  //pad the message depending on the padding parameter
	  if (padding == 2) message += "        "; //pad the message with spaces
	  else if (padding == 1) {temp = 8-(len%8); message += String.fromCharCode (temp,temp,temp,temp,temp,temp,temp,temp); if (temp==8) len+=8;} //PKCS7 padding
	  else if (!padding) message += "\0\0\0\0\0\0\0\0"; //pad the message out with null bytes

	  //store the result here
	  result = "";
	  tempresult = "";

	  if (mode == 1) { //CBC mode
	    cbcleft = (iv.charCodeAt(m++) << 24) | (iv.charCodeAt(m++) << 16) | (iv.charCodeAt(m++) << 8) | iv.charCodeAt(m++);
	    cbcright = (iv.charCodeAt(m++) << 24) | (iv.charCodeAt(m++) << 16) | (iv.charCodeAt(m++) << 8) | iv.charCodeAt(m++);
	    m=0;
	  }

	  //loop through each 64 bit chunk of the message
	  while (m < len) {
	    left = (message.charCodeAt(m++) << 24) | (message.charCodeAt(m++) << 16) | (message.charCodeAt(m++) << 8) | message.charCodeAt(m++);
	    right = (message.charCodeAt(m++) << 24) | (message.charCodeAt(m++) << 16) | (message.charCodeAt(m++) << 8) | message.charCodeAt(m++);

	    //for Cipher Block Chaining mode, xor the message with the previous result
	    if (mode == 1) {if (encrypt) {left ^= cbcleft; right ^= cbcright;} else {cbcleft2 = cbcleft; cbcright2 = cbcright; cbcleft = left; cbcright = right;}}

	    //first each 64 but chunk of the message must be permuted according to IP
	    temp = ((left >>> 4) ^ right) & 0x0f0f0f0f; right ^= temp; left ^= (temp << 4);
	    temp = ((left >>> 16) ^ right) & 0x0000ffff; right ^= temp; left ^= (temp << 16);
	    temp = ((right >>> 2) ^ left) & 0x33333333; left ^= temp; right ^= (temp << 2);
	    temp = ((right >>> 8) ^ left) & 0x00ff00ff; left ^= temp; right ^= (temp << 8);
	    temp = ((left >>> 1) ^ right) & 0x55555555; right ^= temp; left ^= (temp << 1);

	    left = ((left << 1) | (left >>> 31)); 
	    right = ((right << 1) | (right >>> 31)); 

	    //do this either 1 or 3 times for each chunk of the message
	    for (j=0; j<iterations; j+=3) {
	      endloop = looping[j+1];
	      loopinc = looping[j+2];
	      //now go through and perform the encryption or decryption  
	      for (i=looping[j]; i!=endloop; i+=loopinc) { //for efficiency
	        right1 = right ^ keys[i]; 
	        right2 = ((right >>> 4) | (right << 28)) ^ keys[i+1];
	        //the result is attained by passing these bytes through the S selection functions
	        temp = left;
	        left = right;
	        right = temp ^ (spfunction2[(right1 >>> 24) & 0x3f] | spfunction4[(right1 >>> 16) & 0x3f]
	              | spfunction6[(right1 >>>  8) & 0x3f] | spfunction8[right1 & 0x3f]
	              | spfunction1[(right2 >>> 24) & 0x3f] | spfunction3[(right2 >>> 16) & 0x3f]
	              | spfunction5[(right2 >>>  8) & 0x3f] | spfunction7[right2 & 0x3f]);
	      }
	      temp = left; left = right; right = temp; //unreverse left and right
	    } //for either 1 or 3 iterations

	    //move then each one bit to the right
	    left = ((left >>> 1) | (left << 31)); 
	    right = ((right >>> 1) | (right << 31)); 

	    //now perform IP-1, which is IP in the opposite direction
	    temp = ((left >>> 1) ^ right) & 0x55555555; right ^= temp; left ^= (temp << 1);
	    temp = ((right >>> 8) ^ left) & 0x00ff00ff; left ^= temp; right ^= (temp << 8);
	    temp = ((right >>> 2) ^ left) & 0x33333333; left ^= temp; right ^= (temp << 2);
	    temp = ((left >>> 16) ^ right) & 0x0000ffff; right ^= temp; left ^= (temp << 16);
	    temp = ((left >>> 4) ^ right) & 0x0f0f0f0f; right ^= temp; left ^= (temp << 4);

	    //for Cipher Block Chaining mode, xor the message with the previous result
	    if (mode == 1) {if (encrypt) {cbcleft = left; cbcright = right;} else {left ^= cbcleft2; right ^= cbcright2;}}
	    tempresult += String.fromCharCode ((left>>>24), ((left>>>16) & 0xff), ((left>>>8) & 0xff), (left & 0xff), (right>>>24), ((right>>>16) & 0xff), ((right>>>8) & 0xff), (right & 0xff));

	    chunk += 8;
	    if (chunk == 512) {result += tempresult; tempresult = ""; chunk = 0;}
	  } //for every 8 characters, or 64 bits in the message

	  //return the result as an array
	  return result + tempresult;
	} //end of des

	//des_createKeys
	//this takes as input a 64 bit key (even though only 56 bits are used)
	//as an array of 2 integers, and returns 16 48 bit keys
	function des_createKeys (key) {
	  //declaring this locally speeds things up a bit
	  pc2bytes0  = new Array (0,0x4,0x20000000,0x20000004,0x10000,0x10004,0x20010000,0x20010004,0x200,0x204,0x20000200,0x20000204,0x10200,0x10204,0x20010200,0x20010204);
	  pc2bytes1  = new Array (0,0x1,0x100000,0x100001,0x4000000,0x4000001,0x4100000,0x4100001,0x100,0x101,0x100100,0x100101,0x4000100,0x4000101,0x4100100,0x4100101);
	  pc2bytes2  = new Array (0,0x8,0x800,0x808,0x1000000,0x1000008,0x1000800,0x1000808,0,0x8,0x800,0x808,0x1000000,0x1000008,0x1000800,0x1000808);
	  pc2bytes3  = new Array (0,0x200000,0x8000000,0x8200000,0x2000,0x202000,0x8002000,0x8202000,0x20000,0x220000,0x8020000,0x8220000,0x22000,0x222000,0x8022000,0x8222000);
	  pc2bytes4  = new Array (0,0x40000,0x10,0x40010,0,0x40000,0x10,0x40010,0x1000,0x41000,0x1010,0x41010,0x1000,0x41000,0x1010,0x41010);
	  pc2bytes5  = new Array (0,0x400,0x20,0x420,0,0x400,0x20,0x420,0x2000000,0x2000400,0x2000020,0x2000420,0x2000000,0x2000400,0x2000020,0x2000420);
	  pc2bytes6  = new Array (0,0x10000000,0x80000,0x10080000,0x2,0x10000002,0x80002,0x10080002,0,0x10000000,0x80000,0x10080000,0x2,0x10000002,0x80002,0x10080002);
	  pc2bytes7  = new Array (0,0x10000,0x800,0x10800,0x20000000,0x20010000,0x20000800,0x20010800,0x20000,0x30000,0x20800,0x30800,0x20020000,0x20030000,0x20020800,0x20030800);
	  pc2bytes8  = new Array (0,0x40000,0,0x40000,0x2,0x40002,0x2,0x40002,0x2000000,0x2040000,0x2000000,0x2040000,0x2000002,0x2040002,0x2000002,0x2040002);
	  pc2bytes9  = new Array (0,0x10000000,0x8,0x10000008,0,0x10000000,0x8,0x10000008,0x400,0x10000400,0x408,0x10000408,0x400,0x10000400,0x408,0x10000408);
	  pc2bytes10 = new Array (0,0x20,0,0x20,0x100000,0x100020,0x100000,0x100020,0x2000,0x2020,0x2000,0x2020,0x102000,0x102020,0x102000,0x102020);
	  pc2bytes11 = new Array (0,0x1000000,0x200,0x1000200,0x200000,0x1200000,0x200200,0x1200200,0x4000000,0x5000000,0x4000200,0x5000200,0x4200000,0x5200000,0x4200200,0x5200200);
	  pc2bytes12 = new Array (0,0x1000,0x8000000,0x8001000,0x80000,0x81000,0x8080000,0x8081000,0x10,0x1010,0x8000010,0x8001010,0x80010,0x81010,0x8080010,0x8081010);
	  pc2bytes13 = new Array (0,0x4,0x100,0x104,0,0x4,0x100,0x104,0x1,0x5,0x101,0x105,0x1,0x5,0x101,0x105);

	  //how many iterations (1 for des, 3 for triple des)
	  var iterations = key.length > 8 ? 3 : 1; //changed by Paul 16/6/2007 to use Triple DES for 9+ byte keys
	  //stores the return keys
	  var keys = new Array (32 * iterations);
	  //now define the left shifts which need to be done
	  var shifts = new Array (0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0);
	  //other variables
	  var lefttemp, righttemp, m=0, n=0, temp;

	  for (var j=0; j<iterations; j++) { //either 1 or 3 iterations
	    left = (key.charCodeAt(m++) << 24) | (key.charCodeAt(m++) << 16) | (key.charCodeAt(m++) << 8) | key.charCodeAt(m++);
	    right = (key.charCodeAt(m++) << 24) | (key.charCodeAt(m++) << 16) | (key.charCodeAt(m++) << 8) | key.charCodeAt(m++);

	    temp = ((left >>> 4) ^ right) & 0x0f0f0f0f; right ^= temp; left ^= (temp << 4);
	    temp = ((right >>> -16) ^ left) & 0x0000ffff; left ^= temp; right ^= (temp << -16);
	    temp = ((left >>> 2) ^ right) & 0x33333333; right ^= temp; left ^= (temp << 2);
	    temp = ((right >>> -16) ^ left) & 0x0000ffff; left ^= temp; right ^= (temp << -16);
	    temp = ((left >>> 1) ^ right) & 0x55555555; right ^= temp; left ^= (temp << 1);
	    temp = ((right >>> 8) ^ left) & 0x00ff00ff; left ^= temp; right ^= (temp << 8);
	    temp = ((left >>> 1) ^ right) & 0x55555555; right ^= temp; left ^= (temp << 1);

	    //the right side needs to be shifted and to get the last four bits of the left side
	    temp = (left << 8) | ((right >>> 20) & 0x000000f0);
	    //left needs to be put upside down
	    left = (right << 24) | ((right << 8) & 0xff0000) | ((right >>> 8) & 0xff00) | ((right >>> 24) & 0xf0);
	    right = temp;

	    //now go through and perform these shifts on the left and right keys
	    for (var i=0; i < shifts.length; i++) {
	      //shift the keys either one or two bits to the left
	      if (shifts[i]) {left = (left << 2) | (left >>> 26); right = (right << 2) | (right >>> 26);}
	      else {left = (left << 1) | (left >>> 27); right = (right << 1) | (right >>> 27);}
	      left &= -0xf; right &= -0xf;

	      //now apply PC-2, in such a way that E is easier when encrypting or decrypting
	      //this conversion will look like PC-2 except only the last 6 bits of each byte are used
	      //rather than 48 consecutive bits and the order of lines will be according to 
	      //how the S selection functions will be applied: S2, S4, S6, S8, S1, S3, S5, S7
	      lefttemp = pc2bytes0[left >>> 28] | pc2bytes1[(left >>> 24) & 0xf]
	              | pc2bytes2[(left >>> 20) & 0xf] | pc2bytes3[(left >>> 16) & 0xf]
	              | pc2bytes4[(left >>> 12) & 0xf] | pc2bytes5[(left >>> 8) & 0xf]
	              | pc2bytes6[(left >>> 4) & 0xf];
	      righttemp = pc2bytes7[right >>> 28] | pc2bytes8[(right >>> 24) & 0xf]
	                | pc2bytes9[(right >>> 20) & 0xf] | pc2bytes10[(right >>> 16) & 0xf]
	                | pc2bytes11[(right >>> 12) & 0xf] | pc2bytes12[(right >>> 8) & 0xf]
	                | pc2bytes13[(right >>> 4) & 0xf];
	      temp = ((righttemp >>> 16) ^ lefttemp) & 0x0000ffff; 
	      keys[n++] = lefttemp ^ temp; keys[n++] = righttemp ^ (temp << 16);
	    }
	  } //for each iterations
	  //return the keys we've created
	  return keys;
	} //end of des_createKeys

//Convierte una cadena a Base 64. Debido a un error en el algoritmo original, pasaremos
// de cadena a hexadecimal y de hexadecimal a Base64
function stringToBase64 (s) {
	return hexToBase64(stringToHex(s));
}

//Convert a base64 string into a normal string
function base64ToString (s) {
  //the base 64 characters
  var BASE64 = new Array ('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/');
	
  var decode = new Object();
  for (var i=0; i<BASE64.length; i++) {decode[BASE64[i]] = i;} //inverse of the array
  decode['='] = 0; //add the equals sign as well
  var r = "", c1, c2, c3, c4, len=s.length; //define variables
  s += "===="; //just to make sure it is padded correctly
  for (var i=0; i<len; i+=4) { //4 input characters at a time
    c1 = s.charAt(i); //the 1st base64 input characther
    c2 = s.charAt(i+1);
    c3 = s.charAt(i+2);
    c4 = s.charAt(i+3);
    r += String.fromCharCode (((decode[c1] << 2) & 0xff) | (decode[c2] >> 4)); //reform the string
    if (c3 != '=') r += String.fromCharCode (((decode[c2] << 4) & 0xff) | (decode[c3] >> 2));
    if (c4 != '=') r += String.fromCharCode (((decode[c3] << 6) & 0xff) | decode[c4]);
  }
  return r;
}

function stringToHex (s) {
	  var r = "";
	  var hexes = new Array ("0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f");
	  for (var i=0; i<s.length; i++) {r += hexes [s.charCodeAt(i) >> 4] + hexes [s.charCodeAt(i) & 0xf];}
	  return r;
}

// --- Funciones para pasar de Hexadecimal a base64

var tableStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
var tableStr_URL_SAFE = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

function btoa (bin, urlSafe) {
	var table = (urlSafe == true ? tableStr_URL_SAFE : tableStr).split("");
	
	for (var i = 0, j = 0, len = bin.length / 3, base64 = []; i < len; ++i) {
		var a = bin.charCodeAt(j++), b = bin.charCodeAt(j++), c = bin.charCodeAt(j++);
		if ((a | b | c) > 255) throw new Error("String contains an invalid character");
		base64[base64.length] = table[a >> 2] + table[((a << 4) & 63) | (b >> 4)] +
		(isNaN(b) ? "=" : table[((b << 2) & 63) | (c >> 6)]) +
		(isNaN(b + c) ? "=" : table[c & 63]);
	}
	return base64.join("");
}

function hexToBase64(str, urlSafe) {
	var byteString;
	var byteArray = str.replace(/\r|\n/g, "").replace(/([\da-fA-F]{2}) ?/g, "0x$1 ").replace(/ +$/, "").split(" ");
	try {
		byteString = String.fromCharCode.apply(null, byteArray);
	} catch (e) {
		var strTemp = "";
		for (var i = 0, len = byteArray.length; i < len; i++) {
			strTemp += String.fromCharCode(byteArray[i]);
		}
		byteString = strTemp;
	}
	
	return btoa(byteString, urlSafe);
}
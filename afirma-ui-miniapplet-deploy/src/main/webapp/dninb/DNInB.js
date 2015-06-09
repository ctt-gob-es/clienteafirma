
/**
 * 
 */
DNInB = (function (){
	var EXPAND_POLICIY_KEY_AND_VALUE = "expPolicy=FirmaAGE";
	
	//TODO
	var servletAddress = "http://localhost:8080/DNInBProxy/sign";
	
	// Constants
	var MAX_NUMBER = 2147483648;
	
	function cargarMiniApplet(base, keystore) {
		//TODO
	}
	function sign(dataB64, algorithm, format, params, successCallback, errorCallback) {
		//signOperation("sign", dataB64, algorithm, format, params, successCallback, errorCallback);
		DNInBUtils.launcher();
	}
	function coSign(signB64, dataB64, algorithm, format, params, successCallback, errorCallback) {
		//TODO
	}
	function counterSign(signB64, algorithm, format, params, successCallback, errorCallback) {
		//TODO
	}
	
	/**
	 * Identifica si debe expandirse la propiedad de politica de firma.
	 * @param config Configuracion de la firma.
	 * @returns Indica con true si debe expandirse el parametro de politica, false en caso contrario.
	 */
	function isPolicyConfigurated(config) {
		return (!config) ? config.indexOf(EXPAND_POLICIY_KEY_AND_VALUE) > -1 : false;
	}
	
	/**
	 * Expande la variable de firma politica de firma si la encuentra en los extra params.
	 **/
	function expandPolicy(format, config) {
		var expandedPolicy = "";
		if (compareFormats(format, "CAdES")) {
			expandedPolicy = "policyIdentifier=urn:oid:2.16.724.1.3.1.1.2.1.8\n" +
				"policyQualifier=http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf\n" +
				"policyIdentifierHashAlgorithm=http://www.w3.org/2000/09/xmldsig#sha1\n" +
				"policyIdentifierHash=7SxX3erFuH31TvAw9LZ70N7p1vA=";
		}
		else if (compareFormats(format, "XAdES")) {
			expandedPolicy = "policyIdentifier=urn:oid:2.16.724.1.3.1.1.2.1.8\n" +
			"policyQualifier=http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf\n" +
			"policyIdentifierHashAlgorithm=http://www.w3.org/2000/09/xmldsig#sha1\n" +
			"policyIdentifierHash=V8lVVNGDCPen6VELRD1Ja8HARFk=";
		}
		
		if (expandedPolicy != "") {
			config = config.replace(EXPAND_POLICIY_KEY_AND_VALUE, expandedPolicy);
		}
		return config;
	}
	
	/**
	 * Compara que un nombre de formato sea equivalente a un formato de firma monofasico.
	 * Por ejemplo, que XAdEStri sea igual a XAdES.
	 **/
	function compareFormats(format, supportedFormat) {
		format = format.toUpperCase()
		supportedFormat = supportedFormat.toUpperCase()
		return format == supportedFormat ||
				(format.length > supportedFormat.length &&
						format.substr(0, supportedFormat.length) == supportedFormat);
	}
	
	function signOperation(signId, dataB64, algorithm, format, extraParams, successCallback, errorCallback) {
		if (!!dataB64) {
			dataB64 = dataB64.replace(/\+/g, "-").replace(/\//g, "_");
		}
		if (isPolicyConfigurated(extraParams)) {
			extraParams = expandPolicy(format, extraParams);
		}
		
		var idSession = generateNewIdSession();
		var cipherKey = generateCipherKey();
		
		var i = 0;
		var params = new Array();
		if (signId) {					params[i++] = {key:"op", value:encodeURIComponent(signId)}; }
		if (idSession) {				params[i++] = {key:"id", value:encodeURIComponent(idSession)}; }
		if (cipherKey) {				params[i++] = {key:"key", value:encodeURIComponent(cipherKey)}; }
		if (format) {					params[i++] = {key:"format", value:encodeURIComponent(format)}; }
		if (algorithm) {				params[i++] = {key:"algorithm", value:encodeURIComponent(algorithm)}; }
		
		if (extraParams) { 				params[i++] = {key:"properties", value:encodeURIComponent(Base64.encode(extraParams))}; }		
		//TODO
		//"MIIFnTCCBIWgAwIBAgICA+owDQYJKoZIhvcNAQEFBQAwgdoxCzAJBgNVBAYTAkVTMRIwEAYDVQQIEwlCYXJjZWxvbmExSDBGBgNVBAcMP0JhcmNlbG9uYSAoc2VlIGN1cnJlbnQgYWRkcmVzcyBhdCBodHRwczovL3d3dy5hbmYuZXMvYWRkcmVzcy8gKTEnMCUGA1UEChMeQU5GIEF1dG9yaWRhZCBkZSBDZXJ0aWZpY2FjaW9uMRcwFQYDVQQLEw5BTkYgQ2xhc2UgMSBDQTETMBEGA1UEBRMKRy02MzI4NzUxMDEWMBQGA1UEAxMNQU5GIFNlcnZlciBDQTAeFw0wNjEyMzEyMzAwMDBaFw0xNDEyMzEyMzAwMDBaMIGmMRswGQYDVQQDExJBTkYgVXN1YXJpbyBBY3Rpdm8xDDAKBgNVBCoTA0FORjEXMBUGA1UEBBMOVXN1YXJpbyBBY3Rpdm8xEjAQBgNVBAUTCTEyMzQ1Njc4WjEeMBwGCSqGSIb3DQEJARYPdGVzdEBwcnVlYmEuY29tMR8wHQYDVQQLExZDbGFzZSAyIHBlcnNvbmEgZmlzaWNhMQswCQYDVQQGEwJFUzCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAj2qAceOf0pyATEM0BxBK7+eGA0HEZWDZpqdhCeVvsI1AqhLWQpWNg65TGXE8ijzxGU/yS94k/34gPgIkla+p/mrDaNsVY69RcLp1hWYcL61rM//In+hXlA3qUK6as942b55YyzNsbJSQPCNgkiGuIQTo1Xfsfk4XZDi+yNSRgUMCAwEAAaOCAiEwggIdMAkGA1UdEwQCMAAwCwYDVR0PBAQDAgbAMBMGCisGAQQBgY8cFAMEBQwDQU5GMBcGCisGAQQBgY8cFAQECQwHVXN1YXJpbzAWBgorBgEEAYGPHBQFBAgMBkFjdGl2bzAZBgorBgEEAYGPHBQGBAsMCTEyMzQ1Njc4WjCBiAYDVR0gBIGAMH4wfAYKKwYBBAGBjxwDBDBuMD0GCCsGAQUFBwICMDEaL0NlcnRpZmljYWRvIGVtaXRpZG8gcGFyYSByZWFsaXphY2nzbiBkZSBwcnVlYmFzMC0GCCsGAQUFBwIBFiFodHRwczovL3d3dy5hbmYuZXMvQUMvZG9jdW1lbnRvcy8wOAYIKwYBBQUHAQEELDAqMCgGCCsGAQUFBzABhhxodHRwOi8vd3d3LmFuZi5lcy9BQy9SQy9vY3NwMDkGA1UdHwQyMDAwLqAsoCqGKGh0dHA6Ly93d3cuYW5mLmVzL0FDL1JDL0FORkFDQ0xBU0VBMS5jcmwwFwYKKwYBBAGBjxwTAQQJDAcxMjMtMzIxMDEGCisGAQQBgY8cKgYEIwwhaHR0cHM6Ly93d3cuYW5mLmVzL0FDL0FDVEFTLzU2Nzg5MBYGCSsGAQQBgY8cEwQJDAczMjEtMTIzMB0GA1UdDgQWBBSxTxAznF2uoOtMW+fJUoDN6B+rJDAfBgNVHSMEGDAWgBS+O/a0MbdzJEg5xVcTlHWqn4E/LDANBgkqhkiG9w0BAQUFAAOCAQEATQgYAOwxrMRTT2Nhx7pqiNsoGT5dJmeunAv+iU5zx/VoEXB/mx+VtyLfMea3VS9LC23404XS7pz5oPwiVPLsMPZtzOcmfacVnSdRn5J7+qOO8MB+OVlXq/QmARn+1XeBCHaTQ6AMc/pdveEoGktaXwEjTslWyRD9dGDzLp04+FndQAbVcI5xRkb4vToRnhQmloUVddhQAO8usOAIb00GJFNTq4lsyZ1qT1HplQl+ngsSD1HBxkhx10Pm3KuvCunAh4um0QnSeeiq9qWIV0UZrFlMwNRXvH9OVTqSGC4PXjw2zOi2GLUfags1decu7gcGjidlELR/WHU/6lrztfdViQ=="
		params[i++] = {key:"cert", value:encodeURIComponent(extraParams.substring(extraParams.lastIndexOf("cert=")+5,extraParams.length))};
		//TODO
		//if (MiniApplet.isWindows8()) {	params[i++] = {key:"metro", value:MiniApplet.isWindows8ModernUI() ? "true" : "false"}; }
		if (dataB64) {					params[i++] = {key:"dat", value:encodeURIComponent(dataB64)}; }

		sendRequest(servletAddress, params)		
	}
	
	
	
	this.getProtocol = function () {
		if (MiniApplet.isWindows8()) {
			return "afirmametro";
		}
		return "afirma";
	};
	
	/**
	 * Genera un numero aleatorio para utilizar como clave de cifrado.
	 */
	function generateCipherKey() {
		return zeroFill(Math.floor(((Math.random() * MAX_NUMBER) + 1) % 100000000), 8);
	}
	
	/**
	 * Funciones auxiliares del objeto JS del cliente de firma.
	 **/
	function generateNewIdSession() {
		return zeroFill(Math.floor((Math.random() * MAX_NUMBER) + 1), 12);
	}
	
	// Pure javascript functions
	function zeroFill(number, width) {
		width -= number.toString().length;
		if (width > 0) {
			return new Array(width + (/\./.test(number) ? 2 : 1)).join('0')
			+ number;
		}
		return number + ""; // Always return a string
	}
	
	/**
	 * Convierte texto plano en texto base 64.
	 */
	function getBase64FromText(plainText, charset) {
		return Base64.encode(base64Text);
	}
	/**
	 * Convierte texto base 64 en texto plano.
	 */
	function getTextFromBase64(dataB64, charset) {
		return Base64.decode(base64Text);
	}
	
	function sendRequest(path, params, method) {
		
		method = method || "POST"; 
		
		var parametros ="";		
		
		for( i =0; i<params.length; i++){
			
			parametros = parametros + params[i].key +"="+ params[i].value;
			
			if(i < params.length - 1){
				parametros +="&";	
			}
			
		}		
		xmlhttp=new XMLHttpRequest();
		xmlhttp.open(method, path, false);
		xmlhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
		//xmlhttp.setRequestHeader("Content-length", params.length);
		//xmlhttp.setRequestHeader("Connection", "close");
		xmlhttp.send(parametros);
		xmlDoc=xmlhttp.responseXML;
		
		
		
		/*
	    method = method || "post"; 

	    var form = document.createElement("form");
	    form.setAttribute("method", method);
	    form.setAttribute("action", path);

	    for (var i = 0; i < params.length; i++) {
            var hiddenField = document.createElement("input");
            hiddenField.setAttribute("type", "hidden");
            hiddenField.setAttribute("name", params[i].key);
            hiddenField.setAttribute("value", params[i].value);

            form.appendChild(hiddenField);	         
		}

	    document.body.appendChild(form);
	    form.submit();*/
	}
	
	function saveDataToFile(dataB64, title, fileName, extension, description) {
		//TODO
	}
	function getFileNameContentBase64(title, extensions, description, filePath) {
		//TODO
	}
	function getMultiFileNameContentBase64(title, extensions, description, filePath) {
		//TODO
	}
	function echo() {
		//TODO
	}
	function setStickySignatory(sticky) {
		//TODO
	}
	function setLocale(locale) {
		//TODO
	}
	function getErrorMessage() {
		//TODO
	}
	function getErrorType() {
		//TODO
	}
	function getCurrentLog() {
		//TODO
	}
	function setServlets(storageServlet,  retrieverServlet) {
		//TODO
	}
	return {
		sign : sign,
		signOperation : signOperation
	}
})();


document.DNInB = DNInB;


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
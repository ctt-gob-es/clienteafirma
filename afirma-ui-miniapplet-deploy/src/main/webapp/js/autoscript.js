
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

var originalXMLHttpRequest = window.XMLHttpRequest;

var AutoScript = ( function ( window, undefined ) {

		var VERSION = "1.7.0";

		/* ========== DEPRECADO: Se mantiene por compatibilidad con los despliegues del MiniApplet. */
		var JAVA_ARGUMENTS = null;
		var SYSTEM_PROPERTIES = null;
		/* ========== */
		
		var clienteFirma = null;

		var storageServletAddress = null;

		var retrieverServletAddress = null;

		var severeTimeDelay = false;

		var selectedLocale = null;
		
		var stickySignatory = false;
		
		var resetStickySignatory = false;

		var LOCALIZED_STRINGS = new Array();
		LOCALIZED_STRINGS["es_ES"] = {
				checktime_warn: "Se ha detectado un desfase horario entre su sistema y el servidor. Se recomienda que se corrija antes de pulsar Aceptar para continuar.",
				checktime_err: "Se ha detectado un desfase horario entre su sistema y el servidor. Debe corregir la hora de su sistema y recargar esta página antes de continuar.",
				checktime_local_time: "Hora de su sistema",
				checktime_server_time: "Hora del servidor"
		};
		LOCALIZED_STRINGS["gl_ES"] = {
				checktime_warn: "Destectouse un desfase horario entre o seu sistema e o servidor. Recoméndase corrixilo antes de pulsar Aceptar para continuar.",
				checktime_err: "Destectouse un desfase horario entre o seu sistema e o servidor. Debe corrixir a hora do seu sistema antes de continuar.",
				checktime_local_time: "Hora do seu sistema",
				checktime_server_time: "Hora do servidor"
		};

		var DEFAULT_LOCALE = LOCALIZED_STRINGS["es_ES"];

		var currentLocale = DEFAULT_LOCALE;

		var defaultKeyStore = null;

		/* --------------------------------- */
		/* Constantes publicas		         */
		/* --------------------------------- */

		/* Almacenes de certificados */

		var KEYSTORE_WINDOWS = "WINDOWS";

		var KEYSTORE_APPLE = "APPLE";

		var KEYSTORE_PKCS12 = "PKCS12";

		var KEYSTORE_PKCS11 = "PKCS11";

		var KEYSTORE_MOZILLA = "MOZ_UNI";

		var KEYSTORE_SHARED_NSS = "SHARED_NSS";

		var KEYSTORE_JAVA = "JAVA";

		var KEYSTORE_JCEKS = "JCEKS";

		var KEYSTORE_JAVACE = "JAVACE";

		var KEYSTORE_DNIE = "DNIEJAVA";

		/* Valores para la configuracion de la comprobacion de tiempo */

		var CHECKTIME_NO = "CT_NO";

		var CHECKTIME_RECOMMENDED = "CT_RECOMMENDED";

		var CHECKTIME_OBLIGATORY = "CT_OBLIGATORY";

		// Tiempo de espera entre los intentos de conexion con autofirma por WebSocket
		var AUTOFIRMA_LAUNCHING_TIME = 2000;

		// Reintentos de conexion totales para detectar que esta instalado AutoFirma por WebSocket
		var AUTOFIRMA_CONNECTION_RETRIES = 15;

		// Variable que se puede configurar para forzar el uso del modo de comunicacion por servidor intermedio
		// entre la pagina web y AutoFirma
		var forceWSMode = false;
        
        /**
         * Indica si el navegador soporta WebSockets.
         */
        function isWebSocketsSupported() {
        	return 'WebSocket' in window || 'MozWebSocket' in window;
        }

		/** Comprueba si una cadena de texto es una URL (http/https). La alternativa implicaria ser un Base64. */ 
		function isValidUrl(data) { 
			return data != null && data.length > "https://".length &&
				("http:" == data.substr(0, 5) || "https:" == data.substr(0, 6));
		}

		var downloadSuccessFunction = null;
		var downloadErrorFunction = null;
		var downloadTypeData = null;
		
		/**
		 * Realiza la descarga de datos de una URL y, una vez termina, llama al metodo
		 * successFunction pasandole los datos descargados en base 64, o errorFunction,
		 * si fallo en la descarga, al que se pasa el error que produjo el problema.
		 */
		function downloadRemoteData(url, successFunction, errorFunction) {
			
			downloadSuccessFunction = successFunction;
			downloadErrorFunction = errorFunction;
			
			var httpRequest = getHttpRequest();
			httpRequest.open("GET", url, true);
			
			// new browsers (XMLHttpRequest2-compliant)
			if ('responseType' in httpRequest && !!Uint8Array) {
				httpRequest.responseType = 'arraybuffer';
				downloadTypeData = 'arraybuffer';
			}
			// old browsers (XMLHttpRequest-compliant)
			else
				if ('overrideMimeType' in httpRequest) {
				httpRequest.overrideMimeType('text\/plain; charset=x-user-defined');
				downloadTypeData = 'string';
			}
			// IE9 (Microsoft.XMLHTTP-compliant)
			else {
				httpRequest.setRequestHeader('Accept-Charset', 'x-user-defined');
				downloadTypeData = 'bytearray';
			}
			
			// shim for onload for old IE
			if (!('onload' in httpRequest)) {
				httpRequest.onreadystatechange = function () {
					if (this.readyState === 4) {
						this.onload();
					}
				};
			}

			httpRequest.onload = function (evt) {
				if (httpRequest.readyState == 4 && httpRequest.status == 200) {
					if (downloadSuccessFunction) {

						// emulating response field for IE9
						if (!('response' in this)) {
							this.response = new VBArray(this.responseBody).toArray();
						}

						var b64;
						if (downloadTypeData == 'arraybuffer') {
							b64 = Base64.encodeArrayBuffer(this.response);
						}
						else if (downloadTypeData == 'bytearray') { 
							b64 = Base64.encodeByteArray(this.response);
						}
						else if (downloadTypeData == 'string') {
							b64 = Base64.encode(this.response);
						}
						else {
							this.onerror(new Error("No se conoce el modo usado para la descarga de los datos"));
						}
						downloadSuccessFunction(b64);
					}
				}
			}
			try {
				httpRequest.onerror = function(e) {
					if (downloadErrorFunction) {
						downloadErrorFunction(e);
					}
				}
			}
			catch (e) {
				// Vacio
			}
			httpRequest.send();
		}
		
		function getHttpRequest() {
            var xmlHttp = null;
            
        	/* Si se cargo Ajax, puede haber modificado el funcionamiento de la clase XMLHttpRequest,
        	 * asi que vemos si el propio Ajax (Sarissa) guarda la implementacion original; si no,
        	 * usamos la implementacion por defecto que la habremos guardado al principio del script
        	 * en la variable originalXMLHttpRequest. Es importante que se haya cargado este script
        	 * antes que el de Ajax o esta opcion no tendra resultado */
            if (typeof XMLHttpRequest != "undefined") {	// Navegadores actuales
                if (typeof Sarissa !== 'undefined' && Sarissa.originalXMLHttpRequest) {
                	xmlHttp = new Sarissa.originalXMLHttpRequest();
                } else {
                	xmlHttp = new originalXMLHttpRequest();
                }
            } else if (typeof window.ActiveXObject != "undefined") {	// Internet Explorer antiguos
                try {
                    xmlHttp = new ActiveXObject("Msxml2.XMLHTTP.4.0");
                } catch (e) {
                    try {
                        xmlHttp = new ActiveXObject("MSXML2.XMLHTTP");
                    } catch (e) {
                        try {
                            xmlHttp = new ActiveXObject("Microsoft.XMLHTTP");
                        } catch (e) {
                            xmlHttp = null;
                        }
                    }
                }
            }
            return xmlHttp;
		}

		/** Permite establecer que la comunicacion con AutoFirma sea a traves
		 * del servidor intermedio. */
		var setForceWSMode = function (force) {
			forceWSMode = force;
		}
		
        /**
         * DEPRECADO - Siempre devuelve true.
         * Informaba de si el usuario necesitaba instalar la aplicacion nativa para
         * completar el proceso de firma. Ahora siempre es necesario.
         */
        function needNativeAppInstalled() {
        	return true;
        }

        /**
         * DEPRECADO - Siempre devuelve false.
         * Determinaba si se realizaba un despliegue JNLP de AutoFirma WebStart.
         * Ahora nunca se usa AutoFirma WebStart.
         */
        function isJNLP() {
        	return false;
        }
		
		/** DEPRECADO - No tiene efecto.<br>
		 * Permitia forzar el uso de la aplicacion nativa de firma en lugar del MiniApplet.
		 * Ahora nunca se usa el MiniApplet. */
		var setForceAFirma = function (force) {
			
		}

		/** DEPRECADO - No tiene efecto.<br>
		 * Establece la direccion servicio para la generacion del JNLP de
		 * carga de AutoFirma. Ahora nunca se usa AutoFirma WebStart. */
		var setJnlpService = function (jnlp) {

		}
		
		/** Permite habilitar la comprobacion de la hora local contra la hora del servidor y
		 * establecer un tiempo maximo permitido y el comportamiento si se supera.
		 * Parametros:
		 *  - checkType:	Tipo de comprobacion. Admite los valores CT_NO, CT_RECOMMENDED y CT_OBLIGATORY.
		 *  - maxMillis:	Tiempo maximo de desfase en milisegundos.
		 *  - checkURL:		URL contra la que se realizara la peticion para obtener la hora. Si no se
		 *  				indica, se usara una pagina web aleatoria dentro del propio dominio.
		 * Cuando el HTML es local, no se realiza ningun tipo de comprobacion.
		 */
		var checkTime = function (checkType, maxMillis, checkURL) {

			if (!checkType || checkType == CHECKTIME_NO || (!!maxMillis && maxMillis <= 0)) {
				return;
			}

			// Si no se establecio un tiempo de desfase, se tomaran 5 minutos 
			if (!maxMillis) {
				maxMillis = 300000;
			}
			
			try {
				// Si checkURL existe mandamos la peticion ahi, en caso contrario nos inventamos una url.
				var URL;
				if (!!checkURL) {
					URL = checkURL;
				}
				else {
					URL = document.URL + '/' + Math.random();
				}
				// Hacemos una llamada al servidor para conocer su hora
				var xhr = getHttpRequest(); 
				xhr.open('GET', URL, false); 
				xhr.send();
	
				// Recogemos la hora local, nada mas obtener la respuesta del servidor
				var clientDate = new Date();

				// Tomamos la hora a partir de la respuesta del servidor. Si esta es 0, estamos en local
				var serverDate = new Date(xhr.getResponseHeader("Date"));
				if (serverDate == null || serverDate.getTime() == 0) {
					// No hacemos nada si estamos en local 
					return;
				}

				// Evaluamos la desincronizacion
				console.log("Hora cliente: " + clientDate);
				console.log("Hora servidor: " + serverDate);
				
				var delay =  Math.abs(clientDate.getTime() - serverDate.getTime());
				if (delay > maxMillis) {
					 if (checkType == CHECKTIME_RECOMMENDED) {
						 alert(currentLocale.checktime_warn +
								 "\n" + currentLocale.checktime_local_time + ": " + clientDate.toLocaleString() +
								 "\n" + currentLocale.checktime_server_time + ": " + serverDate.toLocaleString());
					 }
					 else if (checkType == CHECKTIME_OBLIGATORY) {
						 severeTimeDelay = true;
						 alert(currentLocale.checktime_err +
								 "\n" + currentLocale.checktime_local_time + ": " + clientDate.toLocaleString() +
								 "\n" + currentLocale.checktime_server_time + ": " + serverDate.toLocaleString());
					 }
				}
			}
			catch (e) {
				console.log("Error en la obtencion de la hora del servidor: " + e);
				return;
			}
		}

		/** Obtiene el nombre del almacen que corresponde al presente navegador o, si se debe acceder
		 * al almacen del sistema, se devuelve null. */
		function getDefaultKeystore() {
			if(Platform.isFirefox()){
				return KEYSTORE_MOZILLA;
			}
			return null;
		}
		
		/** Establece el almacen. */
		var setKeyStore = function (ksType) {
			clienteFirma.setKeyStore(ksType != null ? ksType : defaultKeyStore);
			
			// Al haber cambiado el almacen, el certificado dejara de estar fijado
			resetStickySignatory = true;
			
		}
		
		var selectCertificate = function (params, successCallback, errorCallback) {
			clienteFirma.selectCertificate(params, successCallback, errorCallback);
			resetStickySignatory = false;
		}
			
		var sign = function (dataB64, algorithm, format, params, successCallback, errorCallback) {
			clienteFirma.sign(dataB64, algorithm, format, params, successCallback, errorCallback);
			resetStickySignatory = false;
		}

		var coSign = function (signB64, dataB64, algorithm, format, params, successCallback, errorCallback) {
			clienteFirma.coSign(signB64, algorithm, format, params, successCallback, errorCallback);
			resetStickySignatory = false;
		}
		
		var cosign = function (signB64, algorithm, format, params, successCallback, errorCallback) {
			clienteFirma.coSign(signB64, algorithm, format, params, successCallback, errorCallback);
			resetStickySignatory = false;
		}
		
		var counterSign = function (signB64, algorithm, format, params, successCallback, errorCallback) {
			clienteFirma.counterSign(signB64, algorithm, format, params, successCallback, errorCallback);
			resetStickySignatory = false;
		}
		
		var signAndSaveToFile = function (operationId, dataB64, algorithm, format, params, outputFileName, successCallback, errorCallback) {
			clienteFirma.signAndSaveToFile(operationId, dataB64, algorithm, format, params, outputFileName, successCallback, errorCallback);
			resetStickySignatory = false;
		}

		var signBatch = function (batchB64, batchPreSignerUrl, batchPostSignerUrl, params, successCallback, errorCallback) {
			clienteFirma.signBatch(batchB64, batchPreSignerUrl, batchPostSignerUrl, params, successCallback, errorCallback);
			resetStickySignatory = false;
		}
		
		var getBase64FromText = function (plainText) {
			return plainText != null ? Base64.encode(plainText) : null;
		}

		var getTextFromBase64 = function (dataB64) {
			return dataB64 != null ? Base64.decode(dataB64) : null;
		}

		var saveDataToFile = function (dataB64, title, fileName, extension, description, successCallback, errorCallback) {
			clienteFirma.saveDataToFile(dataB64, title, fileName, extension, description, successCallback, errorCallback);
		}

		var getFileNameContentBase64 = function (title, extensions, description, filePath, successCallback, errorCallback) {
			clienteFirma.getFileNameContentBase64(title, extensions, description, filePath, successCallback, errorCallback)
		}

		var getMultiFileNameContentBase64 = function (title, extensions, description, filePath, successCallback, errorCallback) {
			clienteFirma.getMultiFileNameContentBase64(title, extensions, description, filePath, successCallback, errorCallback)
		}
		
		var getCurrentLog = function (successCallback, errorCallback) {
			clienteFirma.getCurrentLog(successCallback, errorCallback)
		}

		var echo = function () {
			return clienteFirma.echo();
		}

		/**
		 * Establece el valor de la variable "stickySignatory" que permite fijar
		 * un certicado seleccionado para futuras invocaciones, de modo que no
		 * sea necesario volver a seleccionarlo mientras el valor sea true o
		 * caduque la conexion en caso de invocacion por protocolo/socket
		 */
		var setStickySignatory = function(sticky) {
			
			// Si estaba activada la fijacion de certificado y ahora se desactiva,
			// se pedira expresamente al cliente que no lo deje fijado
			if (stickySignatory && !sticky) {
				resetStickySignatory = true;
			}
			
			// Se establecera la variable con el valor seleccionado para su
			// posterior uso en cada invocacion por protocolo
			stickySignatory = sticky;
		}

		var setLocale = function (locale) {
			currentLocale = (locale == null || LOCALIZED_STRINGS[locale] == null ? DEFAULT_LOCALE : LOCALIZED_STRINGS[locale]); 
		}

		var getErrorMessage = function () {
			return clienteFirma.getErrorMessage();
		}

		var getErrorType = function () {
			return clienteFirma.getErrorType();
		}
		
		var setServlets = function (storageServlet,  retrieverServlet) {
			
			storageServletAddress = storageServlet;
			retrieverServletAddress = retrieverServlet;

			if (clienteFirma && clienteFirma.setServlets) {
				clienteFirma.setServlets(storageServlet,  retrieverServlet);
			}
		}

		/**
		 * Abre una URL en el navegador.
		 * @param url URL que se desea abrir.
		 */
		function openUrl (url) {
			
			// Usamos el modo de invocacion mas apropiado segun el entorno
			
			// Redireccion del navegador
			if (Platform.isChrome() || Platform.isIOS()) {
				// Usamos document.location porque tiene mejor soporte por los navegadores que
				// window.location que es el mecanismo estandar
				document.location = url;
			}
			// Apertura de un IFrame
			else {

				// Si ya existe el iframe, lo eliminamos para despues volverlo a crear 
				if (document.getElementById("iframeAfirma") != null) {
					try {
						var element = document.getElementById("iframeAfirma");
						element.outerHTML = "";
						delete element;
					}
					catch (e) {
						// No hacemos nada
					}
				}

				// En el caso de ser una version de internet Explorer que soportase la deteccion de aplicacion
				// capaces de manejar el protocolo, aprovechamos esta caracteristica (Internet Explorer para Windows 8 Modern UI)

				if (navigator.msLaunchUri) {
					navigator.msLaunchUri(
							url,
							null,
							function() {
								// Bloqueamos la conexion para evitar que se sigan haciendo comprobaciones
								wrongInstallation = true;
							}
					);
				}
				else {
					// Abrimos la URL por medio de un iframe
					openUrlWithIframe(url);
				}
			}
		}

		/**
		 * Llama a la aplicacion de firma por medio de un iframe.
		 * @param url URL de invocacion.
		 */
		function openUrlWithIframe (url) {
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
			
			// Ocultamos el frame en los sistemas en los que podemos mediante CSS
			if (!Platform.isInternetExplorer7orLower()) {
				var styleAttr = document.createAttribute("style");
				styleAttr.value = "display: none;";
				iframeElem.setAttributeNode(styleAttr);
			}

			document.body.appendChild(iframeElem);				
		}

		/**
		 * Crea e inicializa el objeto JavaScript encargado de transmitir las
		 * operaciones al cliente de firma. La comunicacion con el cliente de
		 * se realizara mediante websocket cuando:
		 *  - El sistema operativo no sea Android.
		 *  - El sistema operativo no sea iOS.
		 *  - El navegador web soporte websocket.
		 *  - No se fuerce el uso del servidor intermedio.
		 * En caso contrario, la comunicacion se realizara mediante un servidor intermedio.
		 */
		function cargarAppAfirma(clientAddress, keystore, avoidJnlpLoad) {
			
			// Comprobamos que no haya un desfase horario declarado como grave y
			// abortamos la carga en ese caso
			if (severeTimeDelay) {
				if (console && console.log) {
					console.log("Se ha detectado un desfase grave entre la hora del sistema y la del servidor. " +
							"Se cancela la carga del cliente de firma");
				}
				return;
			}

			// Si se fuerza el uso de servidor intermedio o estamos en un dispositivo movil,
			// usamos servidor intermedio
			if (forceWSMode || Platform.isIOS() || Platform.isAndroid()) {
				clienteFirma = new AppAfirmaJSWebService(clientAddress, window, undefined);
			}
			// Si podemos utilizar un WebSocket local y no estamos en Internet Explorer
			// (en el que no podemos asegurar el funcionamiento si se encuentra habilitada
			// una opcion de red concreta), usamos WebSockets 
			else if (isWebSocketsSupported() && !Platform.isInternetExplorer()) {
				clienteFirma = new AppAfirmaWebSocketClient(window, undefined);
			}
			// Si no se esta en una version antigua de Internet Explorer o Safari
			else if (!Platform.isInternetExplorer10orLower() && !Platform.isSafari10()) {
				clienteFirma = new AppAfirmaJSSocket(clientAddress, window, undefined);
			}
			// En cualquier otro caso, usaremos servidor intermedio
			else {
				clienteFirma = new AppAfirmaJSWebService(clientAddress, window, undefined);
				if (!!storageServletAddress || !!retrieverServletAddress) {
					clienteFirma.setServlets(storageServletAddress, retrieverServletAddress);
				}
			}
			
			if (!keystore) {
				keystore = getDefaultKeystore();
			}
			clienteFirma.setKeyStore(keystore);
		}

		/**
		 * Funciones de comprobacion de entorno.
		 */
		var Platform = ( function (window, undefined) {
			/** Indica si el sistema operativo es Android. */
			function isAndroid() {
				return navigator.userAgent.toUpperCase().indexOf("ANDROID") != -1 ||
					navigator.appVersion.toUpperCase().indexOf("ANDROID") != -1 ||
					// Para la deteccion de los Kindle Fire
					navigator.userAgent.toUpperCase().indexOf("SILK/") != -1 ||
					navigator.userAgent.toUpperCase().indexOf("KFJWI") != -1 ||
					navigator.userAgent.toUpperCase().indexOf("KFJWA") != -1 ||
					navigator.userAgent.toUpperCase().indexOf("KFTT") != -1 ||
					navigator.userAgent.toUpperCase().indexOf("KFOT") != -1 ||
					navigator.userAgent.toUpperCase().indexOf("KINDLE FIRE") != -1
					;
			}

			/** Indica si el sistema operativo es iOS. */
			function isIOS() {
				return (navigator.userAgent.toUpperCase().indexOf("IPAD") != -1) ||
				(navigator.userAgent.toUpperCase().indexOf("IPOD") != -1) ||
				(navigator.userAgent.toUpperCase().indexOf("IPHONE") != -1) ||
				 // En iOS 13, Safari tiene el mismo userAgent que el Safari de macOS,
				 // asi que lo distinguimos por los puntos de presion admitidos
				 (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1);
			}

			/** Indica si el navegador es Internet Explorer. */
			function isInternetExplorer() {
				return !!(navigator.userAgent.match(/MSIE/))	/* Internet Explorer 10 o inferior */
				|| !!(navigator.userAgent.match(/Trident/) && navigator.userAgent.match(/rv:11/)) /* Internet Explorer 11 (Opcion 1) */
				|| !!navigator.userAgent.match(/Trident.*rv[ :]*11\./); /* Internet Explorer 11 (Opcion 2) */
			}
			
			/** Indica si el navegador es Internet Explorer 10 o inferior. */
			function isInternetExplorer10orLower() {
				return !!(navigator.userAgent.match(/MSIE/));
			}
			
			/** Indica si el navegador es Internet Explorer 7 o inferior. */
			function isInternetExplorer7orLower() {
				var myNav = navigator.userAgent.toLowerCase();
				return 7 >= (myNav.indexOf('msie') != -1) ? parseInt(myNav.split('msie')[1]) : false;
			}

			/** Determina con un boolean si se accede a la web con Safari 10. */
			function isSafari10() {
				navigator.sayswho= (function(){
				    var ua= navigator.userAgent, tem, 
				    M= ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\/))\/?\s*(\d+)/i) || [];
				    if(/trident/i.test(M[1])){
				        tem=  /\brv[ :]+(\d+)/g.exec(ua) || [];
				        return 'IE '+(tem[1] || '');
				    }
				    if(M[1]=== 'Chrome'){
				        tem= ua.match(/\b(OPR|Edge)\/(\d+)/);
				        if(tem!= null) return tem.slice(1).join(' ').replace('OPR', 'Opera');
				    }
				    M= M[2]? [M[1], M[2]]: [navigator.appName, navigator.appVersion, '-?'];
				    if((tem= ua.match(/version\/(\d+)/i))!= null) M.splice(1, 1, tem[1]);
				    return M.join(' ');
				})();
				return navigator.sayswho == 'Safari 10';
			}
			
			/** Indica si el navegador es Firefox. */
			function isFirefox() {
				return navigator.userAgent.toUpperCase().indexOf("FIREFOX") != -1
			}

			/** Indica si el navegador es Chrome. */
			function isChrome() {
				return navigator.userAgent.toUpperCase().indexOf("CHROME/") != -1 ||
					navigator.userAgent.toUpperCase().indexOf("CHROMIUM") != -1;
			}
			
			/* Metodos que publicamos del objeto */
			return {
				isAndroid : isAndroid,
				isIOS : isIOS,
				isInternetExplorer : isInternetExplorer,
				isInternetExplorer10orLower : isInternetExplorer10orLower,
				isInternetExplorer7orLower : isInternetExplorer7orLower,
				isSafari10 : isSafari10,
				isFirefox : isFirefox,
				isChrome : isChrome				
			};
		})(window, undefined);
		
		/**
		 * Funciones de utilidad.
		 */
		var AfirmaUtils = ( function (window, undefined) {
				
				/* Mayor entero. */
				var MAX_NUMBER = 2147483648;

				/* Caracteres validos para los ID de sesion */
				var VALID_CHARS_TO_ID = "1234567890abcdefghijklmnopqrstuwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

				/** Genera un nuevo identificador de sesion aleatorio. */
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

				/** Genera numeros aleatorios con una distribucion homogenea. */
				var seed;
				function rnd () {
					if (seed == undefined) {
						seed = new Date().getMilliseconds() * 1000 * Math.random();
					}
				    seed = (seed * 9301 + 49297) % 233280;
				    return seed / 233280;
				}
				
				/* Metodos que publicamos del objeto */
				return {
					/** Genera un nuevo identificador de sesion aleatorio. */
					generateNewIdSession : generateNewIdSession				
				};
		})(window, undefined);

		/**
		 * Cliente para la conexi&oacute;n con el Cliente @firma a traves de Secure WebSockets.
		 */
		var AppAfirmaWebSocketClient = ( function (window, undefined) {
			
			var PROTOCOL_VERSION = 3;
			
			var SERVER_HOST = "127.0.0.1";

			var SERVER_PORT = "63117";
			
			var URL_REQUEST = "wss://" + SERVER_HOST + ":" + SERVER_PORT;
			
			var OPERATION_LOAD = "load";
			
			var OPERATION_WITHOUT_RETURN = "save";
			
			var OPERATION_BATCH = "batch";
			
			var OPERATION_LOG = "log";
			
			var OPERATION_SELECT_CERTIFICATE = "certificate";

			var OPERATION_SIGN = "sign";

			/** Informacion de error. */
			var errorMessage = '';
			var errorType = '';

			var idSession;
			
			/** WebSocket para la comunicacion con la aplicacion. Antes de crearlo
			 * (new WebSocket(URL_REQUEST)) es necesario arrancar la aplicacion.  */
			var ws = "";
			
			/** Operacion que se manda a ejecutar. */
			var currentOperation = "";
			
			/** URL de la peticion enviada. */
			var currentOperationUrl = "";
			
			/** Indica si se ha establecido la conexion o no */
			var connected = false;
		
			/** Almacen de claves cargado por defecto */
			var defaultKeyStore = null;

			/** Funcion callback que se lanza al obtener un resultado */ 
			var successCallback = null;
			
			/** Funcion error callback */
			var errorCallback = null;
			
			/** Establece el almacen de certificados de que se debe utilizar por defecto. */
			var setKeyStore = function (keystore) {
				defaultKeyStore = keystore;
			};
			
			/**
			 * Inicia el proceso de seleccion de certificado.
			 */
			var selectCertificate = function (extraParams, successCallbackFunction, errorCallbackFunction) {
				setCallbacks(successCallbackFunction, errorCallbackFunction);
				currentOperation = OPERATION_SELECT_CERTIFICATE;
				var requestData = createSelectCertificateRequest(extraParams);
				execAppIntent(buildUrl(requestData));
			};
			
			/**
			 * Inicia el proceso de firma electronica.
			 */
			var sign = function (dataB64, algorithm, format, extraParams, successCallbackFunction, errorCallbackFunction) {
				signOperation("sign", dataB64, algorithm, format, extraParams, successCallbackFunction, errorCallbackFunction);
			};

			/**
			 * Inicia el proceso de cofirma de una firma electr&oacute;nica. 
			 */
			var coSign = function (signB64, algorithm, format, extraParams, successCallbackFunction, errorCallbackFunction) {
				signOperation("cosign", signB64, algorithm, format, extraParams, successCallbackFunction, errorCallbackFunction);
			};

			/**
			 * Inicia el proceso de contrafirma de una firma electr&oacute;nica.
			 */
			var counterSign = function (signB64, algorithm, format, extraParams, successCallbackFunction, errorCallbackFunction) {
				signOperation("countersign", signB64, algorithm, format, extraParams, successCallbackFunction, errorCallbackFunction);
			};

			/**
			 * Realiza una operacion de firma/multifirma comunicandose con la aplicacion nativa por socket.
			 * @param signId Identificador de la operacion a realizar (sign, cosign y countersign).
			 * @param dataB64 Datos o firma en base 64.
			 * @param algorithm Algoritmo de firma.
			 * @param format Formato de firma.
			 * @param extraParams Par&aacute;metros para la configuraci&oacute;n de la operaci&oacute;n.
			 */
			function signOperation (signId, dataB64, algorithm, format, extraParams, successCallbackFunction, errorCallbackFunction) {
				setCallbacks(successCallbackFunction, errorCallbackFunction);
				currentOperation = OPERATION_SIGN;
				var requestData = createSignRequest(signId, algorithm, format, extraParams,
						normalizeBase64Data(dataB64));
				execAppIntent(buildUrl(requestData));
			}

			/**
			 * Inicia el proceso de firma por lotes.
			 */
			var signAndSaveToFile = function (opId, dataB64, algorithm, format, extraParams, outputFileName, successCallbackFunction, errorCallbackFunction) {
				setCallbacks(successCallbackFunction, errorCallbackFunction);
				currentOperation = OPERATION_SIGN;
				var requestData = createSignAndSaveRequest(opId, normalizeBase64Data(dataB64),
						algorithm, format, extraParams, outputFileName);
				execAppIntent(buildUrl(requestData));
			};

			/**
			 * Inicia el proceso de firma por lotes.
			 */
			var signBatch = function (batchB64, batchPreSignerUrl, batchPostSignerUrl, extraParams, successCallbackFunction, errorCallbackFunction) {
				setCallbacks(successCallbackFunction, errorCallbackFunction);
				currentOperation = OPERATION_BATCH;
				var requestData = createBatchRequest(batchPreSignerUrl, batchPostSignerUrl,
						extraParams, normalizeBase64Data(batchB64));
				execAppIntent(buildUrl(requestData));
			};

			/**
			 * Inicia el proceso de carga de un fichero.
			 * @param title Titulo de la ventana de dialogo
			 * @param extensions Extensiones permitidas
			 * @param description Descripcion del tipo de archivo a cargar
			 * @param filePath Ruta del archivo por defecto
			 * @param successCallbackFunction Funcion de callback tras exito
			 * @param errorCallbackFunction Funcion de callback tras error
			 */
			var getFileNameContentBase64 = function (title, extensions, description, filePath, successCallbackFunction, errorCallbackFunction) {
				setCallbacks(successCallbackFunction, errorCallbackFunction);
				currentOperation = OPERATION_LOAD;
				var requestData = createLoadDataRequest("load", title, extensions, description, filePath, false);
				execAppIntent(buildUrl(requestData));
			}

			/**
			 * Inicia el proceso de carga de uno o varios ficheros.
			 * @param title Titulo de la ventana de dialogo
			 * @param extensions Extensiones permitidas
			 * @param description Descripcion del tipo de archivo a cargar
			 * @param filePath Ruta del archivo por defecto
			 * @param successCallbackFunction Funcion de callback tras exito
			 * @param errorCallbackFunction Funcion de callback tras error
			 */
			var getMultiFileNameContentBase64 = function (title, extensions, description, filePath, successCallbackFunction, errorCallbackFunction) {
				setCallbacks(successCallbackFunction, errorCallbackFunction);
				currentOperation = OPERATION_LOAD;
				var requestData = createLoadDataRequest("load", title, extensions, description, filePath, true);
				execAppIntent(buildUrl(requestData));
			}
			
			/**
			 * Inicia el proceso de obtencion del log actual de la aplicacion.
			 * @param successCallbackFunction Funcion de callback tras exito
			 * @param errorCallbackFunction Funcion de callback tras error
			 */
			var getCurrentLog = function (successCallbackFunction, errorCallbackFunction) {
				setCallbacks(successCallbackFunction, errorCallbackFunction);
				currentOperation = OPERATION_LOG;
				var requestData = createLoadDataRequest("getLog");
				execAppIntent(buildUrl(requestData));
			}
			
			/**
			 * Inicia el proceso de guardado de datos en disco.
			 */
			var saveDataToFile = function (dataB64, title, filename, extension, description, successCallbackFunction, errorCallbackFunction) {
				setCallbacks(successCallbackFunction, errorCallbackFunction);
				currentOperation = OPERATION_WITHOUT_RETURN;
				var requestData = createSaveDataRequest(normalizeBase64Data(dataB64), title, filename, extension, description);
				execAppIntent(buildUrl(requestData));
			}
			
			/** Establece las funciones callback que gestionan el resultado de una operacion. */
			function setCallbacks(successCallbackFunction, errorCallbackFunction) {
				successCallback = successCallbackFunction;
				errorCallback = errorCallbackFunction;
			}
			
			/** Convierte un base64 estandar en base64 URL SAFE. */
			function normalizeBase64Data(dataB64) {
				return !!dataB64 ? dataB64.replace(/\+/g, "-").replace(/\//g, "_") : null;
			}
			
			/** Crea una peticion de seleccion de certificado. */
			function createSelectCertificateRequest(extraParams) {
				var data = new Object();
				data.op = createKeyValuePair ("op", "selectcert");
				data.properties = createKeyValuePair ("properties", extraParams != null ? Base64.encode(extraParams, true) : null, true);
				data.ksb64 = createKeyValuePair ("ksb64", defaultKeyStore != null ? Base64.encode(defaultKeyStore, true) : null, true);
				data.sticky = createKeyValuePair ("sticky", stickySignatory);
				if (resetStickySignatory) {
					data.resetSticky = createKeyValuePair ("resetsticky", resetStickySignatory);
				}
				
				return data;
			}

			/** Crea una peticion de firma/multifirma. */
			function createSignRequest(signId, algorithm, format, extraParams, dataB64) {
				var data = new Object();
				data.op = createKeyValuePair("op", signId);
				data.algorithm = createKeyValuePair ("algorithm", algorithm);
				data.format = createKeyValuePair ("format", format); 
				data.properties = createKeyValuePair ("properties", extraParams != null ? Base64.encode(extraParams, true) : null, true);
				data.ksb64 = createKeyValuePair ("ksb64", defaultKeyStore != null ? Base64.encode(defaultKeyStore, true) : null, true);
				data.sticky = createKeyValuePair ("sticky", stickySignatory);
				if (resetStickySignatory) {
					data.resetSticky = createKeyValuePair ("resetsticky", resetStickySignatory);
				}
				data.dat = createKeyValuePair ("dat", dataB64 == "" ? null : dataB64, true);
				
				return data;
			}

			/**
			 * Realiza una operacion de firma/multifirma seguida del guardado del resultado comunicandose
			 * con la aplicacion nativa por socket.
			 * @param signId Identificador de la operacion a realizar (sign, cosign y countersign).
			 * @param dataB64 Datos o firma en base 64.
			 * @param algorithm Algoritmo de firma.
			 * @param format Formato de firma.
			 * @param extraParams Parametros para la configuraci&oacute;n de la operaci&oacute;n.
			 */
			function createSignAndSaveRequest (signId, dataB64, algorithm, format, extraParams, outputFileName) {

				var data = new Object();
				data.op = createKeyValuePair("op", "signandsave");
				data.cryptoOp = createKeyValuePair("cop", signId);
				data.algorithm = createKeyValuePair ("algorithm", algorithm);
				data.format = createKeyValuePair ("format", format);
				data.properties = createKeyValuePair ("properties", !!extraParams ? Base64.encode(extraParams, true) : null, true);
				data.filename = createKeyValuePair ("filename", outputFileName);
				data.ksb64 = createKeyValuePair ("ksb64", !!defaultKeyStore ? Base64.encode(defaultKeyStore, true) : null, true);
				data.sticky = createKeyValuePair ("sticky", stickySignatory);
				if (resetStickySignatory) {
					data.resetSticky = createKeyValuePair ("resetsticky", resetStickySignatory);
				}
				data.dat = createKeyValuePair ("dat", dataB64 == "" ? null : dataB64, true);
				
				return data;
			}

			/**
			 * Genera el objeto con los datos de la transaccion para la firma
			 */
			function createBatchRequest(batchPreSignerUrl, batchPostSignerUrl, extraParams, batchB64) {
				var data = new Object();
				data.op = createKeyValuePair("op","batch");
				data.batchpresignerurl = createKeyValuePair("batchpresignerurl", batchPreSignerUrl);
				data.batchpostsignerurl = createKeyValuePair("batchpostsignerurl", batchPostSignerUrl);
				data.properties = createKeyValuePair ("properties", extraParams != null ? Base64.encode(extraParams, true) : null, true);
				data.ksb64 = createKeyValuePair ("ksb64", defaultKeyStore != null ? Base64.encode(defaultKeyStore, true) : null, true);
				data.sticky = createKeyValuePair ("sticky", stickySignatory);
				if (resetStickySignatory) {
					data.resetSticky = createKeyValuePair ("resetsticky", resetStickySignatory);
				}
				data.needcert = createKeyValuePair ("needcert", true);
				data.dat = createKeyValuePair ("dat",  batchB64 == "" ? null : batchB64, true);
				
				return data;
			}
			
			/**
			 * Genera el objeto con los datos de la transaccion para la operacion de carga/multicarga
			 */
			function createLoadDataRequest(loadId, title, extensions, description, filePath, multiload) {
				var data = new Object();
				data.op = createKeyValuePair("op", loadId);
				data.title = createKeyValuePair("title", title);
				data.extensions = createKeyValuePair("exts", extensions);
				data.description = createKeyValuePair("desc", description);
				data.filePath = createKeyValuePair("filePath", filePath);
				data.multiload = createKeyValuePair("multiload", multiload);
				
				return data;
			}
			
			/**
			 * Genera el objeto de datos para la operacion de guardar
			 */
			function createSaveDataRequest(dataB64, title, filename, extension, description) {
				var data = new Object();
				data.op = createKeyValuePair ("op", "save");
				data.title = createKeyValuePair ("title", title);
				data.filename = createKeyValuePair ("filename", filename);
				data.extension = createKeyValuePair ("exts", extension);
				data.description = createKeyValuePair ("desc", description);
				data.dat = createKeyValuePair ("dat",  dataB64 == "" ? null : dataB64, true);
				
				return data;
			}

			/** Construye una URL que configura la operacion a realizar. */
			function buildUrl (arr) {

				// Operacion seleccionada
				var params = [];
				// Convertimos el objeto con los datos en un array del tipo key value
				for (var x in arr){
				  params.push(arr[x]);
				}

				// Las URL seran del estilo "afirma://" + Id del tipo de operacion
				var intentURL = 'afirma://' + encodeURIComponent(arr.op.value) + '?';	
				for (var i = 0; i < params.length; i++) {
					if (params[i].value != null && params[i].value != "null") {
						intentURL += (i != 0 ? '&' : '') + params[i].key + '=' +
							(params[i].avoidEncoding === true ?
									params[i].value :
									encodeURIComponent(params[i].value));
					}
				}
				return intentURL;
			}
			
			/** Envia una peticion a la aplicacion de firma. Si no la encuentra abierta, la arranca. */
			function execAppIntent (url) {
				
				// Almacenamos la URL en una propiedad global que se mantendra siempre actualizada porque
				// al invocar muchas peticiones consecutivas, en caso de introducir un retardo con setTimeout,
				// queremos que las peticiones se realicen siempre para la ultima operacion establecida y no
				// la que hubiese antes de empezar la espera (lo que ocurriria si le pasasemos la URL como
				// parametro).
				currentOperationUrl = url;
				
				// Si la aplicacion no esta abierta (primera ejecucion o se ejecuta despues del cierre de la aplicacion)
				// es necesario abrirla y esperar a que este lista
				if (!isAppOpened()) {
					// Invocamos a la aplicacion cliente
					openNativeApp();
					// Enviamos la peticion a la app despues de esperar un tiempo prudencial
					setTimeout(waitAppAndProcessRequest, 3000, AutoScript.AUTOFIRMA_CONNECTION_RETRIES);
				}
				// Si la aplicacion esta abierta, se envia de inmediato la peticion
				else {
					console.log("Enviamos el mensaje al socket abierto");
					processRequest (AutoScript.AUTOFIRMA_CONNECTION_RETRIES)
				}
			}

			/** Indica si se sabe que la aplicacion esta abierta y lista para recibir peticiones. */
			function isAppOpened() {
				return !!ws && connected;
			}
		
			/** Abre la aplicacion para que empiece a escuchar en el puerto por defecto. */
			function openNativeApp () {
				idSession = AfirmaUtils.generateNewIdSession();
				openUrl("afirma://websocket?v=" + PROTOCOL_VERSION + "&idsession=" + idSession);
			}
			

			/**
			 * Crea el websocket con el comportamiento basico.
			 */
			function createWebSocket(url) {
				var webSocket;
				
				try {
					webSocket = new WebSocket(url);
				}
				catch (e) {
					console.log("Error estableciendo el WebSocket: " + e);
				}
				
				webSocket.onopen = function() {
					connected = true;
					console.log("Se abre el socket");
				};
				
				webSocket.onclose = function() {
					connected = false;
					console.log("Se cierra el socket");
				};

				ws.onmessage = function(evt) {
					console.log("Procesado por defecto del mensaje");
				}
				
				ws.onerror = function(evt) {
					console.log("Procesado por defecto del error");
				}
				
				return webSocket;
			}
			
			/** Comprobacion recursiva de la disponibilidad de la aplicacion hasta un maximo del
			 * numero de intentos indicados. */ 
			function waitAppAndProcessRequest (retries) {
				
				if (!connected) {
				
					if (retries > 0) {
						console.log("Creamos el cliente para el socket");

						// Abrimos el websocket
						ws = createWebSocket(URL_REQUEST);

						setTimeout(waitAppAndProcessRequest, AutoScript.AUTOFIRMA_LAUNCHING_TIME, retries - 1);
					}
					else {
						processErrorResponse("java.util.concurrent.TimeoutException", "No se pudo contactar con AutoFirma");
					}
				}
				else {
					// Enviamos la peticion
					console.log("Enviamos el mensaje al socket");
					processRequest (AutoScript.AUTOFIRMA_CONNECTION_RETRIES);
				}
			}
			
			/**
			 * Intenta conectar con la aplicacion nativa mandando una peticion echo al puerto.
			 * Si la aplicacion responde lanzamos la ejecucion del servicio.
			 * Si la aplicacion no responde volvemos a lanzar cada 2 segundos otra peticion echo hasta que una
			 * peticion sea aceptada.
			 */
			function processRequest (retries) {

				// Establecemos que se envie la peticion cuando responda algo
				ws.onmessage = onMessageEchoFunction;
				
				// Establecemos que se reintente la conexion en caso de error
				ws.onerror = function(evt) {
					console.log("Error en la comunicacion por websocket: " + evt);
					sendEcho(ws, idSession, retries - 1);
				}

				// Enviamos una peticion de eco para comprobar que esta operativo, lo que despues hara que
				// se lance la operacion real como se establece en la sentencia anterior
				sendEcho(ws, idSession, retries);
			}

			/** Funcion que identifica la respuesta de una peticion de echo y envia a la aplicacion la operacion real. */
			var onMessageEchoFunction = function() {
				console.log("Respuesta de la peticion de eco");
				
				ws.onmessage = function (evt) {
					console.log("Respuesta obtenida de la operacion enviada");
					processResponse(evt.data);
				}
				
				ws.send(currentOperationUrl);
			};
			
			/** Envia una peticion de eco y, en caso de error, aplica un retardo y lo reintenta hasta
			 * un determinado numero de veces */
			function sendEcho(ws, idSession, retries) {
				
				if (retries <= 0) {
					processErrorResponse("java.util.concurrent.TimeoutException", "No se pudo contactar con AutoFirma");
					return;
				}
				
				try {
					ws.send("echo=-idsession=" + idSession + "@EOF");
				}
				catch (ex) {
					console.log("Error en echo: " + ex);
					setTimeout(sendEcho, AutoScript.AUTOFIRMA_LAUNCHING_TIME, ws, idSession, retries - 1);
				}
			}

			/**
			 * Procesa una respuesta del servicio, tratandola segun convenga si se trata de un error o segun sea
			 * el tipo de operacion que la envie.
			 */
			function processResponse (data) {
				// No se ha obtenido respuesta o se notifica la cancelacion
				if (data == undefined || data == null || data == "CANCEL") {
					processErrorResponse("es.gob.afirma.core.AOCancelledOperationException", "Operacion cancelada por el usuario");
					return;
				}
				
				// Error de memoria
				if (data == "MEMORY_ERROR") {
					processErrorResponse("es.gob.afirma.core.OutOfMemoryError", "El fichero que se pretende firmar o guardar excede de la memoria disponible para aplicacion");
					return;
				}
				
				// Se ha producido un error
				if (data.length > 4 && data.substr(0, 4) == "SAF_") {
					processErrorResponse("java.lang.Exception", data);
					return;
				}

				// Se ha producido un error y no se ha identificado el tipo
				if (data == "NULL") {
					processErrorResponse("java.lang.Exception", "Error desconocido");
					return;
				}

				// Operaciones de carga y guardado
				if (currentOperation == OPERATION_WITHOUT_RETURN) {
					processResponseWithoutReturn(data);
				}
				else if (currentOperation == OPERATION_LOAD) {
					processLoadResponse(data);
				}
				else if (currentOperation == OPERATION_SELECT_CERTIFICATE) {
					processSelectCertificateResponse(data);
				}
				else if (currentOperation == OPERATION_BATCH) {
					processBatchResponse(data);
				}
				else if (currentOperation == OPERATION_LOG) {
					processLogResponse(data);
				}
				else if (currentOperation == OPERATION_SIGN) {
					processSignResponse(data);
				}
				else {
					console.log("Operacion desconocida. Se devuelve directamente su resultado.");
					if (!!successCallback) {
						successCallback(data);
					}
					else {
						console.log("No se ha proporcionado funcion callback para procesar el resultado de la operacion");
					}
				}
			}
			
			/**
			 * Procesa la respuesta cuando se detecta un error.
			 */
			function processErrorResponse(exception, message) {
				errorType = exception;
				errorMessage = message;
				if (!!errorCallback) {
					errorCallback(exception, message);
				}
			}
			
			/**
			 * Procesa la respuesta de las operaciones de carga de ficheros.
			 */
			function processLoadResponse(data) {

				// Compruebo si se trata de una respuesta valida de una operacion de carga/multicarga (load).
				// El separador "|"  distingue los pares "filename-1:dataBase64-1|filename-2:dataBase64-2...", uno por cada archivo cargado.
				// Devolveremos un array en el que cada posicion sera uno de estos pares: "filename-n:dataBase64-n".
				// La funcion de callback realizara el tratamiento deseado,
				// pudiendo obtener cada dato del par teniendo en cuenta el
				// separador ":"
				if (data.indexOf(":") <= 0) {
					processErrorResponse("java.lang.Exception", "Respuesta no valida");
					return;
				}

				// Si no se proporciona funcion de exito, no se procesa la respuesta
				if (!successCallback) {
					console.log("No se ha proporcionado funcion callback para procesar el resultado del lote de firma");
					return;
				}
				
				// Nombre del fichero cargado o array de nombres si era mas de uno
				var filenames;
				// Contenido del fichero cargado o array de contenidos si era mas de uno
				var datasB64;
				
				var fileNamesDataBase64 = data.split("|");

				// Si solo se carga un fichero
				if (fileNamesDataBase64.length == 1) {

					var sepPos = fileNamesDataBase64[0].indexOf(":");

					filenames = fileNamesDataBase64[0].substring(0, sepPos);
					datasB64 = fileNamesDataBase64[0].substring(sepPos + 1).replace(/\-/g, "+").replace(/\_/g, "/");
				}
				// Si se carga mas de un fichero
				else {

					filenames = new Array();
					datasB64 = new Array();

					for (i = 0; i < fileNamesDataBase64.length; i++) {
						var sepPos = fileNamesDataBase64[i].indexOf(":");

						filenames.push(fileNamesDataBase64[i].substring(0, sepPos));
						datasB64.push(fileNamesDataBase64[i].substring(sepPos + 1).replace(/\-/g, "+").replace(/\_/g, "/"));
					}

				}

				successCallback(filenames, datasB64);
			}
			
			/**
			 * Procesa la respuesta de una operacion que no requiere que se procese el resultado.
			 */
			function processResponseWithoutReturn(data) {
				// Termina bien y no devuelve ningun resultado
				if (data == "OK" || data == "SAVE_OK") {
					// Si no se ha indicado funcion de guardado, entonces no se hace nada
					if (!!successCallback) {
						successCallback(data);
					}
				}
				// Termina mal
				else {
					processErrorResponse("java.lang.Exception", "Error desconocido al procesar los datos");
				}
			}


			/**
			 * Procesa la respuesta de una operacion de seleccion de certificado.
			 */
			function processSelectCertificateResponse(data) {
				if (!!successCallback) {
					successCallback(data.replace(/\-/g, "+").replace(/\_/g, "/"));
				}
				else {
					console.log("No se ha proporcionado funcion callback para procesar el certificado seleccionado");
				}
			}

			/**
			 * Procesa la respuesta de una operacion que firma de lote.
			 */
			function processBatchResponse(data) {
				if (!!successCallback) {
					var result;
					var certificate = null;
					var sepPos = data.indexOf("|");
					if (sepPos == -1) {
						result = data;
					}
					else {
						result = data.substring(0, sepPos).replace(/\-/g, "+").replace(/\_/g, "/");
						certificate = data.substring(sepPos + 1).replace(/\-/g, "+").replace(/\_/g, "/");
					}
					successCallback(result, certificate);
				}
				else {
					console.log("No se ha proporcionado funcion callback para procesar el resultado del lote de firma");
				}
			}
			
			/**
			 * Procesa la respuesta de una operacion de solicitud de logs.
			 */
			function processLogResponse(data) {

				// Si no se proporciona funcion de exito, no se procesa la respuesta
				if (!successCallback) {
					console.log("No se ha proporcionado funcion callback para procesar el log");
					return;
				}
				
				var log = " === JAVASCRIPT INFORMATION === " +
				"\nnavigator.appCodeName: " + navigator.appCodeName +
				"\nnavigator.appName: " +  navigator.appName +
				"\nnavigator.appVersion: " + navigator.appVersion +
				"\nnavigator.platform: " + navigator.platform +
				"\nnavigator.userAgent: " + navigator.userAgent+
				"\nnavigator.javaEnabled(): " + navigator.javaEnabled() +
				"\nscreen.width: " + (window.screen ? screen.width : 0) +
				"\nscreen.height: " + (window.screen ? screen.height : 0) +
				"\n\n   === CLIENTE LOG === \n" + data;
				
				successCallback(log);
			}

			/**
			 * Procesa la respuesta de una operacion de firma.
			 */
			function processSignResponse(data) {
				
				// Si no se proporciona funcion de exito, no se procesa la respuesta
				if (!successCallback) {
					console.log("No se ha proporcionado funcion callback para procesar el resultado de la firma");
					return;
				}
				
				// Interpretamos el resultado como un base 64 y el certificado y los datos cifrados
				var signature;
				var certificate = null;
				var extraInfo = null;
				var sepPos = data.indexOf("|");
				
				if (sepPos == -1) {
					signature = Base64.decode(data, true).replace(/\-/g, "+").replace(/\_/g, "/");
				}
				else {
					certificate = data.substring(0, sepPos).replace(/\-/g, "+").replace(/\_/g, "/");
					var sepPos2 = data.indexOf("|", sepPos + 1);
					if (sepPos2 == -1) {
						signature = data.substring(sepPos + 1).replace(/\-/g, "+").replace(/\_/g, "/");
					}
					else {
						signature = data.substring(sepPos + 1, sepPos2).replace(/\-/g, "+").replace(/\_/g, "/");
						extraInfo = Base64.decode(data.substring(sepPos2), true);
					}
				}

				successCallback(signature, certificate, extraInfo);
			}
			
			/** Crea un objeto con los parametros indicados. */
			function createKeyValuePair(key, value, avoidUrlEncoding) {
				var data  = new Object();
				data.key = key;
				data.value = value;
				data.avoidEncoding = !!avoidUrlEncoding;
				return data;
			}
			
			/** 
			 * Funcion para la comprobacion de existencia del objeto. Siempre devuelve
			 * la cadena "Cliente JavaScript".
			 */
			function echo () {
				return "Cliente JavaScript";
			}
			
			/**
			 * Recupera el mensaje de error asociado al ultimo error capturado.
			 */
			function getErrorMessage () {
				return errorMessage;
			}

			/**
			 * Recupera el tipo del ultimo error capturado.
			 */
			function getErrorType () {
				return errorType;
			}

			/**
			 * Funcion para identificar el tipo de objeto del Cliente (javascript, applet,...).
			 */
			function getType () {
				return "javascript";
			}

			/* Metodos que publicamos del objeto AppAfirmaWebSocketClient */
			return {
				echo : echo,
				setKeyStore : setKeyStore,
				sign : sign,
				coSign : coSign,
				counterSign : counterSign,
				signBatch : signBatch,
				selectCertificate : selectCertificate,
				saveDataToFile : saveDataToFile,
				signAndSaveToFile : signAndSaveToFile,
				getFileNameContentBase64: getFileNameContentBase64,
				getMultiFileNameContentBase64 : getMultiFileNameContentBase64,
				setStickySignatory : setStickySignatory,
				setLocale : setLocale,
				getErrorMessage : getErrorMessage,
				getErrorType : getErrorType,
				getCurrentLog : getCurrentLog				
			}
		});

		/**
		 * Cliente para la conexi&oacute;n con el Cliente @firma a traves de una conexion directa
		 * por un socket seguro.
		 */
		var AppAfirmaJSSocket = ( function (clientAddress, window, undefined) {

			/**
			 *  Atributos para la configuracion del objeto sustituto del applet Java de firma
			 */
			var errorMessage = '';
			var errorType = '';

			/** Puerto a traves del que se ha conectado con la aplicacion nativa. */
			var port = "";
			
			var idSession;
			
			var PROTOCOL_VERSION = 1;
			
			/* Tiempo de retardo para peticiones */
			var WAITING_TIME = 500;
			
			var URL_REQUEST = "https://127.0.0.1:";
			
			/* Respuesta del socket a la peticion realizada */
			var totalResponseRequest = "";
			
			/* Numero de fragmentos recibidos del socket */
			var recibidos = 0;
			
			/* Variable de control para la operacion seleccion de certificado */ 
			var isSelectCertOperation = false;
			
			/* Variable de control para la operacion guardar */
			var isSaveOperation = false;
			
			/* Variable de control para la operacion de ejecutar operacion criptografica y guardar */
			var isOpAndSaveOperation = false;
			
			/* Variable de control para la operacion batch */
			var isBatchOperation = false;
			
			/* URL de la peticion HTTPS */
			var urlHttpRequest = "";
			
			/* Maxima longitud permitida para una URL. Si la url se excede se divide
			 * la peticion en fragmentos. Con Internet Explorer y Firefox a partir de
			 * la version 49 se producen corrupciones de datos con el valor original
			 * de 1Mb, asi que se reduce este a un valor seguro para ese navegador. */
			var URL_MAX_SIZE = Platform.isInternetExplorer() ? 12000 : Platform.isFirefox() ? 458752 : 1048576;
			
			/* Indica si se ha establecido la conexion o no */
			var connection = false;
		
			/* Dominio desde el que se realiza la llamada al servicio */
			var baseUri = clientAddress;

			/* Almacen de claves cargado por defecto */
			var defaultKeyStore = null;

			/* Funcion callback que se lanza al obtener un resultado */ 
			var successCallback = null;
			
			/* Funcion error callback */
			var errorCallback = null;

			/* URL de la operacion que se solicita actualmente. */
			var currentOperationUrl = null;
			
			/**
			 * Establece el almacen de certificados de que se debe utilizar por defecto.
			 */
			function setKeyStore (keystore) {
				defaultKeyStore = keystore;
			}
			
			/**
			 * Inicia el proceso de seleccion de certificado.
			 */
			function selectCertificate (extraParams, successCallbackFunction, errorCallbackFunction) {
				successCallback = successCallbackFunction;
				errorCallback = errorCallbackFunction;
				selectCertByService(extraParams);
			}

			function selectCertByService (extraParams) {
				
				var data = new Object();
				data.op = generateDataKeyValue ("op", "selectcert");
				data.properties = generateDataKeyValue ("properties", extraParams != null ? Base64.encode(extraParams) : null);
				data.keystore = generateDataKeyValue ("keystore", defaultKeyStore != null ? defaultKeyStore : null);
				data.ksb64 = generateDataKeyValue ("ksb64", defaultKeyStore != null ? Base64.encode(defaultKeyStore) : null);
				data.sticky = generateDataKeyValue ("sticky", stickySignatory);
				if (resetStickySignatory) {
					data.resetSticky = generateDataKeyValue ("resetsticky", resetStickySignatory);
				}
				
				execAppIntent(buildUrl(data));
			}
			
			/**
			 * Inicia el proceso de firma electronica.
			 * Implementada en el applet Java de firma
			 */
			function sign (dataB64, algorithm, format, extraParams, successCallbackFunction, errorCallbackFunction) {
				successCallback = successCallbackFunction;
				errorCallback = errorCallbackFunction;
				signByService("sign", dataB64, algorithm, format, extraParams);
			}

			/**
			 * Inicia el proceso de cofirma de una firma electr&oacute;nica. 
			 * Implementada en el applet Java de firma.
			 */
			function coSign (signB64, algorithm, format, extraParams, successCallbackFunction, errorCallbackFunction) {
				successCallback = successCallbackFunction;
				errorCallback = errorCallbackFunction;
				signByService("cosign", signB64, algorithm, format, extraParams);
			}

			/**
			 * Inicia el proceso de contrafirma de una firma electr&oacute;nica.
			 * Implementada en el applet Java de firma. 
			 */
			function counterSign (signB64, algorithm, format, extraParams, successCallbackFunction, errorCallbackFunction) {
				successCallback = successCallbackFunction;
				errorCallback = errorCallbackFunction;
				signByService("countersign", signB64, algorithm, format, extraParams);
			}

			/**
			 * Realiza una operacion de firma/multifirma comunicandose con la aplicacion nativa por socket.
			 * @param signId Identificador de la operacion a realizar (sign, cosign y countersign).
			 * @param dataB64 Datos o firma en base 64.
			 * @param algorithm Algoritmo de firma.
			 * @param format Formato de firma.
			 * @param extraParams Par&aacute;metros para la configuraci&oacute;n de la operaci&oacute;n.
			 */
			function signByService (signId, dataB64, algorithm, format, extraParams) {

				if (dataB64 == undefined || dataB64 == "") {
					dataB64 = null;
				}

				if (dataB64 != null && !isValidUrl(dataB64)) {
					dataB64 = dataB64.replace(/\+/g, "-").replace(/\//g, "_");
				}

				var data = generateDataToSign(signId, algorithm, format, extraParams, dataB64, defaultKeyStore);
				
				execAppIntent(buildUrl(data));
			}

			/**
			 * Inicia el proceso de firma por lotes.
			 */
			function signAndSaveToFile (opId, dataB64, algorithm, format, extraParams, outputFileName, successCallbackFunction, errorCallbackFunction) {
				successCallback = successCallbackFunction;
				errorCallback = errorCallbackFunction;
				signAndSaveFileByService(opId, dataB64, algorithm, format, extraParams, outputFileName);
			}

			/**
			 * Realiza una operacion de firma/multifirma seguida del guardado del resultado comunicandose
			 * con la aplicacion nativa por socket.
			 * @param signId Identificador de la operacion a realizar (sign, cosign y countersign).
			 * @param dataB64 Datos o firma en base 64.
			 * @param algorithm Algoritmo de firma.
			 * @param format Formato de firma.
			 * @param extraParams Parametros para la configuraci&oacute;n de la operaci&oacute;n.
			 */
			function signAndSaveFileByService (signId, dataB64, algorithm, format, extraParams, outputFileName) {

				if (dataB64 == undefined || dataB64 == "") {
					dataB64 = null;
				}

				if (dataB64 != null && !isValidUrl(dataB64)) {
					dataB64 = dataB64.replace(/\+/g, "-").replace(/\//g, "_");
				}

				var data = generateDataToSignAndSave(signId, algorithm, format, extraParams, outputFileName, dataB64, defaultKeyStore);

				execAppIntent(buildUrl(data));
			}

			/**
			 * Inicia el proceso de firma por lotes.
			 */
			function signBatch (batchB64, batchPreSignerUrl, batchPostSignerUrl, extraParams, successCallbackFunction, errorCallbackFunction) {
				successCallback = successCallbackFunction;
				errorCallback = errorCallbackFunction;
				signBatchByService(batchB64, batchPreSignerUrl, batchPostSignerUrl, extraParams);
			}
			
			function signBatchByService (batchB64, batchPreSignerUrl, batchPostSignerUrl, extraParams) {
				
				if (batchB64 == undefined || batchB64 == "") {
					batchB64 = null;
				}
				
				if (batchB64 != null) {
					batchB64 = batchB64.replace(/\+/g, "-").replace(/\//g, "_");
				}
				
				var data = generateDataToBatch(defaultKeyStore, batchPreSignerUrl, batchPostSignerUrl, extraParams, batchB64);
				execAppIntent(buildUrl(data));
			}
			
			/**
			 * Genera el objeto con los datos de la transaccion para la firma
			 */
			function generateDataToBatch(keystore, batchPreSignerUrl, batchPostSignerUrl, extraParams, batchB64) {
				var data = new Object();
				data.op = generateDataKeyValue("op","batch");
				data.keystore = generateDataKeyValue("keystore", keystore != null ? keystore : null);
				data.ksb64 = generateDataKeyValue ("ksb64", keystore != null ? Base64.encode(keystore) : null);
				data.batchpresignerurl = generateDataKeyValue("batchpresignerurl", batchPreSignerUrl);
				data.batchpostsignerurl = generateDataKeyValue("batchpostsignerurl", batchPostSignerUrl);
				data.properties = generateDataKeyValue ("properties", extraParams != null ? Base64.encode(extraParams) : null);
				data.dat = generateDataKeyValue ("dat",  batchB64 == "" ? null : batchB64);
				data.sticky = generateDataKeyValue ("sticky", stickySignatory);
				if (resetStickySignatory) {
					data.resetSticky = generateDataKeyValue ("resetsticky", resetStickySignatory);
				}

				return data;
			}

			/**
			 * Construye una URL para la invocaci&oacute;n del Cliente @firma nativo.
			 * params: Par\u00E1metros para la configuraci\u00F3n de la operaci\u00F3n.
			 */
			function buildUrl (arr) {

				// Operacion seleccionada
				var intentURL;
				var params = [];
				// Convertimos el objeto con los datos en un array del tipo key value
				for(var x in arr){
				  params.push(arr[x]);
				}

				if (params != null && params != undefined && params.length > 0) {
					intentURL = 'afirma://' + encodeURIComponent(arr.op.value) + '?';	// Agregamos como dominio el identificador del tipo de operacion
					for (var i = 0; i < params.length; i++) {
						if (params[i].value != null && params[i].value != "null") {
							intentURL += (i != 0 ? '&' : '') + params[i].key + '=' + encodeURIComponent(params[i].value); 
						}
					}
				}
				return intentURL;
			}
			
			function execAppIntent (url) {
				
				// Primera ejecucion, no hay puerto definido
				if (port == "") {
					// Calculamos los puertos
					var ports = getRandomPorts();
					// Invocamos a la aplicacion nativa
					openNativeApp(ports);
					// Enviamos la peticion a la app despues de esperar un tiempo prudencial
					setTimeout(executeEchoByServiceByPort, AutoScript.AUTOFIRMA_LAUNCHING_TIME, ports, url);
				}
				// Se ha ejecutado anteriormente y tenemos un puerto calculado.
				else {
					connection = false;
					executeEchoByService (port, url, AutoScript.AUTOFIRMA_CONNECTION_RETRIES)
				}
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
		
		
			function openNativeApp (ports) {
			
				var portsLine = "";
				for (var i = 0; i < ports.length; i++) {
					portsLine += ports[i];
					if (i < (ports.length - 1)) {
						portsLine += ",";
					}
				}
				idSession = AfirmaUtils.generateNewIdSession();
				
				// Lanzamos la aplicacion nativa
				openUrl("afirma://service?ports=" + portsLine + "&v=" + PROTOCOL_VERSION + "&idsession=" + idSession);
			}

			/**
			 * Genera el objeto con los datos de la transaccion para la operacion
			 * de firma/multifirma
			 */
			function generateDataToSign(signId, algorithm, format, extraParams, dataB64, keystore) {
				var data = new Object();
				data.op = generateDataKeyValue("op", signId);
				data.keystore = generateDataKeyValue ("keystore", keystore != null ? keystore : null);
				data.ksb64 = generateDataKeyValue ("ksb64", keystore != null ? Base64.encode(keystore) : null);
				data.algorithm = generateDataKeyValue ("algorithm", algorithm);
				data.format = generateDataKeyValue ("format", format); 
				data.properties = generateDataKeyValue ("properties", extraParams != null ? Base64.encode(extraParams) : null);
				data.dat = generateDataKeyValue ("dat", dataB64 == "" ? null : dataB64);
				data.sticky = generateDataKeyValue ("sticky", stickySignatory);
				if (resetStickySignatory) {
					data.resetSticky = generateDataKeyValue ("resetsticky", resetStickySignatory);
				}

				return data;
			}

			/**
			 * Genera el objeto con los datos de la transaccion para la operacion
			 * de firma/multifirma seguida del guardado del resultado
			 */
			function generateDataToSignAndSave(signId, algorithm, format, extraParams, outputFileName, dataB64, keystore) {
				var data = new Object();
				data.op = generateDataKeyValue("op", "signandsave");
				data.cryptoOp = generateDataKeyValue("cop", signId);
				data.keystore = generateDataKeyValue ("keystore", keystore != null ? keystore : null);
				data.ksb64 = generateDataKeyValue ("ksb64", keystore != null ? Base64.encode(keystore) : null);
				data.algorithm = generateDataKeyValue ("algorithm", algorithm);
				data.format = generateDataKeyValue ("format", format);
				data.properties = generateDataKeyValue ("properties", extraParams != null ? Base64.encode(extraParams) : null);
				data.filename = generateDataKeyValue ("filename", outputFileName);
				data.dat = generateDataKeyValue ("dat", dataB64 == "" ? null : dataB64);
				data.sticky = generateDataKeyValue ("sticky", stickySignatory);
				if (resetStickySignatory) {
					data.resetSticky = generateDataKeyValue ("resetsticky", resetStickySignatory);
				}

				return data;
			}
			
			/**
			 * Genera el objeto con los datos de la transaccion para la operacion
			 * de carga/multicarga
			 */
			function generateDataToLoad(loadId, title, extensions, description, filePath, multiload) {
				var data = new Object();
				data.op = generateDataKeyValue("op", loadId);
				data.title = generateDataKeyValue("title", title);
				data.extensions = generateDataKeyValue("exts", extensions);
				data.description = generateDataKeyValue("desc", description);
				data.filePath = generateDataKeyValue("filePath", filePath);
				data.multiload = generateDataKeyValue("multiload", multiload);
				return data;
			}
			
			function executeEchoByServiceByPort (ports, url) {
				connection = false;
				var semaphore = new Object();
				semaphore.locked = false;
				executeEchoByService (ports[0], url, AutoScript.AUTOFIRMA_CONNECTION_RETRIES, semaphore);
				executeEchoByService (ports[1], url, AutoScript.AUTOFIRMA_CONNECTION_RETRIES, semaphore);
				executeEchoByService (ports[2], url, AutoScript.AUTOFIRMA_CONNECTION_RETRIES, semaphore);
			}

			/**
			* Intenta conectar con la aplicacion nativa mandando una peticion echo al puerto.
			* Si la aplicacion responde lanzamos la ejecucion del servicio.
			* Si la aplicacion no responde volvemos a lanzar cada 2 segundos otra peticion echo hasta que una
			* peticion sea aceptada.
			*/
			function executeEchoByService (currentPort, url, timeoutResetCounter, semaphore) {

				
				// Almacenamos la URL en una propiedad global que se mantendra siempre actualizada porque
				// al invocar muchas peticiones consecutivas, en caso de introducir un retardo con setTimeout,
				// queremos que las peticiones se realicen siempre para la ultima operacion establecida y no
				// la que hubiese antes de empezar la espera (lo que ocurriria si le pasasemos la URL como
				// parametro).
				currentOperationUrl = url;
				
				var httpRequest = getHttpRequest();
				httpRequest.open("POST", URL_REQUEST + currentPort + "/afirma", true);
				httpRequest.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
				httpRequest.onreadystatechange = function() {
					if (httpRequest.readyState == 4 && httpRequest.status == 200 && Base64.decode(httpRequest.responseText, true) == "OK" && !connection) {
						port = currentPort;
						urlHttpRequest = URL_REQUEST + port + "/afirma";
						connection = true;
						if (semaphore) {
							semaphore.locked = true;
						}
						// Comprobamos si es una operacion de seleccion de certificado
						isSelectCertOperation = currentOperationUrl.indexOf("afirma://selectcert") > -1;
						// Comprobamos si es una operacion de guardado.
						isSaveOperation = currentOperationUrl.indexOf("afirma://save") > -1;
						// Comprobamos si es una operacion de firma por lotes
						isBatchOperation = currentOperationUrl.indexOf("afirma://batch") > -1;
						// Comprobamos si es una operacion criptografica mas guardado del resultado
						isOpAndSaveOperation = currentOperationUrl.indexOf("afirma://signandsave") > -1;
						executeOperationByService();
					}
					else if ((!semaphore || !semaphore.locked) && !connection && httpRequest.readyState != 2 && httpRequest.readyState != 3) {
						timeoutResetCounter--;
						
						// Si ya se conecto antes con la aplicacion pero ahora llevamos la mitad de los intentos
						// sin conectar, consideramos que se ha tumbado y hay que relanzarla
						if (port != "" && timeoutResetCounter < AutoScript.AUTOFIRMA_CONNECTION_RETRIES/2) {
							port = "";
							if (semaphore) {
								semaphore.locked = true;
							}
							timeoutResetCounter = AutoScript.AUTOFIRMA_CONNECTION_RETRIES;
							execAppIntent(currentOperationUrl);							
						}
						// Si hemos agotado todos los reintentos consideramos que la aplicacion no esta instalada
						else if (timeoutResetCounter == 0) {
							if (!semaphore || !semaphore.locked) {
								if (semaphore) {
									semaphore.locked = true;
								}
								errorCallback("es.gob.afirma.standalone.ApplicationNotFoundException", "No se ha podido conectar con AutoFirma.");
							}
							return;
						}
						// Aun quedan reintentos
						else {
							setTimeout(executeEchoByServiceDelayed, AutoScript.AUTOFIRMA_LAUNCHING_TIME, currentPort, timeoutResetCounter, semaphore);
						}
					}
				}
				
				if (!connection) {
					// Mandamos un echo con - por lo que las variables de control se resetearan
					// Se anade EOF para que cuando el socket SSL lea la peticion del buffer sepa
					// que ha llegado al final y no se quede en espera
					httpRequest.send("echo=-idsession=" + idSession + "@EOF");
					//console.log("probamos puerto " +currentPort)
				}
			}

			/** Funcion que ejecuta la llamada a la funcion eco previa a una llamada de operacion
			 * de tal forma que esta preparada para ser lanzada desde un setTimeout. Esta funcion
			 * toma la URL de la operacion de la variable global "currentOperationUrl" ya que, en
			 * una ejecucion rapida de firmas en serie, si se pasase por parametro, en el momento
			 * de la ejecucion podria ejecutar la operacion con una URL de una operacion anterior. */
			function executeEchoByServiceDelayed (currentPort, timeoutResetCounter, semaphore) {
				executeEchoByService (currentPort, currentOperationUrl, timeoutResetCounter, semaphore);
			}
			
			
			/**
			* Comprueba si hay que dividir los datos que se se mandan a la aplicacion nativa.
			* Si hay que dividirlos se llama a la funcion executeOperationRecursive.
			* Si cabe en un solo envio se manda directamente.
			*/
			function executeOperationByService () {
				
				var url = currentOperationUrl;
				
				// Si el envio se debe fragmentar, llamamos a una funcion que se encarga
				// de mandar la peticion recursivamente
				if (url.length > URL_MAX_SIZE) {
					executeOperationRecursive(url, 1, Math.ceil(url.length/URL_MAX_SIZE));
					return;
				}
				
				var httpRequest = getHttpRequest();
				httpRequest.open("POST", urlHttpRequest, true);
				httpRequest.setRequestHeader("Content-type","application/x-www-form-urlencoded");
				
				// El envio no se fragmenta
				httpRequest.onreadystatechange = function() {
					if (httpRequest.status == 404) {
						errorServiceResponseFunction("java.lang.IOException", httpRequest.responseText);
					}
					// Las operaciones que no requieren respuesta, llaman directamente a la funcion de exito 
					if (isSaveOperation) {
						if (httpRequest.readyState == 4 && Base64.decode(httpRequest.responseText) != "") {
							successServiceResponseFunction(Base64.decode(httpRequest.responseText));
						}
						return;
					}
					// El resto de operaciones deben componer el resultado
					else {
						if (httpRequest.readyState == 4 && httpRequest.status == 200 && httpRequest.responseText != "") {
							if (Base64.decode(httpRequest.responseText) == "MEMORY_ERROR"){
								errorServiceResponseFunction("java.lang.OutOfMemoryError", "Problema de memoria en servidor");
								return;
							}
							// Juntamos los fragmentos
							totalResponseRequest = "";
							addFragmentRequest (1, Base64.decode(httpRequest.responseText, true));
						}
						// Volvemos a mandar la peticion si no manda texto en la respuesta y la peticion esta en estado ready
						else if (httpRequest.responseText == "" && httpRequest.status == 0 && httpRequest.readyState == 0) {
							setTimeout(executeOperationByService, WAITING_TIME);
						}
					}
				}
				try {
					httpRequest.onerror = function(e) {
						// status error 0 es que no se ha podido comunicar con la aplicacion
						if (e.target.status == 0) {
							errorServiceResponseFunction("java.lang.IOException", "Se ha perdido la conexion con la aplicacion @firma " + e.target.statusText);
						}
						// error desconocido 
						else {
							errorServiceResponseFunction("java.lang.IOException", "Ocurrio un error de red en la llamada al servicio de firma "+e.target.statusText);
						}
					}
				}
				catch (e) {
					// Vacio
				}
				// se anade EOF para que cuando el socket SSL lea la peticion del buffer sepa que ha llegado al final y no se quede en espera
				httpRequest.send("cmd=" + Base64.encode(url, true) + "idsession=" + idSession + "@EOF");
			}
			
			/**
			* Manda los datos a la aplicacion nativa en varios fragmentos porque ha habido que dividir los datos.
			* Se va mandando cada peticion cuando se reciba la anterior.
			*/
			function executeOperationRecursive (url, i, iFinal) {
				
				var httpRequest = getHttpRequest();
				httpRequest.open("POST", urlHttpRequest, true);
				httpRequest.setRequestHeader("Content-type","application/x-www-form-urlencoded");
				var urlToSend = url.substring(((i-1) * URL_MAX_SIZE), Math.min(URL_MAX_SIZE * i, url.length));
				httpRequest.onreadystatechange = function (evt) {
					if (httpRequest.status == 404) {
						errorServiceResponseFunction("java.lang.Exception", httpRequest.responseText);
						return;
					}
					// Respuesta afirmativa, hay que mandar mas fragmentos
					if (httpRequest.readyState == 4 && httpRequest.status == 200 && httpRequest.responseText != "" ) {
						recibidos++;
						
						// Faltan mas peticiones por enviar
						if (Base64.decode(httpRequest.responseText, true) == "MORE_DATA_NEED") {
							if (recibidos < iFinal ){
								//console.log("recibido el fragmento "+recibidos + "de "+iFinal)
								executeOperationRecursive(url, i+1, iFinal);
							}
						}
						// Todas las peticiones se han recibido, hay que mandar operacion firma
						// Respuesta es OK
						else if (Base64.decode(httpRequest.responseText, true) == "OK") {
							if(recibidos == iFinal){
								//console.log("recibido todo, realizamos la operacion");
								recibidos = 0;
								doFirm();
							}
						}
						else if(Base64.decode(httpRequest.responseText) == "MEMORY_ERROR"){
							successServiceResponseFunction(Base64.decode(httpRequest.responseText));
						}
					}
					else if (httpRequest.responseText == "" && httpRequest.status == 0 && httpRequest.readyState == 0) {
						setTimeout(executeOperationRecursive, WAITING_TIME, url, i, iFinal);
					}
				}
				try {
					httpRequest.onerror = function(e) { 
						// Status error 0 es que no se ha podido comunicar con la aplicacion
						if (e.target.status == 0){
							errorServiceResponseFunction("java.lang.IOException", "Se ha perdido la conexion con la aplicacion @firma " + e.target.statusText);
						}
						// Error desconocido 
						else{
							errorServiceResponseFunction("java.lang.IOException", "Ocurrio un error de red en la llamada al servicio de firma "+e.target.statusText);
						}
					}
				}
				catch (e) {
					// Vacio
				}
				// Se anade EOF para que cuando el socket SSL lea la peticion del buffer sepa que ha llegado al final y no se quede en espera
				httpRequest.send("fragment=@" + i + "@" + iFinal + "@"  + Base64.encode(urlToSend, true) + "idsession=" + idSession +"@EOF");
				//console.log("mandado parte "+i+" de"+iFinal);
	
			}
			
			/**
			 * Realiza una operacion que se ha mandando en varios fragmentos.
			 */
			function doFirm () {
				httpRequest = getHttpRequest();
				httpRequest.open("POST", urlHttpRequest, true);
				httpRequest.setRequestHeader("Content-type","application/x-www-form-urlencoded");
				httpRequest.onreadystatechange = function() {

					if (httpRequest.status == 404) {
						errorServiceResponseFunction("java.lang.Exception", httpRequest.responseText);
					}

					// Se ha realizado la operacion save, no controlamos reintentos ni el exito de la peticion
					// porque no requiere respuesta
					if (isSaveOperation) {
						if (httpRequest.readyState == 4 && Base64.decode(httpRequest.responseText) != "") {
							successServiceResponseFunction(Base64.decode(httpRequest.responseText));
						}
						return;
					}
					else {
						if (httpRequest.readyState == 4 && httpRequest.status == 200 && httpRequest.responseText != "") {
							if(Base64.decode(httpRequest.responseText) == "MEMORY_ERROR"){
								successServiceResponseFunction(Base64.decode(httpRequest.responseText));
							}
							totalResponseRequest = "";
							addFragmentRequest (1, Base64.decode(httpRequest.responseText, true));
						}
						// No recibimos la respuesta, volvemos a llamar.
						else {
							if (httpRequest.status == 0 && httpRequest.readyState == 0 ) {
								setTimeout(doFirm, WAITING_TIME);	
							}
						}
					}
				}
				try {
					httpRequest.onerror = function(e) { 
						// status error 0 es que no se ha podido comunicar con la aplicacion
						if (e.target.status == 0){
							errorServiceResponseFunction("java.lang.IOException", "Se ha perdido la conexion con la aplicacion @firma " + e.target.statusText);
						}
						// error desconocido 
						else{
							errorServiceResponseFunction("java.lang.IOException", "Ocurrio un error de red en la llamada al servicio de firma "+e.target.statusText);
						}
					}
				}
				catch (e) {
					// Vacio
				}
				httpRequest.send("firm=idsession=" + idSession +"@EOF");
			}

			/**
			 * Se encarga de solicitar y montar la respuesta de la operacion realizada.
			 */
			function addFragmentRequest (part, totalParts){
				httpRequest = getHttpRequest();
				httpRequest.open("POST", urlHttpRequest, true);
				httpRequest.setRequestHeader("Content-type","application/x-www-form-urlencoded");
				httpRequest.onreadystatechange = function() {
					if (httpRequest.readyState == 4 && httpRequest.status == 200 && httpRequest.responseText != "") {
						if(Base64.decode(httpRequest.responseText) == "MEMORY_ERROR"){
							errorServiceResponseFunction("java.lang.OutOfMemoryError", "Problema de memoria en servidor");
							return;
						}
						//console.log("recibida la parte " + part);
						totalResponseRequest += Base64.decode(httpRequest.responseText, true);
						// Si estan todas las partes llamamos al successcallback
						if (part == totalParts) {
							// Es una operacion de firma por lotes y tiene un callback propio
							if (isBatchOperation) {
								successBatchResponseFunction(totalResponseRequest);
								isBatchOperation = false;
							}
							// Es una operacion de seleccion de certificados y tiene un callback propio
							else if (isSelectCertOperation) {
								successSelectCertServiceResponseFunction(totalResponseRequest);
								isSelectCertOperation = false;
							}
							// Es cualquier otra operacion que requiere respuesta
							else {
								successServiceResponseFunction(totalResponseRequest);
							}
							totalResponseRequest = "";
						}
						else {
							addFragmentRequest(part + 1, totalParts);
						}
					}
					else {
						if (httpRequest.responseText == "" && httpRequest.status == 0 && httpRequest.readyState == 0){
							setTimeout(addFragmentRequest, WAITING_TIME, part, totalParts);
						}
					}
				}
				try {
					httpRequest.onerror = function(e) { 
						// status error 0 es que no se ha podido comunicar con la aplicacion
						if (e.target.status == 0){
							errorServiceResponseFunction("java.lang.IOException", "Se ha perdido la conexion con la aplicacion @firma " + e.target.statusText);
						}
						// error desconocido 
						else{
							errorServiceResponseFunction("java.lang.IOException", "Ocurrio un error de red en la llamada al servicio de firma "+e.target.statusText);
						}
					}
				}
				catch (e) {
					// Vacio
				}
				if (part <= totalParts){
					// se anade EOF para que cuando el socket SSL lea la peticion del buffer sepa que ha llegado al final y no se quede en espera
					httpRequest.send("send=@"+part+"@"+totalParts+"idsession=" + idSession +"@EOF");
					//console.log("solicitarmos la parte "+part+" de "+ totalParts)
				}
			}
			
			/**
			 * Lee el resultado devuelto por el servicio, 'CANCEL' o empieza por 'SAF-', ejecutara el metodo
			 * de error, si es 'OK' o cualquier otra cosa (que se intepretara como el resultado en base 64)
			 * se ejecutara el metodo de exito. En este ultimo caso, se descifrara el resultado. 
			 * @param data Resultado obtenido.
			 */
			function successSelectCertServiceResponseFunction (data) {
				// No se ha obtenido respuesta o se notifica la cancelacion
				if (data == undefined || data == null || data == "CANCEL") {
					errorCallback("es.gob.afirma.core.AOCancelledOperationException", "Operacion cancelada por el usuario");
					return;
				}

				// Se ha producido un error
				if (data.length > 4 && data.substr(0, 4) == "SAF_") {
					errorCallback("java.lang.Exception", data);
					return;
				}

				// Se ha producido un error y no se ha identificado el tipo
				if (data == "NULL") {
					errorCallback("java.lang.Exception", "Error desconocido");
					return;
				}

				successCallback(data.replace(/\-/g, "+").replace(/\_/g, "/"));
			}

			/**
			 * Lee el resultado devuelto por el servicio, 'CANCEL' o empieza por 'SAF-', ejecutara el metodo
			 * de error, si es 'OK' o cualquier otra cosa (que se intepretara como el resultado en base 64)
			 * se ejecutara el metodo de exito. En este ultimo caso, se descifrara el resultado. 
			 * @param data Resultado obtenido.
			 */
			function successServiceResponseFunction (data) {
				// No se ha obtenido respuesta o se notifica la cancelacion
				if (data == undefined || data == null || data == "CANCEL") {
					errorCallback("es.gob.afirma.core.AOCancelledOperationException", "Operacion cancelada por el usuario");
					return;
				}
				
				// Termina bien y no devuelve ningun resultado o es una operacion guardado
				if (data == "OK") {
					successCallback(data, null);
					return;
				}
				
				// Error de memoria
				if (data == "MEMORY_ERROR") {
					errorCallback("es.gob.afirma.core.OutOfMemoryError", "El fichero que se pretende firmar o guardar excede de la memoria disponible para aplicacion");
					return;
				}
				
				// Se ha producido un error
				if (data.length > 4 && data.substr(0, 4) == "SAF_") {
					errorCallback("java.lang.Exception", data);
					return;
				}

				// Se ha producido un error y no se ha identificado el tipo
				if (data == "NULL") {
					errorCallback("java.lang.Exception", "Error desconocido");
					return;
				}
				
				// Operacion guardado con exito
				if (data == "SAVE_OK") {
					if (successCallback) {
						successCallback(data);
					}
					return;
				}
				
				// Vengo de getCurrentLog
				if (data.length > 150 && data.substr(0, 150).indexOf("<log>") != -1) {
					var log = " === JAVASCRIPT INFORMATION === " +
					"\nnavigator.appCodeName: " + navigator.appCodeName +
					"\nnavigator.appName: " +  navigator.appName +
					"\nnavigator.appVersion: " + navigator.appVersion +
					"\nnavigator.platform: " + navigator.platform +
					"\nnavigator.userAgent: " + navigator.userAgent+
					"\nnavigator.javaEnabled(): " + navigator.javaEnabled() +
					"\nscreen.width: " + (window.screen ? screen.width : 0) +
					"\nscreen.height: " + (window.screen ? screen.height : 0) +
					"\n\n   === CLIENTE LOG === \n" + data;
					successCallback(log);
					return;
				}
				
				// Compruebo si se trata de una operacion de carga/multicarga (load).
				// El separador "|"  distingue los pares "filename-1:dataBase64-1|filename-2:dataBase64-2...", uno por cada archivo cargado.
				// Devolveremos un array en el que cada posicion sera uno de estos pares: "filename-n:dataBase64-n".
				// La funcion de callback realizara el tratamiento deseado,
				// pudiendo obtener cada dato del par teniendo en cuenta el
				// separador ":"
				if (data.indexOf(":") > 0) {
					
					var fileNamesDataBase64 = data.split("|");
					
					if (fileNamesDataBase64.length == 1) {
						
						var sepPos = fileNamesDataBase64[0].indexOf(":");
						var fileNameDataBase64 = fileNamesDataBase64[0];
						
						if (sepPos == -1) {
							fileName = Base64.decode(fileNameDataBase64, true);
						}
						else {
							fileName = fileNameDataBase64.substring(0, sepPos);
							dataB64 = fileNameDataBase64.substring(sepPos + 1).replace(/\-/g, "+").replace(/\_/g, "/");
						}
						
						successCallback(fileName,dataB64);
						
						return;
						
					} else if (fileNamesDataBase64.length > 1 ) {
						
						var fileNameArray = new Array();
						var dataB64Array = new Array();
						
						for (i = 0; i < fileNamesDataBase64.length; i++) {
							var sepPos = fileNamesDataBase64[i].indexOf(":");
							
							if (sepPos == -1) {
								fileNameArray.push(fileNamesDataBase64[i]);
								dataB64Array.push("");
							}
							else {
								fileNameArray.push(fileNamesDataBase64[i].substring(0, sepPos));
								dataB64Array.push(fileNamesDataBase64[i].substring(sepPos + 1).replace(/\-/g, "+").replace(/\_/g, "/"));
							}
						}
										
					}
					
					successCallback(fileNameArray,dataB64Array);
					
					return;
				}
				
				// Interpretamos el resultado como un base 64 y el certificado y los datos cifrados
				var signature;
				var certificate = null;
				var sepPos = data.indexOf("|");
				
				if (sepPos == -1) {
					signature = Base64.decode(data, true).replace(/\-/g, "+").replace(/\_/g, "/");
				}
				else {
					certificate = data.substring(0, sepPos).replace(/\-/g, "+").replace(/\_/g, "/");
					signature = data.substring(sepPos + 1).replace(/\-/g, "+").replace(/\_/g, "/");
				}
				successCallback(signature, certificate);
			}
			
			function errorServiceResponseFunction(exception, message){
				if (errorCallback) {
					errorCallback(exception, message);
				}
			}
			
			function successBatchResponseFunction (data) {
				// No se ha obtenido respuesta o se notifica la cancelacion
				if (data == undefined || data == null || data == "CANCEL") {
					errorCallback("es.gob.afirma.core.AOCancelledOperationException", "Operacion cancelada por el usuario");
					return;
				}
				
				// Se ha producido un error
				if (data.length > 4 && data.substr(0, 4) == "SAF_") {
					errorCallback("java.lang.Exception", data);
					return;
				}

				// Se ha producido un error y no se ha identificado el tipo
				if (data == "NULL") {
					errorCallback("java.lang.Exception", "Error desconocido");
					return;
				}
				successCallback(data);
			}
			
			/**
			 * Inicia el proceso de guardado de una firma.
			 */
			function saveDataToFile (dataB64, title, filename, extension, description, successCallbackSave, errorCallbackSave) {
				successCallback = successCallbackSave;
				errorCallback = errorCallbackSave;
				saveByService(dataB64, title, filename, extension, description);
			}

			function saveByService ( dataB64, title, filename, extension, description) {
				
				if (dataB64 == undefined || dataB64 == "") {
					dataB64 = null;
				}

				if (dataB64 != null && !isValidUrl(dataB64)) {
					dataB64 = dataB64.replace(/\+/g, "-").replace(/\//g, "_");
				}

				var data = generateDataToSave(dataB64, title, filename, extension, description);
				
				execAppIntent(buildUrl(data));
			}

			/**
			* Genera el objeto de datos para la operacion de guardar
			*/
			function generateDataToSave(dataB64, title, filename, extension, description) {
				var data = new Object();
				data.op = generateDataKeyValue ("op", "save");
				data.title = generateDataKeyValue ("title", title);
				data.filename = generateDataKeyValue ("filename", filename);
				data.extension = generateDataKeyValue ("exts", extension);
				data.description = generateDataKeyValue ("desc", description);
				data.dat = generateDataKeyValue ("dat",  dataB64 == "" ? null : dataB64);
				
				return data;
			}

			function generateDataKeyValue(key, value) {
				var data  = new Object();
				data.key = key;
				data.value = value;
				return data;
			}
			
			/**
			 * Convierte texto plano en texto base 64.
			 * Implementada en el applet Java de firma.
			 */
			function getBase64FromText (plainText, charset) {
				return plainText != null ? Base64.encode(plainText) : null;
			}

			/**
			 * Convierte texto base 64 en texto plano.
			 * Implementada en el applet Java de firma.
			 */
			function getTextFromBase64 (base64Text, charset) {
				return base64Text != null ? Base64.decode(base64Text) : null;
			}

			
			/**
			 * Inicia el proceso de carga de un fichero.
			 * Implementada tambien en el applet Java de firma
			 * @param title Titulo de la ventana de dialogo
			 * @param extensions Extensiones permitidasg
			 * @param description Descripcion del tipo de archivo a cargar
			 * @param filePath Ruta del archivo por defecto
			 * @param successCallbackFunction Funcion de callback tras exito
			 * @param errorCallbackFunction Funcion de callback tras error
			 */
			function getFileNameContentBase64 (title, extensions, description, filePath, successCallbackFunction, errorCallbackFunction) {
				successCallback = successCallbackFunction;
				errorCallback = errorCallbackFunction;
				getLoadContentBase64ByService("load", title, extensions, description, filePath, false);
			}

			/**
			 * Inicia el proceso de carga de uno o varios ficheros.
			 * Implementada tambien en el applet Java de firma
			 * @param title Titulo de la ventana de dialogo
			 * @param extensions Extensiones permitidas
			 * @param description Descripcion del tipo de archivo a cargar
			 * @param filePath Ruta del archivo por defecto
			 * @param successCallbackFunction Funcion de callback tras exito
			 * @param errorCallbackFunction Funcion de callback tras error
			 */
			function getMultiFileNameContentBase64 (title, extensions, description, filePath, successCallbackFunction, errorCallbackFunction) {
				successCallback = successCallbackFunction;
				errorCallback = errorCallbackFunction;
				getLoadContentBase64ByService("load", title, extensions, description, filePath, true);
			}
			
			/**
			 * Inicia el proceso de obtencion del log actual de la aplicacion.
			 * Implementada tambien en el applet Java de firma
			 * @param successCallbackFunction Funcion de callback tras exito
			 * @param errorCallbackFunction Funcion de callback tras error
			 */
			function getCurrentLog (successCallbackFunction, errorCallbackFunction) {
				successCallback = successCallbackFunction;
				errorCallback = errorCallbackFunction;
				getCurrentLogByService("getLog");
			}
			
			/**
			 * Realiza una operacion de carga de fichero comunicandose con la
			 * aplicacion nativa por socket.
			 * @param loadId Identificador de la operacion a realizar (load).
			 * @param title Titulo de la ventana de dialogo
			 * @param extensions Extensiones permitidas
			 * @param description Descripcion del tipo de archivo a cargar
			 * @param filePath Ruta del archivo por defecto
			 * @param multiload true si permite la seleccion de varios ficheros,
			 * false si solo se permite seleccionar un fichero.
			 */
			function getLoadContentBase64ByService (loadId, title, extensions, description, filePath, multiload) {
				
				var data = generateDataToLoad(loadId, title, extensions, description, filePath, multiload);
				
				execAppIntent(buildUrl(data));
			}
			
			/**
			 * Realiza una operacion de obtencion de log actual de la aplicacion
			 */
			function getCurrentLogByService() {
				
				var data = generateDataToLoad("getLog");
				
				execAppIntent(buildUrl(data));
			} 

			/** 
			 * Funcion para la comprobacion de existencia del objeto. No hace nada.
			 * Implementada en el applet Java de firma.
			 */
			function echo () {
				return "Cliente JavaScript";
			}
			
			/**
			 * Recupera el mensaje de error asociado al ultimo error capturado.
			 * Implementada en el applet Java de firma.
			 */
			function getErrorMessage () {
				return errorMessage;
			}

			/**
			 * Recupera el tipo del ultimo error capturado.
			 * Implementada en el applet Java de firma.
			 */
			function getErrorType () {
				return errorType;
			}

			/**
			 * Funcion para identificar el tipo de objeto del Cliente (javascript, applet,...).
			 */
			function getType () {
				return "javascript";
			}

			/**
			 * Establece el error indicado como error interno y lanza una excepcion.
			 */
			function throwException (type, message) {
				errorType = type;
				errorMessage = message;
				throw new Error();
			}

			/* Metodos que publicamos del objeto AppAfirmaJS */
			return {
				echo : echo,
				checkTime : checkTime,
				setKeyStore : setKeyStore,
				sign : sign,
				coSign : coSign,
				counterSign : counterSign,
				signBatch : signBatch,
				selectCertificate : selectCertificate,
				saveDataToFile : saveDataToFile,
				signAndSaveToFile : signAndSaveToFile,
				getFileNameContentBase64: getFileNameContentBase64,
				getMultiFileNameContentBase64 : getMultiFileNameContentBase64,
				getBase64FromText : getBase64FromText,
				getTextFromBase64 : getTextFromBase64,
				setStickySignatory : setStickySignatory,
				setLocale : setLocale,
				getErrorMessage : getErrorMessage,
				getErrorType : getErrorType,
				getCurrentLog : getCurrentLog				
			}
		});


		/**
		 * Cliente para la conexi&oacute;n con el Cliente @firma a trav&eacute;s de un servidor intermedio.
		 */
		var AppAfirmaJSWebService = ( function (clientAddress, window, undefined) {

			/* Longitud maxima que generalmente se permite a una URL. */
			var MAX_LONG_GENERAL_URL = 2000;
			
			/** Version del protocolo utilizada. */
			var PROTOCOL_VERSION = 3;
			
			/** Tiempo de espera entre intentos de obtener el resultado de la operacion. */
			var WAITING_CYCLE_MILLIS = Platform.isAndroid() || Platform.isIOS() ? 4000 : 3000;
			
			/** Numero de intentos maximo para obtener el resultado de la operacion sin respuesta
			 * de la aplicacion. */
			var NUM_MAX_ITERATIONS = Platform.isAndroid() || Platform.isIOS() ? 15 : 10;
			
			/** Indica si ya se ha detectado que la aplicacion no esta instalada  */
			var wrongInstallation = true;
			
			var OPERATION_BATCH = "batch";
						
			var OPERATION_SELECT_CERTIFICATE = "certificate";

			var OPERATION_SIGN = "sign";
			
			var currentOperation = OPERATION_SIGN;
			
			/** Certificado en base 64 que se deve usar por defecto cuando la opcion stickySignatory
			 * este activada. */
			var stickyCertificate;

			var errorMessage = '';
			var errorType = '';
			var defaultKeyStore = null;
			var retrieverServletAddress = null;
			var storageServletAddress = null;

			if (clientAddress != undefined && clientAddress != null) {
				if (clientAddress.indexOf("://") != -1 && clientAddress.indexOf("/", clientAddress.indexOf("://") + 3) != -1) {
					var servletsBase = clientAddress.substring(0, clientAddress.indexOf("/", clientAddress.indexOf("://") + 3));
					retrieverServletAddress = servletsBase + "/afirma-signature-retriever/RetrieveService";
					storageServletAddress = servletsBase + "/afirma-signature-storage/StorageService";
				} else {
					retrieverServletAddress = clientAddress + "/afirma-signature-retriever/RetrieveService";
					storageServletAddress = clientAddress + "/afirma-signature-storage/StorageService";
				}
			}

			/**
			 * Establece el almacen de certificados de que se debe utilizar por defecto.
			 */
			function setKeyStore (keystore) {
				defaultKeyStore = keystore;
			}

			/**
			 * Inicia el proceso de seleccion de certificado.
			 */
			function selectCertificate (extraParams, successCallback, errorCallback) {
			
				currentOperation = OPERATION_SELECT_CERTIFICATE;
				
				var idSession = AfirmaUtils.generateNewIdSession();
				var cipherKey = generateCipherKey();

				var opId = "selectcert";
				
				// Si hay un certificado prefijado, lo agregamos a los parametros extra
				if (stickySignatory && !resetStickySignatory && !!stickyCertificate) {
					extraParams = addSignatoryCertificateToExtraParams(stickyCertificate, extraParams);
				}
				
				var i = 0;
				var params = new Array();
				params[i++] = {key:"ver", value:PROTOCOL_VERSION};
				params[i++] = {key:"op", value:opId};
				
				if (idSession != null && idSession != undefined) {		params[i++] = {key:"id", value:idSession}; }
				if (cipherKey != null && cipherKey != undefined) {		params[i++] = {key:"key", value:cipherKey}; }
				if (defaultKeyStore != null &&
						defaultKeyStore != undefined) {					params[i++] = {key:"keystore", value:defaultKeyStore};
																		params[i++] = {key:"ksb64", value:Base64.encode(defaultKeyStore)}; }
				if (storageServletAddress != null &&
						storageServletAddress != undefined) {			params[i++] = {key:"stservlet", value:storageServletAddress}; }
				if (extraParams != null && extraParams != undefined) {	params[i++] = {key:"properties", value:Base64.encode(extraParams)}; }
				if (!Platform.isAndroid() && !Platform.isIOS()) {		params[i++] = {key:"aw", value:"true"}; } // Espera activa
			
				var url = buildUrl(opId, params);

				// Si la URL es muy larga, realizamos un preproceso para que los datos se suban al
				// servidor y la aplicacion nativa los descargue, en lugar de pasarlos directamente 
				if (isURLTooLong(url)) {
					if (storageServletAddress == null || storageServletAddress == undefined) {
						throwException("java.lang.IllegalArgumentException", "No se ha indicado la direccion del servlet para el guardado de datos");
						return;
					}
					else if (retrieverServletAddress == null || retrieverServletAddress == undefined) {
						throwException("java.lang.IllegalArgumentException", "No se ha indicado la direccion del servlet para la recuperacion de datos");
						return;
					}
					sendDataAndExecAppIntent(idSession, cipherKey, storageServletAddress, retrieverServletAddress, opId, params, successCallback, errorCallback)
				}
				else {
					execAppIntent(url, idSession, cipherKey, successCallback, errorCallback);
				}
			}
			
			/**
			 * Inicia el proceso de firma electronica.
			 */
			function sign (dataB64, algorithm, format, extraParams, successCallback, errorCallback) {
				signOperation("sign", dataB64, algorithm, format, extraParams, successCallback, errorCallback);
			}

			/**
			 * Inicia el proceso de cofirma de una firma electr&oacute;nica. 
			 */
			function coSign (signB64, algorithm, format, extraParams, successCallback, errorCallback) {
				signOperation("cosign", signB64, algorithm, format, extraParams, successCallback, errorCallback);
			}
			
			/**
			 * Inicia el proceso de contrafirma de una firma electr&oacute;nica.
			 */
			function counterSign (signB64, algorithm, format, extraParams, successCallback, errorCallback) {
				signOperation("countersign", signB64, algorithm, format, extraParams, successCallback, errorCallback);
			}
			
			/**
			 * Realiza una operacion de firma/multifirma.
			 * @param signId Identificador de la operacion a realizar (sign, cosign y countersign).
			 * @param dataB64 Datos o firma en base 64.
			 * @param algorithm Algoritmo de firma.
			 * @param format Formato de firma.
			 * @param extraParams Par&aacute;metros para la configuraci&oacute;n de la operaci&oacute;n.
			 * @param successCallback M&eacute;todo a ejecutar en caso de &eacute;xito.
			 * @param errorCallback M&eacute;todo a ejecutar en caso de error.
			 */
			function signOperation (signId, dataB64, algorithm, format, extraParams, successCallback, errorCallback) {

				currentOperation = OPERATION_SIGN;
				
				if (dataB64 == undefined || dataB64 == "") {
					dataB64 = null;
				}

				if (dataB64 != null && !isValidUrl(dataB64)) {
					dataB64 = dataB64.replace(/\+/g, "-").replace(/\//g, "_");
					dataB64 = dataB64.replace(/\n/g, "").replace(/\r/g, ""); //eliminamos saltos de carro para que no generen espacios 0x20 al parsear los atributos del XML enviado/recibido (storageServletAddress y retrieverServletAddress) que impiden la firma en AutoFirma
				}

				// Si hay un certificado prefijado, lo agregamos a los parametros extra
				if (stickySignatory && !resetStickySignatory && !!stickyCertificate) {
					extraParams = addSignatoryCertificateToExtraParams(stickyCertificate, extraParams);
				}
				
				var idSession = generateNewIdSession();
				var cipherKey = generateCipherKey();

				var i = 0;
				var params = new Array();
				params[i++] = {key:"ver", value:PROTOCOL_VERSION};
				if (signId != null && signId != undefined) {			params[i++] = {key:"op", value:signId}; }
				if (idSession != null && idSession != undefined) {		params[i++] = {key:"id", value:idSession}; }
				if (cipherKey != null && cipherKey != undefined) {		params[i++] = {key:"key", value:cipherKey}; }
				if (defaultKeyStore != null &&
						defaultKeyStore != undefined) {					params[i++] = {key:"keystore", value:defaultKeyStore};
																		params[i++] = {key:"ksb64", value:Base64.encode(defaultKeyStore)}; }
				if (storageServletAddress != null &&
						storageServletAddress != undefined) {			params[i++] = {key:"stservlet", value:storageServletAddress}; }
				if (format != null && format != undefined) {			params[i++] = {key:"format", value:format}; }
				if (algorithm != null && algorithm != undefined) {		params[i++] = {key:"algorithm", value:algorithm}; }
				if (extraParams != null && extraParams != undefined) { 	params[i++] = {key:"properties", value:Base64.encode(extraParams)}; }
				if (!Platform.isAndroid() && !Platform.isIOS()) {		params[i++] = {key:"aw", value:"true"}; } // Espera activa
				if (dataB64 != null) {									params[i++] = {key:"dat", value:dataB64}; }
			
				var url = buildUrl(signId, params);

				// Si la URL es muy larga, realizamos un preproceso para que los datos se suban al
				// servidor y la aplicacion nativa los descargue, en lugar de pasarlos directamente 
				if (isURLTooLong(url)) {
					if (storageServletAddress == null || storageServletAddress == undefined) {
						throwException("java.lang.IllegalArgumentException", "No se ha indicado la direccion del servlet para el guardado de datos");
						return;
					}

					sendDataAndExecAppIntent(idSession, cipherKey, storageServletAddress, retrieverServletAddress, signId, params, successCallback, errorCallback)
				}
				else {
					execAppIntent(url, idSession, cipherKey, successCallback, errorCallback);
				}
			}

			/**
			 * Realiza una operacion de firma/multifirma y permite guardar el fichero a disco.
			 * @param signId Identificador de la operacion a realizar (sign, cosign y countersign).
			 * @param dataB64 Datos o firma en base 64.
			 * @param algorithm Algoritmo de firma.
			 * @param format Formato de firma.
			 * @param extraParams Par&aacute;metros para la configuraci&oacute;n de la operaci&oacute;n.
			 * @param outputFileName Nombre propuesto para el fichero a guardar.
			 * @param successCallback M&eacute;todo a ejecutar en caso de &eacute;xito.
			 * @param errorCallback M&eacute;todo a ejecutar en caso de error.
			 */
			function signAndSaveToFile (signId, dataB64, algorithm, format, extraParams, outputFileName, successCallback, errorCallback) {

				currentOperation = OPERATION_SIGN;
				
				if (dataB64 == undefined || dataB64 == "") {
					dataB64 = null;
				}

				if (dataB64 != null && !isValidUrl(dataB64)) {
					dataB64 = dataB64.replace(/\+/g, "-").replace(/\//g, "_");
					dataB64 = dataB64.replace(/\n/g, "").replace(/\r/g, ""); //eliminamos saltos de carro para que no generen espacios 0x20 al parsear los atributos del XML enviado/recibido (storageServletAddress y retrieverServletAddress) que impiden la firma en AutoFirma
				}

				// Si hay un certificado prefijado, lo agregamos a los parametros extra
				if (stickySignatory && !resetStickySignatory && !!stickyCertificate) {
					extraParams = addSignatoryCertificateToExtraParams(stickyCertificate, extraParams);
				}
				
				var idSession = generateNewIdSession();
				var cipherKey = generateCipherKey();

				var i = 0;
				var opId = "signandsave";
				var params = new Array();
				
				params[i++] = {key:"ver", value:PROTOCOL_VERSION};
				params[i++] = {key:"op", value:opId};
				if (signId != null && signId != undefined) {			params[i++] = {key:"cop", value:signId}; }
				if (idSession != null && idSession != undefined) {		params[i++] = {key:"id", value:idSession}; }
				if (cipherKey != null && cipherKey != undefined) {		params[i++] = {key:"key", value:cipherKey}; }
				if (defaultKeyStore != null &&
						defaultKeyStore != undefined) {					params[i++] = {key:"keystore", value:defaultKeyStore};
																		params[i++] = {key:"ksb64", value:Base64.encode(defaultKeyStore)}; }
				if (storageServletAddress != null &&
						storageServletAddress != undefined) {			params[i++] = {key:"stservlet", value:storageServletAddress}; }
				if (format != null && format != undefined) {			params[i++] = {key:"format", value:format}; }
				if (algorithm != null && algorithm != undefined) {		params[i++] = {key:"algorithm", value:algorithm}; }
				if (extraParams != null && extraParams != undefined) { 	params[i++] = {key:"properties", value:Base64.encode(extraParams)}; }
				if (outputFileName != null &&
						outputFileName != undefined) {					params[i++] = {key:"filename", value:outputFileName}; }
				if (!Platform.isAndroid() && !Platform.isIOS()) {		params[i++] = {key:"aw", value:"true"}; } // Espera activa
				if (dataB64 != null) {									params[i++] = {key:"dat", value:dataB64}; }
			
				var url = buildUrl(opId, params);

				// Si la URL es muy larga, realizamos un preproceso para que los datos se suban al
				// servidor y la aplicacion nativa los descargue, en lugar de pasarlos directamente 
				if (isURLTooLong(url)) {
					if (storageServletAddress == null || storageServletAddress == undefined) {
						throwException("java.lang.IllegalArgumentException", "No se ha indicado la direccion del servlet para el guardado de datos");
						return;
					}

					sendDataAndExecAppIntent(idSession, cipherKey, storageServletAddress, retrieverServletAddress, opId, params, successCallback, errorCallback)
				}
				else {
					execAppIntent(url, idSession, cipherKey, successCallback, errorCallback);
				}
			}
			
			/**
			 * Ejecuta una operacion de firma de lote.
			 */
			function signBatch (batchB64, batchPreSignerUrl, batchPostSignerUrl, extraParams, successCallback, errorCallback) {
				
				currentOperation = OPERATION_BATCH;
				
				if (batchB64 == undefined || batchB64 == "") {
					batchB64 = null;
				}

				if (batchB64 != null && !isValidUrl(batchB64)) {
					batchB64 = batchB64.replace(/\+/g, "-").replace(/\//g, "_");
					batchB64 = batchB64.replace(/\n/g, "").replace(/\r/g, ""); //eliminamos saltos de carro para que no generen espacios 0x20 al parsear los atributos del XML enviado/recibido (storageServletAddress y retrieverServletAddress) que impiden la firma en AutoFirma
				}

				// Si hay un certificado prefijado, lo agregamos a los parametros extra
				if (stickySignatory && !resetStickySignatory && !!stickyCertificate) {
					extraParams = addSignatoryCertificateToExtraParams(stickyCertificate, extraParams);
				}
				
				var idSession = generateNewIdSession();
				var cipherKey = generateCipherKey();
				
				var opId = "batch";
				
				var i = 0;
				var params = new Array();
				params[i++] = {key:"ver", value:PROTOCOL_VERSION};
				params[i++] = {key:"op", value:opId};
				if (idSession != null && idSession != undefined) {		params[i++] = {key:"id", value:idSession}; }
				if (cipherKey != null && cipherKey != undefined) {		params[i++] = {key:"key", value:cipherKey}; }
				if (defaultKeyStore != null &&
						defaultKeyStore != undefined) {					params[i++] = {key:"keystore", value:defaultKeyStore};
																		params[i++] = {key:"ksb64", value:Base64.encode(defaultKeyStore)}; }
				if (storageServletAddress != null &&
						storageServletAddress != undefined) {			params[i++] = {key:"stservlet", value:storageServletAddress}; }
				if (batchPreSignerUrl != null &&
						batchPreSignerUrl != undefined) {				params[i++] = {key:"batchpresignerurl", value:batchPreSignerUrl}; }				
				if (batchPostSignerUrl != null &&
						batchPostSignerUrl != undefined) {				params[i++] = {key:"batchpostsignerurl", value:batchPostSignerUrl}; }
				if (extraParams != null && extraParams != undefined) { 	params[i++] = {key:"properties", value:Base64.encode(extraParams)}; }
				if (!Platform.isAndroid() && !Platform.isIOS()) {		params[i++] = {key:"aw", value:"true"}; } // Espera activa
				if (!Platform.isAndroid() && !Platform.isIOS()) {		params[i++] = {key:"needcert", value:"true"}; } // Espera activa
				if (batchB64 != null) {									params[i++] = {key:"dat", value:batchB64}; }

				var url = buildUrl(opId, params);

				// Si la URL es muy larga, realizamos un preproceso para que los datos se suban al
				// servidor y la aplicacion nativa los descargue, en lugar de pasarlos directamente 
				if (isURLTooLong(url)) {
					if (storageServletAddress == null || storageServletAddress == undefined) {
						throwException("java.lang.IllegalArgumentException", "No se ha indicado la direccion del servlet para el guardado de datos");
						return;
					}

					sendDataAndExecAppIntent(idSession, cipherKey, storageServletAddress, retrieverServletAddress, opId, params, successCallback, errorCallback)
				}
				else {
					execAppIntent(url, idSession, cipherKey, successCallback, errorCallback);
				}
			}
			
			/**
			 * Guardado de datos en disco. Se realiza mediante la invocacion de una app nativa. 
			 */
			function saveDataToFile (dataB64, title, filename, extension, description, successCallback, errorCallback) {

				if (dataB64 != undefined && dataB64 != null && dataB64 != "") {
					dataB64 = dataB64.replace(/\+/g, "-").replace(/\//g, "_");
				}

				var idSession = generateNewIdSession();
				var cipherKey = generateCipherKey();

				var i = 0;
				var opId = "save";
				var params = new Array();
				params[i++] = {key:"ver", value:PROTOCOL_VERSION};
				params[i++] = {key:"op", value:opId};
				if (idSession != null && idSession != undefined) {		params[i++] = {key:"id", value:idSession}; }
				if (cipherKey != null && cipherKey != undefined) {		params[i++] = {key:"key", value:cipherKey}; }
				if (storageServletAddress != null &&
						storageServletAddress != undefined) {			params[i++] = {key:"stservlet", value:storageServletAddress}; }
				if (title != null && title != undefined) {				params[i++] = {key:"title", value:title}; }
				if (filename != null && filename != undefined) {		params[i++] = {key:"filename", value:filename}; }
				if (extension != null && extension != undefined) {		params[i++] = {key:"extension", value:extension}; }
				if (description != null && description != undefined) {	params[i++] = {key:"description", value:description}; }
				if (!Platform.isAndroid() && !Platform.isIOS()) {		params[i++] = {key:"aw", value:"true"}; } // Espera activa
				if (dataB64 != null && dataB64 != undefined && dataB64 != "") {			params[i++] = {key:"dat", value:dataB64}; }
				
				
				var url = buildUrl(opId, params);

				// Si la URL es muy larga, realizamos un preproceso para que los datos se suban al
				// servidor y la aplicacion nativa los descargue, en lugar de pasarlos directamente 
				if (isURLTooLong(url)) {
					if (storageServletAddress == null || storageServletAddress == undefined) {
						throwException("java.lang.IllegalArgumentException", "No se ha indicado la direccion del servlet para el guardado de datos");
						return;
					}

					sendDataAndExecAppIntent(idSession, cipherKey, storageServletAddress, retrieverServletAddress, opId, params, successCallback, errorCallback)
				}
				else {
					execAppIntent(url, idSession, cipherKey, successCallback, errorCallback);
				}
			}

			/**
			 * Carga de fichero de datos. Se realiza mediante la invocacion de una app nativa. 
			 */
			function getFileNameContentBase64 (title, extensions, description, filePath, successCallback, errorCallback) {
				var errorType = "java.lang.UnsupportedOperationException";
				var errorMessage = "La operacion de carga de fichero no esta disponible por servidor intermedio";
				if (!errorCallback) {
					throwException(errorType, errorMessage);
				}
				else {
					errorCallback(errorType, errorMessage);
				}
			}
			
			/**
			 * Carga de multiples ficheros de datos. Se realiza mediante la invocacion de una app nativa. 
			 */
			function getMultiFileNameContentBase64  (title, extensions, description, filePath, successCallback, errorCallback) {
				var errorType = "java.lang.UnsupportedOperationException";
				var errorMessage = "La operacion de carga de multiples ficheros no esta disponible por servidor intermedio";
				if (!errorCallback) {
					throwException(errorType, errorMessage);
				}
				else {
					errorCallback(errorType, errorMessage);
				}
			}
						
			/** 
			 * Funcion para la comprobacion de existencia del objeto. No hace nada.
			 */
			function echo () {
				return "Cliente JavaScript";
			}

			/**
			 * Recupera el mensaje de error asociado al ultimo error capturado.
			 */
			function getErrorMessage () {
				return errorMessage;
			}

			/**
			 * Recupera el tipo del ultimo error capturado.
			 */
			function getErrorType () {
				return errorType;
			}

			/**
			 * Recupera la cadena "Applet no cargado".
			 */
			function getCurrentLog () {
				return "Applet no cargado";
			}

			/**
			 * Funcion para identificar el tipo de objeto del Cliente (javascript, applet,...).
			 */
			function getType () {
				return "javascript";
			}

			/**
			 * Establece las rutas de los servlets encargados de almacenar y recuperar las firmas de los dispositivos moviles.
			 */
			function setServlets (storageServlet,  retrieverServlet) {
				storageServletAddress = storageServlet;
				retrieverServletAddress = retrieverServlet;
			}

			/**
			 * Establece el error indicado como error interno y lanza una excepcion.
			 */
			function throwException (type, message) {
				errorType = type;
				errorMessage = message;
				throw new Error();
			}


			/**
			 * Comprueba si una URL es demasiado larga para ser usada en una llamada
			 * GET en un Sistema/Navegador concreto.
			 */
			function isURLTooLong(url) {
				return url.length > MAX_LONG_GENERAL_URL;
			}
			
			/* Mayor entero. */
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
			 * Envia los datos al servidor intermedio y luego invoca a la
			 * aplicacion nativa para que los descargue y opere con ellos.
			 * @param idSession Identificador de la operacion con el que se espera recuperar el resultado.
			 * @param cipherKey Clave de cifrado. Si no se indica, no se cifra.
			 * @param storageServletAddress URL del servlet que almacena.
			 * @param retrieverServletAddress URL del servlet que recupera.
			 * @param op Identificador del tipo de operacion (firma, cofirma, guardado,...).
			 * @param params Parametros de configuracion de la operacion.
			 * @param successCallback Funcion callback que debe ejecutarse en caso de exito.
			 * @param errorCallback Funcion callback que debe ejecutarse en caso de error.
			 */
			function sendDataAndExecAppIntent(idSession, cipherKey, storageServletAddress, retrieverServletAddress, op, params, successCallback, errorCallback) {

				// Identificador del fichero (equivalente a un id de sesion) del que deben recuperarse los datos
				var fileId = generateNewIdSession(); 

				// Subimos los datos al servidor intermedio
				var httpRequest = getHttpRequest();
				if (!httpRequest) {
					throwException("java.lang.Exception", "Su navegador no permite preprocesar los datos que desea tratar");
				}
				httpRequest.open("POST", storageServletAddress, true);
				httpRequest.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

				httpRequest.onreadystatechange = function () {
					if (httpRequest.readyState == 4) {
						 if (httpRequest.status == 200) {
	
							url = buildUrlWithoutData(op, fileId, retrieverServletAddress, cipherKey);
							if (isURLTooLong(url)) {
								errorCallback("java.lang.IllegalArgumentException", "La URL de invocacion al servicio de firma es demasiado larga.");
								return;
							}
							execAppIntent(url, idSession, cipherKey, successCallback, errorCallback);
						}
						else {
							console.log("Error al enviar los datos al servidor intermedio. HTTP Status: " + httpRequest.status);
							errorCallback("java.lang.IOException", "Ocurrio un error al enviar los datos a la aplicacion nativa");
						}
					}
				}
				try {
					httpRequest.onerror = function(e) {
						console.log("Error al enviar los datos al servidor intermedio (HTTP Status: " + httpRequest.status + "): " + e.message);
						errorCallback("java.lang.IOException", "Ocurrio un error al enviar los datos al servicio intermedio para la comunicacion con la aplicacion nativa");
					}
				}
				catch (e) {
					// Vacio
				}

				var requestData =
					"op=put&v=1_0&id=" + fileId + "&dat=" + 
					cipher(buildXML(op, params), cipherKey);

				try {
					httpRequest.send(requestData);
				}
				catch(e) {
					errorCallback("java.lang.IOException", "No se pudo conectar con el servidor remoto");
				}
			}
			
			/**
			 * Invoca un Intent con la operacion seleccionada, la configuraci\u00F3n establecida y las campos del
			 * formulario pasados como parametros. Si se define un callback para tratar el caso de exito o error de
			 * la operacion, se intentara descargar el resultado devuelto por la app del servidor intermedio de
			 * comunicacion. 
			 *
			 * @param intentURL URL para la invocacion del Cliente JavaScript
			 * @param idSession Identificador de la sesi\u00F3n para la recuperaci\u00F3n del resultado.
			 * @param cipherKey Clave de cifrado para la respuesta del servidor.
			 * @param successCallback Actuaci\u00F3n a realizar cuando se recupera el resultado de la operaci&oacute;n.
			 * @param errorCallback Actuaci\u00F3n a realizar cuando ocurre un error al recuperar el resultado.
			 */
			function execAppIntent (intentURL, idSession, cipherKey, successCallback, errorCallback) {

				wrongInstallation = false;
			
				// Invocamos al cliente de firma
				openUrl(intentURL);

					// Preguntamos repetidamente por el resultado
				if (!!idSession && (!!successCallback || !!errorCallback)) {
					getStoredFileFromServlet(idSession, retrieverServletAddress, cipherKey, intentURL, successCallback, errorCallback);
				}
			}

			/**
			 * Construye una URL para la invocacion del Cliente @firma nativo.
			 * @param op Funcion a invocar en el cliente nativo.
			 * @param params Parametros para la configuracion de la operacion.
			 */
			function buildUrl (op, params) {

				var urlParams = "";
				if (params != null && params != undefined) {
					for (var i = 0; i < params.length; i++) {
						if (params[i].value != null && params[i].value != "null") {
							urlParams += (i != 0 ? '&' : '') + params[i].key + '=' + encodeURIComponent(params[i].value); 
						}
					}
				}

				// En el caso de Chrome en Android, construimos la URL en forma de Intent
				var url;
				if (Platform.isChrome() && Platform.isAndroid()) {
					url = 'intent://' + op + '?' + urlParams + "#Intent;scheme=afirma;package=es.gob.afirma;end";
				}
				else {
					url = 'afirma://' + op + '?' + urlParams;
				}
				return url;
			}

			/**
			 * Construye un XML con lo valores del array de parametros proporcionado.
			 * @param op Operacion que configuramos
			 * @param params Array con los parametros del array. 
			 * @returns XML.
			 */
			function buildXML (op, params) {
				op = (op == null ? "op" : op);
				var xml = '<' + op +'>';
				if (params != null) {
					for (var i = 0; i < params.length; i++) {
						xml += '<e k="' + params[i].key + '" v="' + params[i].value + '"/>';
					}
				}
				return Base64.encode(xml + '</' + op + '>');
			}

			/**
			 * Crea una URL a partir de los parametros introducidos para la invocaci&oacute;n de
			 * una app nativa para que descargue la configuracion de la operaci&oacute;n a realizar.
			 * @param op Codigo de la operacion a la que tiene que invocar la URL.
			 * @param id Identificador para la descarga.
			 * @param rtServlet Servlet para la descarga de la configuraci&oacute;n.
			 * @param cipherKey Clave para el descifrado.
			 * @returns URL para la llamada a la app con los datos necesarios para que descargue
			 * la configuraci&oacute;n de la operaci&oacute;n a realizar.
			 */
			function buildUrlWithoutData (op, id, rtServlet, cipherKey) {
				var j = 0;
				var newParams = new Array();
				newParams[j++] = {key:"fileid", value:id};
				if (rtServlet != null || rtServlet != undefined) {
					newParams[j++] = {key:"rtservlet", value:rtServlet};
				}
				if (cipherKey != null || cipherKey != undefined) {
					newParams[j++] = {key:"key", value:cipherKey};
				}
				return buildUrl(op, newParams);
			};

			/**
			 * Ejecuta el metodo de error si el html recuperado es tal o el metodo de exito si no lo es,
			 * en cuyo caso previamente descifrara el resultado. Si el valor recuperado no es ni el
			 * de exito ni el de error, indicara si se debe seguir esperando por un resultado. En caso
			 * de indicar false, no se esperara mas; en caso de true, se seguira con la espera; y si se
			 * de vuelve "reset" se debera reiniciar la espera.
			 * @param html Resultado obtenido.
			 * @param cipherKey Clave para el descifrado del resultado si no es un error.
			 * @param successCallback Metodo a ejecutar en caso de exito.
			 * @param errorCallback Metodo a ejecutar en caso de error.
			 * @returns Devuelve true si se ha fallado pero se puede volver a reintentar, false en caso de
			 * error determinante o exito.
			 */
			function successResponseFunction (html, cipherKey, successCallback, errorCallback) {
				
				// Si se obtiene el mensaje de  error de que el identificador no existe, seguimos intentandolo
				if (html.substr(0, 6).toLowerCase() == "err-06") {
					return true;
				}

				// Si se obtiene el mensaje de espera, es que el cliente se ha levantado y ha iniciado el proceso.
				// Siempre que obtengamos este resultado, deberemos reiniciar el tiempo de espera, ya que la aplicacion
				// sigue activa y nos pide mas tiempo
				if (html.substr(0, 5).toLowerCase() == "#wait") {
					return "reset";
				}
				
				// Si se obtiene otro mensaje de error, se deja de intentar y se ejecuta la funcion callback de error
				if (html.substr(0, 4).toLowerCase() == "err-" && html.indexOf(":=") != -1) {
					errorMessage = html.substring(html.indexOf(":=") + 2);
					if (html.substr(0, 7).toLowerCase() == "err-11:") { // Tipo de error asociado a la cancelacion de la operacion
						errorType = "es.gob.afirma.core.AOCancelledOperationException";
					} else {
						errorType = "java.lang.Exception";
					}
					errorCallback(errorType, errorMessage);
					return false;
				}
				
				// Se ha cancelado la operacion
				if (html == "CANCEL" || html == "CANCEL\r\n") {
					errorCallback("es.gob.afirma.core.AOCancelledOperationException", "Operacion cancelada por el usuario");
					return false;
				}
				
				// La operacion ha finalizado correctamene (Funcion de guardado)
				if (html == "OK" || html == "OK\r\n") {
					successCallback();
					return false;
				}
				
				// Se ha producido un error
				if (html.length > 4 && html.substr(0, 4) == "SAF_") {
					errorCallback("java.lang.Exception", html);
					return false;
				}
				
				// Si no se obtuvo un error ni hemos recibido ninguno de los resultados anteriores,
				// procesamos el resultado según el tipo de operacion:
				//  - Si es una firma; se recibira la firma, el certificado + la firma, o el certificado + firma + datos extra.
				// ´- Si es una seleccion de certificado; solo se recibira el certificado.
				//  - Si es una firma de lote; se recibira el resultado del lote, o el resultado del lote + certificado. 
				// Los distintos valores del resultado se separan con una tuberia ('|'). Si se
				// definio una clave de cifrado, consideramos que cada uno de los datos se han
				// cifrado de forma independiente
				
				// Procesamos el resultado de la firma de lote
				if (currentOperation == OPERATION_BATCH) {
					var result;
					var certificate = null;
					var sepPos = html.indexOf('|');

					// En caso de recibir un unico parametro, este sera la firma en el caso de las operaciones de firma y el
					// certificado cuando se pidio seleccionar uno 
					if (sepPos == -1) {
						if (cipherKey != undefined && cipherKey != null) {
							result = decipher(html, cipherKey);
						}
						else {
							result = fromBase64UrlSaveToBase64(html);
						}
					}
					else {
						if (cipherKey != undefined && cipherKey != null) {
							result = decipher(html.substring(0, sepPos), cipherKey, true);
							certificate = decipher(html.substring(sepPos + 1), cipherKey);
						}
						else {
							result = fromBase64UrlSaveToBase64(html.substring(0, sepPos));
							certificate = fromBase64UrlSaveToBase64(html.substring(sepPos + 1));
						}
						// Guardamos el certificado si corresponde
						if (!!stickySignatory) {
							if (!!certificate) {
								stickyCertificate = certificate;
							}
						}
						else {
							stickyCertificate = null;
						}
					}

					successCallback(result, certificate);
					return false;
				}
				// Procesamos el resultado de la seleccion de certificado
				else if (currentOperation == OPERATION_SELECT_CERTIFICATE) {
					var certificate;
					if (cipherKey != undefined && cipherKey != null) {
						certificate = decipher(html, cipherKey);
					}
					else {
						certificate = fromBase64UrlSaveToBase64(html);
					}

					// Guardamos el certificado
					stickyCertificate = !!stickySignatory ? certificate : null;
 					
					successCallback(certificate);
					return false;
				}
				
				// Procesamos suponiendo que la operacion se corresponde con una firma  
				var signature;
				var certificate = null;
				var extraInfo = null;
				var sepPos = html.indexOf('|');

				// En caso de recibir un unico parametro, este sera la firma en el caso de las operaciones de firma y el
				// certificado cuando se pidio seleccionar uno 
				if (sepPos == -1) {
					if (cipherKey != undefined && cipherKey != null) {
						signature = decipher(html, cipherKey);
					}
					else {
						signature = fromBase64UrlSaveToBase64(html);
					}
					
					// Guardamos el dato, por si es necesario para la seleccion
					// de certificado automatica
					if (!!stickySignatory) {
						if (!!signature) {
							stickyCertificate = signature;
						}
					}
					else {
						stickyCertificate = null;
					}
				}
				else {
					var sepPos2 = html.indexOf('|', sepPos + 1);
					if (sepPos2 == -1) {
						if (cipherKey != undefined && cipherKey != null) {
							certificate = decipher(html.substring(0, sepPos), cipherKey, true);
							signature = decipher(html.substring(sepPos + 1), cipherKey);
						}
						else {
							certificate = fromBase64UrlSaveToBase64(html.substring(0, sepPos));
							signature = fromBase64UrlSaveToBase64(html.substring(sepPos + 1));
						}
					}
					else {
						if (cipherKey != undefined && cipherKey != null) {
							certificate = decipher(html.substring(0, sepPos), cipherKey, true);
							signature = decipher(html.substring(sepPos + 1, sepPos2), cipherKey, true);
							extraInfo = Base64.decode(decipher(html.substring(sepPos2 + 1), cipherKey));
						}
						else {
							certificate = fromBase64UrlSaveToBase64(html.substring(0, sepPos));
							signature = fromBase64UrlSaveToBase64(html.substring(sepPos + 1));
							extraInfo = Base64.decode(fromBase64UrlSaveToBase64(html.substring(sepPos2 + 1)));
						}
					}
					
					if (!!stickySignatory) {
						if (!!certificate) {
							stickyCertificate = certificate;
						}
					}
					else {
						stickyCertificate = null;
					}
				}

				successCallback(signature, certificate, extraInfo);
				return false;
			}
			
			function errorResponseFunction (type, message, errorCallback) {

				errorType = (type != null && type.length > 0) ?
						type : "java.lang.Exception";
				errorMessage = (message != null && message.length > 0) ?
						message : "No se ha podido extablecer la comunicaci\u00F3n entre la aplicaci\u00F3n de firma y la p\u00E1gina web";
				if (errorCallback != undefined && errorCallback != null) {
					errorCallback(errorType, errorMessage);
				}
			}
			
			/** Agrega a los extraParams un filtro de seleccion de certificado concreto. */
			function addSignatoryCertificateToExtraParams(certificate, params) {

				// Obtenemos un listado con los parametros
				var paramsList = split(params, "\n");
				
				// Creamos un nuevo listado omitiendo los filtros
				var newParamsList = [];
				for (var i = 0; i < paramsList.length; i++) {
					if (!!paramsList[i] && !checkParamIsFilter(paramsList[i])) {
						newParamsList.push(paramsList[i]);
					}
				}
				
				// Agregamos al listado el nuevo filtro
				newParamsList.push("filters=encodedcert:" + certificate);
				newParamsList.push("headless=true");
				
				// Componemos la cadera con los parametros
				var newParams = "";
				if (newParamsList.length > 0) {
					newParams = newParamsList[0];
					for (var i = 1; i < newParamsList.length; i++) {
						newParams += "\n" + newParamsList[i];
					}
				}
				
				return newParams;
			}
			
			/** Comprueba si un parametro es un filtro. */
			function checkParamIsFilter(param) {
				return	param.indexOf("filters=") == 0 ||
						param.indexOf("filter=") == 0 ||
						param.indexOf("filters.") == 0 ||
						param.indexOf("headless=") == 0;
			}
			
			/** Compone un array con las subcadenas de un texto separadas por una cadena separadora. */
			function split(text, sep) {
				var textArray = [];
				if (!!text) {
					var idx1 = 0;
					var idx2;
					while ((idx2 = text.indexOf(sep, idx1)) != -1) {
						textArray.push(text.substring(idx1, idx2));
						idx1 = idx2 + 1;
					}
					textArray.push(text.substring(idx1));
				}
				
				return textArray;
			}
			
			var iterations = 0;

			function getStoredFileFromServlet (idDocument, servletAddress, cipherKey, intentURL, successCallback, errorCallback) {

				var httpRequest = getHttpRequest();
				if (!httpRequest) {
					throwException("java.lang.Exception", "Su navegador no permite obtener el resulado de la operaci\u00F3n");
				}

				iterations = 0;
				setTimeout(retrieveRequest, WAITING_CYCLE_MILLIS, httpRequest, servletAddress, "op=get&v=1_0&id=" + idDocument + "&it=0", cipherKey, intentURL, idDocument, successCallback, errorCallback);
			}

			function retrieveRequest(httpRequest, url, params, cipherKey, intentURL, idDocument, successCallback, errorCallback) {

				if (wrongInstallation) {
					errorResponseFunction("es.gob.afirma.standalone.ApplicationNotFoundException", "AutoFirma no se encuentra instalado en el sistema.", errorCallback);
					return;
				}
			
				// Contamos la nueva llamada al servidor
				if (iterations > NUM_MAX_ITERATIONS) {
					errorResponseFunction("java.util.concurrent.TimeoutException", "El tiempo para la recepcion de la firma por la pagina web ha expirado", errorCallback);
					return;
				}
				iterations++;

				httpRequest.onreadystatechange = function() {
					if (httpRequest.readyState == 4) {
						if (httpRequest.status == 200) {
							var needContinue = successResponseFunction(httpRequest.responseText, cipherKey, successCallback, errorCallback);
							if (needContinue) {
								// En caso de que la respuesta sea "reset", se reinicia la espera
								var oldIterations = iterations-1;
								if (needContinue == "reset") {
									iterations = 0;
								}
								setTimeout(retrieveRequest, WAITING_CYCLE_MILLIS, httpRequest, url, params.replace("&it=" + (oldIterations), "&it=" + iterations), cipherKey, intentURL, idDocument, successCallback, errorCallback);
							}
						}
						else {
							errorResponseFunction(null, httpRequest.responseText, errorCallback);
						}
					}					
				}
				try {
					httpRequest.onerror = function() {
						errorResponseFunction("java.lang.Exception", "No se pudo conectar con el servidor intermedio para la recuperacion del resultado de la operacion", errorCallback);
					}
				}
				catch (e) {
					// Vacio
				}

				httpRequest.open("POST", url, true);
				httpRequest.setRequestHeader("Content-type","application/x-www-form-urlencoded");
				
				try {
					httpRequest.send(params);
				}
				catch(e) {
					// Error en la llamada para al recuperacion del resultado. No lo encuentra o problema
					// de tipo cross-domain
					errorResponseFunction("java.lang.IOException", "Ocurrio un error de red en la llamada al servicio de firma", errorCallback);
					return;
				}
			}

			/**
			 * Realiza un descifrado DES compatible con Java (Algoritmo DES, modo CBC, sin Padding).
			 * Recibe en base 64 la cadena de texto cifrado antecedido por el padding anadido manualmente
			 * a los datos para permitir el cifrado DES (separado por un punto ('.')), ademas de la clave
			 * para descifrar y, opcionalmente, un booleano que indica si se trata de un cifrado intermedio
			 * devuelto por la aplicacion, lo que permite reajustar el padding.
			 * Como resultado devuelve la cadena de texto descifrada en base 64.
			 */
			function decipher(cipheredData, key, intermediate) {
								
				var dotPos = cipheredData.indexOf('.');
				var padding = cipheredData.substr(0, dotPos);
				
				var deciphered = Cipher.des(key, Cipher.base64ToString(fromBase64UrlSaveToBase64(cipheredData.substr(dotPos + 1))), 0, 0, null);
				return Cipher.stringToBase64(deciphered.substr(0, deciphered.length - parseInt(padding) - (intermediate ? 0 : 8)));
			}
			
			/**
			 * Realiza un cifrado DES compatible con Java (Algoritmo DES, modo CBC, sin Padding).
			 * @param dataB64 Cadena de texto base 64.
			 * @param key Clave de cifrado.
			 * @return Base 64 cifrado.
			 */
			function cipher(dataB64, key) {

				var data = Cipher.base64ToString(fromBase64UrlSaveToBase64(dataB64));
				var padding = (8 - (data.length % 8)) % 8;

				// Los datos cifrados los pasamos a base 64 y, antes de devolverlos le anteponemos el padding que
				// le habra agregado el metodo de cifrado separados por un punto ('.').
				return padding  + "." + Cipher.stringToBase64(Cipher.des(key, data, 1, 0, null)).replace(/\+/g, "-").replace(/\//g, "_");
			}

			/**
			 * Convierte de Base64 URL Save a Base64 normal.
			 */
			function fromBase64UrlSaveToBase64(base64UrlSave) {
				if (!base64UrlSave) {
					return base64UrlSave;
				}
				return base64UrlSave.replace(/\-/g, "+").replace(/\_/g, "/")
			}
			
			/* Metodos que publicamos del objeto AppAfirmaJSWebService */
			return {
				echo : echo,
				setKeyStore : setKeyStore,
				sign : sign,
				coSign : coSign,
				counterSign : counterSign,
				signBatch : signBatch,
				selectCertificate : selectCertificate,
				saveDataToFile : saveDataToFile,
				signAndSaveToFile : signAndSaveToFile,
				getFileNameContentBase64: getFileNameContentBase64,
				getMultiFileNameContentBase64 : getMultiFileNameContentBase64,
				setServlets : setServlets,
				setStickySignatory : setStickySignatory,
				setLocale : setLocale,
				getErrorMessage : getErrorMessage,
				getErrorType : getErrorType,
				getCurrentLog : getCurrentLog
			}
		});
		
		/* Metodos que publicamos del objeto cliente. */
		return {
			
			VERSION : VERSION,
			
			/* Publicamos las variables para la comprobacion de hora. */		
			CHECKTIME_NO : CHECKTIME_NO,
			CHECKTIME_RECOMMENDED : CHECKTIME_RECOMMENDED,
			CHECKTIME_OBLIGATORY : CHECKTIME_OBLIGATORY,
			
			/* Parametros y metodos deprecados */
			JAVA_ARGUMENTS : JAVA_ARGUMENTS,
			SYSTEM_PROPERTIES : SYSTEM_PROPERTIES,
			setForceAFirma : setForceAFirma,
			cargarMiniApplet : cargarAppAfirma,
			echo : echo,
			setJnlpService: setJnlpService,
			isJNLP : isJNLP,
			needNativeAppInstalled : needNativeAppInstalled,
			
			/* Variables para configurar un almacen de certificados concreto. */		
			KEYSTORE_WINDOWS : KEYSTORE_WINDOWS,
			KEYSTORE_APPLE : KEYSTORE_APPLE,
			KEYSTORE_PKCS12 : KEYSTORE_PKCS12,
			KEYSTORE_PKCS11 : KEYSTORE_PKCS11,
			KEYSTORE_MOZILLA : KEYSTORE_MOZILLA,
			KEYSTORE_SHARED_NSS : KEYSTORE_SHARED_NSS,
			KEYSTORE_JAVA : KEYSTORE_JAVA,
			KEYSTORE_JCEKS : KEYSTORE_JCEKS,
			KEYSTORE_JAVACE : KEYSTORE_JAVACE,
			KEYSTORE_DNIE : KEYSTORE_DNIE,

			/* Constantes para la configuracion de reintentos de conexion */
			AUTOFIRMA_LAUNCHING_TIME : AUTOFIRMA_LAUNCHING_TIME,
			AUTOFIRMA_CONNECTION_RETRIES : AUTOFIRMA_CONNECTION_RETRIES,
			
			/* Metodos de conexion con la aplicacion nativa. */
			cargarAppAfirma : cargarAppAfirma,
			sign : sign,
			coSign : coSign,
			cosign : cosign,
			counterSign : counterSign,
			countersign : counterSign,
			signBatch : signBatch,
			selectCertificate : selectCertificate,
			signAndSaveToFile : signAndSaveToFile,
			getCurrentLog : getCurrentLog,
			
			/* Gestion de ficheros. */
			saveDataToFile : saveDataToFile,
			getFileNameContentBase64: getFileNameContentBase64,
			getMultiFileNameContentBase64 : getMultiFileNameContentBase64,

			/* Configuracion */
			checkTime : checkTime,
			setKeyStore : setKeyStore,
			setForceWSMode : setForceWSMode,
			setServlets : setServlets,
			setStickySignatory : setStickySignatory,
			setLocale : setLocale,
			
			/* Gestion de errores */
			getErrorMessage : getErrorMessage,
			getErrorType : getErrorType,
			
			/* Utilidad JavaScript*/
			getBase64FromText : getBase64FromText,
			getTextFromBase64 : getTextFromBase64,
			downloadRemoteData : downloadRemoteData,

			/* Comprobacion de entorno */
			isAndroid : Platform.isAndroid,
			isIOS : Platform.isIOS
		};
})(window, undefined);

/**
 * Mantenemos una copia del objeto de despliegue usando el nombre de variable anterior
 * por compatibilidad con los despliegues actuales.
 */
var MiniApplet = AutoScript;

/** Base64 encode / decode
 *  http://www.webtoolkit.info/ */
var Base64 = {

		/* private property */
		_keyStr : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",
		_keyStr_URL_SAFE : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_=",

		/* public method for encoding */
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

		/* Codifica los datos binarios obtenidos en un ArrayBuffer. */
		encodeArrayBuffer : function(input, URL_SAFE) {
			
			var uInt8Array = new Uint8Array(input);
		    var i = uInt8Array.length;
		    var binaryString = new Array(i);
		    while (i--)
		    {
		      binaryString[i] = String.fromCharCode(uInt8Array[i]);
		    }

		    return URL_SAFE ?
		    		window.btoa(binaryString.join('')).replace('+', '-').replace('/', '_') :
		    			window.btoa(binaryString.join(''));
		},
		
		/** Codifica un array de bytes a Base64.
		 *  @param {Array.<number>|Uint8Array} input An array of bytes (numbers with
		 *     value in [0, 255]) to encode.
		 * @param {boolean=} opt_webSafe Boolean indicating we should use the
		 *     alternative alphabet.
		 * @return {string} The base64 encoded string. */
		encodeByteArray : function(input, URL_SAFE) {

		  var keyStr = (URL_SAFE == true) ? this._keyStr_URL_SAFE : this._keyStr;

		  var output = [];

		  for (var i = 0; i < input.length; i += 3) {
		    var byte1 = input[i];
		    var haveByte2 = i + 1 < input.length;
		    var byte2 = haveByte2 ? input[i + 1] : 0;
		    var haveByte3 = i + 2 < input.length;
		    var byte3 = haveByte3 ? input[i + 2] : 0;

		    var outByte1 = byte1 >> 2;
		    var outByte2 = ((byte1 & 0x03) << 4) | (byte2 >> 4);
		    var outByte3 = ((byte2 & 0x0F) << 2) | (byte3 >> 6);
		    var outByte4 = byte3 & 0x3F;

		    if (!haveByte3) {
		      outByte4 = 64;

		      if (!haveByte2) {
		        outByte3 = 64;
		      }
		    }

		    output.push(keyStr.charAt(outByte1),
		    			keyStr.charAt(outByte2),
		    			keyStr.charAt(outByte3),
		    			keyStr.charAt(outByte4));
		  }

		  return output.join('');
		},
		
		/* public method for decoding */
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

		/* private method for UTF-8 encoding */
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

		/* private method for UTF-8 decoding */
		_utf8_decode : function (utftext) {
			var string = "";
			var i = 0;
			var c = 0;
			var c1 = 0;
			var c2 = 0;

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
					var c3 = utftext.charCodeAt(i+2);
					string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
					i += 3;
				}
			}

			return string;
		}
};

/** Paul Tero, July 2001
 * http://www.tero.co.uk/des/
 * 
 * Optimised for performance with large blocks by Michael Hayworth, November 2001
 * http://www.netdealing.com
 * 
 * THIS SOFTWARE IS PROVIDED "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE. */
var Cipher = {
	tableStr : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
	tableStr_URL_SAFE : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_",
	
	/* des
	 * this takes the key, the message, and whether to encrypt or decrypt */
	des : function  (key, message, encrypt, mode, iv, padding) {
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
		var keys = Cipher.des_createKeys (key);
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
		var result = "";
		var tempresult = "";

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
	},

	//this takes as input a 64 bit key (even though only 56 bits are used)
	//as an array of 2 integers, and returns 16 48 bit keys
	des_createKeys : function (key) {
		//declaring this locally speeds things up a bit
		var pc2bytes0  = new Array (0,0x4,0x20000000,0x20000004,0x10000,0x10004,0x20010000,0x20010004,0x200,0x204,0x20000200,0x20000204,0x10200,0x10204,0x20010200,0x20010204);
		var pc2bytes1  = new Array (0,0x1,0x100000,0x100001,0x4000000,0x4000001,0x4100000,0x4100001,0x100,0x101,0x100100,0x100101,0x4000100,0x4000101,0x4100100,0x4100101);
		var pc2bytes2  = new Array (0,0x8,0x800,0x808,0x1000000,0x1000008,0x1000800,0x1000808,0,0x8,0x800,0x808,0x1000000,0x1000008,0x1000800,0x1000808);
		var pc2bytes3  = new Array (0,0x200000,0x8000000,0x8200000,0x2000,0x202000,0x8002000,0x8202000,0x20000,0x220000,0x8020000,0x8220000,0x22000,0x222000,0x8022000,0x8222000);
		var pc2bytes4  = new Array (0,0x40000,0x10,0x40010,0,0x40000,0x10,0x40010,0x1000,0x41000,0x1010,0x41010,0x1000,0x41000,0x1010,0x41010);
		var pc2bytes5  = new Array (0,0x400,0x20,0x420,0,0x400,0x20,0x420,0x2000000,0x2000400,0x2000020,0x2000420,0x2000000,0x2000400,0x2000020,0x2000420);
		var pc2bytes6  = new Array (0,0x10000000,0x80000,0x10080000,0x2,0x10000002,0x80002,0x10080002,0,0x10000000,0x80000,0x10080000,0x2,0x10000002,0x80002,0x10080002);
		var pc2bytes7  = new Array (0,0x10000,0x800,0x10800,0x20000000,0x20010000,0x20000800,0x20010800,0x20000,0x30000,0x20800,0x30800,0x20020000,0x20030000,0x20020800,0x20030800);
		var pc2bytes8  = new Array (0,0x40000,0,0x40000,0x2,0x40002,0x2,0x40002,0x2000000,0x2040000,0x2000000,0x2040000,0x2000002,0x2040002,0x2000002,0x2040002);
		var pc2bytes9  = new Array (0,0x10000000,0x8,0x10000008,0,0x10000000,0x8,0x10000008,0x400,0x10000400,0x408,0x10000408,0x400,0x10000400,0x408,0x10000408);
		var pc2bytes10 = new Array (0,0x20,0,0x20,0x100000,0x100020,0x100000,0x100020,0x2000,0x2020,0x2000,0x2020,0x102000,0x102020,0x102000,0x102020);
		var pc2bytes11 = new Array (0,0x1000000,0x200,0x1000200,0x200000,0x1200000,0x200200,0x1200200,0x4000000,0x5000000,0x4000200,0x5000200,0x4200000,0x5200000,0x4200200,0x5200200);
		var pc2bytes12 = new Array (0,0x1000,0x8000000,0x8001000,0x80000,0x81000,0x8080000,0x8081000,0x10,0x1010,0x8000010,0x8001010,0x80010,0x81010,0x8080010,0x8081010);
		var pc2bytes13 = new Array (0,0x4,0x100,0x104,0,0x4,0x100,0x104,0x1,0x5,0x101,0x105,0x1,0x5,0x101,0x105);

		//how many iterations (1 for des, 3 for triple des)
		var iterations = key.length > 8 ? 3 : 1; //changed by Paul 16/6/2007 to use Triple DES for 9+ byte keys
		//stores the return keys
		var keys = new Array (32 * iterations);
		//now define the left shifts which need to be done
		var shifts = new Array (0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0);
		//other variables
		var lefttemp, righttemp, m=0, n=0, temp;

		for (var j=0; j<iterations; j++) { //either 1 or 3 iterations
			var left = (key.charCodeAt(m++) << 24) | (key.charCodeAt(m++) << 16) | (key.charCodeAt(m++) << 8) | key.charCodeAt(m++);
			var right = (key.charCodeAt(m++) << 24) | (key.charCodeAt(m++) << 16) | (key.charCodeAt(m++) << 8) | key.charCodeAt(m++);

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
	},

	//Convierte una cadena a Base 64. Debido a un error en el algoritmo original, pasaremos
	// de cadena a hexadecimal y de hexadecimal a Base64
	stringToBase64 : function  (s) {

		// Para realizar la transformacion, primero debemos convertir a hexadecimal y luego a cadena
		
		// A hexadecimal
		var str = "";
		var hexes = new Array ("0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f");
		for (var i=0; i<s.length; i++) {str += hexes [s.charCodeAt(i) >> 4] + hexes [s.charCodeAt(i) & 0xf];}
		
		// A cadena
		var byteString;
		var byteArray = str.replace(/\r|\n/g, "").replace(/([\da-fA-F]{2}) ?/g, "0x$1 ").replace(/ +$/, "").split(" ");
		
		// Vaciamos la cadena por liberar recursos
		str = "";
		
		try {
			byteString = String.fromCharCode.apply(null, byteArray);
		} catch (e) {
			var byteString = "";
			for (var i = 0, len = byteArray.length; i < len; i++) {
				byteString += String.fromCharCode(byteArray[i]);
			}
		}

		return Cipher.btoa(byteString, null);
	},

	//Convert a base64 string into a normal string
	base64ToString : function(s) {
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
	},

	// --- Funciones para pasar de Hexadecimal a base64

	btoa : function  (bin, urlSafe) {
		var table = (urlSafe == true ? Cipher.tableStr_URL_SAFE : Cipher.tableStr).split("");
			
		for (var i = 0, j = 0, len = bin.length / 3, base64 = []; i < len; ++i) {
			var a = bin.charCodeAt(j++), b = bin.charCodeAt(j++), c = bin.charCodeAt(j++);
			if ((a | b | c) > 255) throw new Error("String contains an invalid character");
			base64[base64.length] = table[a >> 2] + table[((a << 4) & 63) | (b >> 4)] +
			(isNaN(b) ? "=" : table[((b << 2) & 63) | (c >> 6)]) +
			(isNaN(b + c) ? "=" : table[c & 63]);
		}
		return base64.join("");
	}
};

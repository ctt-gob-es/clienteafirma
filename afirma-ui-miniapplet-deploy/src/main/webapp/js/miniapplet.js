
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

var MiniApplet = ( function ( window, undefined ) {

		var VERSION = "1.4";

		var JAR_NAME = 'miniapplet-full_1_4.jar';

		var JAVA_ARGUMENTS = '-Xms512M -Xmx512M ';

		var SYSTEM_PROPERTIES = null;

		var clienteFirma = null;

		var codeBase = null;

		var storageServletAddress = null;

		var retrieverServletAddress = null;

		var jnlpServiceAddress = null;

		var setJnlpService = function (jnlp){
			jnlpServiceAddress = jnlp;
		}

		var clientType = null;

		var severeTimeDelay = false;

		var selectedLocale = null;

		var LOCALIZED_STRINGS = new Array();
		LOCALIZED_STRINGS["es_ES"] = {
				checktime_warn: "Se ha detectado un desfase horario entre su sistema y el servidor. Se recomienda que se corrija antes de pulsar Aceptar para continuar.",
				checktime_err: "Se ha detectado un desfase horario entre su sistema y el servidor. Debe corregir la hora de su sistema antes de continuar.",
				checktime_local_time: "Hora de su sistema",
				checktime_server_time: "Hora del servidor"
		};
		LOCALIZED_STRINGS["gl_ES"] = {
				checktime_warn: "Destectouse un desfase horario entre o seu sistema e o servidor. RecomÃ©ndase corrixilo antes de pulsar Aceptar para continuar.",
				checktime_err: "Destectouse un desfase horario entre o seu sistema e o servidor. Debe corrixir a hora do seu sistema antes de continuar.",
				checktime_local_time: "Hora do seu sistema",
				checktime_server_time: "Hora do servidor"
		};

		var DEFAULT_LOCALE = LOCALIZED_STRINGS["es_ES"];

		var currentLocale = DEFAULT_LOCALE;

		var defaultKeyStore = null;

		/* ------------------------------------------------ */
		/* Constantes para la operacion interna del Cliente */
		/* ------------------------------------------------ */

		/* Tamano del buffer con el que se pasa informacion al applet */
		var BUFFER_SIZE = 2 * 1024 * 1024;

		/* Cadena que determina el fin de una respuesta */
		var EOF = "%%EOF%%";

		/* Identifica que se utilizara el MiniApplet. */
		var TYPE_APPLET = "APPLET";

		/* Identifica que se utilizara una aplicacion nativa de firma. */
		var TYPE_JAVASCRIPT_WEB_SERVICE = "JAVASCRIPT_WEB_SERVICE";
		var TYPE_JAVASCRIPT_SOCKET = "JAVASCRIPT_SOCKET";

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

		// Tiempo de espera entre los intentos de conexion con autofirma.
		var AUTOFIRMA_LAUNCHING_TIME = 2000;

		// Reintentos de conexion totales para detectar que esta instalado AutoFirma
		var AUTOFIRMA_CONNECTION_RETRIES = 20;

		// Variable que se puede configurar para forzar el uso del modo de comunicacion por servidor intermedio
		// entre la pÃ¡gina web y AutoFirma
		var forceWSMode = false;

		// Variable que se puede configurar para forzar el uso del modo o afirma://
		var forceAFirma = false;

		// Variable que devuelve si funciona el modo JNLP://
		var bJNLP = true;
		/* ------------------------------------ */
		/* Funciones de comprobacion de entorno */
		/* ------------------------------------ */

		/**
		 * Determina con un boolean si nuestro cliente es Android
		 */
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

		/**
		 * Determina con un boolean si nuestro cliente es iOS.
		 */
		function isIOS() {
			return (navigator.userAgent.toUpperCase().indexOf("IPAD") != -1) ||
			(navigator.userAgent.toUpperCase().indexOf("IPOD") != -1) ||
			(navigator.userAgent.toUpperCase().indexOf("IPHONE") != -1);
		}
		
		/** Determina con un boolean si nos encontramos en Windows 7 */
		function isWindows7() {
			return navigator.userAgent.indexOf("Windows NT 6.1") != -1;		/* Windows 7 */
		}

		/** Determina con un boolean si nos encontramos en Windows 8/8.1 */
		function isWindows8() {
			return navigator.userAgent.indexOf("Windows NT 6.2") != -1 ||	/* Windows 8 */
				navigator.userAgent.indexOf("Windows NT 6.3") != -1;		/* Windows 8.1 */
		}

		/** Determina con un boolean si nos encontramos en Windows RT */
		function isWindowsRT() {
			return isWindows8() && navigator.userAgent.indexOf("ARM;") != -1;
		}

		/** Determina con un boolean si estamos en Internet Explorer */
		function isInternetExplorer() {
			return !!(navigator.userAgent.match(/MSIE/))	/* Internet Explorer 10 o inferior */
					|| !!(navigator.userAgent.match(/Trident/) && navigator.userAgent.match(/rv:11/)) /* Internet Explorer 11 o superior (Opcion 1) */
					|| !!navigator.userAgent.match(/Trident.*rv[ :]*11\./); /* Internet Explorer 11 o superior (Opcion 2) */
		}

		/** Indica si el navegador es Internet Explorer 10 o inferior. */
		function isOldInternetExplorer() {
			return !!(navigator.userAgent.match(/MSIE/));
		}

		function isFirefoxUAM() {
		    return navigator.userAgent.indexOf("UAM") > 0;
		}

		/**
		 * Determina con un boolean si se accede a la web con Firefox
		 */
		function isFirefox(){
			return navigator.userAgent.toUpperCase().indexOf("FIREFOX") != -1
		}

		/**
		 * Determina con un boolean si se accede a la web con Chrome
		 */
		function isChrome() {
			return navigator.userAgent.toUpperCase().indexOf("CHROME") != -1 ||
				navigator.userAgent.toUpperCase().indexOf("CHROMIUM") != -1;
		}

        /**
         * Determina con un boolean si el navegador es Microsoft Edge
         */
        function isEdge() {
        	return !!navigator.userAgent.match(/Edge\/\d+/);
        }

        /**
         * Determina con un boolean si se ejecuta mediante JNLP
         */
        function isJNLP() {
        	return bJNLP;
        }

		/** Indica si el navegador detecta Java. Este valor no es completamente fiable, ya que
		 * Internet Explorer siempre indica que si esta activado. */
		function isJavaEnabled() {
			return navigator.javaEnabled();
		}

		/** Comprueba si una cadena de texto es una URL (http/https). La alternativa implicaria ser un Base64. */
		function isValidUrl(data) {
			return data != null && data.length > "https://".length &&
				("http:" == data.substr(0, 5) || "https:" == data.substr(0, 6));
		}

		var downloadSuccessFunction = null;
		var downloadErrorFunction = null;

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
			httpRequest.overrideMimeType('text\/plain; charset=x-user-defined');
			httpRequest.onreadystatechange = function (evt) {
				if (httpRequest.readyState == 4 && httpRequest.status == 200) {
					if (downloadSuccessFunction) {
						downloadSuccessFunction(Base64.encode(httpRequest.responseText));
					}
//					else {
//						console.log("Se termino la descarga de los datos. No se invoca a ninguna funcion.");
//					}
				}
			}
			httpRequest.onerror = function(e) {
				if (downloadErrorFunction) {
					downloadErrorFunction(e);
				}
//				else {
//					console.log("Error en la descarga de los datos. No se invoca a ninguna funcion.");
//				}
			}
			httpRequest.send();
		}

		function getHttpRequest() {
            var xmlHttp = null;
            if (typeof XMLHttpRequest != "undefined") {	// Navegadores actuales
                xmlHttp = new XMLHttpRequest();
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

		/** Permite establecer que la invocaciÃ³n con AutoFirma sea a traves
		 * del protocolo afirma:// */
		var setForceAFirma = function (force) {
			forceAFirma = force;
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
			try {

				if (checkType == undefined || checkType == null || checkType == CHECKTIME_NO
						|| maxMillis == undefined || maxMillis == null || maxMillis <= 0) {
					return;
				}

				// Si checkURL existe mandamos la peticion ahi, en caso contrario nos inventamos una url.
				var URL;
				if (checkURL) {
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
				// No hacemos nada si estamos en local
				//console.log("Error en la obtencion de la hora del servidor: " + e);
				return;
			}
		}
		/** Carga el MiniApplet. */
		var cargarMiniApplet = function (base, keystore) {

			// Antes que nada, comprobamos que no haya un desfase horario declarado como
			// grave.
			if (severeTimeDelay) {
				return;
			}

			// Sincronizamos las variables que puedan haberse establecido de forma externa
			// antes de la llamada al metodo de carga
			JAVA_ARGUMENTS = MiniApplet.JAVA_ARGUMENTS;
			SYSTEM_PROPERTIES = MiniApplet.SYSTEM_PROPERTIES;

			// Establecemos cual sera el keystore por defecto
			defaultKeyStore = keystore;
			if (!defaultKeyStore) {
				defaultKeyStore = getDefaultKeystore();
			}

			// Si estamos claramente en un sistema movil o que no permite la ejecucion de Java,
			// cargamos directamente el Cliente JavaScript
			if (isAndroid() || isIOS() || isWindowsRT() || isChrome() || isEdge()) {
				cargarAppAfirma(base, defaultKeyStore);
				return;
			}

			// Si estamos en un entorno que permite Java, comprobamos si esta disponible
			// y en caso de no estarlo, tambien cargamos el Cliente JavaScript.
			if (!isJavaEnabled()) {
				cargarAppAfirma(base, defaultKeyStore);
				return;
			}

			// Configuramos los argumentos para la seleccion de almacen
			configureKeyStore();

			// Incluso si el navegador informa que hay Java, puede no haberlo (Internet Explorer
			// siempre dice que hay), asi que cargamos el applet, pero tenemos en cuenta que en
			// caso de error debemos cargar el cliente JavaScript
			codeBase = (base != undefined && base != null) ? base : './';

			var keystoreConfig = keystore;
			if (keystoreConfig == undefined) {
				keystoreConfig = null;
			}

			var attributes = {
					'id': 'miniApplet',
					'name': 'MiniApplet @firma (Gobierno de Espa\u00F1a)',
					'type': 'application/x-java-applet',
					'width': 1,
					'height': 1
			};

			// Los argumentos de java no llegan al propio applet en las pruebas con Java 6 y 7,
			// asi que (salvo los argumentos de carga) vamos a pasarlos como un parametro mas al
			// applet para luego establecerlos internamente.
			var parameters = {
					'keystore': keystoreConfig,
					'userAgent': window.navigator.userAgent,
					'archive': codeBase + '/' + JAR_NAME,
					'code': 'es.gob.afirma.miniapplet.MiniAfirmaApplet',
					'java-vm-args': JAVA_ARGUMENTS,
					'java_arguments': JAVA_ARGUMENTS,
					'system_properties': SYSTEM_PROPERTIES,
					'codebase_lookup': false,
					'separate_jvm': true,
					'locale': selectedLocale
			};

			loadMiniApplet(attributes, parameters);

			if (isFirefox()) {
				window.setTimeout(function() {
					clienteFirma = document.getElementById("miniApplet");

					// Si no esta definido el cliente es porque se ha intentado cargar el applet
					// y no se ha podido, asi que se usara la aplicacion nativa
					if (clienteFirma == null) {
						cargarAppAfirma(codeBase, defaultKeyStore);
					}
					else {
						try {
							clienteFirma.echo();
						}
						catch(e) {
							cargarAppAfirma(codeBase, defaultKeyStore);
						}
					}
				}, 2000);
			}
			else {
				clienteFirma = document.getElementById("miniApplet");

				// Si no esta definido el cliente es porque se ha intentado cargar el applet
				// y no se ha podido, asi que se usara la aplicacion nativa
				if (clienteFirma == null) {
					cargarAppAfirma(codeBase, defaultKeyStore);
				}
			}
		}

		function getDefaultKeystore(){
			if(isFirefox()){
				return KEYSTORE_MOZILLA;
			}
			return null;
		}

		/** Establece los parametros de configuracion para la correcta seleccion del almacen
		 * de claves que se debe cargar. */
		function configureKeyStore() {
			if (isFirefoxUAM()) {
				if (SYSTEM_PROPERTIES == null) {
					SYSTEM_PROPERTIES = "";
				}
				SYSTEM_PROPERTIES += " -Des.gob.afirma.keystores.mozilla.UseEnvironmentVariables=true ";
			}
		}

		/** Carga el MiniApplet. */
		var setKeyStore = function (ksType) {

			forceLoad();

			clienteFirma.setKeyStore(ksType != null ? ksType : defaultKeyStore);
		}

		var selectCertificate = function (params, successCallback, errorCallback) {

			forceLoad();

			if (clientType == TYPE_APPLET) {
				try {
					var certificate = clienteFirma.selectCertificate(params);
					if (successCallback == undefined || successCallback == null) {
						return certificate;
					}
					successCallback(certificate);
				} catch(e) {
					if (errorCallback == undefined || errorCallback == null) {
						throw e;
					}
					errorCallback(clienteFirma.getErrorType(), clienteFirma.getErrorMessage());
				}
			}
			else {
				clienteFirma.selectCertificate(params, successCallback, errorCallback);
			}
		}

		var sign = function (dataB64, algorithm, format, params, successCallback, errorCallback) {

			forceLoad();

			if (clientType == TYPE_APPLET) {
				try {
					setData(dataB64);
					var certSignaturePair = buildData(clienteFirma.sign(algorithm, format, params));
					var sepPos = certSignaturePair.indexOf('|');
					if (successCallback == undefined || successCallback == null) {
						return certSignaturePair.substring(sepPos + 1);
					}
					successCallback(certSignaturePair.substring(sepPos + 1), certSignaturePair.substring(0, sepPos));
				} catch(e) {
					if (errorCallback == undefined || errorCallback == null) {
						throw e;
					}
					errorCallback(clienteFirma.getErrorType(), clienteFirma.getErrorMessage());
				}
			}
			else {
				clienteFirma.sign(dataB64, algorithm, format, params, successCallback, errorCallback);
			}
		}

		var coSign = function (signB64, dataB64, algorithm, format, params, successCallback, errorCallback) {

			forceLoad();

			if (clientType == TYPE_APPLET) {
				try {
					setData(signB64);
					var certSignaturePair = buildData(clienteFirma.coSign(dataB64, algorithm, format, params));
					var sepPos = certSignaturePair.indexOf('|');
					if (successCallback == undefined || successCallback == null) {
						return certSignaturePair.substring(sepPos + 1);
					}
					successCallback(certSignaturePair.substring(sepPos + 1), certSignaturePair.substring(0, sepPos));
				} catch(e) {
					if (errorCallback == undefined || errorCallback == null) {
						throw e;
					}
					errorCallback(clienteFirma.getErrorType(), clienteFirma.getErrorMessage());
				}
			}
			else {
				clienteFirma.coSign(signB64, dataB64, algorithm, format, params, successCallback, errorCallback);
			}
		}


		var counterSign = function (signB64, algorithm, format, params, successCallback, errorCallback) {

			forceLoad();

			if (clientType == TYPE_APPLET) {
				try {
					setData(signB64);
					var certSignaturePair = buildData(clienteFirma.counterSign(algorithm, format, params));
					var sepPos = certSignaturePair.indexOf('|');
					if (successCallback == undefined || successCallback == null) {
						return certSignaturePair.substring(sepPos + 1);
					}
					successCallback(certSignaturePair.substring(sepPos + 1), certSignaturePair.substring(0, sepPos));
				} catch(e) {
					if (errorCallback == undefined || errorCallback == null) {
						throw e;
					}
					errorCallback(clienteFirma.getErrorType(), clienteFirma.getErrorMessage());
				}
			}
			else {
				clienteFirma.counterSign(signB64, algorithm, format, params, successCallback, errorCallback);
			}
		}

		var signAndSaveToFile = function (operationId, dataB64, algorithm, format, params, outputFileName, successCallback, errorCallback) {

			forceLoad();

			if (clientType == TYPE_APPLET) {
				try {
					setData(dataB64);
					var certSignaturePair = buildData(clienteFirma.signAndSaveToFile(operationId, algorithm, format, params, outputFileName));
					var sepPos = certSignaturePair.indexOf('|');
					if (successCallback == undefined || successCallback == null) {
						return certSignaturePair.substring(sepPos + 1);
					}
					successCallback(certSignaturePair.substring(sepPos + 1), certSignaturePair.substring(0, sepPos));
				} catch(e) {
					if (errorCallback == undefined || errorCallback == null) {
						throw e;
					}
					errorCallback(clienteFirma.getErrorType(), clienteFirma.getErrorMessage());
				}
			}
			else {
				clienteFirma.signAndSaveToFile(operationId, dataB64, algorithm, format, params, outputFileName, successCallback, errorCallback);
			}
		}

		var signBatch = function (batchB64, batchPreSignerUrl, batchPostSignerUrl, params, successCallback, errorCallback) {

			forceLoad();

			if (clientType == TYPE_APPLET) {
				try {
					if (successCallback == undefined || successCallback == null) {
						return clienteFirma.signBatch(batchB64, batchPreSignerUrl, batchPostSignerUrl, params);
					}
					successCallback(clienteFirma.signBatch(batchB64, batchPreSignerUrl, batchPostSignerUrl, params));
				}
				catch (e) {
					if (errorCallback == undefined || errorCallback == null) {
						throw e;
					}
					errorCallback(clienteFirma.getErrorType(), clienteFirma.getErrorMessage());
					return;
				}
			}
			else {
				clienteFirma.signBatch(batchB64, batchPreSignerUrl, batchPostSignerUrl, params, successCallback, errorCallback);
			}
		}

		var getBase64FromText = function (plainText, charset) {
			forceLoad();
			return clienteFirma.getBase64FromText(plainText, charset);
		}

		var getTextFromBase64 = function (dataB64, charset) {
			forceLoad();
			return clienteFirma.getTextFromBase64(dataB64, charset);
		}

		var saveDataToFile = function (dataB64, title, fileName, extension, description, successCallback, errorCallback) {
			forceLoad();
			if (clientType == TYPE_APPLET) {
				setData(dataB64);
				return clienteFirma.saveDataToFile(title, fileName, extension, description);
			}
			else {
				clienteFirma.saveDataToFile(dataB64, title, fileName, extension, description, successCallback, errorCallback);
			}
		}

		var getFileNameContentBase64 = function (title, extensions, description, filePath) {
			forceLoad();
			return buildData(clienteFirma.getFileNameContentBase64(title, extensions, description, filePath));
		}

		var getMultiFileNameContentBase64 = function (title, extensions, description, filePath) {
			forceLoad();
			return buildData(clienteFirma.getMultiFileNameContentBase64(title, extensions, description, filePath));
		}

		var echo = function () {
			forceLoad();
			return clienteFirma.echo();
		}

		var setStickySignatory = function (sticky) {
			forceLoad();
			return clienteFirma.setStickySignatory(sticky);
		}

		var setLocale = function (locale) {
			selectedLocale = locale;
			currentLocale = (locale == null || LOCALIZED_STRINGS[locale] == null ? DEFAULT_LOCALE : LOCALIZED_STRINGS[locale]);
		}

		var getErrorMessage = function () {
			forceLoad();
			return clienteFirma.getErrorMessage();
		}

		var getErrorType = function () {
			forceLoad();
			return clienteFirma.getErrorType();
		}

		var getCurrentLog = function () {
			forceLoad();
			return	" === JAVASCRIPT INFORMATION === " +
					"\nnavigator.appCodeName: " + navigator.appCodeName +
					"\nnavigator.appName: " +  navigator.appName +
					"\nnavigator.appVersion: " + navigator.appVersion +
					"\nnavigator.platform: " + navigator.platform +
					"\nnavigator.userAgent: " + navigator.userAgent+
					"\nnavigator.javaEnabled(): " + navigator.javaEnabled() +
					"\nscreen.width: " + (window.screen ? screen.width : 0) +
					"\nscreen.height: " + (window.screen ? screen.height : 0) +
					"\n\n   === CLIENTE LOG === \n" +
					clienteFirma.getCurrentLog();
		}

		var setServlets = function (storageServlet,  retrieverServlet) {

			storageServletAddress = storageServlet;
			retrieverServletAddress = retrieverServlet;

			if (clienteFirma && clienteFirma.setServlets) {
				clienteFirma.setServlets(storageServlet,  retrieverServlet);
			}
		}

		/*************************************************************
		 *  FUNCIONES PARA EL DESPLIEGUE DEL APPLET					 *
		 **************************************************************/

		function loadMiniApplet(attributes, parameters) {
			// Internet Explorer se carga mediante un
			// elemento <object>. El resto con un <embed>.
			if (isInternetExplorer()) {

				var appletTag = "<object classid='clsid:8AD9C840-044E-11D1-B3E9-00805F499D93' width='" + attributes["width"] + "' height='" + attributes["height"] + "' id='" + attributes["id"] + "'>";

				if (attributes != undefined && attributes != null) {
					for (var attribute in attributes) {
						appletTag += "<param name='" + attribute + "' value='" + attributes[attribute] + "' />";
					}
				}

				if (parameters != undefined && parameters != null) {
					for (var parameter in parameters) {
						appletTag += "<param name='" + parameter + "' value='" + parameters[parameter] + "' />";
					}
				}

				appletTag += "</object>";

				// Al agregar estos nodos con append() no se carga automaticamente el applet en IE10 e inferiores, asi que
				// hay que usar document.write() o innerHTML. Para asegurarnos de no pisar HTML previo, crearemos un <div>
				// en la pagina, lo recogeremos e insertaremos dentro suyo el codigo del applet.
				var divElem = document.createElement("div");
				var idAtt = document.createAttribute("id");
				idAtt.value = 'divAfirmaApplet';
				divElem.setAttributeNode(idAtt);

				document.body.appendChild(divElem);

				document.getElementById("divAfirmaApplet").innerHTML = appletTag;
			}
			else {
				var embed = document.createElement("embed");

				if (attributes != undefined && attributes != null) {
					for (var attribute in attributes) {
						var att = document.createAttribute(attribute);
						att.value = attributes[attribute];
						try {
							embed.setAttributeNode(att);
						}
						catch (e) {
							// Probamos este como alternativa en caso de error. Caso detectado en:
							// - IE10 sin modo de compabilidad con el Document Mode de IE7.
							// - Firefox en Mac OS X
							// Este intento no soluciona el error, pero evita que se propague
							embed.setAttribute(attribute, attributes[attribute]);
						}
					}
				}

				if (parameters != undefined && parameters != null) {
					for (var parameter in parameters) {
						var att = document.createAttribute(parameter);
						att.value = parameters[parameter];
						embed.setAttributeNode(att);
					}
				}

				document.body.appendChild(embed);
			}
		}

		/**
		 * Establece los datos que debera procesar el applet MiniApplet.
		 */
		function forceLoad() {

			// Antes que nada, comprobamos que no haya un desfase horario declarado como
			// grave.
			if (severeTimeDelay) {
				return;
			}
			if (clientType == null || clientType == TYPE_JAVASCRIPT_WEB_SERVICE || clientType == TYPE_JAVASCRIPT_SOCKET ) {
				var tempCliente = document.getElementById("miniApplet");
				var appletLoaded;
				try {
					appletLoaded = tempCliente != null && tempCliente.echo() != "Cliente JavaScript";
				}
				catch (e) {
					appletLoaded = false;
				}
				if (appletLoaded) {
					clienteFirma = tempCliente;
					clientType = TYPE_APPLET;
				}
				else if (clientType == null) {
					cargarAppAfirma(codeBase, defaultKeyStore);
				}
				setServlets(storageServletAddress, retrieverServletAddress);
			}
		}

		/**
		 * Establece los datos que debera procesar el applet MiniApplet.
		 */
		function setData(dataB64) {

			if (dataB64 == null) {
				return;
			}
			else if (dataB64.length <= BUFFER_SIZE) {
				clienteFirma.addData(dataB64);
			}
			else {
				clienteFirma.addData(dataB64.substring(0, BUFFER_SIZE));
				setData(dataB64.substring(BUFFER_SIZE));
			}
		}

		/**
		 * Construye el resultado de una funcion a partir de los trozos en la que esta los divide.
		 */
		function buildData(dataB64) {
			var buffer = dataB64;
			var chunk = clienteFirma.getRemainingData();
			while(chunk != EOF) {
				buffer += chunk;
				chunk = clienteFirma.getRemainingData();
			}
			return buffer;
		}

		/**************************************************************
		 **************************************************************
		 **************************************************************
		 **************************************************************
		 *  FUNCIONES DEL CLIENTE JAVASCRIPT						  *
		 **************************************************************
		 **************************************************************
		 **************************************************************
		 **************************************************************/

		/**
		 * Establece el objeto que simula ser el Applet de firma en sistemas en los que no se
		 * soportan los applets. Se usara comunicacion mediante socket cuando:
		 *  - El sistema operativo no sea Android.
		 *  - El sistema operativo no sea iOS.
		 *  - El navegador web no sea Intenet Explorer 10 o inferior (o 11 con modo de compatibilidad)
		 *  - No se fuerce el uso del servidor intermedio.
		 * En caso contrario, la comunicacion se realizara mediante un servidor intermedio.
		 */
		function cargarAppAfirma(clientAddress, keystore) {
			if (!isIOS() && !isAndroid() && !isOldInternetExplorer() && !isWindows7() && !forceWSMode){
				clienteFirma = new AppAfirmaJSSocket(clientAddress, window, undefined);
				clienteFirma.setKeyStore(keystore);
				clientType = TYPE_JAVASCRIPT_SOCKET;
			}
			else {
				clienteFirma = new AppAfirmaJSWebService(clientAddress, window, undefined);
				clienteFirma.setKeyStore(keystore);
				clientType = TYPE_JAVASCRIPT_WEB_SERVICE;
			}

		}

		var AppAfirmaJSSocket = ( function (clientAddress, window, undefined) {

			var UnsupportedOperationException = "java.lang.UnsupportedOperationException";

			/**
			 *  Atributos para la configuracion del objeto sustituto del applet Java de firma
			 */
			var errorMessage = '';
			var errorType = '';

			/** Puerto actual */
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

			/* Maxima longitud permitida para una URL, si la url se excede se divide la peticion en fragmentos */
			var URL_MAX_SIZE = 1048576;

			var connection = false;

			/* Dominio desde el que se realiza la llamada al servicio */
			var baseUri = clientAddress;

			/* Almacen de claves cargado por defecto */
			var defaultKeyStore = null;

			/* Funcion callback que se lanza al obtener un resultado */
			var successCallback = null;

			/* Funcion error callback */
			var errorCallback = null;

			/* Mayor entero */
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

			/**
			 * Genera numeros aleatorios con una distribucion homogenea.
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
			function coSign (signB64, dataB64, algorithm, format, extraParams, successCallbackFunction, errorCallbackFunction) {
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

				var data = generateDataToBatch(defaultKeyStore, storageServletAddress, batchPreSignerUrl, batchPostSignerUrl, extraParams, batchB64);
				execAppIntent(buildUrl(data));
			}

			/**
			 * Genera el objeto con los datos de la transaccion para la firma
			 */
			function generateDataToBatch(defaultKeyStore, storageServletAddress, batchPreSignerUrl, batchPostSignerUrl, extraParams, batchB64) {
				var data = new Object();
				data.op = generateDataKeyValue("op","batch");
				data.keystore = generateDataKeyValue("keystore", defaultKeyStore);
				data.stservlet = generateDataKeyValue("stservlet", storageServletAddress);
				data.batchpresignerurl = generateDataKeyValue("batchpresignerurl", batchPreSignerUrl);
				data.batchpostsignerurl = generateDataKeyValue("batchpostsignerurl", batchPostSignerUrl);
				data.properties = generateDataKeyValue ("properties", extraParams != null ? Base64.encode(extraParams) : null);
				data.dat = generateDataKeyValue ("dat",  batchB64 == "" ? null : batchB64);

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
					setTimeout(executeEchoByServiceByPort, MiniApplet.AUTOFIRMA_LAUNCHING_TIME, ports, url);
				}
				// Se ha ejecutado anteriormente y tenemos un puerto calculado.
				else {
					connection = false;
					executeEchoByService (port, url, MiniApplet.AUTOFIRMA_CONNECTION_RETRIES)
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
				idSession = generateNewIdSession();
				//openUrl("afirma://service?ports=" + portsLine + "&v=" + PROTOCOL_VERSION + "&idsession=" + idSession);
				if(window.protocolCheck&&!forceAFirma){
				  //console.log("Probamos afirma://");
				  //window.protocolCheck("afirma://service?ports=" + portsLine + "&v=" + PROTOCOL_VERSION + "&idsession=" + idSession, function(){
					  console.log("Probamos jnlp://");
					  //alert("No dispone de AutoFirma instalado. Se va a probar la versiÃ³n online");
					  //setTimeout(window.focus,0);
					  window.protocolCheck(jnlpServiceAddress+'?cadenaFirma='+encodeURIComponent("afirma://service?ports=" + portsLine + "&v=" + PROTOCOL_VERSION + "&idsession=" + idSession),
					           function () {
						  		   console.log("Probamos http://");
						  		   bJNLP = false;
					               openUrl(jnlpServiceAddress.replace("jnlp","https")+'?cadenaFirma='+encodeURIComponent("afirma://service?ports=" + portsLine + "&v=" + PROTOCOL_VERSION + "&idsession=" + idSession));
					           });
				  //});
				}else{
					//openUrl(jnlpServiceAddress+'?cadenaFirma='+encodeURIComponent("afirma://service?ports=" + portsLine + "&v=" + PROTOCOL_VERSION + "&idsession=" + idSession));
					bJNLP = false;
					openUrl("afirma://service?ports=" + portsLine + "&v=" + PROTOCOL_VERSION + "&idsession=" + idSession);
				}

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

			/**
			 * Genera el objeto con los datos de la transaccion para la operacion
			 * de firma/multifirma
			 */
			function generateDataToSign(signId, algorithm, format, extraParams, dataB64, keystore) {
				var data = new Object();
				data.op = generateDataKeyValue("op", signId);
				data.keystore = generateDataKeyValue ("keystore", keystore);
				data.algorithm = generateDataKeyValue ("algorithm", algorithm);
				data.format = generateDataKeyValue ("format", format);
				data.properties = generateDataKeyValue ("properties", extraParams != null ? Base64.encode(extraParams) : null);
				data.dat = generateDataKeyValue ("dat", dataB64 == "" ? null : dataB64);
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
				data.keystore = generateDataKeyValue ("keystore", keystore);
				data.algorithm = generateDataKeyValue ("algorithm", algorithm);
				data.format = generateDataKeyValue ("format", format);
				data.properties = generateDataKeyValue ("properties", extraParams != null ? Base64.encode(extraParams) : null);
				data.filename = generateDataKeyValue ("filename", outputFileName);
				data.dat = generateDataKeyValue ("dat", dataB64 == "" ? null : dataB64);
				return data;
			}

			function executeEchoByServiceByPort (ports, url) {
				connection = false;
				var semaphore = new Object();
				semaphore.locked = false;
				executeEchoByService (ports[0], url, MiniApplet.AUTOFIRMA_CONNECTION_RETRIES, semaphore);
				executeEchoByService (ports[1], url, MiniApplet.AUTOFIRMA_CONNECTION_RETRIES, semaphore);
				executeEchoByService (ports[2], url, MiniApplet.AUTOFIRMA_CONNECTION_RETRIES, semaphore);
			}

			/**
			* Intenta conectar con la aplicaciÃ³n nativa mandando una peticion echo al puerto.
			* Si la aplicaciÃ³n responde lanzamos la ejecucion del servicio.
			* Si la aplicaciÃ³n no responde volvemos a lanzar cada 2 segundos otra peticion echo hasta que una
			* peticion sea aceptada.
			*/
			function executeEchoByService (currentPort, url, timeoutResetCounter, semaphore) {
				var httpRequest = getHttpRequest();
				httpRequest.open("POST", URL_REQUEST + currentPort + "/afirma", true);
				httpRequest.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
				httpRequest.onreadystatechange = function() {
					if (httpRequest.readyState == 4 && httpRequest.status == 200 && Base64.decode(httpRequest.responseText, true) == "OK" && !connection) {
						//console.log("puerto asignado puerto:" + currentPort);
						port = currentPort;
						urlHttpRequest = URL_REQUEST + port + "/afirma";
						connection = true;
						if (semaphore) {
							semaphore.locked = true;
						}
						// Comprobamos si es una operacion de seleccion de certificado
						isSelectCertOperation = url.indexOf("afirma://selectcert") > -1;
						// Comprobamos si es una operacion de guardado.
						isSaveOperation = url.indexOf("afirma://save") > -1;
						// Comprobamos si es una operacion de firma por lotes
						isBatchOperation = url.indexOf("afirma://batch") > -1;
						// Comprobamos si es una operacion criptografica mas guardado del resultado
						isOpAndSaveOperation = url.indexOf("afirma://signandsave") > -1;
						executeOperationByService(url);
					}
					else if ((!semaphore || !semaphore.locked) && !connection && httpRequest.readyState != 2 && httpRequest.readyState != 3) {
						timeoutResetCounter--;
						//console.log("Quedan " + timeoutResetCounter + " peticiones a " + currentPort)

						// Si ya se conecto antes con la aplicacion pero ahora llevamos la mitad de los intentos
						// sin conectar, consideramos que se ha tumbado y hay que relanzarla
						if (port != "" && timeoutResetCounter < MiniApplet.AUTOFIRMA_CONNECTION_RETRIES/2) {
							port = "";
							if (semaphore) {
								semaphore.locked = true;
							}
							timeoutResetCounter = MiniApplet.AUTOFIRMA_CONNECTION_RETRIES;
							execAppIntent(url);
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
							setTimeout(executeEchoByService, MiniApplet.AUTOFIRMA_LAUNCHING_TIME, currentPort, url, timeoutResetCounter, semaphore);
						}
					}
				}

				if (!connection) {
					// Mandamos un echo con - por lo que las variables de control se resetearan
					// Se anade EOF para que cuando el socket SSL lea la peticion del buffer sepa que ha llegado al final y no se quede en espera
					httpRequest.send("echo=-idsession=" + idSession + "@EOF");
					//console.log("probamos puerto " +currentPort)
				}
			}

			/**
			* Comprueba si hay que dividir los datos que se se mandan a la aplicacion nativa.
			* Si hay que dividirlos se llama a la funcion executeOperationRecursive.
			* Si cabe en un solo envio se manda directamente.
			*/
			function executeOperationByService (url) {

				var httpRequest = getHttpRequest();
				httpRequest.open("POST", urlHttpRequest, true);
				httpRequest.setRequestHeader("Content-type","application/x-www-form-urlencoded");
				// Como internet explorer aÃ±ade basura hacemos las peticiones muy pequeÃ±as para que funcionen correctamente.
				if (isInternetExplorer()){
					URL_MAX_SIZE = 12000;
				}
				// Si el envio se debe fragmentar, llamamos a una funciÃ³n que se encarga de mandar la peticion recursivamente
				if (url.length > URL_MAX_SIZE) {
					executeOperationRecursive(url, 1, Math.ceil(url.length/URL_MAX_SIZE));
				}
				// El envio no se fragmenta
				else {
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
								setTimeout(executeOperationByService, WAITING_TIME, url);
							}
						}
					}
					httpRequest.onerror = function(e) {
						// status error 0 es que no se ha podido comunicar con la aplicacion
						if (e.target.status == 0) {
							errorServiceResponseFunction("java.lang.IOException", "Se ha perdido la conexiÃ³n con la aplicaciÃ³n @firma "+e.target.statusText);
						}
						// error desconocido
						else {
							errorServiceResponseFunction("java.lang.IOException", "Ocurrio un error de red en la llamada al servicio de firma "+e.target.statusText);
						}
					}
					// se anade EOF para que cuando el socket SSL lea la peticion del buffer sepa que ha llegado al final y no se quede en espera
					httpRequest.send("cmd=" + Base64.encode(url, true) + "idsession=" + idSession + "@EOF");
				}
			}

			/**
			* Manda los datos a la aplicaciÃ³n nativa en varios fragmentos porque ha habido que dividir los datos.
			* Se va mandando cada peticiÃ³n cuando se reciba la anterior.
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
				httpRequest.onerror = function(e) {
					// Status error 0 es que no se ha podido comunicar con la aplicacion
					if (e.target.status == 0){
						errorServiceResponseFunction("java.lang.IOException", "Se ha perdido la conexiÃ³n con la aplicaciÃ³n @firma "+e.target.statusText);
					}
					// Error desconocido
					else{
						errorServiceResponseFunction("java.lang.IOException", "Ocurrio un error de red en la llamada al servicio de firma "+e.target.statusText);
					}
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
				httpRequest.onerror = function(e) {
					// status error 0 es que no se ha podido comunicar con la aplicacion
					if (e.target.status == 0){
						errorServiceResponseFunction("java.lang.IOException", "Se ha perdido la conexiÃ³n con la aplicaciÃ³n @firma "+e.target.statusText);
					}
					// error desconocido
					else{
						errorServiceResponseFunction("java.lang.IOException", "Ocurrio un error de red en la llamada al servicio de firma "+e.target.statusText);
					}
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
				httpRequest.onerror = function(e) {
					// status error 0 es que no se ha podido comunicar con la aplicacion
					if (e.target.status == 0){
						errorServiceResponseFunction("java.lang.IOException", "Se ha perdido la conexiÃ³n con la aplicaciÃ³n @firma "+e.target.statusText);
					}
					// error desconocido
					else{
						errorServiceResponseFunction("java.lang.IOException", "Ocurrio un error de red en la llamada al servicio de firma "+e.target.statusText);
					}
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

				// Interpretamos el resultado como un base 64 y el certificado y los datos cifrados
				var signature;
				var certificate = null;
				var sepPos = data.indexOf("|");
				var multiSepPos = data.indexOf(':');

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
				errorCallback(exception, message);
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
				data.extension = generateDataKeyValue ("extension", extension);
				data.description = generateDataKeyValue ("description", description);
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
			 * Carga de un fichero. Operacion no soportada.
			 * Implementada en el applet Java de firma.
			 */
			function getFileNameContentBase64 (title, extensions, description) {
				throwException(UnsupportedOperationException, "La operacion de carga de ficheros no esta soportada");
			}

			/**
			 * Carga de multiples ficheros. Operacion no soportada.
			 * Implementada en el applet Java de firma.
			 */
			function getMultiFileNameContentBase64 (title, extensions, description) {
				throwException(UnsupportedOperationException, "La operacion de carga de multiples ficheros no esta soportada");
			}

			/**
			 * Funcion para la comprobacion de existencia del objeto. No hace nada.
			 * Implementada en el applet Java de firma.
			 */
			function echo () {
				return "Cliente JavaScript";
			}

			/**
			 * No hace nada.
			 * Implementada en el applet Java de firma.
			 */
			function setStickySignatory (sticky) {
				// No hace nada
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
			 * Recupera el log de la aplicacion. Actualmente, el log solo esta
			 * disponible en el applet, no en las aplicacion moviles.
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
		 * Objeto JavaScript que va a reemplazar al cliente de firma en los entornos en los que
		 * no pueden ejecutarse applets.
		 */
		var AppAfirmaJSWebService = ( function (clientAddress, window, undefined) {

			var UnsupportedOperationException = "java.lang.UnsupportedOperationException";


			/* Longitud maxima de una URL en Android para la invocacion de una aplicacion nativa. */
			var MAX_LONG_ANDROID_URL = 2000;

			/* Longitud maxima de una URL en iOS para la invocacion de una aplicacion nativa. */
			var MAX_LONG_IOS_URL = 80000;

			/* Longitud maxima de una URL en Windows 8 para la invocacion de una aplicacion nativa. */
			var MAX_LONG_WINDOWS8_URL = 2000;

			/* Longitud maxima que generalmente se permite a una URL. */
			var MAX_LONG_GENERAL_URL = 2000;

			/**
			 *  Atributos para la configuracion del objeto sustituto del applet Java de firma
			 */
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
			 * Implementada en el applet Java de firma
			 */
			function selectCertificate (extraParams, successCallback, errorCallback) {
				throwException(UnsupportedOperationException, "La operacion de seleccion de certificados no esta soportada");
			}

			/**
			 * Inicia el proceso de firma electronica.
			 * Implementada en el applet Java de firma
			 */
			function sign (dataB64, algorithm, format, extraParams, successCallback, errorCallback) {
				signOperation("sign", dataB64, algorithm, format, extraParams, successCallback, errorCallback);
			}

			/**
			 * Inicia el proceso de cofirma de una firma electr&oacute;nica.
			 * Implementada en el applet Java de firma.
			 */
			function coSign (signB64, dataB64, algorithm, format, extraParams, successCallback, errorCallback) {
				signOperation("cosign", signB64, algorithm, format, extraParams, successCallback, errorCallback);
			}

			/**
			 * Inicia el proceso de contrafirma de una firma electr&oacute;nica.
			 * Implementada en el applet Java de firma.
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

				if (dataB64 == undefined || dataB64 == "") {
					dataB64 = null;
				}

				if (dataB64 != null && !isValidUrl(dataB64)) {
					dataB64 = dataB64.replace(/\+/g, "-").replace(/\//g, "_");
				}

				var idSession = generateNewIdSession();
				var cipherKey = generateCipherKey();

				var i = 0;
				var params = new Array();
				if (signId != null && signId != undefined) {			params[i++] = {key:"op", value:signId}; }
				if (idSession != null && idSession != undefined) {		params[i++] = {key:"id", value:idSession}; }
				if (cipherKey != null && cipherKey != undefined) {		params[i++] = {key:"key", value:cipherKey}; }
				if (defaultKeyStore != null &&
						defaultKeyStore != undefined) {					params[i++] = {key:"keystore", value:defaultKeyStore}; }
				if (storageServletAddress != null &&
						storageServletAddress != undefined) {			params[i++] = {key:"stservlet", value:storageServletAddress}; }
				if (format != null && format != undefined) {			params[i++] = {key:"format", value:format}; }
				if (algorithm != null && algorithm != undefined) {		params[i++] = {key:"algorithm", value:algorithm}; }
				if (extraParams != null && extraParams != undefined) { 	params[i++] = {key:"properties", value:Base64.encode(extraParams)}; }
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

				if (dataB64 == undefined || dataB64 == "") {
					dataB64 = null;
				}

				if (dataB64 != null && !isValidUrl(dataB64)) {
					dataB64 = dataB64.replace(/\+/g, "-").replace(/\//g, "_");
				}

				var idSession = generateNewIdSession();
				var cipherKey = generateCipherKey();

				var i = 0;
				var opId = "signandsave";
				var params = new Array();

				params[i++] = {key:"op", value:opId};
				if (signId != null && signId != undefined) {			params[i++] = {key:"cop", value:signId}; }
				if (idSession != null && idSession != undefined) {		params[i++] = {key:"id", value:idSession}; }
				if (cipherKey != null && cipherKey != undefined) {		params[i++] = {key:"key", value:cipherKey}; }
				if (defaultKeyStore != null &&
						defaultKeyStore != undefined) {					params[i++] = {key:"keystore", value:defaultKeyStore}; }
				if (storageServletAddress != null &&
						storageServletAddress != undefined) {			params[i++] = {key:"stservlet", value:storageServletAddress}; }
				if (format != null && format != undefined) {			params[i++] = {key:"format", value:format}; }
				if (algorithm != null && algorithm != undefined) {		params[i++] = {key:"algorithm", value:algorithm}; }
				if (extraParams != null && extraParams != undefined) { 	params[i++] = {key:"properties", value:Base64.encode(extraParams)}; }
				if (outputFileName != null &&
						outputFileName != undefined) {					params[i++] = {key:"filename", value:outputFileName}; }
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

				if (batchB64 == undefined || batchB64 == "") {
					batchB64 = null;
				}

				if (batchB64 != null && !isValidUrl(batchB64)) {
					batchB64 = batchB64.replace(/\+/g, "-").replace(/\//g, "_");
				}

				var idSession = generateNewIdSession();
				var cipherKey = generateCipherKey();

				var opId = "batch";

				var i = 0;
				var params = new Array();
				params[i++] = {key:"op", value:opId};
				if (idSession != null && idSession != undefined) {		params[i++] = {key:"id", value:idSession}; }
				if (cipherKey != null && cipherKey != undefined) {		params[i++] = {key:"key", value:cipherKey}; }
				if (defaultKeyStore != null &&
						defaultKeyStore != undefined) {					params[i++] = {key:"keystore", value:defaultKeyStore}; }
				if (storageServletAddress != null &&
						storageServletAddress != undefined) {			params[i++] = {key:"stservlet", value:storageServletAddress}; }
				if (batchPreSignerUrl != null &&
						batchPreSignerUrl != undefined) {				params[i++] = {key:"batchpresignerurl", value:batchPreSignerUrl}; }
				if (batchPostSignerUrl != null &&
						batchPostSignerUrl != undefined) {				params[i++] = {key:"batchpostsignerurl", value:batchPostSignerUrl}; }
				if (extraParams != null && extraParams != undefined) { 	params[i++] = {key:"properties", value:Base64.encode(extraParams)}; }
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
			 * Convierte texto plano en texto base 64.
			 * Implementada en el applet Java de firma.
			 */
			function getBase64FromText (plainText, charset) {
				return plainText != null ? Base64.encode(plainText) : null ;
			}

			/**
			 * Convierte texto base 64 en texto plano.
			 * Implementada en el applet Java de firma.
			 */
			function getTextFromBase64 (base64Text, charset) {
				return Base64.decode(base64Text);
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
				params[i++] = {key:"op", value:opId};
				if (idSession != null && idSession != undefined) {		params[i++] = {key:"id", value:idSession}; }
				if (cipherKey != null && cipherKey != undefined) {		params[i++] = {key:"key", value:cipherKey}; }
				if (storageServletAddress != null &&
						storageServletAddress != undefined) {			params[i++] = {key:"stservlet", value:storageServletAddress}; }
				if (title != null && title != undefined) {				params[i++] = {key:"title", value:title}; }
				if (filename != null && filename != undefined) {		params[i++] = {key:"filename", value:filename}; }
				if (extension != null && extension != undefined) {		params[i++] = {key:"extension", value:extension}; }
				if (description != null && description != undefined) {	params[i++] = {key:"description", value:description}; }
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
			 * Carga de un fichero. Operacion no soportada.
			 * Implementada en el applet Java de firma.
			 */
			function getFileNameContentBase64 (title, extensions, description) {
				throwException(UnsupportedOperationException, "La operacion de carga de ficheros no esta soportada");
			}

			/**
			 * Carga de multiples ficheros. Operacion no soportada.
			 * Implementada en el applet Java de firma.
			 */
			function getMultiFileNameContentBase64 (title, extensions, description) {
				throwException(UnsupportedOperationException, "La operacion de carga de multiples ficheros no esta soportada");
			}

			/**
			 * Funcion para la comprobacion de existencia del objeto. No hace nada.
			 * Implementada en el applet Java de firma.
			 */
			function echo () {
				return "Cliente JavaScript";
			}

			/**
			 * No hace nada.
			 * Implementada en el applet Java de firma.
			 */
			function setStickySignatory (sticky) {
				// No hace nada
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
			 * Recupera el log de la aplicacion. Actualmente, el log solo esta
			 * disponible en el applet, no en las aplicacion moviles.
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
				if (isAndroid()) {
					return url.length > MAX_LONG_ANDROID_URL;
				}
				else if (isIOS()) {
					return url.length > MAX_LONG_IOS_URL;
				}
				else if (isWindows8()) {
					return url.length > MAX_LONG_WINDOWS8_URL;
				}
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
							errorCallback("java.lang.IOException", "Ocurrio un error al enviar los datos a la aplicacion nativa");
						}
					}
				}

				httpRequest.onerror = function(e) {
					errorCallback("java.lang.IOException", "Ocurrio un error al enviar los datos al servicio intermedio para la comunicacion con la aplicacion nativa");
				}

				var requestData =
					"op=put&v=1_0&id=" + fileId + "&dat=" +
					cipher(buildXML(op, params), cipherKey);

				try {
					httpRequest.send(requestData);
				}
				catch(e) {
					errorMessage = "No se pudo conectar con el servidor remoto";
					errorType = "java.io.IOException";
				}
			}

			/**
			 * Invoca un Intent con la operacion seleccionada, la configuraci\u00F3n establecida y las campos del
			 * formulario pasado como parametro. Si se define un callback para tratar el caso de exito o error de
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

				// Invocamos al cliente de firma movil.
				if(window.protocolCheck&&!isIOS()&&!isAndroid()&&!forceAFirma){
//				  console.log("Probamos afirma://");
	//			  window.protocolCheck(intentURL, function(){
					  console.log("Probamos jnlp://");
					  window.protocolCheck(jnlpServiceAddress+'?cadenaFirma='+encodeURIComponent(intentURL),
							  function () {
							  	console.log("Probamos http://");
							  	bJNLP = false;
						        openUrl(jnlpServiceAddress.replace("jnlp","https")+'?cadenaFirma='+encodeURIComponent(intentURL), errorCallback);
						      });
		//			  });
				}else{
					bJNLP = false;
					try {
						openUrl(intentURL, errorCallback);
					}
					catch (e) {
						//console.log("Error al abrir la aplicacion nativa: " + e);
						return;
					}
				}

				if (successCallback != null || errorCallback != null) {
					if (idSession != null && idSession != undefined &&
							((successCallback != undefined && successCallback != null) ||
									(errorCallback != undefined && errorCallback != null))) {
						getStoredFileFromServlet(idSession, retrieverServletAddress, cipherKey, successCallback, errorCallback);
					}
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
				if (isChrome() && isAndroid()) {
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
			 * Llama a la aplicacion de firma a traves de la URL de invocacion sin que afecte
			 * a la pagina que se esta mostrando.
			 * @param url URL de invocacion.
			 * @param errorCallback Funcion de error que deberia lanzarse (ademas de una excepcion),
			 * si no fuese posible abrir la URL. Puede ser nulo.
			 */
			function openUrl (url, errorCallback) {

				// Usamos el modo de invocacion mas apropiado segun el entorno
				if (isChrome() || isIOS()) {
					// Usamos document.location porque tiene mejor soporte por los navegadores que
					// window.location que es el mecanismo estandar
					document.location = url;
				}
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
								function() {
									openUrlWithIframe(url);
								},
								function() {
									if (errorCallback != null && errorCallback != undefined) {
										errorCallback("es.gob.afirma.standalone.ApplicationNotFoundException", "No se ha podido conectar con AutoFirma.");
										throw new Error();
									}
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

				document.body.appendChild(iframeElem);
			}

			/**
			 * Ejecuta el metodo de error si el html recuperado es tal o el metodo de exito si no lo es,
			 * en cuyo caso previamente descifrara el resultado.
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

				// Si se obtiene otro mensaje de error, se deja de intentar y se ejecuta la funcion callback de error
				if (html.substr(0, 4).toLowerCase() == "err-" && html.indexOf(":=") != -1) {
					errorMessage = html.substring(html.indexOf(":=") + 2);
					errorType = "java.lang.Exception";
					errorCallback(errorType, errorMessage);
					return false;
				}

				// Se ha cancelado la operacion
				if (html.indexOf("CANCEL")!= -1) {
					errorCallback("es.gob.afirma.core.AOCancelledOperationException", "Operacion cancelada por el usuario");
					return false;
				}

				// Se ha producido un error
				if (html.length > 4 && html.substr(0, 4) == "SAF_") {
					errorCallback("java.lang.Exception", html);
					return false;
				}

				// Si no se obtuvo un error, habremos recibido la firma y posiblemente el certificado (que antecederia a la
				// firma y se separaria de ella con '|'). Si se definio una clave de cifrado, consideramos que la firma
				// y el certificado (en caso de estar) llegan cifrados. El cifrado de ambos elementos es independiente
				var certificate;
				var signature;
				var sepPos = html == null ? -1 : html.indexOf('|');
				var multiSepPos = html == null ? -1 : html.indexOf(':');
				if (sepPos == -1) {
					if(multiSepPos == -1){
						if (cipherKey != undefined && cipherKey != null) {
							signature = decipher(html, cipherKey);
						}
						else {
							signature = fromBase64UrlSaveToBase64(html);
						}
					}else{
						if (cipherKey != undefined && cipherKey != null) {
							var signatures = html.split(":");
							signature = "";
							for(var i = 0; i<signatures.length; i++){
								if(i>0){
									signature = signature + ":";
								}
								var notLast =  i!=signatures.length-1;
								signature = signature + decipher(signatures[i], cipherKey, notLast);
							}
						}
						else {
							signature = fromBase64UrlSaveToBase64(html);
						}
					}
				}
				else {
					if(multiSepPos == -1){
						if (cipherKey != undefined && cipherKey != null) {
							certificate = decipher(html.substring(0, sepPos), cipherKey, true);
							signature = decipher(html.substring(sepPos + 1), cipherKey);
							certificate = certificate.replace(/\-/g, "+").replace(/\_/g, "/");
							signature = signature.replace(/\-/g, "+").replace(/\_/g, "/");

							//XXX: Solucion provisional derivada de un error de compatibilidad entre
							// las funciones de cifrado de Java y la clase de cifrado utilizada.

							// Cuando se aprecia que el resultado es una firma XML, nos aseguramos de
							// que no haya basura al final buscando el ultimo cierre de etiqueta entre
							// los ultimos caracteres. Si este existe y no es el ultimo, cortamos ahi.
							var i = signature.substring(signature.length - 10).lastIndexOf(">");
							if (i != -1 && i != 9) {
								signature = signature.substring(0, signature.length - 10 + i + 1);
							}
						}
						else {
							certificate = fromBase64UrlSaveToBase64(html.substring(0, sepPos));
							signature = fromBase64UrlSaveToBase64(html.substring(sepPos + 1));
						}
					}else{
						if (cipherKey != undefined && cipherKey != null) {
							certificate = decipher(html.substring(0, sepPos), cipherKey, true);
							var signatures = html.substring(sepPos + 1).split(":");
							signature = "";
							for(var i = 0; i<signatures.length; i++){
								if(i>0){
									signature = signature + ":";
								}
								var notLast =  i!=signatures.length-1;
								signature = signature + decipher(signatures[i], cipherKey, notLast);
							}

						}
						else {
							certificate = fromBase64UrlSaveToBase64(html.substring(0, sepPos));
							signature = fromBase64UrlSaveToBase64(html.substring(sepPos + 1));
						}
					}
				}

				successCallback(signature, certificate);

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

			var NUM_MAX_ITERATIONS = 15;
			var iterations = 0;

			function getStoredFileFromServlet (idDocument, servletAddress, cipherKey, successCallback, errorCallback) {

				var httpRequest = getHttpRequest();
				if (!httpRequest) {
					throwException("java.lang.Exception", "Su navegador no permite obtener el resulado de la operaci\u00F3n");
				}

				iterations = 0;
				setTimeout(retrieveRequest, 4000, httpRequest, servletAddress, "op=get&v=1_0&id=" + idDocument + "&it=0", cipherKey, successCallback, errorCallback);
			}

			function retrieveRequest(httpRequest, url, params, cipherKey, successCallback, errorCallback) {

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
								setTimeout(retrieveRequest, 4000, httpRequest, url, params.replace("&it=" + (iterations-1), "&it=" + iterations), cipherKey, successCallback, errorCallback);
							}
						}
						else {
							errorResponseFunction(null, httpRequest.responseText, errorCallback);
						}
					}
				}

				httpRequest.onerror = function() {
					errorResponseFunction("java.lang.Exception", "No se pudo conectar con el servidor intermedio para la recuperacion del resultado de la operacion", errorCallback);
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
			 * para descifrar.
			 * Como resultado devuelve la cadena de texto descifrada en base 64.
			 */
			function decipher(cipheredData, key, notlast) {
								
				var dotPos = cipheredData.indexOf('.');
				var padding = cipheredData.substr(0, dotPos);
				
				var deciphered = Cipher.des(key, Cipher.base64ToString(fromBase64UrlSaveToBase64(cipheredData.substr(dotPos + 1))), 0, 0, null);
				/*if(padding==0)
					notlast = true;*/
				return Cipher.stringToBase64(deciphered.substr(0, deciphered.length - padding - (notlast?0:8)));

//				return Cipher.stringToBase64(deciphered.substr(0, deciphered.length - (padding == 0 ? 8 : padding) - (notlast?0:8)));
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
				setServlets : setServlets,
				setJnlpService: setJnlpService,
				setStickySignatory : setStickySignatory,
				setLocale : setLocale,
				getErrorMessage : getErrorMessage,
				getErrorType : getErrorType,
				getCurrentLog : getCurrentLog
			}
		});

		/* Metodos que publicamos del objeto MiniApplet */
		return {

			VERSION : VERSION,

			/* Publicamos las variables para la comprobacion de hora. */
			CHECKTIME_NO : CHECKTIME_NO,
			CHECKTIME_RECOMMENDED : CHECKTIME_RECOMMENDED,
			CHECKTIME_OBLIGATORY : CHECKTIME_OBLIGATORY,

			/* Publicamos las variables para establecer parametros personalizados en la JVM */
			JAVA_ARGUMENTS : JAVA_ARGUMENTS,
			SYSTEM_PROPERTIES : SYSTEM_PROPERTIES,

			/* Publicamos las variables para configurar un almacen de certificados concreto. */
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

			/* Constantes para la deteccion de la aplicacion de autofirma */
			AUTOFIRMA_LAUNCHING_TIME : AUTOFIRMA_LAUNCHING_TIME,
			AUTOFIRMA_CONNECTION_RETRIES : AUTOFIRMA_CONNECTION_RETRIES,

			/* Variable para forzar el uso del mecanismo de comunicacion por servidor intermedio */
			setForceWSMode : setForceWSMode,
			setForceAFirma : setForceAFirma,


			/* Metodos visibles. */
			cargarMiniApplet : cargarMiniApplet,
			cargarAppAfirma : cargarAppAfirma,
			echo : echo,
			checkTime : checkTime,
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
			downloadRemoteData : downloadRemoteData,
			setKeyStore : setKeyStore,
			setServlets : setServlets,
			setJnlpService: setJnlpService,
			setStickySignatory : setStickySignatory,
			setLocale : setLocale,
			getErrorMessage : getErrorMessage,
			getErrorType : getErrorType,
			getCurrentLog : getCurrentLog,
			isAndroid : isAndroid,
			isIOS : isIOS,
			isJNLP : isJNLP
		};
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
var Cipher = {
	tableStr : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
	tableStr_URL_SAFE : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_",

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
		return Cipher.hexToBase64(Cipher.stringToHex(s));
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

	stringToHex : function (s) {
		var r = "";
		var hexes = new Array ("0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f");
		for (var i=0; i<s.length; i++) {r += hexes [s.charCodeAt(i) >> 4] + hexes [s.charCodeAt(i) & 0xf];}
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
	},

	hexToBase64 : function(str, urlSafe) {
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

		return Cipher.btoa(byteString, urlSafe);
	}
};

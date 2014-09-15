
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

var Handwritten = {

		JAR_NAME : 'atos_firma2e.jar',

		APPLET_ID : 'handwritten',
		
		clienteFirma : null,

		codeBase : null,
		
		loaded : false,

		/* ------------------------------------ */
		/* Funciones de comprobacion de entorno */
		/* ------------------------------------ */

		/**
		 * Determina con un boolean si nuestro cliente es Android
		 */
		isAndroid : function () {
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
		},

		/**
		 * Determina con un boolean si nuestro cliente es iOS.
		 */
		isIOS : function () {
			return (navigator.userAgent.toUpperCase().indexOf("IPAD") != -1) ||
			(navigator.userAgent.toUpperCase().indexOf("IPOD") != -1) ||
			(navigator.userAgent.toUpperCase().indexOf("IPHONE") != -1);
		},

		/** Comprueba si se permite la ejecucion de ActiveX. */
		isActivexEnabled : function () {
			var supported = null;
			try {
				supported = !!new ActiveXObject("htmlfile");
			} catch (e) {
				supported = false;
			}

			return supported;
		},

		/** Determina con un boolean si nos encontramos en Windows 8/8.1 */
		isWindows8 : function () {
			return navigator.userAgent.indexOf("Windows NT 6.2") != -1 ||	/* Windows 8 */
				navigator.userAgent.indexOf("Windows NT 6.3") != -1;		/* Windows 8.1 */
		},
		
		/** Determina con un boolean si nos encontramos en Windows RT */
		isWindowsRT : function () {
			return Handwritten.isWindows8() && navigator.userAgent.indexOf("ARM;") != -1;
		},

		/** Determina con un boolean si estamos en Internet Explorer */
		isInternetExplorer : function () {
			return !!(navigator.userAgent.match(/MSIE/))	/* Internet Explorer 10 o inferior */
					|| !!(navigator.userAgent.match(/Trident/) && navigator.userAgent.match(/rv:11/)); /* Internet Explorer 11 o superior */
		},
		
		/** Determina con un boolean si nos encontramos en un entorno Windows 8 en modo "Modern UI".
		 * Este metodo no es infalible dado que el navegador no ofrece forma de saberlo.
		 * La comprobacion . */
		isWindows8ModernUI : function () {
			return Handwritten.isWindows8() && !Handwritten.isActivexEnabled() && Handwritten.isInternetExplorer();
		},

		/**
		 * Determina con un boolean si se accede a la web con Chrome
		 */
		isChrome : function () {
			return navigator.userAgent.toUpperCase().indexOf("CHROME") != -1 ||
				navigator.userAgent.toUpperCase().indexOf("CHROMIUM") != -1;
		},
		
		/**
		 * Determina con un boolean si el navegador es Internet Explorer 10
		 */
		isIE10 : function () {
			return navigator.userAgent.toUpperCase().indexOf("MSIE 10.0") != -1;
		},
		
		/**
		 * Determina con un boolean si el navegador es Internet Explorer 11
		 */
		isIE11 : function () {
			return !!navigator.userAgent.match(/Trident.*rv 11\./);
		},

		/** Indica si el navegador detecta Java. Este valor no es completamente fiable, ya que
		 * Internet Explorer siempre indica que si esta activado. */
		isJavaEnabled : function () {
			return navigator.javaEnabled();
		},
		
		/** Carga el Handwritten. */
		cargarApplet : function (base) {

			// Si estamos claramente en un sistema movil o que no permite la ejecucion de Java,
			// cargamos directamente el Cliente JavaScript
			if (Handwritten.isAndroid() || Handwritten.isIOS() || Handwritten.isWindowsRT()) {
				//alert("Este ejemplo solo es compatible con dispositivos de sobremesa");
				return;
			}

			// Si estamos en un entorno que permite Java, comprobamos si esta disponible
			// y en caso de no estarlo, tambien cargamos el Cliente JavaScript.
			if (!Handwritten.isJavaEnabled()) {
				//alert("Este ejemplo solo es compatible con dispositivos de sobremesa");
				return;
			}

			// Incluso si el navegador informa que hay Java, puede no haberlo (Internet Explorer
			// siempre dice que hay), asi que cargamos el applet
			Handwritten.codeBase = (base != undefined && base != null) ? base : './';
			
			var attributes = {
					id: Handwritten.APPLET_ID,
					name: 'Atos Firma2E',
					type: 'application/x-java-applet',
					width: 1,
					height: 1
			};
			
			// Los argumentos de java no llegan al propio applet en las pruebas con Java 6 y 7,
			// asi que (salvo los argumentos de carga) vamos a pasarlos como un parametro mas al
			// applet para luego establecerlos internamente.
			var parameters = {
					archive: Handwritten.codeBase + '/' + Handwritten.JAR_NAME,
					code: 'es.gob.afirma.crypto.handwritten.SignatureApplet',
					java_arguments: '-Xms512M -Xmx512M',
					codebase_lookup: false,
					separate_jvm: true
			};

			this.loadApplet(attributes, parameters);

			Handwritten.clienteFirma = document.getElementById(Handwritten.APPLET_ID);
		},
		
		signWithHtml : function (docUrl, storeServiceUrl, htmlTemplate, signaturePadRectX, signaturePadRectY, signaturePadRectWidth, signaturePadRectHeight, publicKeyB64, keyDn, signerName, signerSurname1, signerSurname2, signerId, extraParams) {
			
			this.forceLoad();
			Handwritten.clienteFirma.signWithHtml(docUrl, storeServiceUrl, htmlTemplate, signaturePadRectX, signaturePadRectY, signaturePadRectWidth, signaturePadRectHeight, publicKeyB64, keyDn, signerName, signerSurname1, signerSurname2, signerId, extraParams);
		},
		
		signWithImage : function (docUrl, storeServiceUrl, jpegImage, signaturePadRectX, signaturePadRectY, signaturePadRectWidth, signaturePadRectHeight, publicKeyB64, keyDn, signerName, signerSurname1, signerSurname2, signerId, extraParams) {
			
			this.forceLoad();
			Handwritten.clienteFirma.signWithImage(docUrl, storeServiceUrl, jpegImage, signaturePadRectX, signaturePadRectY, signaturePadRectWidth, signaturePadRectHeight, publicKeyB64, keyDn, signerName, signerSurname1, signerSurname2, signerId, extraParams);
		},

		echo : function () {
			this.forceLoad();
			return Handwritten.clienteFirma.echo();
		},

		getErrorMessage : function () {
			this.forceLoad();
			return Handwritten.clienteFirma.getErrorMessage();
		},

		/*************************************************************
		 *  FUNCIONES PARA EL DESPLIEGUE DEL APPLET					 *
		 **************************************************************/
		
		loadApplet : function (attributes, parameters) {
			// Internet Explorer (a excepcion de la version 10) se carga mediante un
			// elemento <object>. El resto con un <embed>.
			if (Handwritten.isInternetExplorer()) { // && !Handwritten.isIE10()) {
				
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

				// Al agregar con append() estos nodos no se carga automaticamente el applet en IE10 e inferiores, así que
				// hay que usar document.write() o innerHTML. Para asegurarnos de no pisar HTML previo, crearemos un <div>
				// en la pagina, lo recogeremos e insertaremos dentro suyo el codigo del applet.
				var divElem = document.createElement("div");
				var idAtt = document.createAttribute("id");
				idAtt.value = 'divFirma2EApplet';
				divElem.setAttributeNode(idAtt);

				document.body.appendChild(divElem);
				
				document.getElementById("divFirma2EApplet").innerHTML = appletTag;
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
							// Probamos este como alternativa en caso de error.Solo detectado en IE10
							// sin modo de compabilidad con el Document Mode de IE7. Este intento no
							// soluciona el error, pero evita que se propague 
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
		},
		
		/**
		 * Establece los datos que debera procesar el applet Handwritten. 
		 */
		forceLoad : function () {

			if (!Handwritten.loaded) {
				if (Handwritten.clienteFirma == null) {
					Handwritten.clienteFirma = document.getElementById(Handwritten.APPLET_ID);
				}
				try {
					Handwritten.clienteFirma.echo();
					Handwritten.loaded = true;
				} catch (e) {
					// No se pudo cargar el applet
				}
			}
		}
};
		

var TestEnvironment = {
		 
	/** Identifica el entorno de ejecucion (sistema operativo y su version, navegador y su version,
	 * version de Java...) y configura esta informacion en la variable "jscd" de la ventana.
	 *  - window: Ventana actual. */
	identify : function (window) {

        var unknown = '-';

        // screen
        var screenSize = '';
        if (screen.width) {
            width = (screen.width) ? screen.width : '';
            height = (screen.height) ? screen.height : '';
            screenSize += '' + width + " x " + height;
        }

        //browser
       var nVer = navigator.appVersion;
       var nAgt = navigator.userAgent;
       var browser = navigator.appName;
       var version = '' + parseFloat(navigator.appVersion);
       var majorVersion = parseInt(navigator.appVersion, 10);
       var nameOffset, verOffset, ix;

        // Opera
        if ((verOffset = nAgt.indexOf('Opera')) != -1) {
            browser = 'Opera';
            version = nAgt.substring(verOffset + 6);
            if ((verOffset = nAgt.indexOf('Version')) != -1) {
                version = nAgt.substring(verOffset + 8);
            }
        }
        // MSIE
        else if ((verOffset = nAgt.indexOf('MSIE')) != -1) {
            browser = 'Microsoft Internet Explorer';
            version = nAgt.substring(verOffset + 5);
        }
		  // Edge
        else if ((verOffset = nAgt.indexOf('Edge/')) != -1) {
            browser = 'Microsoft Edge';
			version = nAgt.substring(verOffset + 5, verOffset + 7);
		}
        // Chrome
        else if ((verOffset = nAgt.indexOf('Chrome')) != -1) {
            browser = 'Chrome';
            version = nAgt.substring(verOffset + 7);
        }
        // Safari
        else if ((verOffset = nAgt.indexOf('Safari')) != -1) {
            browser = 'Safari';
            version = nAgt.substring(verOffset + 7);
            if ((verOffset = nAgt.indexOf('Version')) != -1) {
                version = nAgt.substring(verOffset + 8);
            }
        }
        // Firefox
        else if ((verOffset = nAgt.indexOf('Firefox')) != -1) {
            browser = 'Firefox';
            version = nAgt.substring(verOffset + 8);
        }
        // MSIE 11+
        else if (nAgt.indexOf('Trident/') != -1) {
            browser = 'Microsoft Internet Explorer';
            version = nAgt.substring(nAgt.indexOf('rv:') + 3);
        }
		 // Other browsers
        else if ((nameOffset = nAgt.lastIndexOf(' ') + 1) < (verOffset = nAgt.lastIndexOf('/'))) {
            browser = nAgt.substring(nameOffset, verOffset);
            version = nAgt.substring(verOffset + 1);
            if (browser.toLowerCase() == browser.toUpperCase()) {
                browser = navigator.appName;
            }
        }
        // trim the version string
        if ((ix = version.indexOf(';')) != -1) version = version.substring(0, ix);
        if ((ix = version.indexOf(' ')) != -1) version = version.substring(0, ix);
        if ((ix = version.indexOf(')')) != -1) version = version.substring(0, ix);

        majorVersion = parseInt('' + version, 10);
        if (isNaN(majorVersion)) {
            version = '' + parseFloat(navigator.appVersion);
            majorVersion = parseInt(navigator.appVersion, 10);
        }

        // mobile version
        var mobile = /Mobile|mini|Fennec|Android|iP(ad|od|hone)/.test(nVer);

        // cookie
        var cookieEnabled = (navigator.cookieEnabled) ? true : false;

        if (typeof navigator.cookieEnabled == 'undefined' && !cookieEnabled) {
            document.cookie = 'testcookie';
            cookieEnabled = (document.cookie.indexOf('testcookie') != -1) ? true : false;
        }

        // system
        var os = unknown;
        var clientStrings = [
            {s:'Windows 3.11', r:/Win16/},
            {s:'Windows 95', r:/(Windows 95|Win95|Windows_95)/},
            {s:'Windows ME', r:/(Win 9x 4.90|Windows ME)/},
            {s:'Windows 98', r:/(Windows 98|Win98)/},
            {s:'Windows CE', r:/Windows CE/},
            {s:'Windows 2000', r:/(Windows NT 5.0|Windows 2000)/},
            {s:'Windows XP', r:/(Windows NT 5.1|Windows XP)/},
            {s:'Windows Server 2003', r:/Windows NT 5.2/},
            {s:'Windows Vista', r:/Windows NT 6.0/},
            {s:'Windows 7', r:/(Windows 7|Windows NT 6.1)/},
            {s:'Windows 8.1', r:/(Windows 8.1|Windows NT 6.3)/},
            {s:'Windows 8', r:/(Windows 8|Windows NT 6.2)/},
			{s:'Windows 10', r:/(Windows 10|Windows NT 10.0)/},
            {s:'Windows NT 4.0', r:/(Windows NT 4.0|WinNT4.0|WinNT|Windows NT)/},
            {s:'Windows ME', r:/Windows ME/},
            {s:'Android', r:/Android/},
            {s:'Open BSD', r:/OpenBSD/},
            {s:'Sun OS', r:/SunOS/},
            {s:'Linux', r:/(Linux|X11)/},
            {s:'iOS', r:/(iPhone|iPad|iPod)/},
            {s:'Mac OS X', r:/Mac OS X/},
            {s:'Mac OS', r:/(MacPPC|MacIntel|Mac_PowerPC|Macintosh)/},
            {s:'QNX', r:/QNX/},
            {s:'UNIX', r:/UNIX/},
            {s:'BeOS', r:/BeOS/},
            {s:'OS/2', r:/OS\/2/},
            {s:'Search Bot', r:/(nuhk|Googlebot|Yammybot|Openbot|Slurp|MSNBot|Ask Jeeves\/Teoma|ia_archiver)/}
        ];
        for (var id in clientStrings) {
            var cs = clientStrings[id];
            if (cs.r.test(nAgt)) {
                os = cs.s;
                break;
            }
        }

        var osVersion = unknown;

        if (/Windows/.test(os)) {
            osVersion = /Windows (.*)/.exec(os)[1];
            os = 'Windows';
        }

        switch (os) {
            case 'Mac OS X':
                osVersion = /Mac OS X (10[\.\_\d]+)/.exec(nAgt)[1];
                break;

            case 'Android':
                osVersion = /Android ([\.\_\d]+)/.exec(nAgt)[1];
                break;

            case 'iOS':
                osVersion = /OS (\d+)_(\d+)_?(\d+)?/.exec(nVer);
                osVersion = osVersion[1] + '.' + osVersion[2] + '.' + (osVersion[3] | 0);
                break;
        }

        // flash (you'll need to include swfobject)
        /* script src="//ajax.googleapis.com/ajax/libs/swfobject/2.2/swfobject.js" */
        var flashVersion = 'no check';
        if (typeof swfobject != 'undefined') {
            var fv = swfobject.getFlashPlayerVersion();
            if (fv.major > 0) {
                flashVersion = fv.major + '.' + fv.minor + ' r' + fv.release;
            }
            else  {
                flashVersion = unknown;
            }
        }

        // Calculamos la version de Java
        var javaVersion = TestEnvironment.getJavaVersionFromPlugin();

	    window.jscd = {
	        screen: screenSize,
	        browser: browser,
	        browserVersion: version,
	        mobile: mobile,
	        os: os,
	        osVersion: osVersion,
	        cookies: cookieEnabled,
	        flashVersion: flashVersion,
	        javaVersion: javaVersion,
	        javaArch: null
	    };
	},
 
	/** Obtiene la version de Java a partir de la biblioteca deployJava que se comunica con el Java Plugin. */
	getJavaVersionFromPlugin : function () {
		try {
			var maxVersion = "0";
			var JREs = deployJava.getJREs();
			if (JREs != undefined && JREs != null) {
				for (var i = 0; i < JREs.length; i++) {
					maxVersion = TestEnvironment.calculateMaxJavaVersion(maxVersion, JREs[i]);
				}
			}
			return maxVersion;
		}
		catch (e) {
			// En caso de no poder extraer la version de Java a traves del plugin, se establece a '0'
			return "0";
		}
	},

	/** Actualiza la version de Java configurada en Obtiene la version de Java configurada en "jscd"
	 *  con la version notificada por el propio applet ya cargado.
	 *  - Informacion devuelta por el metodo echo() del applet. */
	updateJavaInfoFromApplet : function (echo) {
		
		var header = "Java version: ";
		if (echo == undefined || echo == null || echo.indexOf(header) == -1) {
			return null;
		}
		
		var i = echo.indexOf(header);
		var j = echo.indexOf('\n', i);

		jscd.javaVersion = this.trim(echo.substring(i + header.length, j));
		
		i = echo.indexOf(':', j) + 1;
		
		jscd.javaArch = this.trim(echo.substring(i));
	},
	
	/** Funcion trim de cadena, dado que no existe para IE8. */
	trim : function (text) {
		if (text.trim != null) {
			return text.trim();
		}
		var i = 0;
		while (text[i] == ' ' || text[i] == '\t' || text[i] == '\n' || text[i] == '\r') {
			i++;
		}
		var j = text.length;
		while (text[j - 1] == ' ' || text[j - 1] == '\t' || text[j - 1] == '\n' || text[j - 1] == '\r') {
			j--;
		}
		return j <= i ? "" : text.substring(i, j); 
	},
	
	/** Identifica la mayor vesion de Java de entre las proporcionadas. */
	calculateMaxJavaVersion : function (v1, v2) {
		if (v1[0] > v2[0]) {
			return v1;
		}
		else if (v1[0] < v2[0]) {
			return v2;
		}
		if (v1[2] > v2[2]) {
			return v1;
		}
		else if (v1[2] < v2[2]) {
			return v2;
		}
		if (v1[4] > v2[4]) {
			return v1;
		}
		else if (v1[4] < v2[4]) {
			return v2;
		}
		// Si v1 no tiene update, o v2 es mayor o son iguales, asi que devolvemos v2
		if (v1.indexOf('_') == -1) {
			return v2;
		}
		return parseInt(v1.substr(v1.indexOf('_') + 1)) > parseInt(v2.substr(v2.indexOf('_') + 1)) ? v1 : v2;
	}
 };
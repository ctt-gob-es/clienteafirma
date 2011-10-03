/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

/**
 *
 * Version: 2.0.0
 *
 * Depende de deployJava.js y de constantes.js [opcional].
 *
 * Si se ha definido baseDownloadURL se usa como URL base para la descarga de los instalables.
 *
 * cargarAppletFirma(build):
 *      Carga el applet de firma en la variable "clienteFirma". El applet no esta cargado hasta que clienteFirmaCargado==true.
 */

var instalador;
var instaladorCargado = false;
var clienteFirma;

function cargarAppletFirma(build)
{
	/* Si ya esta cargado, no continuamos con el proceso */
	if (clienteFirma != undefined) {
		return;
	}

	/* Definimos las contruccion que se va a utilizar */
	var confBuild = configureBuild(build);

	/* Cargamos el instalador cuando se cumplan las siguiente condiciones:
	 *  - El navegador no sea Google Chrome.
	 *	- El sistema operativo no sea Mac OS X.
	 *  - Se use Java 5 o, en Windows, una JVM de 64 bits.
	 */
	if (deployJava.browserName2 != 'Chrome') {
		if (!isMacOSX()) {
			if (isJava5() || (isWindows() && !isJava32bits())) {
				instalar(confBuild);
			}
		}
	}
	
	var jarArchive = (baseDownloadURL != undefined ? baseDownloadURL : '.') + '/' + confBuild + "_j6_afirma5_core__V3.2.jar";
	if (deployJava.versionCheck('1.6.0+') == false) {
                jarArchive = (baseDownloadURL != undefined ? baseDownloadURL : '.') + '/' + confBuild + "_j5_afirma5_core__V3.2.jar";
	}

	var installercodeBase = base;
	if (installercodeBase == undefined || installercodeBase == null) {
		installercodeBase = '.';
	}
	var codeBase = baseDownloadURL;
	if (codeBase == undefined || codeBase == null) {
		codeBase = '.';
	}

	var attributes = {
		id: 'firmaApplet',
	 	width: 1,
		height: 1
	};
	var parameters = {
		jnlp_href: installercodeBase + "/" + confBuild + '_afirma.jnlp',
		userAgent: window.navigator.userAgent,
		appName: window.navigator.appName,
		showExpiratedCertificates: showExpiratedCertificates,
		showMozillaSmartCardWarning: showMozillaSmartCardWarning,
		code: 'es.gob.afirma.cliente.SignApplet.class',
		archive: jarArchive,
		codebase: codeBase,
		java_arguments: '-Djnlp.versionEnabled=true -Djnlp.packEnabled=true -Xms512M -Xmx512M',
		separate_jvm: true
	};

 	deployJava.runApplet(attributes, parameters, '1.5');

	clienteFirma = document.getElementById("firmaApplet");

	/* Realizamos una espera para que de tiempo a cargarse el applet */
	for (var i = 0; i < 100; i++) {
		try {
			setTimeout("clienteFirma != undefined && clienteFirma.isInitialized()", 100);
			break;
		} catch (e) {
			/*
			 * Capturamos la excepcion que se produciria si no se hubiese cargado aun el applet, aunque no se lanzaria
			 * una vez estuviese cargado aunque no iniciado
			 */
		}
	}
}


function instalar(build)
{
	// Cargamos el applet instalador si no lo esta ya
	if(instalador == undefined)
	{
		cargarAppletInstalador(build);
	}
}


function cargarAppletInstalador(build)
{
	if(instalador == undefined)
	{
		/* Definicion de las constantes necesarias */
		var codeBaseVar = '.';
		if(base != undefined) {
			if(base.toLowerCase().substring(0, 7) != "file://" && 
					base.toLowerCase().substring(0, 7) != "http://" &&
					base.toLowerCase().substring(0, 8) != "https://") {
				codeBaseVar = './' + base;
			}
			else {
				codeBaseVar = base;
			}
		}

		/* Si hay definida una URL desde la que descargar los instalables, la establecemos */
		var baseDownloadVar = '.';
		if(baseDownloadURL != undefined) {
			if(baseDownloadURL.toLowerCase().substring(0, 7) != "file://" && 
					baseDownloadURL.toLowerCase().substring(0, 7) != "http://" &&
					baseDownloadURL.toLowerCase().substring(0, 8) != "https://") {

				var url = document.location.toString();
				baseDownloadVar = url.substring(0, url.lastIndexOf("/")) + '/' + baseDownloadURL;
			}
			else {
				baseDownloadVar = baseDownloadURL;
			}
		}
		
		var attributes = {id:'instaladorApplet',
					code:'es.gob.afirma.install.AfirmaBootLoader.class',	
					archive:codeBaseVar+'/afirmaBootLoader.jar',
					width:1, height:1};
					
		var params = {baseDownloadURL: baseDownloadVar,
					installType: build};
					
		var version = '1.5';

		try {
			deployJava.runApplet(attributes, params, version);
		} catch(e) {}

		instalador = document.getElementById("instaladorApplet");
		
		for(var i=0; i<100; i++) {
			try {
				setTimeout("instalador != undefined", 100);
				instaladorCargado = true;
				break;
			} catch(e) {
				instaladorCargado = false;
				// Capturamos la excepcion que se produciria si no se hubiese cargado aun el applet, aunque no se lanzaria
				// una vez estuviese cargado aunque no iniciado
			}
		}
	}
}


/**
 * Si no se ha indicado una construccion por parametro, ni hay establecida una por defecto en "constantes.js", se instala la 'LITE'
 */
function configureBuild(build)
{
	var confBuild = null;
	if(build != null && build != undefined)
	{
		confBuild = build;
	}
	else if(defaultBuild != null && defaultBuild != undefined) {
		confBuild = defaultBuild;
	}
	else {
		confBuild = 'LITE';
	}
	return confBuild;
}

 function isJava5()
 {
	return deployJava.versionCheck("1.5");
 }

 function isMacOSX()
 {
	return navigator.appVersion.indexOf("Mac") != -1;
 }
 
 function isWindows()
 {
	return navigator.appVersion.indexOf("Win") != -1;
 }
 
function isJava32bits()
{
	var is32BitBrowser = true;
	if( window.navigator.cpuClass != null && window.navigator.cpuClass.toLowerCase() == "x64" ) {
		is32BitBrowser = false;
	}
	if( window.navigator.platform.toLowerCase() == "win64" ) {
		is32BitBrowser = false;
	}
	return is32BitBrowser;
}
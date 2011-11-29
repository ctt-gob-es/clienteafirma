
var clienteFirma;

function cargarMiniApplet()
{
	var installercodeBase = base;
	if (installercodeBase == undefined || installercodeBase == null) {
		installercodeBase = '.';
	}
	var codeBase = baseDownloadURL;
	if (codeBase == undefined || codeBase == null) {
		codeBase = '.';
	}

	var jarArchive = codeBase + '/' + "miniapplet.jar";
	
	var attributes = {
		id: 'miniApplet',
	 	width: 1,
		height: 1
	};
	var parameters = {
		jnlp_href: installercodeBase + '/miniapplet.jnlp',
		userAgent: window.navigator.userAgent,
		appName: window.navigator.appName,
		code: 'es.gob.afirma.miniapplet.MiniAfirmaApplet.class',
		archive: jarArchive,
		codebase: codeBase,
		java_arguments: '-Djnlp.versionEnabled=true -Djnlp.packEnabled=true -Xms512M -Xmx512M',
		separate_jvm: true
	};

 	deployJava.runApplet(attributes, parameters, '1.6');

	clienteFirma = document.getElementById("miniApplet");
}

/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

/**
 * Establece los valores del applet para cifrado. Ver constantes.js.
 */ 
function configuraCifrador()
{
	var command	= "";
	if( cipherAlgorithm != undefined ){
		command	+= "clienteFirma.setCipherAlgorithm('"+cipherAlgorithm+"');";
	}else{
	    command	+= "clienteFirma.setCipherAlgorithm('AES');";
	}
	if (key != undefined){
	    command += "clienteFirma.setKey('"+key+"');";
	}else{
	    if( keyMode != undefined ){
		    command	+= "clienteFirma.setKeyMode('"+keyMode+"');";
	    }else{
	        command	+= "clienteFirma.setKeyMode('AUTOGENERATE');";
	    }
	}

	whenTry("clienteFirmaCargado == true", command);
}

/**
 * Prepara el cliente para cifrar o descifrar.
 */
function initialize(){
	clienteFirma.initialize();
	clienteFirma.setShowErrors(showErrors=='true');
	configuraCifrador();
}

function establecerKey(clave){
    clienteFirma.setKey(clave);
}

function establecerPassword(clave){
	clienteFirma.setPassword(clave);
}

function cifrarDatos(datos){
	clienteFirma.setPlainData(datos);
	clienteFirma.cipherData();
}

function descifrarDatos(datos){
	clienteFirma.setCipherData(datos);
	clienteFirma.decipherData();
}

function cifrarFichero(uri){
	clienteFirma.cipherFile(uri);
}

function obtenerResultadoCifrado(){
	return clienteFirma.getCipherData();
}

function obtenerResultadoPlano(){
	return clienteFirma.getPlainData();
}

function descifrarFichero(uri){
	clienteFirma.decipherFile(uri);
}

function cambiaAlgoritmo(alg){
	clienteFirma.setCipherAlgorithm(alg);
}

function cambiaModoDeClave(modo){
	clienteFirma.setKeyMode(modo);
}

function obtenerAlgoritmo(){
	return clienteFirma.getCipherAlgorithm();
}

function recuperaEnvelopedDataIE(data){
	var EnvelopedData = new ActiveXObject("CAPICOM.EnvelopedData");
	var Utilities = new ActiveXObject("CAPICOM.Utilities");
    EnvelopedData.Decrypt(data);
    var b64ResMSIE=Utilities.Base64Encode(EnvelopedData.Content);
    return clienteFirma.getTextFromBase64(b64ResMSIE);
}

function recuperaEnvelopedDataMozilla(data){
    clienteFirma.setData(data);
	if(clienteFirma.recoverCMS()){
		return clienteFirma.getData();
	}else{
	    alert("Error el la recuperaci\u00F3n del CMS");
	    return "";
	}
}
	
function recuperaEnvelopedData(data){
    if(_ie)
        return recuperaEnvelopedDataIE(data);
    else
        return recuperaEnvelopedDataMozilla(data);
}

function recuperaEncryptedData(data){
 	clienteFirma.setData(data);
	if(clienteFirma.recoverCMS()){
		return clienteFirma.getData();
	}else{
	    alert("Error el la recuperaci\u00F3n del CMS");
	    return "";
	}
}
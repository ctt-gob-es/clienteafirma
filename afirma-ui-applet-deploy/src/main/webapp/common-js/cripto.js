/* Copyright (C) 2014 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@seap.minhap.es
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

	eval(command);
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
//    if(_ie)
//        return recuperaEnvelopedDataIE(data);
//    else
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
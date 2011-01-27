/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores,
 * según las condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se distribuyera
 * este fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

// ShortPathName.cpp : Defines the entry point for the DLL application.
//

#include <jni.h>
#include "stdafx.h"
#include "es_gob_afirma_misc_AOWinNativeUtil.h"


#ifdef _MANAGED
#pragma managed(push, off)
#endif



JNIEXPORT jstring JNICALL Java_es_gob_afirma_misc_AOWinNativeUtil_getShortPathName
  (JNIEnv *env, jobject obj, jstring nombreLargo)
{	

	long longBuffer;
	TCHAR* nombreCorto = NULL;
	LPCSTR pNombreLargo = (LPCSTR)(*env).GetStringUTFChars(nombreLargo, JNI_FALSE);

	//Obtiene la longitud necesaria para el buffer
    longBuffer = GetShortPathNameA(pNombreLargo, NULL, 0);
	if (longBuffer == 0)
		return NULL;

	//Instancia el buffer con la longitud obtenida
	nombreCorto = new TCHAR[longBuffer];
	LPSTR pNombreCorto = (LPSTR)nombreCorto;

	//Obtiene el nombre corto de la ruta
	longBuffer = GetShortPathNameA(pNombreLargo, pNombreCorto, longBuffer);
	if (longBuffer == 0)
		return NULL;

	jstring jNombreCorto = (*env).NewStringUTF(pNombreCorto);

	return jNombreCorto;
}

JNIEXPORT jstring JNICALL Java_es_gob_afirma_misc_AOWinNativeUtil_getLongPathName
  (JNIEnv *env, jobject obj, jstring nombreCorto)
{	

	long shortBuffer;
	TCHAR* nombreLargo = NULL;
	LPCSTR pNombreCorto = (LPCSTR)(*env).GetStringUTFChars(nombreCorto, JNI_FALSE);

	//Obtiene la longitud necesaria para el buffer
    shortBuffer = GetLongPathNameA(pNombreCorto, NULL, 0);
	if (shortBuffer == 0) return NULL;

	//Instancia el buffer con la longitud obtenida
	nombreLargo = new TCHAR[shortBuffer];
	LPSTR pNombreLargo = (LPSTR)nombreLargo;

	//Obtiene el nombre largo de la ruta
	shortBuffer = GetLongPathNameA(pNombreCorto, pNombreLargo, shortBuffer);
	if (shortBuffer == 0) return NULL;

	jstring jNombreLargo = (*env).NewStringUTF(pNombreLargo);

	return jNombreLargo;
}


#ifdef _MANAGED
#pragma managed(pop)
#endif


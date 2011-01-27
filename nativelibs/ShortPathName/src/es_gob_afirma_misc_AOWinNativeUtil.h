/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores,
 * según las condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se distribuyera
 * este fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


#include "jni.h"
/* Header for class es_gob_afirma_misc_AOWinNativeUtil */

#ifndef _Included_es_gob_afirma_misc_AOWinNativeUtil
#define _Included_es_gob_afirma_misc_AOWinNativeUtil
#ifdef __cplusplus
extern "C" {
#endif
/*
 * Class:     es_gob_afirma_misc_AOWinNativeUtil
 * Method:    getShortPathName
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_es_gob_afirma_misc_AOWinNativeUtil_getShortPathName  (JNIEnv *, jobject, jstring);

/*
 * Class:     es_gob_afirma_misc_AOWinNativeUtil
 * Method:    getLongPathName
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_es_gob_afirma_misc_AOWinNativeUtil_getLongPathName  (JNIEnv *, jobject, jstring);

#ifdef __cplusplus
}
#endif
#endif

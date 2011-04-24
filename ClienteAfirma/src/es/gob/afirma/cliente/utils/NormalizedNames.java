/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.cliente.utils;

import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOKeyStore;

/**
 * Normaliza las cadenas de texto introducidas por los usuarios/integradores para
 * la configuraci&oacute;n de los distintos almacenes de certificados, formatos y
 * algoritmos.
 */
public class NormalizedNames {

	/**
	 * Normaliza el nombre identificador de un modo de firma. En caso de no identificar el modo,
	 * se devolver&aacute; la misma cadena de entrada.
	 * @param name Nombre de modo.
	 * @return Nombre normalizado del modo.
	 */
	public static String normalizeModeName(String name) {

		// Dejamos que el objeto que llame al metodo se encargue del error
		if(name == null)	return null;

		String modeLw = name.toLowerCase();
		if(modeLw.equals("explicit")) { //$NON-NLS-1$
			return AOConstants.SIGN_MODE_EXPLICIT;
		} else if(modeLw.equals("implicit")) { //$NON-NLS-1$
			return AOConstants.SIGN_MODE_IMPLICIT;
		}
		return name;
	}
	
	/**
	 * Normaliza el nombre identificador de un algoritmo de firma. En caso de no identificar el nombre,
	 * se devuelve la misma cadena de entrada.
	 * @param name Nombre de algoritmo.
	 * @return Nombre normalizado del algoritmo.
	 */
	public static String normalizeAlgorithmName(String name) {

		// Dejamos que el objeto que llame al metodo se encargue del error
		if(name == null) return null;

		if(name.equalsIgnoreCase("sha1WithRsaEncryption") || name.equalsIgnoreCase("SHA1withRSA") || name.equalsIgnoreCase("SHA-1withRSA") || name.equalsIgnoreCase("SHA1RSA")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			return AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
		} else if(name.equalsIgnoreCase("md5WithRsaEncryption") || name.equalsIgnoreCase("MD5withRSA")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_ALGORITHM_MD5WITHRSA;
		}
		return name;
	}
	
	/**
	 * Normaliza el nombre identificador de un formato de firma. En caso de identificar el formato
	 * se devuelve la misma cadena de entrada.
	 * @param name Nombre de formato.
	 * @return Nombre normalizado del formato.
	 */
	public static final String normalizeFormatName(final String name) {

		// Dejamos que el objeto que llame al metodo se encargue del error
		if(name == null)	return null;

		if(name.equalsIgnoreCase("CMS") || name.equalsIgnoreCase("CMS-BES") || //$NON-NLS-1$ //$NON-NLS-2$
				name.equalsIgnoreCase("PKCS7") || name.equalsIgnoreCase("PKCS#7")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_CMS;
		} else if(name.equalsIgnoreCase("CADES") || name.equalsIgnoreCase("CADES-BES")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_CADES;
		} else if(name.equalsIgnoreCase("NONE") || name.equalsIgnoreCase("RAW")  || //$NON-NLS-1$ //$NON-NLS-2$
				name.equalsIgnoreCase("PKCS1") || name.equalsIgnoreCase("PKCS#1")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_PKCS1;
		} else if(name.equalsIgnoreCase("XADES") || name.equalsIgnoreCase("XADES-BES") || //$NON-NLS-1$ //$NON-NLS-2$
				name.equalsIgnoreCase("XAdES Detached") || name.equalsIgnoreCase("XADES_DETACHED")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XADES_DETACHED;
//		} else if(oldName.equalsIgnoreCase("XAdES Externally Detached") || oldName.equalsIgnoreCase("XADES_EXTERNALLY_DETACHED")) { //$NON-NLS-1$ //$NON-NLS-2$
//			return AOConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED;
		} else if(name.equalsIgnoreCase("XAdES Enveloped") || name.equalsIgnoreCase("XAdES_Enveloped")) {  //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XADES_ENVELOPED;
		} else if(name.equalsIgnoreCase("XAdES Enveloping") || name.equalsIgnoreCase("XAdES_Enveloping")) {  //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XADES_ENVELOPING;
		} else if(name.equalsIgnoreCase("XMLDSIGN") || name.equalsIgnoreCase("XMLDSIGN-BES") || //$NON-NLS-1$ //$NON-NLS-2$
				name.equalsIgnoreCase("XMLDSIG") || name.equalsIgnoreCase("XMLDSIG-BES") || //$NON-NLS-1$ //$NON-NLS-2$
				name.equalsIgnoreCase("XMLDSign Detached") || name.equalsIgnoreCase("XMLDSig Detached") || //$NON-NLS-1$ //$NON-NLS-2$
				name.equalsIgnoreCase("XMLDSIGN_DETACHED") || name.equalsIgnoreCase("XMLDSIG_DETACHED")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XMLDSIG_DETACHED;
//		} else if(oldName.equalsIgnoreCase("XMLdSig Externally Detached") || oldName.equalsIgnoreCase("XMLDSIG_EXTERNALLY_DETACHED") || //$NON-NLS-1$ //$NON-NLS-2$
//				oldName.equalsIgnoreCase("XMLdSign Externally Detached") || oldName.equalsIgnoreCase("XMLDSIGN_EXTERNALLY_DETACHED")) { //$NON-NLS-1$ //$NON-NLS-2$
//			return AOConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED;
		} else if(name.equalsIgnoreCase("XMLDSign Enveloped") || name.equalsIgnoreCase("XMLDSig Enveloped") ||  //$NON-NLS-1$ //$NON-NLS-2$
				name.equalsIgnoreCase("XMLDSIGN_Enveloped") || name.equalsIgnoreCase("XMLDSIG_Enveloped")) { //$NON-NLS-1$ //$NON-NLS-2$ 
			return AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED;
		} else if(name.equalsIgnoreCase("XMLDSign Enveloping") || name.equalsIgnoreCase("XMLDSig Enveloping") || //$NON-NLS-1$ //$NON-NLS-2$
				name.equalsIgnoreCase("XMLDSIGN_Enveloping") || name.equalsIgnoreCase("XMLDSIG_Enveloping")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING;
		} else if(name.equalsIgnoreCase("PDF") || name.equalsIgnoreCase("Adobe PDF")) {  //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_PDF;
		} else if(name.equalsIgnoreCase("ODF (Open Document Format)") || name.equalsIgnoreCase("ODF") || name.equalsIgnoreCase("ODT") || //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				name.equalsIgnoreCase("ODS") || name.equalsIgnoreCase("ODP")  //$NON-NLS-1$ //$NON-NLS-2$
				|| name.equalsIgnoreCase("OpenOffice") || name.equalsIgnoreCase("OOo") || name.equalsIgnoreCase("OpenOffice.org")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			return AOConstants.SIGN_FORMAT_ODF;
		} else if(name.equalsIgnoreCase("OOXML (Office Open XML)") || name.equalsIgnoreCase("OOXML") || name.equalsIgnoreCase("DOCX") || //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				name.equalsIgnoreCase("XSLX") || name.equalsIgnoreCase("PPTX") || name.equalsIgnoreCase("PPSX")  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				|| name.equalsIgnoreCase("Office") || name.equalsIgnoreCase("Microsoft Office")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_OOXML;
		}

		// Si no es conocido el formato, dejamos el nombre tal cual
		return name;
	}
	
	/**
	 * Normaliza el nombre de un keystore para as&iacute; poder obtenerlo a trav&eacute;s del
	 * {@link AOKeyStoreManager}. Si no se consigue identificar el keystore se devuelve
	 * la misma cadena que se ha indicado.
	 * @param type Tipo de keystore.
	 * @return Nombre normalizado del keystore.
	 */
	public static String normalizeKeyStoreName(String type) {

		String typeLw = type.toLowerCase();
		if(typeLw.equals("windows") || typeLw.equals("internet explorer") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("ie") || typeLw.equals("microsoft") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("windows-my") || typeLw.equals("windowsmy")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOKeyStore.WINDOWS.getDescription();
		} else if(typeLw.equals("winaddressbook") || typeLw.equals("addressbook") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("win-others") || typeLw.equals("winothers") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("windows-others") || typeLw.equals("windowsothers")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOKeyStore.WINADDRESSBOOK.getDescription();
		} else if(typeLw.equals("mac os x") || typeLw.equals("macos x") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("macosx") || typeLw.equals("safari") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("apple") || typeLw.equals("apple safari") //$NON-NLS-1$ //$NON-NLS-2$ 
				|| typeLw.equals("keychainstore")) { //$NON-NLS-1$
			return AOKeyStore.APPLE.getDescription();
		} else if(typeLw.equals("mozilla") || typeLw.equals("firefox") || typeLw.equals("ff")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ 
			return AOKeyStore.MOZ_UNI.getDescription();
		} else if(typeLw.equals("pkcs#12") || typeLw.equals("pkcs12") || typeLw.equals("p12") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				|| typeLw.equals("pfx")) { //$NON-NLS-1$
			return AOKeyStore.PKCS12.getDescription();
		} else if(typeLw.equals("pkcs#11") || typeLw.equals("pkcs11") || typeLw.equals("p11")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			return AOKeyStore.PKCS11.getDescription();
		} else if(typeLw.equals("java") || typeLw.equals("jks") || typeLw.equals("java keystore") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				|| typeLw.equals("javakeystore")) { //$NON-NLS-1$
			return AOKeyStore.JAVA.getDescription();
		} else if(typeLw.equals("single") || typeLw.equals("pkcs7") || typeLw.equals("pkcs#7") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				|| typeLw.equals("x509") || typeLw.equals("x.509") || typeLw.equals("cer")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			return AOKeyStore.SINGLE.getDescription();
		} else if(typeLw.equals("jceks") || typeLw.equals("java cryptography extension keystore")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOKeyStore.JCEKS.getDescription();
		} else if(typeLw.equals("javace") || typeLw.equals("caseexactjks") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("java keystore (case exact)") || typeLw.equals("jks (case exact)")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOKeyStore.JAVACE.getDescription();
		} else if(typeLw.equals("win-ca") || typeLw.equals("winca") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("windows-ca") || typeLw.equals("windowsca")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOKeyStore.WINCA.getDescription();
		} else if(typeLw.equals("windows-root") || typeLw.equals("windowsroot") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("winroot")) { //$NON-NLS-1$
			return AOKeyStore.WINROOT.getDescription();
		}
		return type;
	}
}

package es.gob.afirma.keystores;

import es.gob.afirma.core.ErrorCode;

public class KeyStoreErrorCode {

	public static class Internal {

		public static ErrorCode LOADING_KEYSTORE_INTERNAL_ERROR				= new ErrorCode("201101", "Error genérico al cargar el almacen de certificados"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode LOADING_PRIVATE_KEY_ERROR					= new ErrorCode("201103", "Error al cargar la clave del certificado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode LOADING_CERTIFICATE_ERROR					= new ErrorCode("201106", "Error al cargar el certificado suelto o en PKCS#7"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode LOADING_WINDOWS_KEYSTORE_ERROR				= new ErrorCode("201107", "Error al cargar el almacen de certificados de Windows"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode LOADING_MOZILLA_KEYSTORE_ERROR				= new ErrorCode("201108", "Error al cargar el almacen de certificados de Mozilla"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode LOADING_APPLE_KEYSTORE_ERROR				= new ErrorCode("201109", "Error al cargar el almacen de certificados Apple KeyRing"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode LOADING_PKCS12_KEYSTORE_ERROR				= new ErrorCode("201110", "Error al cargar el almacen de certificados PKCS#12"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode LOADING_JAVA_KEYSTORE_ERROR					= new ErrorCode("201111", "Error al cargar el almacen de certificados JKS"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode LOADING_PUBLIC_WINDOWS_KEYSTORE_ERROR		= new ErrorCode("201112", "Error al cargar el almacen de certificados de claves publicas de Windows"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode LOADING_PKCS11_KEYSTORE_ERROR				= new ErrorCode("201113", "Error al cargar el almacen de certificados a traves del PKCS#11"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode LOADING_JMULTICARD_KEYSTORE_ERROR			= new ErrorCode("201114", "Error al cargar el almacen de certificados con JMulticard"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}

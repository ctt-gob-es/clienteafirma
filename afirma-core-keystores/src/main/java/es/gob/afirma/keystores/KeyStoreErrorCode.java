package es.gob.afirma.keystores;

import es.gob.afirma.core.ErrorCode;

public class KeyStoreErrorCode {

	public static class Hardware {

		public static final ErrorCode SMARTCARD_LOCKED							= new ErrorCode("102025", "La tarjeta inteligente esta bloqueada"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Internal {

		public static final ErrorCode LOADING_KEYSTORE_INTERNAL_ERROR			= new ErrorCode("201101", "Error generico al cargar el almacen de certificados"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_PRIVATE_KEY_ERROR					= new ErrorCode("201103", "Error al cargar la clave del certificado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_CERTIFICATE_ERROR					= new ErrorCode("201106", "Error al cargar el certificado suelto o en PKCS#7"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_WINDOWS_KEYSTORE_ERROR			= new ErrorCode("201107", "Error al cargar el almacen de certificados de Windows"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_MOZILLA_KEYSTORE_ERROR			= new ErrorCode("201108", "Error al cargar el almacen de certificados de Mozilla"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_APPLE_KEYSTORE_ERROR				= new ErrorCode("201109", "Error al cargar el almacen de certificados Apple KeyRing"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_PKCS12_KEYSTORE_ERROR				= new ErrorCode("201110", "Error al cargar el almacen de certificados PKCS#12"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_JAVA_KEYSTORE_ERROR				= new ErrorCode("201111", "Error al cargar el almacen de certificados JKS"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_PUBLIC_WINDOWS_KEYSTORE_ERROR		= new ErrorCode("201112", "Error al cargar el almacen de certificados de claves publicas de Windows"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_PKCS11_KEYSTORE_ERROR				= new ErrorCode("201113", "Error al cargar el almacen de certificados a traves del PKCS#11"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_JMULTICARD_KEYSTORE_ERROR			= new ErrorCode("201114", "Error al cargar el almacen de certificados con JMulticard"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_FILE_CERTSTORE_ERROR				= new ErrorCode("201115", "Error al cargar un certificado desde fichero"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class ThirdParty {

		public static final ErrorCode SUN_MSCAPI_PROVIDER_NOT_FOUND				= new ErrorCode("300003", "No se ha encontrado el proveedor MSCAPI de Sun"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode AFIRMA_MSCAPI_ADDRESBOOK_PROVIDER_NOT_FOUND	= new ErrorCode("300004", "No se ha encontrado el proveedor MSCAPI del Cliente @firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode AFIRMA_SINGLE_CERT_PROVIDER_NOT_FOUND		= new ErrorCode("300005", "No se ha encontrado el proveedor SingleCert del Cliente @firma"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Functional {

		public static final ErrorCode UNSUPPORTED_KEYSTORE						= new ErrorCode("511001", "El almacen de claves solicitado no es compatible con el entorno de ejecucion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNDEFINED_DEFAULT_KEYSTORE				= new ErrorCode("511002", "No hay definido un almacen de claves por defecto para el sistema operativo del usuario"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}

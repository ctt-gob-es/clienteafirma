package es.gob.afirma.triphase.signer.processors;

import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOFacturaESigner;

public class PreProcessorFactory {

	public static TriPhasePreProcessor getPreProcessor(final byte[] data) {
		final String format = getSignFormat(data);
		return getPreProcessor(format);
	}

	public static TriPhasePreProcessor getPreProcessor(final String format) {

		if (AOSignConstants.SIGN_FORMAT_PADES.equalsIgnoreCase(format) ||
			AOSignConstants.SIGN_FORMAT_PADES_TRI.equalsIgnoreCase(format)) {
					return new PAdESTriPhasePreProcessor();
		}
		if (AOSignConstants.SIGN_FORMAT_CADES.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_CADES_TRI.equalsIgnoreCase(format)) {
					return new CAdESTriPhasePreProcessor();
		}
		if (AOSignConstants.SIGN_FORMAT_XADES.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_XADES_TRI.equalsIgnoreCase(format)) {
					return new XAdESTriPhasePreProcessor();
		}
		if (AOSignConstants.SIGN_FORMAT_CADES_ASIC_S.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_CADES_ASIC_S_TRI.equalsIgnoreCase(format)) {
					return new CAdESASiCSTriPhasePreProcessor();
		}
		if (AOSignConstants.SIGN_FORMAT_XADES_ASIC_S.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_XADES_ASIC_S_TRI.equalsIgnoreCase(format)) {
					return new XAdESASiCSTriPhasePreProcessor();
		}
		if (AOSignConstants.SIGN_FORMAT_FACTURAE.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_FACTURAE_TRI.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_FACTURAE_ALT1.equalsIgnoreCase(format)) {
					return new FacturaETriPhasePreProcessor();
		}
		if (AOSignConstants.SIGN_FORMAT_PKCS1.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_PKCS1_TRI.equalsIgnoreCase(format)) {
					return new Pkcs1TriPhasePreProcessor();
		}
		throw new IllegalArgumentException("Formato de firma no soportado: " + format); //$NON-NLS-1$
	}

	private static String getSignFormat(final byte[] data) {
		if (data == null || data.length < 1) {
			throw new IllegalArgumentException(
				"Los datos a firmar no pueden ser nulos ni vacios" //$NON-NLS-1$
			);
		}
		// PDF siempre con PAdES
		if (isPDF(data)) {
			return AOSignConstants.SIGN_FORMAT_PADES;
		}
		// FacturaE siempre con FacturaE
		if (isFacturae(data)) {
			return AOSignConstants.SIGN_FORMAT_FACTURAE;
		}
		// Otro XML siempre con XAdES
		if (AOFileUtils.isXML(data)) {
			return AOSignConstants.SIGN_FORMAT_XADES;
		}
		// En cualquier otro caso, CAdES
		return AOSignConstants.SIGN_FORMAT_CADES;
	}

    /** Comprueba si los datos introducidos se corresponden a un fichero PDF.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son un PDF. */
    private static boolean isPDF(final byte[] data) {
        try {
            return new AOPDFSigner().isValidDataFile(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Indica si los datos son una factura electr&oacute;nica.
     * @param file Datos a comprobar
     * @return <code>true</code> si los datos son una <a href="http://www.facturae.es/">factura electr&oacute;nica</a>,
     *         <code>false</code> en caso contrario */
    private static boolean isFacturae(final byte[] file) {
        try {
            return new AOFacturaESigner().isValidDataFile(file);
        }
        catch(final Exception e) {
            return false;
        }
    }
}

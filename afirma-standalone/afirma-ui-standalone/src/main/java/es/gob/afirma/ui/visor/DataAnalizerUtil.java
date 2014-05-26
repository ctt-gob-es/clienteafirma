package es.gob.afirma.ui.visor;

import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cms.AOCMSSigner;
import es.gob.afirma.signers.odf.AOODFSigner;
import es.gob.afirma.signers.ooxml.AOOOXMLSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xmldsig.AOXMLDSigSigner;

/**
 * M&eacute;todos de utilidad para el an&aacute;lisis de ficheros de datos.
 * @author Carlos Gamuci
 */
public final class DataAnalizerUtil {

	private DataAnalizerUtil() {
		// No permitimos la instanciacion
	}

	/**
     * Comprueba si los datos introducidos se corresponden con una factura
     * electr&oacute;nica firmada.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son una firma XML soportada.
     */
    public static boolean isSignedInvoice(final byte[] data) {

        try {
            return new AOFacturaESigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }


    /**
     * Comprueba si los datos introducidos se corresponden a una firma XML soportada.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son una firma XML soportada.
     */
    public static boolean isSignedXML(final byte[] data) {

        try {
            return new AOXMLDSigSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /**
     * Comprueba si los datos introducidos se corresponden a una firma ODF soportada.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son una firma ODF soportada.
     */
    public static boolean isSignedODF(final byte[] data) {

        try {
            return new AOODFSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /**
     * Comprueba si los datos introducidos se corresponden a una firma OOXML soportada.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son una firma OOXML soportada.
     */
    public static boolean isSignedOOXML(final byte[] data) {

        try {
            return new AOOOXMLSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /**
     * Comprueba si los datos introducidos se corresponden a un fichero PDF firmado.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son un PDF firmado.
     */
    public static boolean isSignedPDF(final byte[] data) {
        try {
            return new AOPDFSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /**
     * Comprueba si los datos introducidos se corresponden a una firma binaria soportada.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son una firma binaria soportada.
     */
    public static boolean isSignedBinary(final byte[] data) {

        try {
            return new AOCMSSigner().isSign(data) || new AOCAdESSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }
}

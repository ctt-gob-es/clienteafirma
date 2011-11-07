/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.core.signers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;

/** Factor&iacute;a que gestiona todos los formatos de firma disponibles en cada
 * momento en el cliente. */
public final class AOSignerFactory {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static AOSignerFactory signerFactory = null;

    private static List<String> signersID;

    private static Map<String, AOSigner> signers;

    /** Listado completo de formatos de firma soportados y el manejador de firma
     * asociado. */
    private static final String[][] ID_SIGNERS = {
            {
                    AOSignConstants.SIGN_FORMAT_CADES, "es.gob.afirma.signers.cades.AOCAdESSigner" //$NON-NLS-1$
            }, {
                    AOSignConstants.SIGN_FORMAT_CMS, "es.gob.afirma.signers.cms.AOCMSSigner" //$NON-NLS-1$
            }, {
                    AOSignConstants.SIGN_FORMAT_XADES_DETACHED, "es.gob.afirma.signers.xades.AOXAdESSigner" //$NON-NLS-1$
            }, {
                    AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED, "es.gob.afirma.signers.xades.AOXAdESSigner" //$NON-NLS-1$
            }, {
                    AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, "es.gob.afirma.signers.xades.AOXAdESSigner" //$NON-NLS-1$
            }, {
                    AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED, "es.gob.afirma.signers.xml.xmldsig.AOXMLDSigSigner" //$NON-NLS-1$
            }, {
                    AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED, "es.gob.afirma.signers.xml.xmldsig.AOXMLDSigSigner" //$NON-NLS-1$
            }, {
                    AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, "es.gob.afirma.signers.xml.xmldsig.AOXMLDSigSigner" //$NON-NLS-1$
            }, {
                    AOSignConstants.SIGN_FORMAT_PDF, "es.gob.afirma.signers.pades.AOPDFSigner" //$NON-NLS-1$
            }, {
                    AOSignConstants.SIGN_FORMAT_ODF, "es.gob.afirma.signers.odf.AOODFSigner" //$NON-NLS-1$
            }, {
                    AOSignConstants.SIGN_FORMAT_OOXML, "es.gob.afirma.signers.ooxml.AOOOXMLSigner" //$NON-NLS-1$
            }, {
                    AOSignConstants.SIGN_FORMAT_PKCS1, "es.gob.afirma.signers.pkcs1.AOPKCS1Signer" //$NON-NLS-1$
            }
    };

    private AOSignerFactory() {
        // No permitimos la instanciacion
    }

    /** Obtiene una instancia de la factor&iacute;a.
     * @return Instancia de la factor&iacute;a */
    public static AOSignerFactory getInstance() {
        if (signerFactory != null) {
            return signerFactory;
        }
        return initialize();
    }

    /** Carga los manejadores de firma para poder hacer uso de la
     * factor&iacute;a.
     * @return La factor&iacute;a inicializada. */
    private static AOSignerFactory initialize() {

        // Cargamos
        signerFactory = new AOSignerFactory();
        signersID = new ArrayList<String>();
        signers = new HashMap<String, AOSigner>();

        for (int i = 0; i < ID_SIGNERS.length; i++) {
            try {
                final AOSigner signer = (AOSigner) AOUtil.classForName(ID_SIGNERS[i][1]).newInstance();
                signersID.add(ID_SIGNERS[i][0]);
                if (!signers.containsKey(ID_SIGNERS[i][1])) {
                    signers.put(ID_SIGNERS[i][1], signer);
                }
                // LOGGER.info("Se ha configurado el formato de firma: "+ID_SIGNERS[i][0]);
            }
            catch (final Exception e) {
                // No se encuentra el plugin para el formato de firma
                LOGGER.warning("No se encuentra el plugin para el formato: " + ID_SIGNERS[i][0]); //$NON-NLS-1$
            }
        }

        return signerFactory;
    }

    /** Recupera un manejador de firma capaz de tratar la firma indicada. En caso
     * de no tener cargado ning&uacute;n manejador compatible se
     * devolver&aacute; <code>null</code>.
     * @param signData
     *        Firma electr&oacute;nica
     * @return Manejador de firma */
    public static AOSigner getSigner(final byte[] signData) {

        if (signData == null) {
            throw new IllegalArgumentException("No se han indicado datos de firma"); //$NON-NLS-1$
        }

        // Inicializamos las referencias estaticas
        if (signerFactory == null) {
            initialize();
        }

        final Set<String> checkedFormats = new HashSet<String>(signers.size() - 1);

        // Recorremos los formatos soportados
        for (int i = 0; i < signersID.size(); i++) {
            // Buscamos el manejador del formato
            for (int j = 0; j < ID_SIGNERS.length; j++) {
                // Si no lo hemos probado antes, lo intentamos
                if (signersID.get(i).equals(ID_SIGNERS[j][0]) && !checkedFormats.contains(ID_SIGNERS[j][1])) {
                    final AOSigner signer = signers.get(ID_SIGNERS[j][1]);
                    if (signer.isSign(signData)) {
                        return signer;
                    }
                    checkedFormats.add(ID_SIGNERS[j][1]);
                }
            }
        }
        return null;
    }

    /** Obtiene un manejador para un formato de firma dado. En caso de no
     * encontrar ninguno, se devuelve <code>null</code>.
     * @param signFormat
     *        Formato de firma para el cual solicitamos el manejador.
     * @return Manejador capaz de firmar en el formato indicado. */
    public static AOSigner getSigner(final String signFormat) {

        // Inicializamos las referencias estaticas
        if (signerFactory == null) {
            initialize();
        }
        
        if (signersID.contains(signFormat)) {
            for (final String[] element : ID_SIGNERS) {
                if (element[0].equals(signFormat)) {
                    return signers.get(element[1]);
                }
            }
        }

        LOGGER.warning("El formato de firma '" + signFormat + "' no esta soportado, se devolvera null"); //$NON-NLS-1$ //$NON-NLS-2$

        return null;
    }

    /** Obtiene el manejador del formato de firma por defecto establecido.
     * @return Manejador de firma */
    public AOSigner getDefaultSigner() {
        for (final String[] element : ID_SIGNERS) {
            if (element[0].equals(AOSignConstants.DEFAULT_SIGN_FORMAT)) {
                final AOSigner signer = signers.get(element[1]);
                if (signer == null) {
                    LOGGER.severe("No se ha encontrado el manejador del formato de firma por defecto ('" + AOSignConstants.DEFAULT_SIGN_FORMAT + "')"); //$NON-NLS-1$ //$NON-NLS-2$
                }
                return signer;
            }
        }
        LOGGER.severe("El formato de firma por defecto ('" + AOSignConstants.DEFAULT_SIGN_FORMAT + "') no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
    }

    /** Recupera el listado de los manejadores de firma disponibles en el sistema
     * seg&uacute;n el orden en el que se insertasen en el sistema. Aunque un
     * manejador soporte m&aacute:s de un formato de firma, s&oacute;lo
     * aparecer&aacute; &uacute;nica vez.
     * @return Listado de manejadores. */
    public AOSigner[] getSigners() {
        final ArrayList<AOSigner> result = new ArrayList<AOSigner>(signers.size());
        final Set<String> classes = new HashSet<String>(signers.size());

        // Recorremos los formatos soportados
        for (int i = 0; i < signersID.size(); i++) {
            // Buscamos el manejador del formato
            for (int j = 0; j < ID_SIGNERS.length; j++) {
                if (signersID.get(i).equals(ID_SIGNERS[j][0])) {
                    // Si el manejador no se ha agregado ya a la lista de
                    // resultados, se agrega
                    if (!classes.contains(ID_SIGNERS[j][1])) {
                        classes.add(ID_SIGNERS[j][1]);
                        result.add(signers.get(ID_SIGNERS[j][1]));
                    }
                    break;
                }
            }
        }
        return result.toArray(new AOSigner[result.size()]);
    }

    /** Recupera el listado de identificadores de formatos de firma actualmente
     * disponibles en un orden predefinido.
     * @return Identificadors de formato de firma. */
    public String[] getSignersID() {
        return signersID.toArray(new String[signersID.size()]);
    }

    /** Devuelve el listado de formatos de firma soportados.
     * @return Indentificadores de los formatos de firma soportados. */
    @Override
    public String toString() {
        final StringBuilder exstr = new StringBuilder(); 
        for (final String format : signersID) {
            exstr.append(format).append("\n"); //$NON-NLS-1$
        }
        return exstr.toString();
    }

}

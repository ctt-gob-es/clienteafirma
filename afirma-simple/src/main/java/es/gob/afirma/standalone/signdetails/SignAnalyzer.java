package es.gob.afirma.standalone.signdetails;

import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.standalone.crypto.CompleteSignInfo;
import es.gob.afirma.standalone.crypto.TimestampsAnalyzer;

public interface SignAnalyzer {

	Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	AOTreeModel getSignersTree();

	String getSignFormat();

	String getDataLocation();

	List<SignDetails> getAllSignDetails();

    /** Recupera la informaci&oacute;n de la firma indicada.
     * @param signData Firma.
     * @return Informaci&oacute;n de la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura de los datos */
    static CompleteSignInfo getSignInfo(final byte[] signData) throws IOException {
        final CompleteSignInfo signInfo = new CompleteSignInfo();
        signInfo.setSignData(signData);
        final AOSigner signer = AOSignerFactory.getSigner(signData);
        if (signer == null) {
        	LOGGER.warning("Formato de firma no reconocido"); //$NON-NLS-1$
            throw new IllegalArgumentException("Formato de firma no reconocido"); //$NON-NLS-1$
        }
        try {
            signInfo.setSignInfo(signer.getSignInfo(signData));
        }
        catch (final Exception e) {
        	LOGGER.log(Level.WARNING, "Error al leer la informacion de la firma", e); //$NON-NLS-1$
        }
        try {
        	signInfo.setSignsTree(signer.getSignersStructure(signData, true));
        }
        catch (final Exception e) {
        	LOGGER.log(Level.WARNING, "Error al extraer el arbol de firmantes", e);  //$NON-NLS-1$
        	signInfo.setSignsTree(null);
        }
        try {
            signInfo.setData(signer.getData(signData));
        }
        catch (final Exception e) {
        	LOGGER.log(Level.WARNING, "Error al extraer los datos firmados", e);  //$NON-NLS-1$
        }
        try {
        	signInfo.setTimestampsInfo(
    			TimestampsAnalyzer.getTimestamps(signData)
			);
        }
        catch (final Exception e) {
        	LOGGER.log(Level.WARNING, "Error al extraer los sellos de tiempo", e);  //$NON-NLS-1$
        }
        return signInfo;
    }

}

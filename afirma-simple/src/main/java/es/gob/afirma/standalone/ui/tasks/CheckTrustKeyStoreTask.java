package es.gob.afirma.standalone.ui.tasks;

import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.SimpleErrorCode;
import es.gob.afirma.standalone.protocol.SecureSocketUtils;

public class CheckTrustKeyStoreTask extends SwingWorker<Void, Void>{
	
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	
	private Object parent;
	
	public CheckTrustKeyStoreTask(Object parent) {
		this.parent = parent;
	}

    @Override
    public Void doInBackground() {

    	LOGGER.info("Iniciando hilo para la comprobacion de la correcta instalacion del almacen de confianza: "); //$NON-NLS-1$  

    	// Si al intentar obtener el contexto SSL, se recibe alguna excepcion, se mostrar el error
    	try {
    		SecureSocketUtils.getSecureSSLContext();
    	} catch (Exception e) {
			LOGGER.severe("No se ha encontrado el almacen de claves de Autofirma"); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
					this.parent,
					SimpleAfirmaMessages.getString("TrustedKeyStoreError.0"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					new AOException(SimpleErrorCode.Internal.TRUSTSTORE_INCORRECT_INSTALLATION));
    	}
    	
    	return null;
    	
    }
	
}

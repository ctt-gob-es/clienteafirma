/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.miniapplet;

import java.io.IOException;
import java.security.AccessController;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivilegedActionException;
import java.util.Properties;
import java.util.logging.Logger;

import javax.jnlp.FileOpenService;
import javax.jnlp.FileSaveService;
import javax.jnlp.ServiceManager;
import javax.swing.JApplet;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.miniapplet.actions.GetFileContentAction;
import es.gob.afirma.miniapplet.actions.GetFilenameAction;
import es.gob.afirma.miniapplet.actions.SaveFileAction;
import es.gob.afirma.miniapplet.actions.SelectPrivateKeyAction;
import es.gob.afirma.miniapplet.actions.SignAction;
import es.gob.afirma.util.signers.AOSignerFactory;

/** MiniApplet de firma del proyecto Afirma.
 */
public class MiniAfirmaApplet extends JApplet implements MiniAfirma {

    private static final long serialVersionUID = -4364574240099120486L;
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
    private static final String APPLET_PARAM_USER_AGENT = "userAgent"; //$NON-NLS-1$
    
    private String userAgent = null;
    
    private FileOpenService fos = null;
    private FileSaveService fss = null;
    
    private String pathHint = Platform.getUserHome();

    @Override
    public String sign(String dataB64, String algorithm, String format, String[] extraParams) throws IOException, PrivilegedActionException {
    	if (dataB64 == null) {
    		throw new NullPointerException("Se han introducido datos nulos para firmar"); //$NON-NLS-1$
    	}
    	
    	SelectPrivateKeyAction action = new SelectPrivateKeyAction(Platform.getOS(), Platform.getBrowser(this.userAgent), this); 
    	
    	PrivateKeyEntry keyEntry = AccessController.doPrivileged(action);

    	AOSigner signer = AOSignerFactory.getSigner(format);
    	Properties params = ExtraParamsProcessor.convertToProperties(extraParams);
    	SignAction signAction = new SignAction(signer, Base64.decode(dataB64), algorithm, keyEntry, params); 
    	
        return Base64.encodeBytes(AccessController.doPrivileged(signAction));
    }

    @Override
    public String coSign(String sign, String data, String algorithm, String format, String extraParams) {
        return null;
    }


    @Override
    public String counterSign(String sign, String algorithm, String format, String extraParams) {
        return null;
    }
    
    @Override
    public String getSignersStructure(String signB64) throws IOException {
    	if (signB64 == null) {
    		throw new NullPointerException("Se ha introducido un firma nula para la extraccion de firmantes"); //$NON-NLS-1$
    	}
    	
    	byte[] sign = Base64.decode(signB64);
    	
    	AOSigner signer = AOSignerFactory.getSigner(sign);
    	
        return AOUtil.showTreeAsString(signer.getSignersStructure(sign, false), null, null);
    }


    @Override
    public boolean saveDataToFile(String data, String fileName, String extension) throws PrivilegedActionException, IOException {
        if (data == null) {
            LOGGER.warning("Se ha solicitado guardar en disco un contenido nulo, se ignorara la peticion"); //$NON-NLS-1$
            return false;
        }

        SaveFileAction saveFileAction = new SaveFileAction(
        		this.fss, Base64.decode(data), new String[] { extension }, fileName, this.pathHint);
        
    	try {
    		return AccessController.doPrivileged(saveFileAction).booleanValue();
    	} catch (AOCancelledOperationException e) {
    		return false;
    	}
    }

    @Override
    public String loadFilename(String exts) throws IOException, PrivilegedActionException {
    	
    	String[] extensions = (exts == null ? null : exts.split(",")); //$NON-NLS-1$
    	
    	try {
    		return AccessController.doPrivileged(new GetFilenameAction(this.fos, extensions));
    	} catch (AOCancelledOperationException e) {
    		return null;
    	}
    }
    
    @Override
    public String getFileContent() throws IOException, PrivilegedActionException {
    	byte[] data;
    	try {
    		data = AccessController.doPrivileged(new GetFileContentAction(this.fos));
    	} catch (AOCancelledOperationException e) {
    		return null;
    	}

    	return Base64.encodeBytes(data);
    }

    @Override
    public String getTextFromBase64(String base64Data, String charset) throws IOException {
    	if (charset != null) {
    		return new String(Base64.decode(base64Data), charset);
    	}
    	return new String(Base64.decode(base64Data));
    }

    @Override
    public String getBase64FromText(String plainText) {
        return Base64.encodeBytes(plainText.getBytes());
    }
    
    @Override
    public void init() {

    	if (this.fos == null) {
    		try {
    			this.fos = (FileOpenService) ServiceManager.lookup("javax.jnlp.FileOpenService"); //$NON-NLS-1$
    		}
    		catch(final Exception e) {
    			LOGGER.severe("Error obteniendo el servicio JNLP de carga de ficheros: " + e); //$NON-NLS-1$
    		}
    	}
        if (this.fss == null) {
            try {
                this.fss = (FileSaveService) ServiceManager.lookup("javax.jnlp.FileSaveService"); //$NON-NLS-1$
            }
            catch(final Exception e) {
                LOGGER.severe("Error obteniendo el servicio JNLP de guardado de ficheros: " + e); //$NON-NLS-1$
            }
        }
    	
    	this.userAgent = this.getParameter(APPLET_PARAM_USER_AGENT);
    }

}

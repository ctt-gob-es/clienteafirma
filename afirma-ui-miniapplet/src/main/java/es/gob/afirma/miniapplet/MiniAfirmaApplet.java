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
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.miniapplet.actions.CoSignAction;
import es.gob.afirma.miniapplet.actions.CounterSignAction;
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
    
    /** Identificador del navegador Web que carga el applet. */
    private String userAgent = null;
    
    /** Servicio para la carga de ficheros. */
    private FileOpenService fos = null;
    
    /** Servicio para el guardado de ficheros. */
    private FileSaveService fss = null;
    
    /** Ruta recomendada para la apertura de di&aacute;logos se seleccion y guardado de ficheros. */
    private String pathHint = Platform.getUserHome();

    @Override
    public String sign(String dataB64, String algorithm, String format, String[] extraParams) throws IOException, AOFormatFileException, PrivilegedActionException {
    	if (dataB64 == null) {
    		throw new NullPointerException("Se han introducido datos nulos para firmar"); //$NON-NLS-1$
    	}
    	
    	AOSigner signer = this.selectSigner(format, null);
    	Properties params = ExtraParamsProcessor.convertToProperties(extraParams);
    	PrivateKeyEntry keyEntry = this.selectPrivateKey(params);

    	SignAction signAction = new SignAction(signer, Base64.decode(dataB64), algorithm, keyEntry, params); 
    	
        return Base64.encodeBytes(AccessController.doPrivileged(signAction));
    }

    @Override
    public String coSign(String signB64, String dataB64, String algorithm, String format, String[] extraParams) throws IOException, AOFormatFileException, PrivilegedActionException {
    	if (signB64 == null) {
    		throw new NullPointerException("Se ha introducido una firma nula para contrafirmar"); //$NON-NLS-1$
    	}
    	
    	byte[] sign = Base64.decode(signB64);
    	byte[] data = (dataB64 == null ? null : Base64.decode(dataB64));
    	
    	AOSigner signer = this.selectSigner(format, sign);
    	Properties params = ExtraParamsProcessor.convertToProperties(extraParams);
    	PrivateKeyEntry keyEntry = this.selectPrivateKey(params);

    	CoSignAction coSignAction = new CoSignAction(signer, sign, data, algorithm, keyEntry, params); 
    	
    	return Base64.encodeBytes(AccessController.doPrivileged(coSignAction));
    }


    @Override
    public String counterSign(String signB64, String algorithm, String format, String[] extraParams) throws IOException, AOFormatFileException, PrivilegedActionException {
    	if (signB64 == null) {
    		throw new NullPointerException("Se ha introducido una firma nula para contrafirmar"); //$NON-NLS-1$
    	}
    	
    	byte[] sign = Base64.decode(signB64);
    	
    	AOSigner signer = this.selectSigner(format, sign);
    	Properties params = ExtraParamsProcessor.convertToProperties(extraParams);
    	PrivateKeyEntry keyEntry = this.selectPrivateKey(params);
    	
    	CounterSignAction counterSignAction = new CounterSignAction(signer, sign, algorithm, keyEntry, params); 
    	
    	return Base64.encodeBytes(AccessController.doPrivileged(counterSignAction));
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

    /**
     * Permite que el usuario seleccione un certificado del almac&eacute;n por defecto y devuelve
     * su clave privada.
     * @param params Configuraci&oacute;n general establecida para la operaci&oacute;n.
     * @return Clave privada asociada al certificado seleccionado.
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     * @throws AOCancelledOperationException Cuando se cancela la operaci&oacute;n.
     */
    private PrivateKeyEntry selectPrivateKey(Properties params) throws PrivilegedActionException {
    	SelectPrivateKeyAction action = new SelectPrivateKeyAction(
    			Platform.getOS(), Platform.getBrowser(this.userAgent), new CertFilterManager(params), this);
    	
    	return AccessController.doPrivileged(action);
    }
    
    /**
     * Devuelve un manejador de firma compatible con un formato de firma o, de no establecerse, con
     * una firma electr&oacute;nica concreta.
     * @param format Formato de firma.
     * @param sign Firma electr&oacute;nica.
     * @return Manejador de firma.
     * @throws AOFormatFileException Cuando el formato o la firma no estan soportados.
     * @throws NullPointerException Cuando no se indica ni formato ni firma como par&aacute;nmetro.
     */
    private AOSigner selectSigner(String format, byte[] sign) throws AOFormatFileException {
    	AOSigner signer = null;
    	String signerErrorMessage;
    	if (format != null) {
    		signer = AOSignerFactory.getSigner(format);
    		signerErrorMessage = "El formato de firma indicado no esta soportado"; //$NON-NLS-1$
    	} else if (sign != null) {
    		signer = AOSignerFactory.getSigner(sign);
    		signerErrorMessage = "Los datos introducidos no se corresponden con una firma soportada"; //$NON-NLS-1$
    	} else {
    		throw new NullPointerException("No se ha indicado el formato ni la firma que se desea tratar"); //$NON-NLS-1$
    	}
    	if (signer == null) {
    		throw new AOFormatFileException(signerErrorMessage);
    	}
    	return signer;
    }
}

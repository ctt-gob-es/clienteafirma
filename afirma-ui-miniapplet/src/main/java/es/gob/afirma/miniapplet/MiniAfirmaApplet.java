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

import java.io.File;
import java.io.IOException;
import java.security.AccessController;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivilegedActionException;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JApplet;
import javax.swing.UIManager;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.miniapplet.actions.CoSignAction;
import es.gob.afirma.miniapplet.actions.CounterSignAction;
import es.gob.afirma.miniapplet.actions.GetFileContentAction;
import es.gob.afirma.miniapplet.actions.GetFilePathAction;
import es.gob.afirma.miniapplet.actions.SaveFileAction;
import es.gob.afirma.miniapplet.actions.SelectPrivateKeyAction;
import es.gob.afirma.miniapplet.actions.SelectSignerAction;
import es.gob.afirma.miniapplet.actions.SignAction;


/** MiniApplet de firma del proyecto Afirma.
 */
public final class MiniAfirmaApplet extends JApplet implements MiniAfirma {

    private static final long serialVersionUID = -4364574240099120486L;
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
    private static final String APPLET_PARAM_USER_AGENT = "userAgent"; //$NON-NLS-1$
    
    /** Identificador del navegador Web que carga el applet. */
    private String userAgent = null;
        
    /** Ruta recomendada para la apertura de di&aacute;logos se seleccion y guardado de ficheros. */
    private File pathHint = new File(Platform.getUserHome());

    @Override
    public String sign(final String dataB64, final String algorithm, final String format, final String extraParams) throws IOException, AOFormatFileException, PrivilegedActionException {
    	if (dataB64 == null) {
    		throw new NullPointerException("Se han introducido datos nulos para firmar"); //$NON-NLS-1$
    	}
    	
    	final String signAlgo = (algorithm == null || algorithm.trim().length() < 1 ? null : algorithm.trim());
    	final String signFormat = (format == null || format.trim().length() < 1 ? null : format.trim());
    	
    	final AOSigner signer = this.selectSigner(signFormat, null);
    	final Properties params = ExtraParamsProcessor.convertToProperties(extraParams);
    	final PrivateKeyEntry keyEntry = this.selectPrivateKey(params);

    	final SignAction signAction = new SignAction(signer, Base64.decode(dataB64), signAlgo, keyEntry, params); 
    	
        return Base64.encodeBytes(AccessController.doPrivileged(signAction));
    }

    @Override
    public String coSign(final String signB64, final String dataB64, final String algorithm, final String format, final String extraParams) throws IOException, AOFormatFileException, PrivilegedActionException {
    	if (signB64 == null) {
    		throw new NullPointerException("Se ha introducido una firma nula para contrafirmar"); //$NON-NLS-1$
    	}
    	
    	final String signAlgo = (algorithm == null || algorithm.trim().length() < 1 ? null : algorithm.trim());
    	final String signFormat = (format == null || format.trim().length() < 1 ? null : format.trim());
    	
    	final byte[] sign = Base64.decode(signB64);
    	final byte[] data = (dataB64 == null ? null : Base64.decode(dataB64));
    	
    	final AOSigner signer = this.selectSigner(signFormat, sign);
    	final Properties params = ExtraParamsProcessor.convertToProperties(extraParams);
    	final PrivateKeyEntry keyEntry = this.selectPrivateKey(params);

    	final CoSignAction coSignAction = new CoSignAction(signer, sign, data, signAlgo, keyEntry, params); 
    	
    	return Base64.encodeBytes(AccessController.doPrivileged(coSignAction));
    }


    @Override
    public String counterSign(final String signB64, final String algorithm, final String format, final String extraParams) throws IOException, AOFormatFileException, PrivilegedActionException {
    	if (signB64 == null) {
    		throw new NullPointerException("Se ha introducido una firma nula para contrafirmar"); //$NON-NLS-1$
    	}
    	final String signAlgo = (algorithm == null || algorithm.trim().length() < 1 ? null : algorithm.trim());
    	final String signFormat = (format == null || format.trim().length() < 1 ? null : format.trim());
    	
    	final byte[] sign = Base64.decode(signB64);
    	
    	final AOSigner signer = this.selectSigner(signFormat, sign);
    	final Properties params = ExtraParamsProcessor.convertToProperties(extraParams);
    	final PrivateKeyEntry keyEntry = this.selectPrivateKey(params);
    	
    	final CounterSignAction counterSignAction = new CounterSignAction(signer, sign, signAlgo, keyEntry, params); 
    	
    	return Base64.encodeBytes(AccessController.doPrivileged(counterSignAction));
    }
    
    @Override
    public String getSignersStructure(final String signB64) throws IOException, AOFormatFileException, PrivilegedActionException {
    	if (signB64 == null) {
    		throw new NullPointerException("Se ha introducido un firma nula para la extraccion de firmantes"); //$NON-NLS-1$
    	}
    	
    	final byte[] sign = Base64.decode(signB64);
    	final AOSigner signer = this.getSigner(sign);
    	if (signer == null) {
    		throw new AOFormatFileException("Los datos introducidos no se corresponden con una firma soportada"); //$NON-NLS-1$
    	}
    	
        return AOUtil.showTreeAsString(signer.getSignersStructure(sign, false), null, null);
    }


    @Override
    public boolean saveDataToFile(final String data, final String title, final String fileName, final String extension, final String description) throws PrivilegedActionException, IOException {
        if (data == null) {
            LOGGER.warning("Se ha solicitado guardar en disco un contenido nulo, se ignorara la peticion"); //$NON-NLS-1$
            return false;
        }

        final String titleDialog = (title == null || title.trim().length() < 1 ? null : title.trim());
        
        final String[] extensions = (extension == null || extension.trim().length() < 1 ?
        		null : new String[] { extension.trim() });
        
        final String descFiles = (extensions != null && description != null && description.trim().length() > 0 ?
				description.trim() : (extensions != null ? extension : null));
        
		final File fileHint = (fileName != null && fileName.trim().length() > 0 ?
				new File(this.pathHint, fileName) : this.pathHint);
				
        final SaveFileAction saveFileAction = new SaveFileAction(titleDialog, Base64.decode(data),
        		extensions, descFiles, fileHint, this);
        
    	try {
    		return AccessController.doPrivileged(saveFileAction).booleanValue();
    	} 
    	catch (final AOCancelledOperationException e) {
    		return false;
    	}
    }

    @Override
    public String loadFilePath(final String title, final String extensions, final String description) throws IOException, PrivilegedActionException {
    	
    	String titleDialog = (title == null || title.trim().length() < 1 ? null : title.trim());
        
        String[] exts = (extensions == null || extensions.trim().length() < 1 ?
        		null : new String[] { extensions.trim() });
        
        String descFiles = (exts != null && description != null && description.trim().length() > 0 ?
				description.trim() : (exts != null ? extensions : null));
        
    	try {
    		return AccessController.doPrivileged(new GetFilePathAction(
    				titleDialog, exts, descFiles, this));
    	} catch (AOCancelledOperationException e) {
    		return null;
    	}
    }
    
    @Override
    public String getFileContent(final String title, final String extensions, final String description) throws IOException, PrivilegedActionException {
    	
    	String titleDialog = (title == null || title.trim().length() < 1 ? null : title.trim());
        
        String[] exts = (extensions == null || extensions.trim().length() < 1 ?
        		null : new String[] { extensions.trim() });
        
        String descFiles = (exts != null && description != null && description.trim().length() > 0 ?
				description.trim() : (exts != null ? extensions : null));
    	
    	byte[] data;
    	try {
    		data = AccessController.doPrivileged(new GetFileContentAction(
    				titleDialog, exts, descFiles, this));
    	} catch (AOCancelledOperationException e) {
    		return null;
    	}

    	return Base64.encodeBytes(data);
    }

    @Override
    public String getTextFromBase64(final String base64Data, final String charset) throws IOException {
    	if (base64Data == null) {
    		return null;
    	}
    	if (charset != null && charset.trim().length() > 0) {
    		return new String(Base64.decode(base64Data), charset);
    	}
    	return new String(Base64.decode(base64Data));
    }

    @Override
    public String getBase64FromText(final String plainText, final String charset) throws IOException {
    	if (plainText == null) {
    		return null;
    	}
    	if (charset != null && charset.trim().length() > 0) {
    		return Base64.encodeBytes(plainText.getBytes(charset));
    	}
        return Base64.encodeBytes(plainText.getBytes());
    }
    
    @Override
    public void init() {
    	this.userAgent = this.getParameter(APPLET_PARAM_USER_AGENT);
    	
    	this.configureLookAndFeel();
    	
    	LOGGER.info("Miniapplet Afirma"); //$NON-NLS-1$
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
     * @throws PrivilegedActionException Cuando ocurre un error de seguridad.
     * @throws NullPointerException Cuando no se indica ni formato ni firma como par&aacute;nmetro.
     */
    private AOSigner selectSigner(String format, byte[] sign) throws AOFormatFileException, PrivilegedActionException {
    	AOSigner signer = null;
    	String signerErrorMessage;
    	if (format != null) {
    		signer = this.getSigner(format);
    		signerErrorMessage = "El formato de firma indicado no esta soportado"; //$NON-NLS-1$
    		if (signer != null && sign != null) {
    			if (!signer.isSign(sign)) {
    				signer = null;
    				signerErrorMessage = "La firma electronica no es compatible con el formato de firma indicado"; //$NON-NLS-1$
    			}
    		}
    	} else if (sign != null) {
    		signer = this.getSigner(sign);
    		signerErrorMessage = "Los datos introducidos no se corresponden con una firma soportada"; //$NON-NLS-1$
    	} else {
    		throw new NullPointerException("No se ha indicado el formato ni la firma que se desea tratar"); //$NON-NLS-1$
    	}
    	if (signer == null) {
    		throw new AOFormatFileException(signerErrorMessage);
    	}
    	return signer;
    }
    
    /**
     * Recupera un manejador de firma compatible con el formato indicado. Si no se encuentra uno
     * compatible, se devuelve {@code null}.
     * @param format Nombre de un formato de firma.
     * @return Manejador de firma.
     * @throws PrivilegedActionException Cuando ocurre un problema de seguridad.
     */
    private AOSigner getSigner(final String format) throws PrivilegedActionException {
    	SelectSignerAction action = new SelectSignerAction(format);
    	return AccessController.doPrivileged(action);
    }
    
    /**
     * Recupera un manejador de firma compatible con la firma indicada. Si no se encuentra uno
     * compatible, se devuelve {@code null}.
     * @param signature Firma electr&oacute;nica.
     * @return Manejador de firma.
     * @throws PrivilegedActionException Cuando ocurre un problema de seguridad.
     */
    private AOSigner getSigner(final byte[] signature) throws PrivilegedActionException {
    	SelectSignerAction action = new SelectSignerAction(signature);
    	return AccessController.doPrivileged(action);
    }
    
    /**
     * Configura la apariencia de los di&aacute;logos Java siguiendo la configuraci&oacute;n
     * establecida en el sistema. 
     */
    private void configureLookAndFeel() {
        final String lookandfeel = UIManager.getSystemLookAndFeelClassName();
        try {
            UIManager.setLookAndFeel(lookandfeel);
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido establecer el Look&Feel '" + lookandfeel + //$NON-NLS-1$
            			   "', las ventanas careceran de decoracion: " //$NON-NLS-1$
                           + e);
        }

        // Nos aseguramos de que los dialogos salgan decorados
        javax.swing.JDialog.setDefaultLookAndFeelDecorated(true);
        javax.swing.JFrame.setDefaultLookAndFeelDecorated(true);
    }
}

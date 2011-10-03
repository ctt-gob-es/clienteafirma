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

import java.io.ByteArrayInputStream;
import java.util.logging.Logger;

import javax.jnlp.FileContents;
import javax.jnlp.FileOpenService;
import javax.jnlp.FileSaveService;
import javax.jnlp.ServiceManager;
import javax.swing.JApplet;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;

/** MiniApplet de firma del proyecto Afirma.
 */
public class MiniAfirmaApplet extends JApplet implements MiniAfirma {

    private static final long serialVersionUID = -4364574240099120486L;
    
    private static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
    private FileOpenService fos = null;
    private FileSaveService fss = null;
    
    private String pathHint = Platform.getUserHome();

    @Override
    public String sign(String data, String algorithm, String format, String extraParams) {
        return null;
    }

    @Override
    public String coSign(String sign, String data, String algorithm, String format, String extraParams) {
        return null;
    }

    @Override
    public String getSignersStructure(String sign) {
        return null;
    }

    @Override
    public String counterSign(String sign, String algorithm, String format, String extraParams) {
        return null;
    }

    @Override
    public boolean saveDataToFile(String data, String fileName, String extension) {
        if (data == null) {
            LOGGER.warning("Se ha solicitado guardar en disco un contenido nulo, se ignorara la peticion"); //$NON-NLS-1$
            return false;
        }
        if (this.fss == null) {
            try {
                this.fss = (FileSaveService) ServiceManager.lookup("javax.jnlp.FileSaveService"); //$NON-NLS-1$
            }
            catch(final Exception e) {
                LOGGER.severe("Error obteniendo el servicio JNLP de salvado de ficheros, no se guardaron los datos: " + e); //$NON-NLS-1$
                return false;
            }
        }

        final FileContents fc;
        try {
            fc = this.fss.saveFileDialog(this.pathHint, new String[] { extension }, new ByteArrayInputStream(Base64.decode(data)), fileName);
        }
        catch (final Exception e) {
            LOGGER.severe("Error guardando los datos: " + e); //$NON-NLS-1$
            return false;
        }
        
        if (fc == null) {
            LOGGER.info("El usuario cancelo el guardado de datos"); //$NON-NLS-1$
        }
        
        return true;
    }

    @Override
    public String getFileContent() {
        return null;
    }

    @Override
    public String getTextFromBase64(String data, String charset) {
        return null;
    }

    @Override
    public String getBase64FromText(String plainText) {
        return null;
    }

    @Override
    public String loadFilePath(String title, String exts, String description) {
        return null;
    }
    
    @Override
    public void init() {
        saveDataToFile("UFJVRUJBIERFIFRFWFRP=", "borrame", ".txt"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        saveDataToFile("UFJVRUJBIERFIFRFWFRP=", "borrame2", ".txt"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

}

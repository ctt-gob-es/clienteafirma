package es.gob.afirma.miniapplet;

import java.awt.Component;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/**
 * Acci&oacute;n para la recuperaci&oacute;n del nombre y contenido de un fichero.
 * @author Carlos Gamuci Mill&aacute;n
 */
final class GetFileNameContentAction implements PrivilegedExceptionAction<String> {
    
	private static final String SEPARATOR = "|"; //$NON-NLS-1$
	
    private final String title;
    private final String[] exts;
    private final String desc;
    private final boolean asBase64;
    private final Component parent;
    
    /**
     * Crea la acci&oacute;n para la recuperaci&oacute;n del nombre y el contenido de un fichero.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param exts Extensiones de fichero aceptadas por defecto.
     * @param desc Descripci&opacute;n del tipo de fichero aceptado por defecto.
     * @param asBase64 {@code true}= Base64, {@code false}=Texto.
     * @param parent Componente padre sobre el que se mostrar&aacute; el di&aacute;logo.
     */
    GetFileNameContentAction(final String title, final String[] exts, final String desc, final boolean asBase64, final Component parent) {
    	this.title = title;
        this.exts = (exts != null ? exts.clone() : null);
        this.desc = desc;
        this.asBase64 = asBase64;
        this.parent = parent;        
    }
    
    /**
     * Muestra un di&aacute;logo modal para la carga de un fichero y recuperar el nombre
     * del mismo y su contenido.
     * @return Nombre del fichero, el car&aacute;cter "|" y el contenido del fichero (Texto o
     * BinarioB64 seg&uacute;n el indicador {@code asBase64}).
     * @throws AOCancelledOperationException Cuando se cancela la operaci&oacute;n de selecci&oacute;n.
     * @throws IOException Cuando se produce un error al leer el fichero.
     */
	public String run() throws IOException {
        final String path = (new FileSelectionDialog(this.title, this.exts, this.desc, this.parent)).getPath();
        final File file = new File(path);
        final FileInputStream is = new FileInputStream(file);
        byte[] contentFic = null;
        try {
        	contentFic = AOUtil.getDataFromInputStream(is);
        } 
        finally {
        	try {
        		is.close();
        	} 
        	catch (final Exception e) {
        		/* Ignoramos este error */
        	}
        }
        return file.getName() + SEPARATOR + (this.asBase64 ?
        		Base64.encodeBytes(contentFic) : new String(contentFic));
	}
}
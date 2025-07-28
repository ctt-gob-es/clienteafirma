package es.gob.afirma.standalone.ui;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.LoggerUtil;

/**
 * Clase con el comportamiento que inicia la carga de ficheros en el panel de
 * firma como respuesta a que se arrastren sobre &eacute;l.
 */
public class DropDataFileListener implements DropTargetListener {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final LoadDataFileListener listener;

	/**
	 * Construye la clase para la carga de ficheros.
	 * @param listener Objeto que lanzar&aacute; la operaci&oacute;n de carga.
	 */
	public DropDataFileListener(final LoadDataFileListener listener) {
		this.listener = listener;
	}

    @Override
    public void dragEnter(final DropTargetDragEvent dtde) {
        if (!dtde.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
            dtde.rejectDrag();
        }
    }

    @Override
    public void dropActionChanged(final DropTargetDragEvent dtde) { /* No implementado */}

    @Override
    public void dragOver(final DropTargetDragEvent dtde) { /* No implementado */ }

    @Override
    public void dragExit(final DropTargetEvent dte) { /* No implementado */ }

    @Override
    public void drop(final DropTargetDropEvent dtde) {

    	 final Transferable tr = dtde.getTransferable();
         if (tr.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
             dtde.acceptDrop(DnDConstants.ACTION_COPY);
             final Object transData;
             try {
                 transData = tr.getTransferData(DataFlavor.javaFileListFlavor);
             }
             catch (final Exception e) {
                 LOGGER.warning(
                     "Ha fallado la operacion de arrastrar y soltar: " + e //$NON-NLS-1$
                 );
                 dtde.dropComplete(false);
                 return;
             }
             if (transData instanceof List) {
            	 dtde.getDropTargetContext().dropComplete(true);
            	 final List<?> fileList = (List<?>) transData;
            	 if (fileList.isEmpty()) {
            		 dtde.dropComplete(false);
            		 return;
            	 }

            	 final List<File> files = new ArrayList<>();
            	 for (final Object fileObject : fileList) {
            		 File file;
            		 final String filename = fileObject.toString();
            		 if (filename.startsWith("http://") || //$NON-NLS-1$
            				 filename.startsWith("https://") || //$NON-NLS-1$
            				 filename.startsWith("ftp://")) { //$NON-NLS-1$
            			 LOGGER.warning("Solo se soportan ficheros locales. Se ignorara: " + filename); //$NON-NLS-1$
            			 continue;
            		 }
            		 else if (filename.startsWith("file://")) { //$NON-NLS-1$
            			 try {
            				 file = new File(new URI(filename));
            			 }
            			 catch (final Exception e) {
            				 LOGGER.warning(String.format("Error al cargar el fichero '%s', se ignorara: ", LoggerUtil.getCleanUserHomePath(filename)) + e); //$NON-NLS-1$
            				 continue;
            			 }
            		 }
            		 else {
            			 file = new File(filename);
            		 }
            		 files.add(file);
            	 }

            	 // Llamamos al metodo para la carga de los ficheros seleccionados
            	 this.listener.loadFiles(files.toArray(new File[files.size()]), null);
             }
         }
         else {
             dtde.rejectDrop();
             dtde.dropComplete(false);
         }
	}

}

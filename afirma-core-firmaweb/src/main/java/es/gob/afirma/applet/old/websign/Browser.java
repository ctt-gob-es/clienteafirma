/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.applet.old.websign;

import java.awt.Frame;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.logging.Logger;

import javax.swing.UIManager;

/** Clase espec&iacute;fica para firma Web. */
public final class Browser {

    static {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        }
        catch (final Exception evt) {
            Logger.getLogger("es.gob.afirma").warning("No se pudo cargar el Look & Feel de Windows" + evt); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /** Obtiene una firma Web.
     * @param html
     *        HTML a firmar
     * @param hashAlg
     *        Algoritmo de huella digital a usar
     * @return Firma Web
     * @throws IOException
     *         Si ocurren errores de entrada / salida
     * @throws NoSuchAlgorithmException
     *         Si el algoritmo de huella digital proporcionado no es
     *         v&aacute;lido */
    public FirmadorWeb.FirmaWeb browse(final String html, final String hashAlg) throws IOException, NoSuchAlgorithmException {
        final BrowserDialog bd;
        if (Frame.getFrames() != null && Frame.getFrames().length > 0) {
            bd = new BrowserDialog(html, Frame.getFrames()[0]);
        }
        else {
            bd = new BrowserDialog(html, new Frame());
        }

        bd.setVisible(true);
        final boolean firmar = bd.isFirmar();

        if (firmar) {
            return new FirmadorWeb().firmar(html,
                                                        AFirmaWebSignHTMLDocument.getAttachedFiles().toArray(new Attachment[AFirmaWebSignHTMLDocument.getAttachedFiles().size()]),
                                                        hashAlg);
        }
        return null;

    }
}

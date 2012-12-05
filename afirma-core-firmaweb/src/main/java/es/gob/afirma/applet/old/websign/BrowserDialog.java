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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringReader;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.Document;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.ui.AOUIFactory;

final class BrowserDialog extends JDialog {

    private static final long serialVersionUID = 1554097041346695276L;

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private boolean firmar = false;

    void setFirmar(final boolean f) {
        this.firmar = f;
    }

    private final Action afirmar = new AbstractAction() {
        private static final long serialVersionUID = 1L;

        {
            putValue(Action.NAME, WebSignMessages.getString("BrowserDialog.0")); //$NON-NLS-1$
        }

        @Override
		public void actionPerformed(final ActionEvent e) {
            BrowserDialog.this.setFirmar(true);
            dispose();
        }
    };

    private final Action anoFirmar = new AbstractAction() {
        private static final long serialVersionUID = 1L;

        {
            putValue(Action.NAME, WebSignMessages.getString("BrowserDialog.1")); //$NON-NLS-1$
        }

        @Override
		public void actionPerformed(final ActionEvent e) {
            BrowserDialog.this.setFirmar(false);
            dispose();
        }
    };

    private final HTMLEditorKit kit = new HTMLEditorKit() {
        private static final long serialVersionUID = 1L;

        @Override
        public Document createDefaultDocument() {
            final HTMLDocument doc = new AFirmaWebSignHTMLDocument(getStyleSheet());
            doc.setAsynchronousLoadPriority(4);
            doc.setTokenThreshold(100);
            return doc;
        }
    };

    private final HyperlinkListener linkListener = new HyperlinkListener() {
        @Override
		public void hyperlinkUpdate(final HyperlinkEvent hl) {
            if (hl.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED) && hl.getDescription().startsWith("afirma:saveFile(")) { //$NON-NLS-1$
                final String strId = hl.getDescription().substring(hl.getDescription().indexOf('(') + 1, hl.getDescription().indexOf(')'));
                final int id = Integer.parseInt(strId);
                final Attachment fich = AFirmaWebSignHTMLDocument.getAttachedFiles().get(id);

                final int r = JOptionPane.showConfirmDialog(
                	BrowserDialog.this,
                    WebSignMessages.getString("BrowserDialog.2"), //$NON-NLS-1$
                    WebSignMessages.getString("BrowserDialog.3"), //$NON-NLS-1$
                    JOptionPane.YES_NO_OPTION
                );
                if (r == JOptionPane.YES_OPTION) {
                    try {
                    	AOUIFactory.getSaveDataToFile(
                    		AOUtil.getDataFromInputStream(fich.getContentInputStream()),
                			WebSignMessages.getString("BrowserDialog.4"), //$NON-NLS-1$
                			fich.getFile(),
                			null,
                			BrowserDialog.this
            			);
                    }
                    catch(final AOCancelledOperationException e) {
                        JOptionPane.showMessageDialog(
                    		BrowserDialog.this,
                            WebSignMessages.getString("BrowserDialog.5"), //$NON-NLS-1$
                            WebSignMessages.getString("BrowserDialog.6"), //$NON-NLS-1$
                            JOptionPane.WARNING_MESSAGE
                        );
                    }
                    catch (final FileNotFoundException exc) {
                        LOGGER.severe("No se encontro el adjunto: " + exc); //$NON-NLS-1$
                        JOptionPane.showMessageDialog(BrowserDialog.this,
                                                      WebSignMessages.getString("BrowserDialog.8"), //$NON-NLS-1$
                                                      WebSignMessages.getString("BrowserDialog.9"), //$NON-NLS-1$
                                                      JOptionPane.ERROR_MESSAGE);
                    }
                    catch (final IOException exc) {
                        LOGGER.severe("Error al leer el adjunto: " + exc); //$NON-NLS-1$
                        JOptionPane.showMessageDialog(BrowserDialog.this, WebSignMessages.getString("BrowserDialog.11"), WebSignMessages.getString("BrowserDialog.9"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
                    }
                }
            }
        }
    };

    BrowserDialog(final String html, final Frame parent) {
        super(parent, WebSignMessages.getString("BrowserDialog.13"), true); //$NON-NLS-1$

        this.getAccessibleContext().setAccessibleParent(parent);

        final JEditorPane ep = new JEditorPane();
        ep.setEditable(false);
        ep.setEnabled(false);
        ep.addHyperlinkListener(this.linkListener);
        getContentPane().add(new JScrollPane(ep), BorderLayout.CENTER);

        setSize(600, 600);

        final JPanel sur = new JPanel();

        sur.add(new JButton(this.afirmar), BorderLayout.WEST);
        sur.add(new JButton(this.anoFirmar), BorderLayout.EAST);
        getContentPane().add(sur, BorderLayout.SOUTH);

        ep.setEditorKit(this.kit);
        try {
            final Document doc = ep.getDocument();
            this.kit.read(new StringReader(html), doc, 0);

            disableContent(ep);
        }
        catch (final Exception e) {
            LOGGER.severe(WebSignMessages.getString("BrowserDialog.14") + e); //$NON-NLS-1$
        }
    }

    private void disableContent(final Container cnt) {
        final int numChildrens = cnt.getComponentCount();
        for (int i = 0; i < numChildrens; i++) {
            final Component children = cnt.getComponent(i);
            if (children instanceof Container) {
                disableContent((Container) children);
            }
            children.setEnabled(false);
        }
        cnt.setEnabled(false);
    } // disableContent

    @Override
    public void setVisible(final boolean b) {
        JOptionPane.showMessageDialog(
    		this,
            WebSignMessages.getString("BrowserDialog.15"), //$NON-NLS-1$
            WebSignMessages.getString("BrowserDialog.6"), //$NON-NLS-1$
            JOptionPane.WARNING_MESSAGE
        );
        super.setVisible(b);
    }

    public boolean isFirmar() {
        return this.firmar;
    }
}

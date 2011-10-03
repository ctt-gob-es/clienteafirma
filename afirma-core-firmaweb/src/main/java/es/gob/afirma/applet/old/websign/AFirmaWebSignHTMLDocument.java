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

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.text.MutableAttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;

final class AFirmaWebSignHTMLDocument extends HTMLDocument {
    private static final long serialVersionUID = 1L;

    static List<Attachment> files = new ArrayList<Attachment>();
    
    static List<Attachment> getAttachedFiles() {
        return files;
    }

    AFirmaWebSignHTMLDocument(final StyleSheet styles) {
        super(styles);
        files.clear();
    }

    @Override
    public HTMLEditorKit.ParserCallback getReader(final int pos) {
        return new CustomReader(pos);
    }

    private class CustomReader extends HTMLDocument.HTMLReader {
        CustomReader(final int pos) {
            super(pos);
            registerTag(AFirmaTag.INSTANCE, new AFirmaTagAction());
        }

        @Override
        public void handleText(final char[] data, final int pos) {
            super.handleText(data, pos);
        }

        @Override
        public void handleComment(final char[] data, final int pos) {
            // No implementado
        }

        @Override
        public void handleStartTag(final HTML.Tag t, final MutableAttributeSet a, final int pos) {
            if (t.toString().toLowerCase().equals("afirma")) { //$NON-NLS-1$
                final String type = (String) a.getAttribute(HTML.Attribute.TYPE);
                if (type != null && type.equals("file")) { //$NON-NLS-1$
                    final String uri = (String) a.getAttribute(HTML.Attribute.HREF);
                    startFile(uri);
                }
            }
            else {
                super.handleStartTag(t, a, pos);
            }
            this.tagLevel++;
        }

        @Override
        public void handleEndTag(final HTML.Tag t, final int pos) {
            if (!t.toString().toLowerCase().equals("afirma")) { //$NON-NLS-1$
                super.handleEndTag(t, pos);
            }
            this.tagLevel--;
        }

        @Override
        public void handleSimpleTag(final HTML.Tag t, final MutableAttributeSet a, final int pos) {
            if (t.toString().toLowerCase().equals("afirma")) { //$NON-NLS-1$
                if (!a.isDefined(HTML.Attribute.ENDTAG)) {
                    final String type = (String) a.getAttribute(HTML.Attribute.TYPE);
                    if (type != null && type.equals("file")) { //$NON-NLS-1$
                        final String uri = (String) a.getAttribute(HTML.Attribute.HREF);
                        try {
                            AFirmaWebSignHTMLDocument.files.add(new Attachment(URLDecoder.decode(uri, "UTF-8"))); //$NON-NLS-1$
                        }
                        catch (final IOException e) {
                            AFirmaWebSignHTMLDocument.files.add(new Attachment(uri));
                        }
                    }
                }
            }
            else {
                super.handleSimpleTag(t, a, pos);
            }
        }

        @Override
        public void handleError(final String errorMsg, final int pos) {
            Logger.getLogger("es.gob.afirma").severe("Error: " + errorMsg + ", posicion: " + pos); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            super.handleError(errorMsg, pos);
        }

        private int tagLevel;

        void startFile(final String uri) {
            try {
                AFirmaWebSignHTMLDocument.files.add(new Attachment(URLDecoder.decode(uri, "UTF-8"))); //$NON-NLS-1$
            }
            catch (final UnsupportedEncodingException e) {
                AFirmaWebSignHTMLDocument.files.add(new Attachment(uri));
            }
        }

        private class AFirmaTagAction extends HTMLDocument.HTMLReader.TagAction {

            AFirmaTagAction() {
                super();
            }

            @Override
            public void start(final HTML.Tag t, final MutableAttributeSet a) {
                final String type = (String) a.getAttribute("type"); //$NON-NLS-1$
                if (type.equalsIgnoreCase("file")) { //$NON-NLS-1$
                    final String uri = (String) a.getAttribute("uri"); //$NON-NLS-1$
                    startFile(uri);
                }
            }

            @Override
            public void end(final HTML.Tag t) {
                // No implementado
            }

        }

    }

}

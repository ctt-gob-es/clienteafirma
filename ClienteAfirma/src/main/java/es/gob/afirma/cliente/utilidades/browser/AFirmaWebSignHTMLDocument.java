/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente.utilidades.browser;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.text.BadLocationException;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;

final class AFirmaWebSignHTMLDocument extends HTMLDocument {
    private static final long serialVersionUID = 1L;

    static List<Attachment> files = new ArrayList<Attachment>();

    AFirmaWebSignHTMLDocument(StyleSheet styles) {
        super(styles);
        files.clear();
    }

    @Override
    public HTMLEditorKit.ParserCallback getReader(int pos) {
        return new CustomReader(pos);
    }

    class CustomReader extends HTMLDocument.HTMLReader {
        public CustomReader(final int pos) {
            super(pos);
            registerTag(AFirmaTag.INSTANCE, new AFirmaTagAction());
            Logger.getLogger("es.gob.afirma").info("Posicion del lector: " + pos);
        }

        @Override
        public void handleText(final char[] data, final int pos) {
            Logger.getLogger("es.gob.afirma").info("Posicion del lector: " + new String(data));
            super.handleText(data, pos);
        }

        @Override
        public void handleComment(final char[] data, final int pos) {}

        @Override
        public void handleStartTag(final HTML.Tag t, final MutableAttributeSet a, final int pos) {
            if (t.toString().toLowerCase().equals("afirma")) {
                String type = (String) a.getAttribute(HTML.Attribute.TYPE);
                if (type != null && type.equals("file")) {
                    String uri = (String) a.getAttribute(HTML.Attribute.HREF);
                    startFile(uri);
                }
                else {
                    Logger.getLogger("es.gob.afirma").info("1. tipo erroneo: " + type);
                    Enumeration<?> e = a.getAttributeNames();
                    while (e.hasMoreElements()) {
                        Logger.getLogger("es.gob.afirma").info("ATT: " + e.nextElement());
                    }
                }
            }
            else {
                Logger.getLogger("es.gob.afirma").info("Inicio: " + t.toString());
                super.handleStartTag(t, a, pos);
            }
            tagLevel++;
        }

        @Override
        public void handleEndTag(HTML.Tag t, int pos) {
            Logger.getLogger("es.gob.afirma").info("Fin: " + t.toString());
            if (t.toString().toLowerCase().equals("afirma")) {
                Logger.getLogger("es.gob.afirma").info("Fin de afirma");
                // if(inFile)
                // {
                // endFile();
                // }
            }
            else {
                super.handleEndTag(t, pos);
            }
            tagLevel--;
        }

        @Override
        public void handleSimpleTag(HTML.Tag t, MutableAttributeSet a, int pos) {
            if (t.toString().toLowerCase().equals("afirma")) {
                if (a.isDefined(HTML.Attribute.ENDTAG)) {
                    Logger.getLogger("es.gob.afirma").info("Fin de afirma");
                    // if(inFile)
                    // {
                    // endFile();
                    // }
                }
                else {
                    String type = (String) a.getAttribute(HTML.Attribute.TYPE);
                    if (type != null && type.equals("file")) {
                        String uri = (String) a.getAttribute(HTML.Attribute.HREF);
                        try {
                            AFirmaWebSignHTMLDocument.files.add(new Attachment(URLDecoder.decode(uri, "UTF-8")));
                        }
                        catch (IOException e) {
                            AFirmaWebSignHTMLDocument.files.add(new Attachment(uri));
                        }
                    }
                    else {
                        Logger.getLogger("es.gob.afirma").info("2. tipo erroneo: " + type);
                        Enumeration<?> e = a.getAttributeNames();
                        while (e.hasMoreElements()) {
                            Logger.getLogger("es.gob.afirma").info("ATT: " + e.nextElement());
                        }
                    }
                }
            }
            else {
                Logger.getLogger("es.gob.afirma").info("Simple: " + t.toString());
                super.handleSimpleTag(t, a, pos);
            }
        }

        @Override
        public void handleError(String errorMsg, int pos) {
            Logger.getLogger("es.gob.afirma").severe("Error: " + errorMsg + ", posicion: " + pos);
            super.handleError(errorMsg, pos);
        }

        int tagLevel;

        // private boolean inFile = false;
        //
        // private String inFileUri = null;

        private void startFile(final String uri) {
            try {
                AFirmaWebSignHTMLDocument.files.add(new Attachment(URLDecoder.decode(uri, "UTF-8")));
            }
            catch (UnsupportedEncodingException e) {
                AFirmaWebSignHTMLDocument.files.add(new Attachment(uri));
            }

        }

        // public void endFile()
        // {
        // // logger.debug("End " + inFileUri);
        // inFile = false;
        // inFileUri = null;
        // }

        private class AFirmaTagAction extends HTMLDocument.HTMLReader.TagAction {

            AFirmaTagAction() {
                super();
            }

            @Override
            public void start(HTML.Tag t, MutableAttributeSet a) {
                String type = (String) a.getAttribute("type");
                if (type.equalsIgnoreCase("file")) {
                    String uri = (String) a.getAttribute("uri");
                    startFile(uri);
                }
            }

            // public void start(HTML.Tag t)
            // {
            // Logger.getLogger("es.gob.afirma").warning("TAG SIN ATTR: "+
            // t.toString());
            // }

            // public void end(HTML.Tag t, MutableAttributeSet a)
            // {
            // Logger.getLogger("es.gob.afirma").warning("fin(" + t.toString() +
            // ", " + a + ")"+ t.toString());
            // endFile();
            // }

            @Override
            public void end(HTML.Tag t) {
                // endFile();
            }

        }

    }

}

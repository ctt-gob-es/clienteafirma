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

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLEncoder;
import java.security.DigestOutputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/** Clase para la realizaci&oacute;n de firmas de p&aacute;ginas Web XHTML. */
public final class FirmadorWeb {

    /** Firma de una p&aacute;gina Web XHTML. */
    public static class FirmaWeb {

        final File tmpWebDataFile;

        FirmaWeb(final File tmpWebDataFile) {
            this.tmpWebDataFile = tmpWebDataFile;
        }
    }

    @SuppressWarnings("restriction")
    FirmaWeb firmar(final String html, final Attachment[] attachments, final String hashAlgorithm) throws IOException, NoSuchAlgorithmException {
        final File tmpWebDataFile = File.createTempFile("afirma5_firmaweb", ".tmp"); //$NON-NLS-1$ //$NON-NLS-2$
        tmpWebDataFile.deleteOnExit();

        final OutputStream os = new FileOutputStream(tmpWebDataFile);
        try {
            // Usamos el DigestOutputStream para calcular el hash a la vez que
            // lo escribimos
            final DigestOutputStream dos = new DigestOutputStream(os, MessageDigest.getInstance(hashAlgorithm.toUpperCase()));

            // Lo usaremos para escribir los ficheros en b64
            final BufferedOutputStream bos = new BufferedOutputStream(dos);
            // Base64OutputStrea b64os= new Base64OutputStream(dos);

            // Escribimos el HTML
            dos.write(html.getBytes());
            dos.flush();

            // Anadimos los ficheros
            for (final Attachment attach : attachments) {
                if (attach.getURL().trim().length() > 0) {
                    // Escribimos el tag de apertura
                    final String openTag = "<afirma type='filecontent' path='" + URLEncoder.encode(attach.getURL(), "UTF-8") + "'><!--\n"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    dos.write(openTag.getBytes());
                    dos.flush();

                    // Leemos el fichero con ventana grafica
                    final FileInputStream attachIS = attach.getContentInputStream();
                    try {
                        // Volcamos el fichero en b64
                        int nBytes;
                        final byte[] buffer = new byte[1024];
                        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                        while ((nBytes = attachIS.read(buffer)) != -1) {
                            baos.write(buffer, 0, nBytes);
                        }
                        try {
                            attachIS.close();
                        }
                        catch (final Exception e) {
                            // Ignoramos los errores en el cierre
                        }

                        bos.write(new sun.misc.BASE64Encoder().encode(baos.toByteArray()).getBytes());
                        bos.flush();
                    }
                    finally {
                        // Cerramos el fichero que estamos leyendo
                        attachIS.close();
                    }

                    // Escribimos el tag de cierre
                    final String closeTag = "\n--></afirma>"; //$NON-NLS-1$
                    dos.write(closeTag.getBytes());
                    dos.flush();
                }
            }

            tryClose(bos);
            tryClose(dos);

        }
        finally {
            tryClose(os);
        }

        return new FirmaWeb(tmpWebDataFile);
    }

    private void tryClose(final OutputStream os) {
        try {
            if (os != null) {
                os.close();
            }
        }
        catch (final IOException e) {
            // Ignoramos los errores en el ciere
        }
    }
}

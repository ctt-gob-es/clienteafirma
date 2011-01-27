/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.cliente.utilidades.browser;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLEncoder;
import java.security.DigestOutputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.logging.Logger;

import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.aoclasses.GraphicalFileInputStream;


public class FirmadorWeb
{

    public static class FirmaWeb
    {
        //private final byte[] hash;
        //private final String hashAlgorithm;
        public final File   tmpWebDataFile;
        
        FirmaWeb(byte[] hash, String hashAlgorithm, File   tmpWebDataFile)
        {
            //this.hash= hash;
            //this.hashAlgorithm= hashAlgorithm;
            this.tmpWebDataFile= tmpWebDataFile;
        }
    }

    FirmaWeb firmar(String html, Attachment[] attachments, String hashAlgorithm) throws IOException, NoSuchAlgorithmException
    {
        byte[] hash;
        File tmpWebDataFile= File.createTempFile("afirma5_firmaweb", ".tmp");
        tmpWebDataFile.deleteOnExit();
        
        OutputStream os= new FileOutputStream(tmpWebDataFile);
        try
        {
            // Usamos el DigestOutputStream para calcular el hash a la vez que lo escribimos
            DigestOutputStream dos= new DigestOutputStream(os, MessageDigest.getInstance(hashAlgorithm.toUpperCase()));

            // Lo usaremos para escribir los ficheros en b64
            BufferedOutputStream bos = new BufferedOutputStream(dos);
//            Base64OutputStrea b64os= new Base64OutputStream(dos);
            
            // Escribimos el HTML
            dos.write(html.getBytes());
            dos.flush();

            // Añadimos los ficheros
            for(int i=0; i<attachments.length; i++)
            {
                Attachment attach= attachments[i];

                if(attach.url.trim().length()>0)
                {
                    // Escribimos el tag de apertura
                    String openTag= "<afirma type='filecontent' path='"+URLEncoder.encode(attach.url, "UTF-8")+"'><!--\n";
                    dos.write(openTag.getBytes());
                    dos.flush();
    
                    // Leemos el fichero con ventana gráfica
                    GraphicalFileInputStream attachIS= attach.getContentInputStream();
                    try
                    {
                        // Volcamos el fichero en b64
                        Logger.getLogger("es.map,afirma").info("Tama\u00F1o del buffer: "+1024);
                        int nBytes;
                		byte[] buffer = new byte[1024];
                		ByteArrayOutputStream baos = new ByteArrayOutputStream();
                		while((nBytes = attachIS.read(buffer)) != -1) {
                			baos.write(buffer, 0, nBytes);
                		}
                        try {attachIS.close();} catch (Exception e) {
                        	Logger.getLogger("es.gob.afirma").warning("No se pudo cerrar el flujo del fichero adjunto");
                        }                     
                		
                        new AOCryptoUtil.RawBASE64Encoder().encode(new ByteArrayInputStream(baos.toByteArray()), bos);
                        bos.flush();
                    }
                    finally
                    {
                        // Cerramos el fichero que estamos leyendo
                        attachIS.close();
                    }
                
                    // Escribimos el tag de cierre
                    String closeTag= "\n--></afirma>";
                    dos.write(closeTag.getBytes());
                    dos.flush();
                }
            }
            
            tryClose(bos);
            tryClose(dos);

            hash= dos.getMessageDigest().digest();
        }
        finally
        {
            tryClose(os);
        }
        
        return new FirmaWeb(hash, hashAlgorithm, tmpWebDataFile);
    }
    
    private void tryClose(OutputStream os) {
        try {
            if(os!= null) os.close();
        }
        catch(IOException e) {}
    }
}

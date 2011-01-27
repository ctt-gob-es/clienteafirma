/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.misc;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

/**
 * Clase con m&eacute;todos para el trabajo con ficheros.
 */
public class AOFileUtils {

  /**
   * Crea un fichero ZIP en disco apto para manejarse.
   * @param zipFileData Los datos del zip.
   * @return  Devuelve un fichero Zip.
   * @throws ZipException Cuando los datos no eran realmente un Zip.
   * @throws IOException Cuando ocurre un error al leer los datos o crear el temporal para abrir el Zip. 
   */
  public static ZipFile createTempZipFile(byte[] zipFileData) throws ZipException, IOException {

      // Creamos un fichero temporal
      File tempFile = File.createTempFile("afirmazip", null);
      FileOutputStream fos = new FileOutputStream(tempFile);
      fos.write(zipFileData);
      fos.flush();
      fos.close();
      
      try {
      	tempFile.deleteOnExit();
      } catch (Exception e) {}
      
      return new ZipFile(tempFile);
  }
}

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

import java.io.File;
import java.io.FileNotFoundException;
import java.util.logging.Logger;

import es.gob.aoclasses.GraphicalFileInputStream;

class Attachment
{
    public final String url;
    //public final byte[] bytes;
    
    public Attachment(String url)
    {
        this.url= url;
    //    this.bytes= bytes;
    }
    
    public String getName()
    {
        int p1= url.lastIndexOf(File.pathSeparatorChar);
        return p1<url.length()?url.substring(p1+1):"";
    }
    
    public File getFile()
    {
        return new File(url);
    }

    public GraphicalFileInputStream getContentInputStream() throws FileNotFoundException
    {
        Logger.getLogger("es.gob.afirma").info(url);
        GraphicalFileInputStream is= new GraphicalFileInputStream(new File(url));
        return is;
    }
}

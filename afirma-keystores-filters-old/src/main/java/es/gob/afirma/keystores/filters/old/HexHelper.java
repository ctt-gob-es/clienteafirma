/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.keystores.filters.old;


final class HexHelper {
    private static final HexHelper INSTANCE = new HexHelper();

    private static final char[] HEX_CHARS= new char[]{
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
    
    private HexHelper()
    {
        super();
    }

    public static HexHelper getInstance()
    {
        return INSTANCE;
    }

    public String toString(byte[] buff)
    {
        return toHex(buff);
    }

    private String toHex(byte b)
    {
        int v= (b<0)?(128+(b&127)):b;
        int d0= v%16;
        int d1= v/16;
                
        return new String(new char[]{HEX_CHARS[d1], HEX_CHARS[d0]});
    }

    public String toHex(byte[] data)
    {
        final String hex;
        
        if(data.length>0)
        {
            final StringBuffer sb= new StringBuffer(data.length *3);
            
            sb.append(toHex(data[0]));
            for(int i=1; i<data.length; i++)
            {
                sb.append(':').append(toHex(data[i]));
            }
            
            hex= sb.toString();
        }
        else
        {
            hex= ""; //$NON-NLS-1$
        }
        
        return hex;
    }
}
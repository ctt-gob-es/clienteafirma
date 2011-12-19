package es.gob.afirma.signature;

import java.io.ByteArrayInputStream;
import java.util.HashSet;
import java.util.Set;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/** Clases de utilidad para la gesti&oacute;n de facturas electr&oacute;nicas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class FacturaeHelper {
    
    private FacturaeHelper() {
        // No permitimos la instanciacion
    }
    
    /** Indica si los datos son una factura electr&oacute;nica.
     * @param file Datos a comprobar
     * @return <code>true</code> si los datos son una <a href="http://www.facturae.es/">factura electr&oacute;nica</a>,
     *         <code>false</code> en caso contrario */
    public static boolean isFacturae(final byte[] file) {
        if (file == null || file.length == 0) {
            return false;
        }
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        
        try {
            final Document doc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(file));
            final Element rootNode = doc.getDocumentElement();
            final String rootNodePrefix = rootNode.getPrefix();
            
            if (!(((rootNodePrefix != null) ? (rootNodePrefix + ":") : "") + "Facturae").equals(rootNode.getNodeName())) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                return false;
            }
            
            final Set<String> childs = new HashSet<String>(3);
            childs.add("FileHeader"); //$NON-NLS-1$
            childs.add("Parties"); //$NON-NLS-1$
            childs.add("Invoices"); //$NON-NLS-1$
            
            final NodeList nl = rootNode.getChildNodes();
            for (int i=0;i<nl.getLength();i++) {
                final String nodeName = nl.item(i).getNodeName();
                if (childs.contains(nodeName)) {
                    childs.remove(nodeName);
                }
            }
            if (childs.size() > 0) {
                return false;
            }
            
        }
        catch (final Exception e) {
            return false;
        }
        return true;
    }

}

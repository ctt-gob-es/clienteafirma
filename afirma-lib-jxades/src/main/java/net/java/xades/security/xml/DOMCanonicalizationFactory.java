package net.java.xades.security.xml;

import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.security.Init;
import com.sun.org.apache.xml.internal.security.c14n.CanonicalizationException;
import com.sun.org.apache.xml.internal.security.c14n.Canonicalizer;
import com.sun.org.apache.xml.internal.security.c14n.InvalidCanonicalizerException;

public class DOMCanonicalizationFactory
{
    public static byte[] c14n(String uri, Node node) throws InvalidCanonicalizerException,
            CanonicalizationException, ParserConfigurationException, IOException, SAXException
    {
        byte[] result = null;

        if (!Init.isInitialized())
        {
            Init.init();
        }

        Canonicalizer c14n = Canonicalizer.getInstance(uri);
        result = c14n.canonicalizeSubtree(node);

        return result;
    }
}

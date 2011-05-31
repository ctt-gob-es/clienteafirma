package es.gob.afirma.cliente.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import es.gob.afirma.cliente.test.ciphers.TestCipher;
import es.gob.afirma.cliente.test.keystore.TestKeyStores;
import es.gob.afirma.cliente.test.signers.TestCAdES;
import es.gob.afirma.cliente.test.signers.TestCMS;
import es.gob.afirma.cliente.test.signers.TestODF;
import es.gob.afirma.cliente.test.signers.TestOOXML;
import es.gob.afirma.cliente.test.signers.TestPDF;
import es.gob.afirma.cliente.test.signers.TestXAdES;
import es.gob.afirma.cliente.test.signers.TestXMLDSig;

/** Todos los tests del Cliente Afirma. */
@RunWith(Suite.class)
@Suite.SuiteClasses({
        TestCipher.class,
        TestKeyStores.class,
        TestCAdES.class,
        TestCMS.class,
        TestODF.class,
        TestOOXML.class,
        TestPDF.class,
        TestXAdES.class,
        TestXMLDSig.class
})
public class AllTests {

}

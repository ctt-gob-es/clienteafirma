package test.es.gob.jmulticard.apdu;

import junit.framework.TestCase;
import es.gob.jmulticard.apdu.dnie.GetChipInfoApduCommand;

/** Prueba del m&eacute;todo {@linkplain es.gob.jmulticard.apdu.CommandApdu#setLe(int)} de 
 * la clase {@linkplain es.gob.jmulticard.apdu.CommandApdu}
 * @author Alberto Mart&iacute;nez */
public class TestCommandApdu extends TestCase {

    /** {@linkplain es.gob.jmulticard.apdu.CommandApdu#setLe(int)} */
    public final static void testSetLe() {
        final GetChipInfoApduCommand command = new GetChipInfoApduCommand();
        command.setLe(0);
    }
}
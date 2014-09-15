package es.gob.afirma.crypto.handwritten.data;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

import com.stepover.opensignatureapi.helpers.Base64;

final class DataConnection extends URLConnection {

    DataConnection(final URL u) {
        super(u);
    }

    @Override
    public void connect() throws IOException {
        this.connected = true;
    }

    @Override
    public InputStream getInputStream() throws IOException {

    	System.out.println("DECODIFICAMOS EL PROTOCOLO DATA");
    	final byte[] image = Base64.decode(
			this.url.toString().replaceFirst("^.*;base64,", "")  //$NON-NLS-1$//$NON-NLS-2$
		);
        return new ByteArrayInputStream(
    		image
		);
    }

}

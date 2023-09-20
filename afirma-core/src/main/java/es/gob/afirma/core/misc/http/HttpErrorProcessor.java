package es.gob.afirma.core.misc.http;

import java.io.IOException;
import java.util.Properties;

public interface HttpErrorProcessor {

	byte[] processHttpError(IOException e, UrlHttpManager urlManager, String url, int timeout,
							UrlHttpMethod method, Properties requestProperties)
									throws IOException;
}

package es.gob.afirma.core.misc.http;

import java.io.IOException;

public interface HttpProcessor {

	byte[] processHttpError(final String url, final int timeout,
							final String contentType, final String accept,
							final UrlHttpMethod method) throws IOException;
}

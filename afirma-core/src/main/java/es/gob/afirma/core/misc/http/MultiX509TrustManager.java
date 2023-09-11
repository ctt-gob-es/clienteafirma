package es.gob.afirma.core.misc.http;

import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.net.ssl.X509TrustManager;

public class MultiX509TrustManager implements X509TrustManager {

	private final X509TrustManager[] tms;

	public MultiX509TrustManager(final X509TrustManager[] tms) {
		this.tms = tms;
	}

	@Override
	public void checkClientTrusted(final X509Certificate[] chain, final String authType) throws CertificateException {
		boolean trusted = false;
		CertificateException ex = null;
		for (int i = 0; i < this.tms.length && !trusted; i++) {
			try {
				this.tms[i].checkClientTrusted(chain, authType);
				trusted = true;
			}
			catch (final CertificateException e) {
				ex = e;
			}
		}
		if (!trusted && ex != null) {
			throw ex;
		}
	}

	@Override
	public void checkServerTrusted(final X509Certificate[] chain, final String authType) throws CertificateException {

		boolean trusted = false;
		CertificateException ex = null;
		for (int i = 0; i < this.tms.length && !trusted; i++) {
			try {
				this.tms[i].checkServerTrusted(chain, authType);
				trusted = true;
			}
			catch (final CertificateException e) {
				ex = e;
			}
		}
		if (!trusted && ex != null) {
			throw ex;
		}
	}

	@Override
	public X509Certificate[] getAcceptedIssuers() {
		final List<X509Certificate> acceptedIssuers = new ArrayList<>();
		for (final X509TrustManager tm : this.tms) {
			acceptedIssuers.addAll(Arrays.asList(tm.getAcceptedIssuers()));
		}
		return acceptedIssuers.toArray(new X509Certificate[0]);
	}
}

package es.gob.afirma.standalone.protocol;

import es.gob.afirma.core.misc.protocol.ProtocolVersion;

/**
 * Arranca el canal local con la misma configuraci&oacute;n que usa el producto
 * ({@link ServiceInvocationManager} + {@link SecureSocketUtils}) para que
 * <code>scripts/interop/tls-probe.sh</code> pueda sondearlo con <code>openssl s_client</code>.
 *
 * No abre interfaz gr&aacute;fica. El almac&eacute;n <code>autofirma.pfx</code> debe estar en el
 * directorio de la aplicaci&oacute;n (el padre de <code>target/classes</code>). La contrase&ntilde;a
 * la resuelve {@link SecureSocketUtils} a trav&eacute;s del secreto local (T4): si solo existe
 * el almac&eacute;n heredado, se genera el secreto y se migra antes de abrir el socket.
 *
 * Uso: java -Dinterop.port=8881 -Dinterop.session=interop ... TlsProbeServer
 */
public final class TlsProbeServer {

	private TlsProbeServer() {
		// No instanciable
	}

	/** Punto de entrada.
	 * @param args Sin uso.
	 * @throws Exception Si el canal no se puede abrir. */
	public static void main(final String[] args) throws Exception {

		final int port = Integer.parseInt(System.getProperty("interop.port", "8881")); //$NON-NLS-1$ //$NON-NLS-2$
		final String session = System.getProperty("interop.session", "interop"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("[tls-probe] abriendo canal local en el puerto " + port); //$NON-NLS-1$

		ServiceInvocationManager.startService(
			new ChannelInfo(session, new int[] { port }),
			ProtocolVersion.getInstance(ProtocolVersion.VERSION_3)
		);
	}
}

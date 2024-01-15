/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.signer.processors;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;

/** Procesador de firmas trif&aacute;sicas XAdES FacturaE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class FacturaETriPhasePreProcessor extends XAdESTriPhasePreProcessor {

	/** Construye un procesador de firmas trif&aacute;sicas XAdES FacturaE. */
	public FacturaETriPhasePreProcessor() {
		super(true);
	}

	@Override
	public TriphaseData preProcessPreCoSign(final byte[] data,
			                          final String algorithm,
			                          final X509Certificate[] cert,
			                          final Properties extraParams,
			                          final boolean checkSignatures) throws IOException, AOException {
		throw new UnsupportedOperationException("No se soporta la multifirma de firmas FacturaE"); //$NON-NLS-1$
	}

	@Override
	public TriphaseData preProcessPreCounterSign(final byte[] sign,
			                               final String algorithm,
			                               final X509Certificate[] cert,
			                               final Properties extraParams,
			                               final CounterSignTarget targets,
				                           final boolean checkSignatures) throws IOException,
			                                                                       AOException {
		throw new UnsupportedOperationException("No se soporta la multifirma de firmas FacturaE"); //$NON-NLS-1$
	}

}

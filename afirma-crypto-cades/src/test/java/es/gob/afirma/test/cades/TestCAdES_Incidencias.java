/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.test.cades;

import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.cades.AOCAdESSigner;


/**
 * Pruebas del m&oacute;dulo CAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCAdES_Incidencias {

	/**
	 * Prueba de firma convencional.
	 * @throws Exception en cualquier error
	 */
	@SuppressWarnings("static-method")
	@Test
	public void inc71506_ExtraccionDatosCadesCanarias() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$


		InputStream is = TestCAdES_Incidencias.class.getResourceAsStream("/firma_inc71506.csig"); //$NON-NLS-1$
		byte[] signature = AOUtil.getDataFromInputStream(is);
		is.close();

		final AOSigner signer = new AOCAdESSigner();
		Assert.assertNotNull("El arbol de firmas no puede ser nulo", signer.getSignersStructure(signature, true)); //$NON-NLS-1$
	}

}

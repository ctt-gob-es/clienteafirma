/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.util.Properties;

import org.junit.Assert;
import org.junit.Test;

/** Pruebas del paso de propiedades desde JavaScript a Java. */
public final class TestExtraParamsProcessor {

    /** Prueba del paso de propiedades desde String con l&iacute;neas delimitadas por <code>\n</code> a <code>Properties</code> de Java. */
	@SuppressWarnings("static-method")
	@Test
	public void testExtraParamProcessor() {

		final String entries =
				"Clave1=valor\n" + //$NON-NLS-1$
				"2=valor\n" + //$NON-NLS-1$
				"clave3=v\n" + //$NON-NLS-1$
				"4=v\n" + //$NON-NLS-1$
				"=v\n" + //$NON-NLS-1$
				"5=\n" + //$NON-NLS-1$
				"=\n" + //$NON-NLS-1$
				"\n" + //$NON-NLS-1$
				"=valor\n" + //$NON-NLS-1$
				"clave6=\n" + //$NON-NLS-1$
				"clave7=val=or\n" + //$NON-NLS-1$
				"clave8=valor\n" + //$NON-NLS-1$
				"cla=ve9=valor\n" + //$NON-NLS-1$
				"clave0\n"; //$NON-NLS-1$

		final Properties params = ExtraParamsProcessor.convertToProperties(entries);
		Assert.assertNotNull(params);

		for (final String key : params.keySet().toArray(new String[0])) {
			System.out.println(key + " = " + params.getProperty(key)); //$NON-NLS-1$
		}
		Assert.assertEquals(11, params.size());
	}

}

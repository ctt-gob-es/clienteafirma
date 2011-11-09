package es.gob.afirma.miniapplet;

import java.util.Properties;

import junit.framework.Assert;

import org.junit.Test;

/** Pruebas del paso de propiedades desde JavaScript a Java. */
public class ExtraParamsProcessorTest {

    /** Prueba del paso de propiedades desde String con l&iacute;neas delimitadas por <code>\n</code> a <code>Properties</code> de Java. */
	@Test
	public void testExtraParamProcessor() {

		String entries =
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

		Properties params = ExtraParamsProcessor.convertToProperties(entries);
		Assert.assertNotNull(params);

		for (String key : params.keySet().toArray(new String[0])) {
			System.out.println(key + " = " + params.getProperty(key)); //$NON-NLS-1$
		}
		Assert.assertEquals(9, params.size());
	}

}

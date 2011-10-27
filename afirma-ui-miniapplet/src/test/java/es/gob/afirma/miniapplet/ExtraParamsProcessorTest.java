package es.gob.afirma.miniapplet;

import java.util.Properties;

import junit.framework.Assert;

import org.junit.Test;

public class ExtraParamsProcessorTest {

	@Test
	public void testExtraParamProcessor() {

		String[] entries = new String[] {
				"Clave1=valor", //$NON-NLS-1$
				"2=valor", //$NON-NLS-1$
				"clave3=v", //$NON-NLS-1$
				"4=v", //$NON-NLS-1$
				"=v", //$NON-NLS-1$
				"5=", //$NON-NLS-1$
				"=", //$NON-NLS-1$
				"", //$NON-NLS-1$
				"=valor", //$NON-NLS-1$
				"clave6=", //$NON-NLS-1$
				"clave7=val=or", //$NON-NLS-1$
				"clave8=valor", //$NON-NLS-1$
				"cla=ve9=valor", //$NON-NLS-1$
				"clave0", //$NON-NLS-1$
				null
		};

		Properties params = ExtraParamsProcessor.convertToProperties(entries);
		Assert.assertNotNull(params);

		for (String key : params.keySet().toArray(new String[0])) {
			System.out.println(key + " = " + params.getProperty(key)); //$NON-NLS-1$
		}
		Assert.assertEquals(9, params.size());
	}

}

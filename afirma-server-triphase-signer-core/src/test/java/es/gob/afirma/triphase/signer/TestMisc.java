package es.gob.afirma.triphase.signer;

import java.util.ArrayList;
import java.util.List;

import org.junit.Ignore;
import org.junit.Test;

/** Pruebas varias.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestMisc {

	private static final String ID_STR = "Id=\""; //$NON-NLS-1$

	/** Prueba de limpieza de delimitadores de nodos. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testNodeClean() {

		final String cleanValue = "<ds:KeyInfo xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Id=\"Signature-763862fd-6530-40d4-a5f8-d12726489625-KeyInfo\">"; //$NON-NLS-1$

		final String orDel = "<ds:KeyInfo Id=\"Signature-763862fd-6530-40d4-a5f8-d12726489625-KeyInfo\">"; //$NON-NLS-1$
		final String orXml = "so\"me<thi> n<gs ome th>ing" + cleanValue + "some<thi> n<gs \"ome th>ing" + "</ds:KeyInfo>" + "some<thi> n<gs o\"me th>ing"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		final String nodeStart = orDel.substring(
			0,
			orDel.indexOf(' ')
		);

		// Obtenemos todo los indices de ocurrencias del texto de inicio del nodo, por
		// si hay varias ocurrencias
		final List<Integer> indexes = new ArrayList<>();
		for (int index = orXml.indexOf(nodeStart); index >= 0; index = orXml.indexOf(nodeStart, index + 1)) {
		    indexes.add(Integer.valueOf(index));
		}

		String retDel = null;
		for (final Integer beginIndex : indexes) {
			retDel = orXml.substring(
				beginIndex.intValue(),
				orXml.indexOf('>', beginIndex.intValue()) + 1
			);

			// En este punto 'retDel' es un candidato, comprobamos que el Id sea el mismo
			if (retDel.contains(ID_STR) && retDel.contains(ID_STR)) {
				final String id = orXml.substring(
					orXml.indexOf(ID_STR, beginIndex.intValue()),
					orXml.indexOf('"',  orXml.indexOf(ID_STR, beginIndex.intValue()) + ID_STR.length())
				);
				if (retDel.contains(id)) {
					break;
				}
			}
			else {
				break;
			}
		}
		System.out.println(retDel);
	}

}

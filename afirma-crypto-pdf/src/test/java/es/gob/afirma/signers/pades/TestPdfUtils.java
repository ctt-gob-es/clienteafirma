package es.gob.afirma.signers.pades;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.signers.pades.PdfUtil;

public class TestPdfUtils {

	/**
	 * Comprueba el funcionamiento del algoritmo de extracci&oacute;n
	 * de los rangos de p&aacute;gina.
	 */
	@Test
	public void testPageRanges() {

		final int TOTAL_PAGES = 10;

		final List<Integer> pages = new ArrayList<>();
		PdfUtil.getPagesRange("7", TOTAL_PAGES, pages);
		checkExpected(pages, new int[] {7});

		pages.clear();
		PdfUtil.getPagesRange(" 3 ", TOTAL_PAGES, pages);
		checkExpected(pages, new int[] {3});

		pages.clear();
		PdfUtil.getPagesRange("5-8", TOTAL_PAGES, pages);
		checkExpected(pages, new int[] {5, 6, 7, 8});

		pages.clear();
		PdfUtil.getPagesRange("8--1", TOTAL_PAGES, pages);
		checkExpected(pages, new int[] {8, 9, 10});

		pages.clear();
		PdfUtil.getPagesRange("-3--1", TOTAL_PAGES, pages);
		checkExpected(pages, new int[] {8, 9, 10});

		pages.clear();
		PdfUtil.getPagesRange(" -3 - -1 ", TOTAL_PAGES, pages);
		checkExpected(pages, new int[] {8, 9, 10});

		pages.clear();
		PdfUtil.getPagesRange("0", TOTAL_PAGES, pages);
		checkExpected(pages, new int[] {1});

		pages.clear();
		PdfUtil.getPagesRange("20", TOTAL_PAGES, pages);
		checkExpected(pages, new int[] {TOTAL_PAGES});

		pages.clear();
		PdfUtil.getPagesRange("-20", TOTAL_PAGES, pages);
		checkExpected(pages, new int[] {1});

		pages.clear();
		try {
			PdfUtil.getPagesRange("5-3", TOTAL_PAGES, pages);
			Assert.fail("Se ha aceptado un rango no valido: 5-3");
		}
		catch (final Exception e) {
			// OK
		}

		pages.clear();
		try {
			PdfUtil.getPagesRange("-1--3", TOTAL_PAGES, pages);
			Assert.fail("Se ha aceptado un rango no valido: -1--3"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			// OK
		}

		pages.clear();
		try {
			PdfUtil.getPagesRange("1a-5", TOTAL_PAGES, pages);
			Assert.fail("Se ha aceptado un rango no valido: 1a-5"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			// OK
		}
	}

	private static void checkExpected(final List<Integer> pagesList, final int[] expected) {

		final Integer[] pages = pagesList.toArray(new Integer[0]);

		Assert.assertEquals("No se han cargado todas las paginas del rango", expected.length, pages.length); //$NON-NLS-1$

		Arrays.sort(pages);

		for (int i = 0; i < pages.length; i++) {
			Assert.assertEquals("Encontrada pagina fuera del rango esperado", expected[i], pages[i].intValue()); //$NON-NLS-1$
		}
	}
}

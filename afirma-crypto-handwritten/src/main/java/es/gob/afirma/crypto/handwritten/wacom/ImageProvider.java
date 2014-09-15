package es.gob.afirma.crypto.handwritten.wacom;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;

import com.WacomGSS.STU.Protocol.PenData;

interface ImageProvider {

	Image getImage();

	void drawInk(final Graphics2D gfx, final PenData pd0, final PenData pd1);

	PenData[] getPenData();

	void clickFromClientToCaptureDevice(final Point pt);

}

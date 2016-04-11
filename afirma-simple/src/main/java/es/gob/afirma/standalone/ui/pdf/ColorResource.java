package es.gob.afirma.standalone.ui.pdf;

import java.awt.Color;
import java.awt.Image;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.logging.Logger;

import javax.imageio.ImageIO;

import es.gob.afirma.core.misc.Base64;

enum ColorResource {

	BLACK(
		SignPdfUiMessages.getString("ColorResource.0"), //$NON-NLS-1$
		new Color(0, 0, 0),
		"black", //$NON-NLS-1$
		"iVBORw0KGgoAAAANSUhEUgAAAA8AAAAKAQMAAABR1Za6AAAAB3RJTUUH4AQCEDkN6QqNHgAAAAlwSFlzAAALEwAACxMBAJqcGAAAAARnQU1BAACxjwv8YQUAAAAGUExURQAAAP///6XZn90AAAALSURBVHjaY2DABwAAHgABZVmecQAAAABJRU5ErkJggg==" //$NON-NLS-1$
	),
	WHITE(
		SignPdfUiMessages.getString("ColorResource.1"), //$NON-NLS-1$
		new Color(255, 255, 255),
		"white", //$NON-NLS-1$
		"iVBORw0KGgoAAAANSUhEUgAAAA8AAAAKCAYAAABrGwT5AAAAE0lEQVR42mP4TwFgGNU8qpkQAAA5QlXHEslhzQAAAABJRU5ErkJggg==" //$NON-NLS-1$
	),
	LIGHTGRAY(
		SignPdfUiMessages.getString("ColorResource.2"), //$NON-NLS-1$
		new Color(128, 128, 128),
		"lightGray", //$NON-NLS-1$
		"iVBORw0KGgoAAAANSUhEUgAAAA8AAAAKCAYAAABrGwT5AAAAFklEQVR42mM4cODAf3Ixw6jmUc2EMAAJyeb6dfPeQwAAAABJRU5ErkJggg==" //$NON-NLS-1$
	),
	GRAY(
		SignPdfUiMessages.getString("ColorResource.3"), //$NON-NLS-1$
		new Color(192, 192, 192),
		"gray", //$NON-NLS-1$
		"iVBORw0KGgoAAAANSUhEUgAAAA8AAAAKCAYAAABrGwT5AAAAFklEQVR42mNoaGj4Ty5mGNU8qpkQBgDBIHZ6rI/MaQAAAABJRU5ErkJggg==" //$NON-NLS-1$
	),
	DARKGRAY(
		SignPdfUiMessages.getString("ColorResource.4"), //$NON-NLS-1$
		new Color(64, 64, 64),
		"darkGray", //$NON-NLS-1$
		"iVBORw0KGgoAAAANSUhEUgAAAA8AAAAKCAYAAABrGwT5AAAAFklEQVR42mNwcHD4Ty5mGNU8qpkQBgB4hgX6KvpHxgAAAABJRU5ErkJggg==" //$NON-NLS-1$
	),
	RED(
		SignPdfUiMessages.getString("ColorResource.5"), //$NON-NLS-1$
		new Color(255, 0, 0),
		"red", //$NON-NLS-1$
		"iVBORw0KGgoAAAANSUhEUgAAAA8AAAAKCAYAAABrGwT5AAAAFklEQVR42mP4z8Dwn1zMMKp5VDMhDAAd0irkzp6dHwAAAABJRU5ErkJggg==" //$NON-NLS-1$
	),
	PINK(
		SignPdfUiMessages.getString("ColorResource.6"), //$NON-NLS-1$
		new Color(255, 175, 175),
		"pink", //$NON-NLS-1$
		"iVBORw0KGgoAAAANSUhEUgAAAA8AAAAKCAYAAABrGwT5AAAAFklEQVR42mP4v379f3Ixw6jmUc2EMACA8vf4UWDE9wAAAABJRU5ErkJggg==" //$NON-NLS-1$
	);

	private final String colorName;
	private Color color = null;
	private final String pdfKey;
	private Image image;

	private ColorResource(final String name,
						  final Color col,
				          final String key,
				          final String base64PngBytes) {
		this.colorName = name;
		this.color = col;
		this.pdfKey = key;
		try {
			this.image = ImageIO.read(new ByteArrayInputStream(Base64.decode(base64PngBytes)));
		}
		catch (final IOException e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"Error cargando la imagen del color " + name + ": " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			this.image = null;
		}
	}

	@Override
	public String toString() {
		return this.colorName;
	}

	Image getImage() {
		return this.image;
	}

	Color getColor() {
		return this.color;
	}

	static ColorResource[] getAllColorResources() {
		return new ColorResource[] {
			BLACK,
			WHITE,
			LIGHTGRAY,
			GRAY,
			DARKGRAY,
			RED,
			PINK
		};
	}

	String getPdfColorKey() {
		return this.pdfKey;
	}
}

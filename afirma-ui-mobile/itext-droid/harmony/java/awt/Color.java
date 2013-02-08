/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
/**
 * @author Oleg V. Khaschansky
 */
package harmony.java.awt;

import harmony.java.awt.geom.AffineTransform;
import harmony.java.awt.geom.Rectangle2D;

import java.io.Serializable;
import java.util.Arrays;

import org.apache.harmony.awt.internal.nls.Messages;

public class Color implements Serializable {
	private static final long serialVersionUID = 118526816881161077L;

	/*
	 * The values of the following colors are based on 1.5 release behavior
	 * which can be revealed using the following or similar code: Color c =
	 * Color.white; System.out.println(c);
	 */

	public static final Color white = new Color(255, 255, 255);

	public static final Color WHITE = white;

	public static final Color lightGray = new Color(192, 192, 192);

	public static final Color LIGHT_GRAY = lightGray;

	public static final Color gray = new Color(128, 128, 128);

	public static final Color GRAY = gray;

	public static final Color darkGray = new Color(64, 64, 64);

	public static final Color DARK_GRAY = darkGray;

	public static final Color black = new Color(0, 0, 0);

	public static final Color BLACK = black;

	public static final Color red = new Color(255, 0, 0);

	public static final Color RED = red;

	public static final Color pink = new Color(255, 175, 175);

	public static final Color PINK = pink;

	public static final Color orange = new Color(255, 200, 0);

	public static final Color ORANGE = orange;

	public static final Color yellow = new Color(255, 255, 0);

	public static final Color YELLOW = yellow;

	public static final Color green = new Color(0, 255, 0);

	public static final Color GREEN = green;

	public static final Color magenta = new Color(255, 0, 255);

	public static final Color MAGENTA = magenta;

	public static final Color cyan = new Color(0, 255, 255);

	public static final Color CYAN = cyan;

	public static final Color blue = new Color(0, 0, 255);

	public static final Color BLUE = blue;

	/**
	 * integer RGB value
	 */
	int value;

	/**
	 * Float sRGB value.
	 */
	private float[] frgbvalue;

	/**
	 * Color in an arbitrary color space with <code>float</code> components. If
	 * null, other value should be used.
	 */
	private float fvalue[];

	/**
	 * Float alpha value. If frgbvalue is null, this is not valid data.
	 */
	private float falpha;

	/**
	 * The color's color space if applicable.
	 */
	// private ColorSpace cs;

	/*
	 * The value of the SCALE_FACTOR is based on 1.5 release behavior which can
	 * be revealed using the following code: Color c = new Color(100, 100, 100);
	 * Color bc = c.brighter(); System.out.println("Brighter factor: " +
	 * ((float)c.getRed())/((float)bc.getRed())); Color dc = c.darker();
	 * System.out.println("Darker factor: " +
	 * ((float)dc.getRed())/((float)c.getRed())); The result is the same for
	 * brighter and darker methods, so we need only one scale factor for both.
	 */
	private static final double SCALE_FACTOR = 0.7;

	private static final int MIN_SCALABLE = 3; // should increase when
												// multiplied by SCALE_FACTOR

	// transient private PaintContext currentPaintContext;
	//
	// public Color(ColorSpace cspace, float[] components, float alpha) {
	// int nComps = cspace.getNumComponents();
	// float comp;
	// fvalue = new float[nComps];
	//
	// for(int i=0 ; i<nComps; i++) {
	// comp = components[i];
	// if(comp < 0.0f || comp > 1.0f) {
	// // awt.107=Color parameter outside of expected range: component {0}.
	// throw new IllegalArgumentException(
	//                        Messages.getString("awt.107", i)); //$NON-NLS-1$
	// }
	// fvalue[i] = components[i];
	// }
	//
	// if (alpha < 0.0f || alpha > 1.0f) {
	// // awt.108=Alpha value outside of expected range.
	//            throw new IllegalArgumentException(Messages.getString("awt.108")); //$NON-NLS-1$
	// }
	// falpha = alpha;
	//
	// cs = cspace;
	//
	// frgbvalue = cs.toRGB(fvalue);
	//
	// value = ((int)(frgbvalue[2]*255)) |
	// (((int)(frgbvalue[1]*255)) << 8 ) |
	// (((int)(frgbvalue[0]*255)) << 16 ) |
	// (((int)(falpha*255)) << 24 );
	// }

	public Color(int rgba, boolean hasAlpha) {
		if (!hasAlpha) {
			value = rgba | 0xFF000000;
		} else {
			value = rgba;
		}
	}

	public Color(int r, int g, int b, int a) {
		if ((r & 0xFF) != r || (g & 0xFF) != g || (b & 0xFF) != b || (a & 0xFF) != a) {
			// awt.109=Color parameter outside of expected range.
			throw new IllegalArgumentException(Messages.getString("awt.109")); //$NON-NLS-1$
		}
		value = b | (g << 8) | (r << 16) | (a << 24);
	}

	public Color(int r, int g, int b) {
		if ((r & 0xFF) != r || (g & 0xFF) != g || (b & 0xFF) != b) {
			// awt.109=Color parameter outside of expected range.
			throw new IllegalArgumentException(Messages.getString("awt.109")); //$NON-NLS-1$
		}
		// 0xFF for alpha channel
		value = b | (g << 8) | (r << 16) | 0xFF000000;
	}

	public Color(int rgb) {
		value = rgb | 0xFF000000;
	}

	public Color(float r, float g, float b, float a) {
		this((int) (r * 255 + 0.5), (int) (g * 255 + 0.5), (int) (b * 255 + 0.5), (int) (a * 255 + 0.5));
		falpha = a;
		fvalue = new float[3];
		fvalue[0] = r;
		fvalue[1] = g;
		fvalue[2] = b;
		frgbvalue = fvalue;
	}

	public Color(float r, float g, float b) {
		this(r, g, b, 1.0f);
	}

	// public PaintContext createContext(
	// ColorModel cm,
	// Rectangle r,
	// Rectangle2D r2d,
	// AffineTransform xform,
	// RenderingHints rhs
	// ) {
	// if(currentPaintContext != null) {
	// return currentPaintContext;
	// }
	// currentPaintContext = new Color.ColorPaintContext(value);
	// return currentPaintContext;
	// }

	@Override
	public String toString() {
		/*
		 * The format of the string is based on 1.5 release behavior which can
		 * be revealed using the following code:
		 * 
		 * Color c = new Color(1, 2, 3); System.out.println(c);
		 */

		return getClass().getName() + "[r=" + getRed() + //$NON-NLS-1$
				",g=" + getGreen() + //$NON-NLS-1$
				",b=" + getBlue() + //$NON-NLS-1$
				"]"; //$NON-NLS-1$
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Color) {
			return ((Color) obj).value == this.value;
		}
		return false;
	}

	// public float[] getComponents(ColorSpace colorSpace, float[] components) {
	// int nComps = colorSpace.getNumComponents();
	// if(components == null) {
	// components = new float[nComps+1];
	// }
	//
	// getColorComponents(colorSpace, components);
	//
	// if(frgbvalue != null) {
	// components[nComps] = falpha;
	// } else {
	// components[nComps] = getAlpha()/255f;
	// }
	//
	// return components;
	// }
	//
	// public float[] getColorComponents(ColorSpace colorSpace, float[]
	// components) {
	// float[] cieXYZComponents =
	// getColorSpace().toCIEXYZ(getColorComponents(null));
	// float[] csComponents = colorSpace.fromCIEXYZ(cieXYZComponents);
	//
	// if(components == null) {
	// return csComponents;
	// }
	//
	// for(int i=0; i<csComponents.length; i++) {
	// components[i] = csComponents[i];
	// }
	//
	// return components;
	// }
	//
	// public ColorSpace getColorSpace() {
	// if (cs == null) {
	// cs = ColorSpace.getInstance(ColorSpace.CS_sRGB);
	// }
	//
	// return cs;
	// }

	public Color darker() {
		return new Color((int) (getRed() * SCALE_FACTOR), (int) (getGreen() * SCALE_FACTOR),
				(int) (getBlue() * SCALE_FACTOR));
	}

	public Color brighter() {

		int r = getRed();
		int b = getBlue();
		int g = getGreen();

		if (r == 0 && b == 0 && g == 0) {
			return new Color(MIN_SCALABLE, MIN_SCALABLE, MIN_SCALABLE);
		}

		if (r < MIN_SCALABLE && r != 0) {
			r = MIN_SCALABLE;
		} else {
			r = (int) (r / SCALE_FACTOR);
			r = (r > 255) ? 255 : r;
		}

		if (b < MIN_SCALABLE && b != 0) {
			b = MIN_SCALABLE;
		} else {
			b = (int) (b / SCALE_FACTOR);
			b = (b > 255) ? 255 : b;
		}

		if (g < MIN_SCALABLE && g != 0) {
			g = MIN_SCALABLE;
		} else {
			g = (int) (g / SCALE_FACTOR);
			g = (g > 255) ? 255 : g;
		}

		return new Color(r, g, b);
	}

	public float[] getRGBComponents(float[] components) {
		if (components == null) {
			components = new float[4];
		}

		if (frgbvalue != null) {
			components[3] = falpha;
		} else {
			components[3] = getAlpha() / 255f;
		}

		getRGBColorComponents(components);

		return components;
	}

	public float[] getRGBColorComponents(float[] components) {
		if (components == null) {
			components = new float[3];
		}

		if (frgbvalue != null) {
			components[2] = frgbvalue[2];
			components[1] = frgbvalue[1];
			components[0] = frgbvalue[0];
		} else {
			components[2] = getBlue() / 255f;
			components[1] = getGreen() / 255f;
			components[0] = getRed() / 255f;
		}

		return components;
	}

	public float[] getComponents(float[] components) {
		if (fvalue == null) {
			return getRGBComponents(components);
		}

		int nColorComps = fvalue.length;

		if (components == null) {
			components = new float[nColorComps + 1];
		}

		getColorComponents(components);

		components[nColorComps] = falpha;

		return components;
	}

	public float[] getColorComponents(float[] components) {
		if (fvalue == null) {
			return getRGBColorComponents(components);
		}

		if (components == null) {
			components = new float[fvalue.length];
		}

		for (int i = 0; i < fvalue.length; i++) {
			components[i] = fvalue[i];
		}

		return components;
	}

	@Override
	public int hashCode() {
		return value;
	}

	public int getTransparency() {
		switch (getAlpha()) {
		case 0xff:
			return Transparency.OPAQUE;
		case 0:
			return Transparency.BITMASK;
		default:
			return Transparency.TRANSLUCENT;
		}
	}

	public int getRed() {
		return (value >> 16) & 0xFF;
	}

	public int getRGB() {
		return value;
	}

	public int getGreen() {
		return (value >> 8) & 0xFF;
	}

	public int getBlue() {
		return value & 0xFF;
	}

	public int getAlpha() {
		return (value >> 24) & 0xFF;
	}

	public static Color getColor(String nm, Color def) {
		Integer integer = Integer.getInteger(nm);

		if (integer == null) {
			return def;
		}

		return new Color(integer.intValue());
	}

	public static Color getColor(String nm, int def) {
		Integer integer = Integer.getInteger(nm);

		if (integer == null) {
			return new Color(def);
		}

		return new Color(integer.intValue());
	}

	public static Color getColor(String nm) {
		Integer integer = Integer.getInteger(nm);

		if (integer == null) {
			return null;
		}

		return new Color(integer.intValue());
	}

	public static Color decode(String nm) throws NumberFormatException {
		Integer integer = Integer.decode(nm);
		return new Color(integer.intValue());
	}

	public static Color getHSBColor(float h, float s, float b) {
		return new Color(HSBtoRGB(h, s, b));
	}

	public static float[] RGBtoHSB(int r, int g, int b, float[] hsbvals) {
		if (hsbvals == null) {
			hsbvals = new float[3];
		}

		int V = Math.max(b, Math.max(r, g));
		int temp = Math.min(b, Math.min(r, g));

		float H, S, B;

		B = V / 255.f;

		if (V == temp) {
			H = S = 0;
		} else {
			S = (V - temp) / ((float) V);

			float Cr = (V - r) / (float) (V - temp);
			float Cg = (V - g) / (float) (V - temp);
			float Cb = (V - b) / (float) (V - temp);

			if (r == V) {
				H = Cb - Cg;
			} else if (g == V) {
				H = 2 + Cr - Cb;
			} else {
				H = 4 + Cg - Cr;
			}

			H /= 6.f;
			if (H < 0) {
				H++;
			}
		}

		hsbvals[0] = H;
		hsbvals[1] = S;
		hsbvals[2] = B;

		return hsbvals;
	}

	public static int HSBtoRGB(float hue, float saturation, float brightness) {
		float fr, fg, fb;

		if (saturation == 0) {
			fr = fg = fb = brightness;
		} else {
			float H = (hue - (float) Math.floor(hue)) * 6;
			int I = (int) Math.floor(H);
			float F = H - I;
			float M = brightness * (1 - saturation);
			float N = brightness * (1 - saturation * F);
			float K = brightness * (1 - saturation * (1 - F));

			switch (I) {
			case 0:
				fr = brightness;
				fg = K;
				fb = M;
				break;
			case 1:
				fr = N;
				fg = brightness;
				fb = M;
				break;
			case 2:
				fr = M;
				fg = brightness;
				fb = K;
				break;
			case 3:
				fr = M;
				fg = N;
				fb = brightness;
				break;
			case 4:
				fr = K;
				fg = M;
				fb = brightness;
				break;
			case 5:
				fr = brightness;
				fg = M;
				fb = N;
				break;
			default:
				fr = fb = fg = 0; // impossible, to supress compiler error
			}
		}

		int r = (int) (fr * 255. + 0.5);
		int g = (int) (fg * 255. + 0.5);
		int b = (int) (fb * 255. + 0.5);

		return (r << 16) | (g << 8) | b | 0xFF000000;
	}

	// class ColorPaintContext implements PaintContext {
	// int rgbValue;
	// WritableRaster savedRaster;
	// ColorModel cm;
	//
	// protected ColorPaintContext(int rgb) {
	// rgbValue = rgb;
	// if((rgb & 0xFF000000) == 0xFF000000){
	// cm = new DirectColorModel(24, 0xFF0000, 0xFF00, 0xFF);
	// } else {
	// cm = ColorModel.getRGBdefault();
	// }
	// }
	//
	// public void dispose() {
	// savedRaster = null;
	// }
	//
	// public ColorModel getColorModel() {
	// return cm;
	// }
	//
	// public Raster getRaster(int x, int y, int w, int h) {
	// if (savedRaster == null ||
	// w != savedRaster.getWidth() ||
	// h != savedRaster.getHeight()) {
	// savedRaster =
	// getColorModel().createCompatibleWritableRaster(w, h);
	//
	// // Suppose we have here simple INT/RGB color/sample model
	// DataBufferInt intBuffer =
	// (DataBufferInt) savedRaster.getDataBuffer();
	// int rgbValues[] = intBuffer.getData();
	// int rgbFillValue = rgbValue;
	// Arrays.fill(rgbValues, rgbFillValue);
	// }
	//
	// return savedRaster;
	// }
	// }
}

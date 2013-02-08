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
package harmony.java.awt.color;

import java.io.Serializable;

import org.apache.harmony.awt.gl.color.LUTColorConverter;
import org.apache.harmony.awt.internal.nls.Messages;

public abstract class ColorSpace implements Serializable {

	private static final long serialVersionUID = -409452704308689724L;

	public static final int TYPE_XYZ = 0;

	public static final int TYPE_Lab = 1;

	public static final int TYPE_Luv = 2;

	public static final int TYPE_YCbCr = 3;

	public static final int TYPE_Yxy = 4;

	public static final int TYPE_RGB = 5;

	public static final int TYPE_GRAY = 6;

	public static final int TYPE_HSV = 7;

	public static final int TYPE_HLS = 8;

	public static final int TYPE_CMYK = 9;

	public static final int TYPE_CMY = 11;

	public static final int TYPE_2CLR = 12;

	public static final int TYPE_3CLR = 13;

	public static final int TYPE_4CLR = 14;

	public static final int TYPE_5CLR = 15;

	public static final int TYPE_6CLR = 16;

	public static final int TYPE_7CLR = 17;

	public static final int TYPE_8CLR = 18;

	public static final int TYPE_9CLR = 19;

	public static final int TYPE_ACLR = 20;

	public static final int TYPE_BCLR = 21;

	public static final int TYPE_CCLR = 22;

	public static final int TYPE_DCLR = 23;

	public static final int TYPE_ECLR = 24;

	public static final int TYPE_FCLR = 25;

	public static final int CS_sRGB = 1000;

	public static final int CS_LINEAR_RGB = 1004;

	public static final int CS_CIEXYZ = 1001;

	public static final int CS_PYCC = 1002;

	public static final int CS_GRAY = 1003;

	private static ColorSpace cs_Gray = null;
	private static ColorSpace cs_PYCC = null;
	private static ColorSpace cs_CIEXYZ = null;
	private static ColorSpace cs_LRGB = null;
	private static ColorSpace cs_sRGB = null;

	private int type;
	private int numComponents;

	protected ColorSpace(int type, int numcomponents) {
		this.numComponents = numcomponents;
		this.type = type;
	}

	public String getName(int idx) {
		if (idx < 0 || idx > numComponents - 1) {
			// awt.16A=Invalid component index: {0}
			throw new IllegalArgumentException(Messages.getString("awt.16A", idx)); //$NON-NLS-1$
		}

		return "Unnamed color component #" + idx; //$NON-NLS-1$
	}

	public abstract float[] toRGB(float[] colorvalue);

	public abstract float[] toCIEXYZ(float[] colorvalue);

	public abstract float[] fromRGB(float[] rgbvalue);

	public abstract float[] fromCIEXYZ(float[] colorvalue);

	public float getMinValue(int component) {
		if (component < 0 || component > numComponents - 1) {
			// awt.16A=Invalid component index: {0}
			throw new IllegalArgumentException(Messages.getString("awt.16A", component)); //$NON-NLS-1$
		}
		return 0;
	}

	public float getMaxValue(int component) {
		if (component < 0 || component > numComponents - 1) {
			// awt.16A=Invalid component index: {0}
			throw new IllegalArgumentException(Messages.getString("awt.16A", component)); //$NON-NLS-1$
		}
		return 1;
	}

	public boolean isCS_sRGB() {
		// If our color space is sRGB, then cs_sRGB
		// is already initialized
		return (this == cs_sRGB);
	}

	public int getType() {
		return type;
	}

	public int getNumComponents() {
		return numComponents;
	}

	public static ColorSpace getInstance(int colorspace) {
		switch (colorspace) {
		case CS_sRGB:
			if (cs_sRGB == null) {
				cs_sRGB = new ICC_ColorSpace(new ICC_ProfileStub(CS_sRGB));
				LUTColorConverter.sRGB_CS = cs_sRGB;
				// ICC_Profile.getInstance (CS_sRGB));
			}
			return cs_sRGB;
		case CS_CIEXYZ:
			if (cs_CIEXYZ == null) {
				cs_CIEXYZ = new ICC_ColorSpace(new ICC_ProfileStub(CS_CIEXYZ));
				// ICC_Profile.getInstance (CS_CIEXYZ));
			}
			return cs_CIEXYZ;
		case CS_GRAY:
			if (cs_Gray == null) {
				cs_Gray = new ICC_ColorSpace(new ICC_ProfileStub(CS_GRAY));
				LUTColorConverter.LINEAR_GRAY_CS = cs_Gray;
				// ICC_Profile.getInstance (CS_GRAY));
			}
			return cs_Gray;
		case CS_PYCC:
			if (cs_PYCC == null) {
				cs_PYCC = new ICC_ColorSpace(new ICC_ProfileStub(CS_PYCC));
				// ICC_Profile.getInstance (CS_PYCC));
			}
			return cs_PYCC;
		case CS_LINEAR_RGB:
			if (cs_LRGB == null) {
				cs_LRGB = new ICC_ColorSpace(new ICC_ProfileStub(CS_LINEAR_RGB));
				LUTColorConverter.LINEAR_GRAY_CS = cs_Gray;
				// ICC_Profile.getInstance (CS_LINEAR_RGB));
			}
			return cs_LRGB;
		default:
		}

		// Unknown argument passed
		// awt.16B=Not a predefined colorspace
		throw new IllegalArgumentException(Messages.getString("awt.16B")); //$NON-NLS-1$
	}
}
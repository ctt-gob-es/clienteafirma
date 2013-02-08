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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.io.OutputStream;
import java.io.Serializable;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.StringTokenizer;

import org.apache.harmony.awt.gl.color.ICC_ProfileHelper;
import org.apache.harmony.awt.gl.color.NativeCMM;
import org.apache.harmony.awt.internal.nls.Messages;

public class ICC_Profile implements Serializable {

	private static final long serialVersionUID = -3938515861990936766L;

	// NOTE: Constant field values are noted in 1.5 specification.

	public static final int CLASS_INPUT = 0;

	public static final int CLASS_DISPLAY = 1;

	public static final int CLASS_OUTPUT = 2;

	public static final int CLASS_DEVICELINK = 3;

	public static final int CLASS_COLORSPACECONVERSION = 4;

	public static final int CLASS_ABSTRACT = 5;

	public static final int CLASS_NAMEDCOLOR = 6;

	public static final int icSigXYZData = 1482250784;

	public static final int icSigLabData = 1281450528;

	public static final int icSigLuvData = 1282766368;

	public static final int icSigYCbCrData = 1497588338;

	public static final int icSigYxyData = 1501067552;

	public static final int icSigRgbData = 1380401696;

	public static final int icSigGrayData = 1196573017;

	public static final int icSigHsvData = 1213421088;

	public static final int icSigHlsData = 1212961568;

	public static final int icSigCmykData = 1129142603;

	public static final int icSigCmyData = 1129142560;

	public static final int icSigSpace2CLR = 843271250;

	public static final int icSigSpace3CLR = 860048466;

	public static final int icSigSpace4CLR = 876825682;

	public static final int icSigSpace5CLR = 893602898;

	public static final int icSigSpace6CLR = 910380114;

	public static final int icSigSpace7CLR = 927157330;

	public static final int icSigSpace8CLR = 943934546;

	public static final int icSigSpace9CLR = 960711762;

	public static final int icSigSpaceACLR = 1094929490;

	public static final int icSigSpaceBCLR = 1111706706;

	public static final int icSigSpaceCCLR = 1128483922;

	public static final int icSigSpaceDCLR = 1145261138;

	public static final int icSigSpaceECLR = 1162038354;

	public static final int icSigSpaceFCLR = 1178815570;

	public static final int icSigInputClass = 1935896178;

	public static final int icSigDisplayClass = 1835955314;

	public static final int icSigOutputClass = 1886549106;

	public static final int icSigLinkClass = 1818848875;

	public static final int icSigAbstractClass = 1633842036;

	public static final int icSigColorantOrderTag = 1668051567;

	public static final int icSigColorantTableTag = 1668051572;

	public static final int icSigColorSpaceClass = 1936744803;

	public static final int icSigNamedColorClass = 1852662636;

	public static final int icPerceptual = 0;

	public static final int icRelativeColorimetric = 1;

	public static final int icSaturation = 2;

	public static final int icAbsoluteColorimetric = 3;

	public static final int icSigHead = 1751474532;

	public static final int icSigAToB0Tag = 1093812784;

	public static final int icSigAToB1Tag = 1093812785;

	public static final int icSigAToB2Tag = 1093812786;

	public static final int icSigBlueColorantTag = 1649957210;

	public static final int icSigBlueMatrixColumnTag = 1649957210;

	public static final int icSigBlueTRCTag = 1649693251;

	public static final int icSigBToA0Tag = 1110589744;

	public static final int icSigBToA1Tag = 1110589745;

	public static final int icSigBToA2Tag = 1110589746;

	public static final int icSigCalibrationDateTimeTag = 1667329140;

	public static final int icSigCharTargetTag = 1952543335;

	public static final int icSigCopyrightTag = 1668313716;

	public static final int icSigCrdInfoTag = 1668441193;

	public static final int icSigDeviceMfgDescTag = 1684893284;

	public static final int icSigDeviceModelDescTag = 1684890724;

	public static final int icSigDeviceSettingsTag = 1684371059;

	public static final int icSigGamutTag = 1734438260;

	public static final int icSigGrayTRCTag = 1800688195;

	public static final int icSigGreenColorantTag = 1733843290;

	public static final int icSigGreenMatrixColumnTag = 1733843290;

	public static final int icSigGreenTRCTag = 1733579331;

	public static final int icSigLuminanceTag = 1819635049;

	public static final int icSigMeasurementTag = 1835360627;

	public static final int icSigMediaBlackPointTag = 1651208308;

	public static final int icSigMediaWhitePointTag = 2004119668;

	public static final int icSigNamedColor2Tag = 1852009522;

	public static final int icSigOutputResponseTag = 1919251312;

	public static final int icSigPreview0Tag = 1886545200;

	public static final int icSigPreview1Tag = 1886545201;

	public static final int icSigPreview2Tag = 1886545202;

	public static final int icSigProfileDescriptionTag = 1684370275;

	public static final int icSigProfileSequenceDescTag = 1886610801;

	public static final int icSigPs2CRD0Tag = 1886610480;

	public static final int icSigPs2CRD1Tag = 1886610481;

	public static final int icSigPs2CRD2Tag = 1886610482;

	public static final int icSigPs2CRD3Tag = 1886610483;

	public static final int icSigPs2CSATag = 1886597747;

	public static final int icSigPs2RenderingIntentTag = 1886597737;

	public static final int icSigRedColorantTag = 1918392666;

	public static final int icSigRedMatrixColumnTag = 1918392666;

	public static final int icSigRedTRCTag = 1918128707;

	public static final int icSigScreeningDescTag = 1935897188;

	public static final int icSigScreeningTag = 1935897198;

	public static final int icSigTechnologyTag = 1952801640;

	public static final int icSigUcrBgTag = 1650877472;

	public static final int icSigViewingCondDescTag = 1987405156;

	public static final int icSigViewingConditionsTag = 1986618743;

	public static final int icSigChromaticAdaptationTag = 1667785060;

	public static final int icSigChromaticityTag = 1667789421;

	public static final int icHdrSize = 0;

	public static final int icHdrCmmId = 4;

	public static final int icHdrVersion = 8;

	public static final int icHdrDeviceClass = 12;

	public static final int icHdrColorSpace = 16;

	public static final int icHdrPcs = 20;

	public static final int icHdrDate = 24;

	public static final int icHdrMagic = 36;

	public static final int icHdrPlatform = 40;

	public static final int icHdrProfileID = 84;

	public static final int icHdrFlags = 44;

	public static final int icHdrManufacturer = 48;

	public static final int icHdrModel = 52;

	public static final int icHdrAttributes = 56;

	public static final int icHdrRenderingIntent = 64;

	public static final int icHdrIlluminant = 68;

	public static final int icHdrCreator = 80;

	public static final int icICCAbsoluteColorimetric = 3;

	public static final int icMediaRelativeColorimetric = 1;

	public static final int icTagType = 0;

	public static final int icTagReserved = 4;

	public static final int icCurveCount = 8;

	public static final int icCurveData = 12;

	public static final int icXYZNumberX = 8;

	/**
	 * Size of a profile header
	 */
	private static final int headerSize = 128;

	/**
	 * header magic number
	 */
	private static final int headerMagicNumber = 0x61637370;

	// Cache of predefined profiles
	private static ICC_Profile sRGBProfile;
	private static ICC_Profile xyzProfile;
	private static ICC_Profile grayProfile;
	private static ICC_Profile pyccProfile;
	private static ICC_Profile linearRGBProfile;

	/**
	 * Handle to the current profile
	 */
	private transient long profileHandle = 0;

	/**
	 * If handle is used by another class this object is not responsible for
	 * closing profile
	 */
	private transient boolean handleStolen = false;

	/**
	 * Cached header data
	 */
	private transient byte[] headerData = null;

	/**
	 * Serialization support
	 */
	private transient ICC_Profile openedProfileObject;

	private ICC_Profile(byte[] data) {
		profileHandle = NativeCMM.cmmOpenProfile(data);
		NativeCMM.addHandle(this, profileHandle);
	}

	/**
	 * Used to instantiate dummy ICC_ProfileStub objects
	 */
	ICC_Profile() {
	}

	/**
	 * Used to instantiate subclasses (ICC_ProfileGrey and ICC_ProfileRGB)
	 * 
	 * @param profileHandle
	 *            - should be valid handle to opened color profile
	 */
	ICC_Profile(long profileHandle) {
		this.profileHandle = profileHandle;
		// A new object reference, need to add it.
		NativeCMM.addHandle(this, profileHandle);
	}

	public void write(String fileName) throws IOException {
		FileOutputStream oStream = new FileOutputStream(fileName);
		oStream.write(getData());
		oStream.close();
	}

	/**
	 * Serializable implementation
	 * 
	 * @param s
	 * @throws IOException
	 */
	private void writeObject(ObjectOutputStream s) throws IOException {
		s.defaultWriteObject();
		s.writeObject(null);
		s.writeObject(getData());
	}

	/**
	 * Serializable implementation
	 * 
	 * @param s
	 * @throws IOException
	 * @throws ClassNotFoundException
	 */
	private void readObject(ObjectInputStream s) throws IOException, ClassNotFoundException {
		s.defaultReadObject();
		String colorSpaceStr = (String) s.readObject();
		byte[] data = (byte[]) s.readObject();

		if (colorSpaceStr != null) {
			if (colorSpaceStr.equals("CS_sRGB")) { //$NON-NLS-1$
				openedProfileObject = getInstance(ColorSpace.CS_sRGB);
			} else if (colorSpaceStr.equals("CS_GRAY")) { //$NON-NLS-1$
				openedProfileObject = getInstance(ColorSpace.CS_GRAY);
			} else if (colorSpaceStr.equals("CS_LINEAR_RGB")) { //$NON-NLS-1$
				openedProfileObject = getInstance(ColorSpace.CS_LINEAR_RGB);
			} else if (colorSpaceStr.equals("CS_CIEXYZ")) { //$NON-NLS-1$
				openedProfileObject = getInstance(ColorSpace.CS_CIEXYZ);
			} else if (colorSpaceStr.equals("CS_PYCC")) { //$NON-NLS-1$
				openedProfileObject = getInstance(ColorSpace.CS_PYCC);
			} else {
				openedProfileObject = ICC_Profile.getInstance(data);
			}
		} else {
			openedProfileObject = ICC_Profile.getInstance(data);
		}
	}

	protected Object readResolve() throws ObjectStreamException {
		return openedProfileObject;
	}

	public void write(OutputStream s) throws IOException {
		s.write(getData());
	}

	public void setData(int tagSignature, byte[] tagData) {
		NativeCMM.cmmSetProfileElement(profileHandle, tagSignature, tagData);
		// Remove cached header data if header is modified
		if (tagSignature == icSigHead) {
			headerData = null;
		}
	}

	public byte[] getData(int tagSignature) {
		int tagSize = 0;
		try {
			tagSize = NativeCMM.cmmGetProfileElementSize(profileHandle, tagSignature);
		} catch (CMMException e) {
			// We'll get this exception if there's no element with
			// the specified tag signature
			return null;
		}

		byte[] data = new byte[tagSize];
		NativeCMM.cmmGetProfileElement(profileHandle, tagSignature, data);
		return data;
	}

	public byte[] getData() {
		int profileSize = NativeCMM.cmmGetProfileSize(profileHandle);
		byte[] data = new byte[profileSize];
		NativeCMM.cmmGetProfile(profileHandle, data);
		return data;
	}

	@Override
	protected void finalize() {
		if (profileHandle != 0 && !handleStolen) {
			NativeCMM.cmmCloseProfile(profileHandle);
		}

		// Always remove because key no more exist
		// when object is destroyed
		NativeCMM.removeHandle(this);
	}

	public int getProfileClass() {
		int deviceClassSignature = getIntFromHeader(icHdrDeviceClass);

		switch (deviceClassSignature) {
		case icSigColorSpaceClass:
			return CLASS_COLORSPACECONVERSION;
		case icSigDisplayClass:
			return CLASS_DISPLAY;
		case icSigOutputClass:
			return CLASS_OUTPUT;
		case icSigInputClass:
			return CLASS_INPUT;
		case icSigLinkClass:
			return CLASS_DEVICELINK;
		case icSigAbstractClass:
			return CLASS_ABSTRACT;
		case icSigNamedColorClass:
			return CLASS_NAMEDCOLOR;
		default:
		}

		// Not an ICC profile class
		// awt.15F=Profile class does not comply with ICC specification
		throw new IllegalArgumentException(Messages.getString("awt.15F")); //$NON-NLS-1$

	}

	public int getPCSType() {
		return csFromSignature(getIntFromHeader(icHdrPcs));
	}

	public int getNumComponents() {
		switch (getIntFromHeader(icHdrColorSpace)) {
		// The most common cases go first to increase speed
		case icSigRgbData:
		case icSigXYZData:
		case icSigLabData:
			return 3;
		case icSigCmykData:
			return 4;
			// Then all other
		case icSigGrayData:
			return 1;
		case icSigSpace2CLR:
			return 2;
		case icSigYCbCrData:
		case icSigLuvData:
		case icSigYxyData:
		case icSigHlsData:
		case icSigHsvData:
		case icSigCmyData:
		case icSigSpace3CLR:
			return 3;
		case icSigSpace4CLR:
			return 4;
		case icSigSpace5CLR:
			return 5;
		case icSigSpace6CLR:
			return 6;
		case icSigSpace7CLR:
			return 7;
		case icSigSpace8CLR:
			return 8;
		case icSigSpace9CLR:
			return 9;
		case icSigSpaceACLR:
			return 10;
		case icSigSpaceBCLR:
			return 11;
		case icSigSpaceCCLR:
			return 12;
		case icSigSpaceDCLR:
			return 13;
		case icSigSpaceECLR:
			return 14;
		case icSigSpaceFCLR:
			return 15;
		default:
		}

		// awt.160=Color space doesn't comply with ICC specification
		throw new ProfileDataException(Messages.getString("awt.160") //$NON-NLS-1$
		);
	}

	public int getMinorVersion() {
		return getByteFromHeader(icHdrVersion + 1);
	}

	public int getMajorVersion() {
		return getByteFromHeader(icHdrVersion);
	}

	public int getColorSpaceType() {
		return csFromSignature(getIntFromHeader(icHdrColorSpace));
	}

	/**
	 * Tries to open file at the specified path. Path entries can be divided by
	 * separator char
	 * 
	 * @param path
	 * @param fileName
	 * @return
	 */
	private static FileInputStream tryPath(String path, String fileName) {
		if (path == null) {
			return null;
		}

		StringTokenizer st = new StringTokenizer(path, File.pathSeparator);

		while (st.hasMoreTokens()) {
			String pathEntry = st.nextToken();
			try {
				return new FileInputStream(pathEntry + File.separatorChar + fileName);
			} catch (FileNotFoundException e) {
			}
		}
		return null;
	}

	public static ICC_Profile getInstance(String fileName) throws IOException {
		final String fName = fileName; // to use in the privileged block

		FileInputStream fiStream = AccessController.doPrivileged(new PrivilegedAction<FileInputStream>() {
			public FileInputStream run() {
				// Open absolute path
				try {
					return new FileInputStream(fName);
				} catch (FileNotFoundException e) {
				}

				// Check java.iccprofile.path entries
				FileInputStream fiStream = tryPath(org.apache.harmony.awt.Utils
						.getSystemProperty("java.iccprofile.path"), fName); //$NON-NLS-1$
				if (fiStream != null) {
					return fiStream;
				}

				// Check java.class.path entries
				fiStream = tryPath(org.apache.harmony.awt.Utils.getSystemProperty("java.class.path"), fName); //$NON-NLS-1$
				if (fiStream != null) {
					return fiStream;
				}

				// Check directory with java sample profiles
				String home = org.apache.harmony.awt.Utils.getSystemProperty("java.home"); //$NON-NLS-1$
				if (home != null) {
					fiStream = tryPath(home + File.separatorChar + "lib" + File.separatorChar + "cmm", fName //$NON-NLS-1$ //$NON-NLS-2$
					);
				}

				return fiStream;
			}
		});

		if (fiStream == null) {
			// awt.161=Unable to open file {0}
			throw new IOException(Messages.getString("awt.161", fileName)); //$NON-NLS-1$
		}

		ICC_Profile pf = getInstance(fiStream);
		fiStream.close();
		return pf;
	}

	public static ICC_Profile getInstance(InputStream s) throws IOException {
		byte[] header = new byte[headerSize];
		// awt.162=Invalid ICC Profile Data
		String invalidDataMessage = Messages.getString("awt.162"); //$NON-NLS-1$

		// Get header from the input stream
		if (s.read(header) != headerSize) {
			throw new IllegalArgumentException(invalidDataMessage);
		}

		// Check the profile data for consistency
		if (ICC_ProfileHelper.getBigEndianFromByteArray(header, icHdrMagic) != headerMagicNumber) {
			throw new IllegalArgumentException(invalidDataMessage);
		}

		// Get profile size from header, create an array for profile data
		int profileSize = ICC_ProfileHelper.getBigEndianFromByteArray(header, icHdrSize);
		byte[] profileData = new byte[profileSize];

		// Copy header into it
		System.arraycopy(header, 0, profileData, 0, headerSize);

		// Read the profile itself
		if (s.read(profileData, headerSize, profileSize - headerSize) != profileSize - headerSize) {
			throw new IllegalArgumentException(invalidDataMessage);
		}

		return getInstance(profileData);
	}

	public static ICC_Profile getInstance(byte[] data) {
		ICC_Profile res = null;

		try {
			res = new ICC_Profile(data);
		} catch (CMMException e) {
			// awt.162=Invalid ICC Profile Data
			throw new IllegalArgumentException(Messages.getString("awt.162")); //$NON-NLS-1$
		}

		if (System.getProperty("os.name").toLowerCase().indexOf("windows") >= 0) { //$NON-NLS-1$ //$NON-NLS-2$
			try {
				if (res.getColorSpaceType() == ColorSpace.TYPE_RGB && res.getDataSize(icSigMediaWhitePointTag) > 0
						&& res.getDataSize(icSigRedColorantTag) > 0 && res.getDataSize(icSigGreenColorantTag) > 0
						&& res.getDataSize(icSigBlueColorantTag) > 0 && res.getDataSize(icSigRedTRCTag) > 0
						&& res.getDataSize(icSigGreenTRCTag) > 0 && res.getDataSize(icSigBlueTRCTag) > 0) {
					res = new ICC_ProfileRGB(res.getProfileHandle());
				} else if (res.getColorSpaceType() == ColorSpace.TYPE_GRAY
						&& res.getDataSize(icSigMediaWhitePointTag) > 0 && res.getDataSize(icSigGrayTRCTag) > 0) {
					res = new ICC_ProfileGray(res.getProfileHandle());
				}

			} catch (CMMException e) { /* return res in this case */
			}
		}

		return res;
	}

	public static ICC_Profile getInstance(int cspace) {
		try {
			switch (cspace) {

			case ColorSpace.CS_sRGB:
				if (sRGBProfile == null) {
					sRGBProfile = getInstance("sRGB.pf"); //$NON-NLS-1$
				}
				return sRGBProfile;

			case ColorSpace.CS_CIEXYZ:
				if (xyzProfile == null) {
					xyzProfile = getInstance("CIEXYZ.pf"); //$NON-NLS-1$
				}
				return xyzProfile;

			case ColorSpace.CS_GRAY:
				if (grayProfile == null) {
					grayProfile = getInstance("GRAY.pf"); //$NON-NLS-1$
				}
				return grayProfile;

			case ColorSpace.CS_PYCC:
				if (pyccProfile == null) {
					pyccProfile = getInstance("PYCC.pf"); //$NON-NLS-1$
				}
				return pyccProfile;

			case ColorSpace.CS_LINEAR_RGB:
				if (linearRGBProfile == null) {
					linearRGBProfile = getInstance("LINEAR_RGB.pf"); //$NON-NLS-1$
				}
				return linearRGBProfile;
			}

		} catch (IOException e) {
			// awt.163=Can't open color profile
			throw new IllegalArgumentException(Messages.getString("awt.163")); //$NON-NLS-1$
		}

		// awt.164=Not a predefined color space
		throw new IllegalArgumentException(Messages.getString("awt.164")); //$NON-NLS-1$
	}

	/**
	 * Reads integer from the profile header at the specified position
	 * 
	 * @param idx
	 *            - offset in bytes from the beginning of the header
	 * @return
	 */
	private int getIntFromHeader(int idx) {
		if (headerData == null) {
			headerData = getData(icSigHead);
		}

		return ((headerData[idx] & 0xFF) << 24) | ((headerData[idx + 1] & 0xFF) << 16)
				| ((headerData[idx + 2] & 0xFF) << 8) | ((headerData[idx + 3] & 0xFF));
	}

	/**
	 * Reads byte from the profile header at the specified position
	 * 
	 * @param idx
	 *            - offset in bytes from the beginning of the header
	 * @return
	 */
	private byte getByteFromHeader(int idx) {
		if (headerData == null) {
			headerData = getData(icSigHead);
		}

		return headerData[idx];
	}

	/**
	 * Converts ICC color space signature to the java predefined color space
	 * type
	 * 
	 * @param signature
	 * @return
	 */
	private int csFromSignature(int signature) {
		switch (signature) {
		case icSigRgbData:
			return ColorSpace.TYPE_RGB;
		case icSigXYZData:
			return ColorSpace.TYPE_XYZ;
		case icSigCmykData:
			return ColorSpace.TYPE_CMYK;
		case icSigLabData:
			return ColorSpace.TYPE_Lab;
		case icSigGrayData:
			return ColorSpace.TYPE_GRAY;
		case icSigHlsData:
			return ColorSpace.TYPE_HLS;
		case icSigLuvData:
			return ColorSpace.TYPE_Luv;
		case icSigYCbCrData:
			return ColorSpace.TYPE_YCbCr;
		case icSigYxyData:
			return ColorSpace.TYPE_Yxy;
		case icSigHsvData:
			return ColorSpace.TYPE_HSV;
		case icSigCmyData:
			return ColorSpace.TYPE_CMY;
		case icSigSpace2CLR:
			return ColorSpace.TYPE_2CLR;
		case icSigSpace3CLR:
			return ColorSpace.TYPE_3CLR;
		case icSigSpace4CLR:
			return ColorSpace.TYPE_4CLR;
		case icSigSpace5CLR:
			return ColorSpace.TYPE_5CLR;
		case icSigSpace6CLR:
			return ColorSpace.TYPE_6CLR;
		case icSigSpace7CLR:
			return ColorSpace.TYPE_7CLR;
		case icSigSpace8CLR:
			return ColorSpace.TYPE_8CLR;
		case icSigSpace9CLR:
			return ColorSpace.TYPE_9CLR;
		case icSigSpaceACLR:
			return ColorSpace.TYPE_ACLR;
		case icSigSpaceBCLR:
			return ColorSpace.TYPE_BCLR;
		case icSigSpaceCCLR:
			return ColorSpace.TYPE_CCLR;
		case icSigSpaceDCLR:
			return ColorSpace.TYPE_DCLR;
		case icSigSpaceECLR:
			return ColorSpace.TYPE_ECLR;
		case icSigSpaceFCLR:
			return ColorSpace.TYPE_FCLR;
		default:
		}

		// awt.165=Color space doesn't comply with ICC specification
		throw new IllegalArgumentException(Messages.getString("awt.165")); //$NON-NLS-1$
	}

	private long getProfileHandle() {
		handleStolen = true;
		return profileHandle;
	}

	private int getDataSize(int tagSignature) {
		return NativeCMM.cmmGetProfileElementSize(profileHandle, tagSignature);
	}

	/**
	 * Reads XYZ value from the tag data
	 * 
	 * @param tagSignature
	 * @return
	 */
	float[] getXYZValue(int tagSignature) {
		float[] res = new float[3];
		byte[] data = getData(tagSignature);

		// Convert from ICC s15Fixed16Number type
		// 1 (float) = 0x10000 (s15Fixed16Number),
		// hence dividing by 0x10000
		res[0] = ICC_ProfileHelper.getIntFromByteArray(data, 0) / 65536.f;
		res[1] = ICC_ProfileHelper.getIntFromByteArray(data, 4) / 65536.f;
		res[2] = ICC_ProfileHelper.getIntFromByteArray(data, 8) / 65536.f;

		return res;
	}

	float[] getMediaWhitePoint() {
		return getXYZValue(icSigMediaWhitePointTag);
	}

	/**
	 * If TRC is not a table returns gamma via return value and sets dataTRC to
	 * null. If TRC is a table returns 0 and fills dataTRC with values.
	 * 
	 * @param tagSignature
	 * @param dataTRC
	 * @return - gamma or zero if TRC is a table
	 */
	private float getGammaOrTRC(int tagSignature, short[] dataTRC) {
		byte[] data = getData(tagSignature);
		int trcSize = ICC_ProfileHelper.getIntFromByteArray(data, icCurveCount);

		dataTRC = null;

		if (trcSize == 0) {
			return 1.0f;
		}

		if (trcSize == 1) {
			// Cast from ICC u8Fixed8Number to float
			return ICC_ProfileHelper.getShortFromByteArray(data, icCurveData) / 256.f;
		}

		// TRC is a table
		dataTRC = new short[trcSize];
		for (int i = 0, pos = icCurveData; i < trcSize; i++, pos += 2) {
			dataTRC[i] = ICC_ProfileHelper.getShortFromByteArray(data, pos);
		}
		return 0;
	}

	@SuppressWarnings("null")
	float getGamma(int tagSignature) {
		short[] dataTRC = null;
		float gamma = getGammaOrTRC(tagSignature, dataTRC);

		if (dataTRC == null) {
			return gamma;
		}
		// awt.166=TRC is not a simple gamma value.
		throw new ProfileDataException(Messages.getString("awt.166")); //$NON-NLS-1$
	}

	@SuppressWarnings("null")
	short[] getTRC(int tagSignature) {
		short[] dataTRC = null;
		getGammaOrTRC(tagSignature, dataTRC);

		if (dataTRC == null) {
			// awt.167=TRC is a gamma value, not a table.
			throw new ProfileDataException(Messages.getString("awt.167")); //$NON-NLS-1$
		}
		return dataTRC;
	}
}

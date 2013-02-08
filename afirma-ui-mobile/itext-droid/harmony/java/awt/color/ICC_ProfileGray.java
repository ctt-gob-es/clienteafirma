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

public class ICC_ProfileGray extends ICC_Profile {
	private static final long serialVersionUID = -1124721290732002649L;

	ICC_ProfileGray(long profileHandle) {
		super(profileHandle);
	}

	public short[] getTRC() {
		return super.getTRC(icSigGrayTRCTag);
	}

	@Override
	public float[] getMediaWhitePoint() {
		return super.getMediaWhitePoint();
	}

	public float getGamma() {
		return super.getGamma(icSigGrayTRCTag);
	}
}

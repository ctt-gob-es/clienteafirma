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

package org.apache.harmony.awt;

import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;

public final class Utils {

	public static String getSystemProperty(final String name) {
		return getSystemProperty(name, null);
	}

	public static String getSystemProperty(final String name, final String value) {
		return AccessController.doPrivileged(new PrivilegedAction<String>() {
			public String run() {
				return System.getProperty(name, value);
			}
		});
	}

	public static void loadLibrary(final String name) throws UnsatisfiedLinkError, NullPointerException {
		try {
			AccessController.doPrivileged(new PrivilegedExceptionAction<Object>() {
				public Object run() {
					System.loadLibrary(name);
					return null;
				}
			});
		} catch (final PrivilegedActionException ex) {
			final Throwable cause = ex.getCause();

			if (cause instanceof UnsatisfiedLinkError) {
				throw (UnsatisfiedLinkError) cause;
			}

			if (cause instanceof NullPointerException) {
				throw (NullPointerException) cause;
			}
		}
	}
}

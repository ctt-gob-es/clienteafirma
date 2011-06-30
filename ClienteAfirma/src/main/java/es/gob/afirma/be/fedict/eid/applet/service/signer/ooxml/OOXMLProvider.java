/*
 * eID Applet Project.
 * Copyright (C) 2009 FedICT.
 * Copyright (C) 2009 Frank Cornelis.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License version
 * 3.0 as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, see 
 * http://www.gnu.org/licenses/.
 */

/*
 * Copyright (C) 2008-2009 FedICT.
 * Copyright (C) 2009 Frank Cornelis.
 * This file is part of the eID Applet Project.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package es.gob.afirma.be.fedict.eid.applet.service.signer.ooxml;

import java.security.Provider;
import java.security.Security;

/** Security Provider for Office OpenXML.
 * @author Frank Cornelis */
public class OOXMLProvider extends Provider {

    private static final long serialVersionUID = 1L;

    public static final String NAME = "OOXMLProvider";

    private OOXMLProvider() {
        super(NAME, 1.0, "OOXML Security Provider");
        put("TransformService." + RelationshipTransformService.TRANSFORM_URI, RelationshipTransformService.class.getName());
        put("TransformService." + RelationshipTransformService.TRANSFORM_URI + " MechanismType", "DOM");
    }

    /** Installs this security provider. */
    public static void install() {
        Provider provider = Security.getProvider(NAME);
        if (null == provider) {
            Security.addProvider(new OOXMLProvider());
        }
    }
}

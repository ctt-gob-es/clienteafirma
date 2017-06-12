/*
Copyright (c) 2000-2006 The Legion Of The Bouncy Castle
Copyright (c) 2006, CARDON DE LICHTBUER Rodolphe
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list
of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The name of the author or contributors may not be used to endorse or promote
products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.

 */

package es.gob.afirma.keystores.filters.rfc;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1Object;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.DERSequence;
import org.spongycastle.asn1.x509.qualified.QCStatement;

/** Extensi&oacute;n <code>QCStatements</code> de un certificado X.509 (RFC 3039).
   <p>Esta extensi&oacute;n puede ser tanto cr&iacute;tica como no cr&iacute;tica.</p>
   <p>Basado en el c&oacute;digo de <a href="http://rcardon.free.fr/websign/">WebSign Project</a>.</p>
   <pre>
      qcStatements  EXTENSION ::= {
          SYNTAX             QCStatements
          IDENTIFIED BY      id-pe-qcStatements }

      id-pe-qcStatements     OBJECT IDENTIFIER ::= { id-pe 3 }

      QCStatements ::= SEQUENCE OF QCStatement

      QCStatement ::= SEQUENCE {
          statementId   QC-STATEMENT.&amp;Id({SupportedStatements}),
          statementInfo QC-STATEMENT.&amp;Type
          ({SupportedStatements}{&#64;statementId}) OPTIONAL }

      SupportedStatements QC-STATEMENT ::= { qcStatement-1,...}
   </pre> */
final class QCStatements extends ASN1Object {

    private final List<QCStatement> qCStatements;

    private QCStatements(final ASN1Sequence seq) {
        this.qCStatements = new ArrayList<>();
        for(int i =0;i<seq.size();i++){
            this.qCStatements.add(QCStatement.getInstance(seq.getObjectAt(i)));
        }
    }

    static QCStatements getInstance(final Object obj){
        if(obj instanceof QCStatements){
            return (QCStatements)obj;
        }
        if(obj instanceof ASN1Sequence){
            return new QCStatements((ASN1Sequence)obj);
        }
        throw new IllegalArgumentException("Secuencia invalida"); //$NON-NLS-1$
    }

    List<QCStatement> getQCStatement() {
        return this.qCStatements;
    }

    @Override
	public ASN1Primitive toASN1Primitive(){
        final ASN1EncodableVector vec = new ASN1EncodableVector();
        final Iterator<QCStatement> it = this.qCStatements.iterator();
        while(it.hasNext()){
            vec.add(it.next().toASN1Primitive());
        }
        return new DERSequence(vec);
    }

}

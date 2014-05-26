using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using es.gob.afirma.core.misc;
using es.gob.afirma.core.csutils;

namespace es.gob.afirma.core.signers
{
    class AdESPolicy
    {
        private String policyIdentifier = null;
        private String policyIdentifierHash = null;
        private String policyIdentifierHashAlgorithm = null;
        private Uri policyQualifier = null;


        private void SetValues(String identifier,
                               String identifierHash,
                               String identifierHashAlgorithm,
                               String qualifier)
        {

            if (identifier == null || "".Equals(identifier))
            {
                throw new ArgumentNullException("El identificador de politica no puede ser nulo ni vacio");
            }

            this.policyIdentifier = identifier;

            if (identifierHash != null && (!"0".Equals(identifierHash)) && (identifierHashAlgorithm == null || "".Equals(identifierHashAlgorithm)))
            {
                throw new ArgumentNullException("Si se indica la huella digital del identificador de politica es obligatorio indicar tambien el algoritmo");
            }

            if (identifierHash == null)
            {
                try
                {
                    //this.policyIdentifierHash = System.Convert.ToBase64String(Digester.Digest(UrlReader.ReadUri(new Uri(identifier)), "SHA-512"));
                    //MODIFICACION DAL -> NO DEBERÍA SER "new Uri(qualifier)" EN VEZ DE "new Uri(identifier)" ????
                    if (qualifier != null)
                    {
                        this.policyIdentifierHash = System.Convert.ToBase64String(Digester.Digest(UrlReader.ReadUri(new Uri(qualifier)), "SHA-512"));
                        this.policyIdentifierHashAlgorithm = "SHA-512";
                    }
                }
                catch (Exception e)
                {
                    throw new ArgumentNullException("Si no se especifica la huella digital de la politica es necesario que el identificador sea una URL accesible universalmente: " + e, e);
                }
            }
            else
            {
                if ("0".Equals(identifierHash))
                {
                    this.policyIdentifierHash = null;
                }
                else
                {
                    if (!AOUtil.IsBase64(new System.Text.UTF8Encoding().GetBytes(identifierHash)))
                    {
                        throw new ArgumentNullException("La huella digital de la politica debe estar en formato Base64");
                    }
                    try
                    {
                        this.policyIdentifierHashAlgorithm = AOSignConstants.GetDigestAlgorithmName(identifierHashAlgorithm);
                    }
                    catch (Exception e)
                    {
                        throw new ArgumentNullException("El algoritmo de huella digital no esta soportado: " + identifierHashAlgorithm, e);
                    }
                    this.policyIdentifierHash = identifierHash;
                }
            }

            if (qualifier != null && (!"".Equals(qualifier)))
            {
                try
                {
                    this.policyQualifier = new Uri(qualifier);
                }
                catch (Exception e)
                {
                    throw new ArgumentNullException("El calificador de la politica debe ser una URL valida", e);
                }
            }
        }

        public AdESPolicy(Dictionary<String, String> extraParams)
        {
            if (extraParams == null)
            {
                throw new ArgumentNullException("Es necesario proporciona las propiedades de la politica");
            }
            String policyID = extraParams["policyIdentifier"];
            if (policyID != null)
            {
                SetValues(
                 policyID,
                 extraParams["policyIdentifierHash"],
                 extraParams["policyIdentifierHashAlgorithm"],
                 extraParams["policyQualifier"]
                );
            }
        }

        public AdESPolicy(String identifier,
                       String identifierHash,
                       String identifierHashAlgorithm,
                       String qualifier)
        {
            SetValues(identifier, identifierHash, identifierHashAlgorithm, qualifier);
        }

        public String GetPolicyIdentifier()
        {
            return this.policyIdentifier;
        }

        public String GetPolicyIdentifierHash()
        {
            return this.policyIdentifierHash;
        }

        public String GetPolicyIdentifierHashAlgorithm()
        {
            return this.policyIdentifierHashAlgorithm;
        }

        public Uri GetPolicyQualifier()
        {
            return this.policyQualifier;
        }

        public override bool Equals(Object o)
        {
            if (o == null)
            {
                return false;
            }

            AdESPolicy other = o as AdESPolicy;

            if ((Object)other == null)
            {
                return false;
            }

            return other.GetPolicyIdentifier().Equals(GetPolicyIdentifier()) &&
                   other.GetPolicyIdentifierHash().Equals(GetPolicyIdentifierHash()) &&
                   other.GetPolicyIdentifierHashAlgorithm().Equals(GetPolicyIdentifierHashAlgorithm());
        }

        public override int GetHashCode()
        {
            return GetPolicyIdentifier().GetHashCode();
        }
    }
}
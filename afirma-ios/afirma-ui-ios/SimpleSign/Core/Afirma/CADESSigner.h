//
//  CADESSigner.h
//  SignSample02
//
//

#ifndef SignSample02_CADESSigner_h
#define SignSample02_CADESSigner_h

void getSignedDataStructure(SignedData_t **sig_dat,
                            X509 *certificateX509,
                            char *contentData,
                            const char *certBuffer,
                            int certLenght,
                            char *dataSigned,
                            int lengthdataSigned,
                            char *messageDigest,
                            int  lengthMessageDigest,
                            char *contentDescription,
                            char *policyOID,
                            char *policyHash,
                            char *policyHashAlg,
                            char *policyUri,
                            char *certHash,
                            int lengthCertHash,
                            char *hashAlgorithm,
                            int signingCertificateV2,
                            char *signAlgorithm,
                            struct tm *local);

#endif

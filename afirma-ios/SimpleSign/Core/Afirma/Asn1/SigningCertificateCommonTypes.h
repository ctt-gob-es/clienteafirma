//
//  SigningCertificateCommonTypes.h
//  SignSample02
//
//  Created by Tomas Garcia-Meras on 24/8/14.
//  Copyright (c) 2014 Atos. All rights reserved.
//

#ifndef SignSample02_SigningCertificateCommonTypes_h
#define SignSample02_SigningCertificateCommonTypes_h


struct certs {
    A_SEQUENCE_OF(struct ESSCertIDv2) list;
    
    /* Context for parsing across buffer boundaries */
    asn_struct_ctx_t _asn_ctx;
};

struct policies {
    A_SEQUENCE_OF(struct PolicyInformation) list;
    
    /* Context for parsing across buffer boundaries */
    asn_struct_ctx_t _asn_ctx;
};

#endif

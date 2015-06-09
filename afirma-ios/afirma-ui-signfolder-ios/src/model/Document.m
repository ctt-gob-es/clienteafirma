//
//  RequestDoc.m
//  @FirmaWSProject
//
//  Created by Antonio Fiñana on 30/10/12.
//  Copyright (c) 2012 Atos. All rights reserved.
//

#import "Document.h"

static NSString *const kDocumentExtensionPades = @"pdf";
static NSString *const kDocumentExtensionCades = @"cades";
static NSString *const kDocumentExtensionXades = @"xades";
static NSString *const kDocumentSignatureFormatPades = @"PAdES";
static NSString *const kDocumentSignatureFormatXades = @"XAdES";
static NSString *const kDocumentMimeTypePDF = @"application/pdf";

@implementation Document

- (NSString *)getSignatureExtension
{
    NSString *extension = kDocumentExtensionCades;

    if ([[_sigfrmt lowercaseString] isEqualToString:[kDocumentSignatureFormatPades lowercaseString]]) {
        extension = kDocumentExtensionPades;
    } else if ([[_sigfrmt lowercaseString] isEqualToString:[kDocumentSignatureFormatXades lowercaseString]]) {
        extension = kDocumentExtensionXades;
    }

    return extension;
}

- (void)prepareForRequestWithCode:(PFRequestCode)code
{
    switch (code) {
        case PFRequestCodeDocumentPreviewReport:
            _mmtp = kDocumentMimeTypePDF;
            break;
        case PFRequestCodeDocumentPreviewSign:
            _meta = nil;
            break;
        default:
            break;
    }
}

@end

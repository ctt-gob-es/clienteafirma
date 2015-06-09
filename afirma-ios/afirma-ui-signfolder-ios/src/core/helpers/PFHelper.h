//
//  PFHelper.h
//  PortaFirmasUniv
//
//  Created by Rocio Tovar on 9/3/15.
//  Copyright (c) 2015 Atos. All rights reserved.
//

#import <Foundation/Foundation.h>

typedef NS_ENUM (NSInteger, PFRequestType)
{
    PFRequestTypeSign,
    PFRequestTypeApprove
};

typedef NS_ENUM (NSInteger, PFRequestStatus)
{
    PFRequestStatusSigned,
    PFRequestStatusRejected,
    PFRequestStatusPending
};

typedef NS_ENUM (NSInteger, PFRequestCode)
{
    PFRequestCodeList = 2,
    PFRequestCodeReject = 3,
    PFRequestCodeDocumentPreview = 5,
    PFRequestCodeAppList = 6,
    PFRequestCodeApprove = 7,
    PFRequestCodeDocumentPreviewSign = 8,
    PFRequestCodeDocumentPreviewReport = 9
};

typedef NS_ENUM (NSInteger, PFWaitingResponseType)
{
    PFWaitingResponseTypeList,
    PFWaitingResponseTypeRejection,
    PFWaitingResponseTypeApproval,
    PFWaitingResponseTypeSign,
    PFWaitingResponseTypeDetail
};

typedef NS_ENUM (NSInteger, PFSortPickerRow)
{
    PFSortPickerRowDate,
    PFSortPickerRowSubject,
    PFSortPickerRowApp
};

static const NSInteger kPFAlertViewCancelButtonIndex = 0;

static NSString *const kPFTrue = @"true";
static NSString *const kPFFalse = @"false";
static NSString *const kPFDefaultDateFormat = @"dd/MM/yyyy";

static NSString *const kPFFilterKeySortCriteria = @"orderAttribute";
static NSString *const kPFFilterValueSortCriteriaDate = @"fmodified";
static NSString *const kPFFilterValueSortCriteriaSubject = @"dsubject";
static NSString *const kPFFilterValueSortCriteriaApp = @"application";

static NSString *const kPFFilterKeySort = @"orderAscDesc";
static NSString *const kPFFilterValueSortAsc = @"asc";
static NSString *const kPFFilterValueSortDesc = @"desc";

static NSString *const kPFFilterKeySubject = @"searchFilter";
static NSString *const kPFFilterKeyApp = @"applicationFilter";
static NSString *const kPFFilterKeyDateStart = @"initDateFilter";
static NSString *const kPFFilterKeyDateEnd = @"endDateFilter";

static NSString *const kPFUserDefaultsKeyCurrentServer = @"currentServer";
static NSString *const kPFUserDefaultsKeyCurrentCertificate = @"currentCertificate";
static NSString *const kPFUserDefaultsKeyAlias = @"alias";
static NSString *const kPFUserDefaultsKeyURL = @"URL";
static NSString *const kPFUserDefaultsKeyServersArray = @"serversArray";

static NSString *const kPFCertInfoKeyIssuer = @"issuer";
static NSString *const kPFCertInfoKeySubject = @"subject";
static NSString *const kPFCertInfoKeyStartDate = @"startDate";
static NSString *const kPFCertInfoKeyEndDate = @"endDate";
static NSString *const kPFCertInfoKeyPurpose = @"purpose";

@interface PFHelper : NSObject

+ (PFRequestType)getPFRequestTypeFromString:(NSString *)string;
+ (PFRequestStatus)getPFRequestStatusFromClass:(Class)classObject;
+ (PFRequestCode)getPFRequestCodeForSection:(NSInteger)section;
+ (NSString *)getPFSortCriteriaValueForRow:(PFSortPickerRow)row;

@end

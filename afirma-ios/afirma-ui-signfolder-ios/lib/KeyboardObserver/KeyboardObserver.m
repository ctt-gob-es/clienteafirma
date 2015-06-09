//
//  KeyboardObserver.m
//  bluekiwi
//
//  Created by David Arrufat on 27/01/14.
//  Copyright (c) 2014 Tempos 21. All rights reserved.
//

#import "KeyboardObserver.h"

@interface KeyboardObserver ()

@property KeyboardStatus keyboardStatus;
@property (nonatomic,strong) NSMutableArray *observers;
@property CGRect keyboardFrame;

@end

@implementation KeyboardObserver

+ (KeyboardObserver *)getInstance {
    static KeyboardObserver *_getInstance = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        _getInstance = [[self alloc] init];
    });
    return _getInstance;
}

- (id) init
{
    self = [super init];
    if (self) {
        _keyboardStatus = KeyboardStatusHidden;
        self.observers = [@[]mutableCopy];
        
        NSNotificationCenter *nc = [NSNotificationCenter defaultCenter];
        [nc addObserver:self
               selector:@selector(keyboardWasShown:)
                   name:UIKeyboardDidShowNotification
                 object:nil];
        
        [nc addObserver:self
               selector:@selector(keyboardWillHide:)
                   name:UIKeyboardWillHideNotification
                 object:nil];
        
        [nc addObserver:self
               selector:@selector(keyboardDidHide:)
                   name:UIKeyboardDidHideNotification
                 object:nil];

    }
    return self;
}

- (void) dealloc
{
    self.observers = nil;
    NSNotificationCenter *nc = [NSNotificationCenter defaultCenter];
    [nc removeObserver:self name:UIKeyboardDidShowNotification object:nil];
    [nc removeObserver:self name:UIKeyboardWillHideNotification object:nil];
    [nc removeObserver:self name:UIKeyboardDidHideNotification object:nil];
}

- (void)keyboardWasShown:(NSNotification *)notification {
    _keyboardStatus = KeyboardStatusShowed;
    NSDictionary* info = [notification userInfo];
    _keyboardFrame = [[info objectForKey:UIKeyboardFrameEndUserInfoKey] CGRectValue];
    [self handleKeyboardChange];
}

- (void)keyboardWillHide:(NSNotification*)notification {
    _keyboardStatus = KeyboardStatusHiding;
    [self handleKeyboardChange];
}

- (void)keyboardDidHide:(NSNotification*)notification {
    _keyboardStatus = KeyboardStatusHidden;
    [self handleKeyboardChange];
}


- (BOOL) isShowed
{
    return _keyboardStatus == KeyboardStatusShowed;
}

- (BOOL) isHiding
{
    return _keyboardStatus == KeyboardStatusHiding;
}

- (BOOL) isHidden
{
    return _keyboardStatus == KeyboardStatusHidden;
}

- (CGRect)getKeyboardFrame
{
    return _keyboardFrame;
}

- (void) addObserver:(NSObject*)observer
{
    [_observers addObject:observer];
}

- (void) removeObserver:(NSObject *)observer
{
    [_observers removeObject:observer];
}

//WARNING: Implement this method in each observer!
- (void) handleKeyboardChange
{
    for (NSObject *observer in _observers) {
        if ([observer respondsToSelector:@selector(handleKeyboardChange)]) {
            [observer performSelector:@selector(handleKeyboardChange) withObject:nil afterDelay:0];
        }
    }
}

@end

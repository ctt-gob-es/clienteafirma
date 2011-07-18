
#import <JavaVM/jni.h>
#import <Cocoa/Cocoa.h>

JNIEXPORT void JNICALL Java_es_gob_afirma_standalone_ui_MacHelpHooker_showHelp(JNIEnv *env, jclass clazz) {
	[[NSApplication sharedApplication] performSelectorOnMainThread:@selector(showHelp:) withObject:NULL waitUntilDone:NO];
}

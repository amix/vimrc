/* -framework Foundation -Os -Wmost -dead_strip */
/* Returns list of superclasses ready to be used by grep. */
#import <Foundation/Foundation.h>
#import <objc/objc-runtime.h>

void usage()
{
	fprintf(stderr, "Usage: superclasses class_name framework\n");
}

void print_superclasses(const char classname[], const char framework[])
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

	/* Foundation is already included, so no need to load it again. */
	if (strncmp(framework, "Foundation", 256) != 0) {
		NSString *bundle =
			[@"/System/Library/Frameworks/" stringByAppendingString:
									[[NSString stringWithUTF8String:framework]
									 stringByAppendingPathExtension:@"framework"]];
		[[NSBundle bundleWithPath:bundle] load];
	}

	Class aClass = NSClassFromString([NSString stringWithUTF8String:classname]);
	char buf[BUFSIZ];

	strncpy(buf, classname, BUFSIZ);
	while ((aClass = class_getSuperclass(aClass)) != nil) {
		strncat(buf, "\\|", BUFSIZ);
		strncat(buf, [NSStringFromClass(aClass) UTF8String], BUFSIZ);
	}
	printf("%s\n", buf);

	[pool drain];
}

int main(int argc, char const* argv[])
{
	if (argc < 3 || argv[1][0] == '-')
		usage();
	else {
		int i;
		for (i = 1; i < argc - 1; i += 2)
			print_superclasses(argv[i], argv[i + 1]);
	}
	return 0;
}

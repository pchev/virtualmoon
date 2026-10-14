/*
 * vmamenu - launcher stub for the "VMA Menu" sidecar app.
 *
 * The combined "Virtual Moon Atlas.app" holds the whole suite; cclun is its
 * launcher / help hub. To make that hub independently Spotlight-launchable we
 * ship a tiny sidecar bundle ("VMA Menu", folder name cclun.app) that just
 * re-execs the real cclun inside the fat bundle.
 *
 * This was a #!/bin/sh script until 2026-09, but an .app whose
 * CFBundleExecutable is a script triggers "install Rosetta" on an Apple
 * Silicon Mac with Rosetta absent: LaunchServices does its bundle-level
 * architecture check before it reads the shebang, finds no Mach-O, and
 * reports kLSArchitectureNotSupportedErr (-10669). A real arm64 executable
 * avoids that entirely.
 *
 * Behaviour: from
 *   <dir>/cclun.app/Contents/MacOS/vmamenu
 * strip four path components to <dir>, then exec
 *   <dir>/Virtual Moon Atlas.app/Contents/MacOS/cclun
 * forwarding argv. Both .apps must sit in the same folder (the README says so).
 *
 * Build (done by tools/macos/package-app.sh):
 *   cc -arch arm64 -O2 -o vmamenu tools/macos/vmamenu.c
 */
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <mach-o/dyld.h>

/* Strip the last path component of s in place. No-op at the root or with no
   slash. Does not touch a trailing slash beyond the one it removes. */
static void strip_component(char *s)
{
    char *slash = strrchr(s, '/');
    if (slash == s)
        slash[1] = '\0';        /* keep the leading "/" */
    else if (slash)
        slash[0] = '\0';
}

int main(int argc, char **argv)
{
    (void)argc;             /* argv is NULL-terminated; argc unused */
    char exe[PATH_MAX];
    uint32_t size = sizeof(exe);
    if (_NSGetExecutablePath(exe, &size) != 0) {
        fprintf(stderr, "vmamenu: executable path too long\n");
        return 127;
    }

    /* exe = <dir>/cclun.app/Contents/MacOS/vmamenu
       four strips -> <dir> */
    strip_component(exe);   /* .../MacOS      */
    strip_component(exe);   /* .../Contents   */
    strip_component(exe);   /* .../cclun.app  */
    strip_component(exe);   /* <dir>          */

    char target[PATH_MAX];
    int n = snprintf(target, sizeof(target),
                     "%s/Virtual Moon Atlas.app/Contents/MacOS/cclun", exe);
    if (n < 0 || (size_t)n >= sizeof(target)) {
        fprintf(stderr, "vmamenu: target path too long\n");
        return 127;
    }

    argv[0] = target;
    execv(target, argv);

    /* only reached if execv failed */
    perror("vmamenu: cannot exec cclun");
    fprintf(stderr, "vmamenu: expected it at %s\n", target);
    return 127;
}

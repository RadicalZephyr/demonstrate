#include <sys/ioctl.h>

int setctty(void) {
    return ioctl(0, TIOCSCTTY, 1);
}

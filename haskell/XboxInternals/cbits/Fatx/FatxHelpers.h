/* Most parts of this class were originally developed by Lander Griffith (https://github.com/landr0id/).
   Much of his code is used throughout this class or very slightly modified */

#ifndef FATXHELPERS_H
#define FATXHELPERS_H

#define DOWN_TO_NEAREST_SECTOR(x) (0xFFFFFFFFFFFFFE00 & x)
#define UP_TO_NEAREST_SECTOR(x) ((x + 0x1FF) & 0xFFFFFFFFFFFFFE00)

#include "../winnames.h"
#include "XboxInternals_global.h"

class XBOXINTERNALSSHARED_EXPORT FATXHelpers
{
};

#endif // FATXHELPERS_H

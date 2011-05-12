#ifndef __RIDGECONV_H__
#define __RIDGECONV_H__

#include "ridgeio.h"

int conv_csv (RioData *data, const char *filename);
int conv_svg (RioData *data, const char *filename);
int conv_pdf (RioData *data, const char *filename);
int conv_tif (RioData *data, const char *filename);
int conv_png (RioData *data, const char *filename);

#endif /* !__RIDGECONV_H__ */
